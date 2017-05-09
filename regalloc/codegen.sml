signature CODEGEN = sig
  val codegen : Absyn.prog -> Mips.program
end

structure Codegen :> CODEGEN = 
  struct

    structure A = Absyn
    structure M = Mips
    structure E = ErrorMsg

  local
    (* Last label emitted. *)
    val last_lab = ref (NONE: M.lab option)
    (* List of instructions being generated in the current codeblock.
     * For efficiency, this list is kept in reverse order. *)
    val ilist = ref (nil:M.instruction list)
    (* List of codeblocks generated, in reverse order *)
    val blist = ref (nil:M.codeblock list)
    (* List of functions generated, in reverse order *)
    val flist = ref (nil:M.funcode list)
  in
    (* Here's the protocol for using these functions,
       described as a regular expression:

       init_lists ( (emit_label emit* )+ finish_fun )* finish_prog
    *)

    fun init_lists () = (ilist := nil; blist := nil; flist := nil; 
                         last_lab := NONE)

    fun finish_block () = 
           case (!last_lab, !ilist)
            of (NONE, nil) => ()
             | (NONE, _) => E.impossible "No start label"
             | (SOME lab, il) => 
                  (blist := (lab, rev il) :: (!blist);
                   ilist := nil;
                   last_lab := NONE)

    fun finish_fun () = (finish_block();
                         flist := (rev(!blist))::(!flist);
                         blist := nil)

    fun finish_prog() = 
	   case !last_lab
             of SOME _ => E.impossible "finish_prog without finish_fun"
              | NONE => rev(!flist) before flist := nil

    (* Append an instruction to the list of generated instructions. *)
    fun emit i = ilist := i::(!ilist)

    fun emit_label l = (finish_block(); last_lab := SOME l)
  end

    val newline_lab = M.thislab "NL"

    (* Memory management functions. *) 

    val heap_size = 32000 (* in bytes -- should be less than 64KB *)
    val init_lab = M.thislab("init")
    val alloc_lab = M.thislab("alloc")

    (* Emits a call to alloc, to allocate 'size' bytes, and put the 
     * returned address in 'ret_reg'. *) 
    fun emit_alloc_call (size:M.immed, ret_reg:M.reg) = 
      (emit (M.Li(M.reg("$a0"), size));
       emit (M.Jal(alloc_lab));
       emit (M.Move(ret_reg, M.reg("$v0"))))
          
    fun emit_alloc_func () = 
      (emit_label alloc_lab;
       emit (M.Lw(M.reg "$v0", (M.immed 0, M.reg "$gp")));
       emit (M.Arith3(M.Add, M.reg "$t0",M.reg "$v0", M.reg("$a0")));
       emit (M.Sw(M.reg "$t0", (M.immed 0, M.reg "$gp")));
       emit_label (M.thislab "alloc.epilog");
       emit (M.Jr(M.reg("$ra"), M.reg "$v0" :: M.calleeSaved));
       finish_fun())

    fun emit_init_func () = 
     let val ra_tmp = M.newReg()
      in emit_label (M.thislab "main");
         emit (M.Move(ra_tmp, M.reg "$ra"));
         emit (M.Li(M.reg("$a0"), M.immed(heap_size)));
         emit (M.Li(M.reg("$v0"), M.immed(9)));
         emit (M.Syscall);
         emit (M.Sw(M.reg "$v0", (M.immed 0, M.reg "$gp")));
         emit (M.Jal(M.thislab "_main"));
         emit (M.Move(M.reg "$ra", ra_tmp));
         emit_label (M.thislab "main.epilog");
         emit (M.Jr(M.reg("$ra"), M.reg "$v0" :: M.calleeSaved));
         finish_fun()
      end

    fun emit_printint_func() =
      (emit_label (M.thislab "_printint");
       emit (M.Li(M.reg("$v0"), M.immed(1)));
       emit (M.Syscall);
       (* Print a newline after the integer, for clarity. *)
       emit (M.La(M.reg("$a0"), newline_lab));
       emit (M.Li(M.reg("$v0"), M.immed(4)));
       emit (M.Syscall);
       emit_label (M.thislab "_printint.epilog");
       emit (M.Jr(M.reg("$ra"),M.reg "$v0" :: M.calleeSaved));
       finish_fun())

    datatype value = Reg of M.reg | Lab of M.lab

    (* A function environment maps: A.id -> M.lab * A.func *)

    fun fun_label id = M.thislab("_" ^ Symbol.name id)

    fun add_fun_to_env (id,env) = 
            Symbol.enter (env, id, Lab(fun_label id))

    (* A variable environment maps: A.id -> M.reg *)

    fun fun2mips_arith_op A.Add = M.Add
      | fun2mips_arith_op A.Sub = M.Sub
      | fun2mips_arith_op A.Mul = M.Mulo
      | fun2mips_arith_op A.LT  = M.Slt
      | fun2mips_arith_op A.Eq  = M.Seq
      | fun2mips_arith_op _      = E.impossible "Arith op expected"

    (* Remove Pos and Constrain, to simplify pattern matching. *)
    fun strip(A.Pos(_,e))     = strip e
      | strip(A.Constrain(e,_)) = strip e
      | strip(A.Op(oper,el))  = A.Op(oper, map strip el)
      | strip(A.Tuple(el))    = A.Tuple(map strip el)
      | strip(A.Proj(i,e))    = A.Proj(i,strip e)
      | strip(A.If(e1,e2,e3)) = A.If(strip e1, strip e2, strip e3)
      | strip(A.Call(e1,e2))  = A.Call(strip e1, strip e2)
      | strip(A.While(e1,e2)) = A.While(strip e1, strip e2)
      | strip(A.Let(i,e1,e2)) = A.Let(i,strip e1, strip e2)
      | strip(e)                = e

    (* gen_exp: generates code for one expression 
     *    inputs: env:  environment 
     *            exp:  the expression to emit code for
     *    output: M.reg -- if ret value is <>, we return r0
     *)
  fun gen_exp env : A.exp -> M.reg = 
  let
      fun gen (A.Id id) =       
              (case Symbol.look (env, id) of
                 SOME (Reg r) => r
               | SOME (Lab lab) => 
	               let val r = M.newReg()
	                in emit (M.La(r, lab));
	                   r
	               end
               | NONE => E.impossible ("Can't find " ^ Symbol.name id))
        | gen (A.Int num) = 
          let val r = M.newReg()
          in emit (M.Li(r, M.immed num)); r
          end 
        | gen (A.Op(A.Ref, exps)) = 
          let 
            val r' = gen_exp env (List.nth(exps, 0)) 
            val r = M.newReg()
          in 
            (emit_alloc_call(M.immed 4, r);
             emit (M.Sw(r', (M.immed 0, r)));
             r)
          end
        | gen (A.Op(A.Get, exps)) = 
          let
            val exp = List.nth(exps, 0)
            val r = M.newReg()
          in emit (M.Lw(r, (M.immed 0, gen_exp env exp))); r
          end
        | gen (A.Op(A.Set, exps)) = 
          let 
            val exp1 = List.nth(exps, 0)
            val exp2 = List.nth(exps, 1)
            val r = gen_exp env exp2
          in emit (M.Sw(r, (M.immed 0, gen_exp env exp1))); M.reg "$zero"
          end
        (* maximum munch *)
        | gen (A.Op(A.Add, [A.Int num1, A.Int num2])) = 
          let val r = M.newReg()
          in emit (M.Li(r, M.immed (num1+num2))); r
          end
        | gen (A.Op(A.Sub, [A.Int num1, A.Int num2])) = 
          let val r = M.newReg()
          in emit (M.Li(r, M.immed (num1-num2))); r
          end
        | gen (A.Op(A.Mul, [A.Int num1, A.Int num2])) = 
          let val r = M.newReg()
          in emit (M.Li(r, M.immed (num1*num2))); r
          end
        | gen (A.Op(A.Add, [exp, A.Int num])) =
          let val r = M.newReg()
          in emit (M.Arithi(M.Addi, r, gen_exp env exp, M.immed num)); r
          end
        | gen (A.Op(A.Add, [A.Int num, exp])) =
          let val r = M.newReg()
          in emit (M.Arithi(M.Addi, r, gen_exp env exp, M.immed num)); r
          end
        | gen (A.Op(oper, exps)) = 
          let 
            val r1 = gen_exp env (List.nth(exps, 0)) 
            val r2 = gen_exp env (List.nth(exps, 1))
            val r = M.newReg()
          in emit (M.Arith3(fun2mips_arith_op oper, r, r1, r2)); r
          end
        | gen (A.Tuple []) = M.reg "$zero"
        | gen (A.Tuple exps) = 
          let
            val r = M.newReg()
            fun sw_tuple i [] = ()
              | sw_tuple i (e::es) = (emit (M.Sw(gen_exp env e, (M.immed i, r))); sw_tuple (i+4) es)
          in 
            (emit_alloc_call(M.immed (4 * List.length(exps)), r);
            sw_tuple 0 exps;
            r)
          end
        | gen (A.Proj (i, exp)) =
          let val r = M.newReg()
          in emit (M.Lw(r, (M.immed (4*i), gen_exp env exp))); r
          end
        | gen (A.If(exp, exp1, exp2)) =
          let
            val falselab = M.freshlab()
            val exitlab = M.freshlab()
            val r = M.newReg()
          in
            (emit (M.Branchz(M.Eq, gen_exp env exp, falselab));
             emit (M.Move(r, gen_exp env exp1));
             emit (M.J exitlab);
             emit_label (falselab);
             emit (M.Move(r, gen_exp env exp2));
             emit_label (exitlab);
            r)
          end
        | gen (A.While(exp1, exp2)) =
          let 
            val entrylab = M.freshlab()
            val exitlab = M.freshlab()
          in
            (emit_label(entrylab);
             emit (M.Branchz(M.Eq, gen_exp env exp1, exitlab));
             gen_exp env exp2;
             emit (M.J entrylab);
             emit_label(exitlab);
            M.reg("$zero"))
          end
        | gen (A.Call(exp1, exp2)) = 
          let 
            val r' = gen_exp env exp1
            val r = M.newReg()
          in 
             emit (M.Move(M.reg("$a0"), gen_exp env exp2));
             emit (M.Jalr(M.reg("$ra"), r', [], []));
             emit (M.Move(r, M.reg("$v0")));
             r
          end
        | gen (A.Let(id, exp1, exp2)) = 
          let val r = gen_exp env exp1
          in gen_exp (Symbol.enter(env, id, Reg(r))) exp2
          end
        | gen (A.Constrain(exp, tp)) = gen_exp env exp 
        | gen _ = E.impossible "unimplemented translation"

     in gen
    end

    (* gen_func: generates code for one function
     *    inputs: fenv: functions environment
     *            func: the function to be generated
     *)
    fun gen_func (fenv, (f,x,t1,t2,exp)) =
    let 
      val calleeSaved = List.map (fn reg => (reg, M.newReg())) M.calleeSaved
      val raTemp = M.newReg()
      val a0Temp = M.newReg()
    in
          (emit_label (fun_label f);
           List.app (fn (reg, temp) => emit (M.Move(temp, reg))) calleeSaved;
           emit (M.Move(raTemp, M.reg("$ra")));
           emit (M.Move(a0Temp, M.reg("$a0")));
           emit (M.Move(M.reg("$v0"), gen_exp (Symbol.enter(fenv, x, Reg(a0Temp))) (strip exp)));
           List.app (fn (reg, temp) => emit (M.Move(reg, temp))) calleeSaved;
           emit (M.Move(M.reg("$ra"), raTemp));
           emit_label (Symbol.symbol(Symbol.name (fun_label f) ^ ".epilog"));
           emit (M.Jr(M.reg("$ra"),M.reg "$v0" :: M.calleeSaved));
           finish_fun ()
          )
    end
    (* codegen: generates code for a program 
     *    input:  A.prog
     *    output: M.program 
     *)
    fun codegen (fundec_list :A.prog) = 
      (* 1. Generate functions-env
       * 2. Emit runtime-system functions
       * 3. For each function, generate code for it
       *)
      let
        val fenv = foldl add_fun_to_env Symbol.empty 
                     (map #1 Absyn.externals @ map (#1 o #2) fundec_list)
      in
         init_lists(); 
         emit_init_func();
         emit_alloc_func();
         emit_printint_func();
         List.app (fn (_,fd) => gen_func (fenv, fd)) fundec_list;          
         ([(newline_lab,"\\n")], finish_prog())
      end
  end
