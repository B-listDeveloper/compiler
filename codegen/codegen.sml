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
    (*fun strip(A.Pos(_,e))     = strip e
      | strip(A.Constrain(e,_)) = strip e
      |*) 
    fun strip(A.Op(oper,el))  = A.Op(oper, map strip el)
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
    let fun gen (A.Id id) =       
      (case Symbol.look (env, id) of
         SOME (Reg r) => r
       | SOME (Lab lab) => 
	         let val r = M.newReg()
	           in emit (M.La(r, lab));
	           r
	         end
       | NONE => E.impossible ("Can't find " ^ Symbol.name id))
    | gen (A.Int i) = 
        let val result = M.newReg () in
        emit (M.Li (result, M.immed i));
        result
        end
    | gen (A.Op (oper, exps)) = 
        (case (oper, map (fn x => gen_exp env x) exps) of
          (A.Add, [M.reg r1, M.reg r2]) => 
            let val result = M.newReg () in
            emit (M.Arith3 (fun2mips_arith_op A.Add, result, r1, r2));
            result
            end
        | (A.Sub, [M.reg r1, M.reg r2]) =>
            let val result = M.newReg () in
            emit (M.Arith3 (fun2mips_arith_op A.Sub, result, r1, r2));
            result
            end 
        | (A.Mul, [M.reg r1, M.reg r2]) => 
            let val result = M.newReg () in
            emit (M.Arith3 (fun2mips_arith_op A.Mul, result, r1, r2));
            result
            end
        | (A.LT, [M.reg r1, M.reg r2]) => 
            let val result = M.newReg () in
            emit (M.Arith3 (fun2mips_arith_op A.LT, result, r1, r2));
            result
            end
        | (A.Eq, [M.reg r1, M.reg r2]) => 
            let val result = M.newReg () in
            emit (M.Arith3 (fun2mips_arith_op A.Eq, result, r1, r2));
            result
            end
        | (A.Ref, [M.reg r1]) => 
            let val result = M.newReg () in
            emit_alloc_call (M.wordSize, result);
            emit (M.Sw (r1, (M.immed 0, result)));
            result
            end 
        | (A.Get, [M.reg r1]) => 
            let val result = M.newReg () in
            emit (M.Lw (result, (M.immed 0, r1)));
            result
            end
        | (A.Set, [M.reg r1, M.reg r2]) => 
            (emit (M.Sw (r2, (M.immed 0, r1)));
            r1))
    | gen (A.Tuple exps) = 
        let val result = M.newReg () in
          emit (result, M.reg "$gp");
          (case exps of
            [] => result
          | (e :: rest) =>
              let val addr = M.newReg () 
                  val r = gen_exp env e in
              emit_alloc_call (M.wordSize, addr);
              emit (M.Sw (r, (M.immed 0, addr)));
              gen_exp env rest
              end)
        end
(*
        (case exps of
          [] => 
            let val result = M.newReg () in
              emit (M.Move (result, M.reg "$gp"));
              result
            end
        | (e :: rest) =>
            let val result = M.newReg () 
                val r = gen_exp env e in
            emit_alloc_call (M.wordSize, result);
            emit (M.Sw (r, (M.immed 0, result)));
            gen_exp env rest
            end)
*)
    | gen (A.Proj (i, e)) = 
        let val addr = gen_exp env e 
            val result = M.newReg () 
            val wo = M.newReg () in
        emit (M.Li (M.reg "$t0", M.immed i));
        emit (M.Li (M.reg "$t1", M.wordSizeImmed));
        emit (M.Arith3 (M.Mulo, wo, "$t0", "$t1"));
        emit (M.Arith3 (M.Add, "$t0", addr, wo));
        emit (M.Lw (result, (M.immed 0, "$t0")));
        result
        end
    | gen (A.If (e1, e2, e3)) = 
        let val result = M.newReg () in
     
        end
    | gen (A.Call (f, args)) = 

    | gen (A.Let (id, e1, e2)) = 
        
    | gen (A.Constrain (e, tp)) = E.impossible "unimplemented translation"
    | gen (A.Pos (pos, e)) = E.impossible "unimplemented translation"
    | gen _ = E.impossible "unimplemented translation"
    in gen
    end

    fun save_callee () =
      let val regs = M.calleeSaved 
          fun f l r = 
            case r of 
              [] => l
            | x :: xs =>
                let val tmp = M.newReg () in
                emit (M.Move (tmp, M.reg ("$s" ^ Int.toString (length l))));
                f (l @ [tmp]) xs
                end in
          f [] regs 
      end

    fun restore ra a0 callee =
      let fun f l =
        case l of
          [] => ()
        | x :: xs => 
            (emit (M.Move (M.reg ("$s" ^ Int.toString (length xs)), x));
            f xs) in
      f callee
      end
    (* gen_func: generates code for one function
     *    inputs: fenv: functions environment
     *            func: the function to be generated
     *)
    fun gen_func (fenv, (f,x,t1,t2,exp)) = 
      (* IMPLEMENT ME! *)
      let val fenv' = Symbol.enter (fenv, x, Reg (M.reg "$a0"))
          val callee = save_callee ()
          val a0_tmp = M.newReg () 
          val ra_tmp = M.newReg () in
      emit_label (fun_label f);
      emit (M.Move (ra_tmp, M.reg "$ra"));
      emit (M.Move (a0_tmp, M.reg "$a0"));
      save_callee ();
      gen_exp fenv' (strip exp);
      restore callee;
      emit (M.Move (M.reg "$v0", M.reg "$t0"));
      emit (M.Move (M.reg "$ra", ra));
      emit_label (Symbol.symbol(Symbol.name (fun_label f) ^ ".epilog"));
      emit (M.Jr (M.reg "$ra", M.reg "$v0" :: M.calleeSaved));
      finish_fun ()
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
