signature TYPECHECK =
sig
  val tc : Absyn.prog -> unit
  (* if there are errors, these are reported through ErrorMsg.error *)

  val sub: Absyn.tp * Absyn.tp -> bool
  val join: (string->unit) -> Absyn.tp * Absyn.tp -> Absyn.tp
end

structure TypeCheck :> TYPECHECK =
struct

 structure A = Absyn
     
 fun list2string nil = ""
   | list2string [t] = t
   | list2string (h::t) = h ^ "," ^ list2string t

 fun tp2string A.Inttp = "int"
   | tp2string (A.Tupletp tps) = "<" ^ (list2string (map tp2string tps)) ^ ">"
   | tp2string (A.Arrowtp (tp1, tp2)) = tp2string tp1 ^ " -> " ^ tp2string tp2
   | tp2string (A.Reftp tp) = tp2string tp ^ " ref"

 type context = A.tp Symbol.table

 exception UNIMPLEMENTED
 exception Type

(* subtyping *)
 fun sub (t1,t2) = 
   case (t1, t2) of
     (A.Inttp, A.Inttp) => true
   | (A.Tupletp (tp1h :: tp1r), A.Tupletp (tp2h :: tp2r)) => 
       if sub (tp1h, tp2h) then sub (A.Tupletp tp1r, A.Tupletp tp2r)
       else false
   | (A.Tupletp [], A.Tupletp (tp2h :: tp2r)) => false
   | (A.Tupletp (tp1h :: tp1r), A.Tupletp []) => true
   | (A.Tupletp [], A.Tupletp []) => true
   | (A.Arrowtp (tp1, tp2), A.Arrowtp (tp1', tp2')) => sub (tp1', tp1) andalso sub (tp2, tp2')
   | (A.Reftp tp1, A.Reftp tp2) => sub (tp1, tp2) andalso sub (tp2, tp1)
   | (_, _) => false

 fun check_sub pos (tp1, tp2) = 
   if sub (tp1, tp2) then ()
   else ErrorMsg.error (pos, "result type doesn't match with the function type")

 (* subtype join *)  
 fun join complain (t1, t2) : A.tp =
   case (t1, t2) of
     (A.Inttp, A.Inttp) => A.Inttp
   | (A.Tupletp tp1, A.Tupletp tp2) =>
        let fun join_list complain (t1, t2) : A.tp list =
          (case (t1, t2) of
            (A.Tupletp (tp1h :: tp1r), A.Tupletp (tp2h :: tp2r)) => 
               (join complain (tp1h, tp2h)) :: (join_list complain (A.Tupletp tp1r, A.Tupletp tp2r))
          | (A.Tupletp [], A.Tupletp (tp2h :: tp2r)) => []
          | (A.Tupletp (tp1h :: tp1r), A.Tupletp []) => []
          | (A.Tupletp [], A.Tupletp []) => []
          | _ => (complain "this one should never be reached"; raise Type)) in
        A.Tupletp (join_list complain (A.Tupletp tp1, A.Tupletp tp2))
        end
   | (A.Arrowtp (tp1, tp1'), A.Arrowtp (tp2, tp2')) => 
        let fun meet complain (t1, t2) : A.tp = 
          (case (t1, t2) of
            (A.Inttp, A.Inttp) => A.Inttp
          | (A.Tupletp tp1, A.Tupletp tp2) => 
              let fun meet_list complain (t1, t2) : A.tp list =
                (case (t1, t2) of
                  (A.Tupletp (tp1h :: tp1r), A.Tupletp (tp2h :: tp2r)) => 
                     (meet complain (tp1h, tp2h)) :: (meet_list complain (A.Tupletp tp1r, A.Tupletp tp2r))
                | (A.Tupletp [], A.Tupletp (tp2h :: tp2r)) => 
                     tp2h :: (meet_list complain (A.Tupletp [], A.Tupletp tp2r))
                | (A.Tupletp (tp1h :: tp1r), A.Tupletp []) => 
                     tp1h :: (meet_list complain (A.Tupletp [], A.Tupletp tp1r))
                | (A.Tupletp [], A.Tupletp []) => []
                | _ => (complain "this one should never be reached"; raise Type)) in
              A.Tupletp (meet_list complain (A.Tupletp tp1, A.Tupletp tp2))
              end
          | (A.Arrowtp (tp1, tp1'), A.Arrowtp (tp2, tp2')) => 
               A.Arrowtp (join complain (tp1, tp2), meet complain (tp1', tp2')) (* join and meet are switched because we compute meet in here *)
          | (A.Reftp tp1, A.Reftp tp2) => A.Reftp (meet complain (tp1, tp2))
          | _ => (complain "t1 and t2 do not meet"; raise Type)) in
        A.Arrowtp (meet complain (tp1, tp2), join complain (tp1', tp2'))
        end
   | (A.Reftp tp1, A.Reftp tp2) => A.Reftp (join complain (tp1, tp2))
   | _ => (complain "t1 and t2 do not join"; raise Type)

(* expression typing *)
 fun tc_exp ctxt pos e : A.tp = 
   case e of
     A.Id id =>
       (case Symbol.look (ctxt, id) of
          SOME x => x (* G |-- id : G(id) *)
        | NONE => (ErrorMsg.error (pos, "Unknown ID: " ^ Symbol.name id); raise Type))
   | A.Int i => A.Inttp (* G |-- num : int *)
   | A.Op (oper, exps) => 
       (case (oper, map (fn x => tc_exp ctxt pos x) exps) of
          (* G |-- exp1 bin exp2 : tp3 *)
          (A.Add, [A.Inttp, A.Inttp]) => A.Inttp
        | (A.Sub, [A.Inttp, A.Inttp]) => A.Inttp
        | (A.Mul, [A.Inttp, A.Inttp]) => A.Inttp
        | (A.LT, [A.Inttp, A.Inttp]) => A.Inttp
        | (A.Eq, [A.Inttp, A.Inttp]) => A.Inttp
        | (A.Ref, [tp1]) => A.Reftp tp1 (* G |-- ref (exp : tp) : tp ref *)
        | (A.Get, [A.Reftp tp1]) => tp1 (* G |-- ! exp : tp *)
        | (A.Set, [A.Reftp tp1, tp2]) =>
             if sub (tp1, tp2) then A.Tupletp [] (* G |-- exp1 := exp2 : <> *)
             else (ErrorMsg.error (pos, "assign a wrong type to ref"); raise Type)
        | (_, _) => (ErrorMsg.error (pos, "Invalid op type"); raise Type))
   | A.If (e1, e2, e3) => 
       (case tc_exp ctxt pos e1 of
          A.Inttp => 
            (case (tc_exp ctxt pos e2, tc_exp ctxt pos e3) of
               (A.Tupletp [], _) => A.Tupletp [] (* G |-- if exp1 then exp2 : <> *)
             | (tp2, tp3) => join (fn x => ErrorMsg.error (pos, x)) (tp2, tp3))
        | _ => (ErrorMsg.error (pos, "Not int on if predicate"); raise Type))
   | A.Tuple es => A.Tupletp (map (fn x => tc_exp ctxt pos x) es) (* G |-- <exp0, ..., expn> : <tp0, ..., tpn> *)
   | A.Proj (i, e1) => 
       (case tc_exp ctxt pos e1 of
          A.Tupletp tpl => 
            if i < length tpl then List.nth (tpl, i) (* G |-- #i exp : tpi *)
            else (ErrorMsg.error (pos, "proj index is out of bound"); raise Type)
        | _ => (ErrorMsg.error (pos, "exp has to be a tuple"); raise Type))
   | A.While (e1, e2) => 
       (case (tc_exp ctxt pos e1, tc_exp ctxt pos e2) of
          (A.Inttp, A.Tupletp []) => A.Tupletp [] (* G |-- while exp1 do exp2 : <> *)
        | (_, _) => (ErrorMsg.error (pos, "Not int on while predicate"); raise Type))
   | A.Call (f, args) => 
       (case (tc_exp ctxt pos f, tc_exp ctxt pos args) of
          (A.Arrowtp (tp1, tp2), tp3) => 
             if sub (tp3, tp1) then tp2 
             else (ErrorMsg.error (pos, "argument type error"); raise Type)
        | _ => (ErrorMsg.error (pos, "exp is not a function type"); raise Type)) (* G |-- exp1 exp2 : tp2 *)
   | A.Let (id, e1, e2) => 
       let val tp1 = tc_exp ctxt pos e1
           val ctxt' = Symbol.enter (ctxt, id, tp1) in
       tc_exp ctxt' pos e2 (* G |-- exp1 ; exp2 : tp2 *)
       end
   | A.Constrain (e1, tp) => 
       if sub (tc_exp ctxt pos e1, tp) then tp
       else (ErrorMsg.error (pos, "type constraint doesn't match"); raise Type) (* G |-- exp : tp : tp *)
   | A.Pos (pos', e1) => tc_exp ctxt pos' e1

 fun tc_fundec ctxt ((pos, (f, x, tp1, tp2, exp)): A.fundec) =
 let val ctxt' = Symbol.enter(ctxt,x,tp1)
     val tp = (tc_exp ctxt' pos exp)
  in check_sub pos (tp, tp2)
 end 

 fun do_another_fun ((pos, fdec as (f_id, _, a_tp, f_tp, _)), ctxt) = 
   if Symbol.name (f_id) = "main" andalso (a_tp <> A.Inttp orelse f_tp <> A.Inttp)
   then (ErrorMsg.error (pos, "the type of main function isn't int -> int"); raise Type)
   else 
     (case Symbol.look (ctxt, f_id) of
        SOME x => (ErrorMsg.error (pos, "a function with the same name is already declared"); raise Type)
      | NONE => Symbol.enter (ctxt, f_id, A.Arrowtp (a_tp, f_tp)))

 fun build_global_context (fundecs) =
   let val ctxt = Symbol.enter (foldl do_another_fun Symbol.empty fundecs, Symbol.symbol "printint", A.Arrowtp (A.Inttp, A.Tupletp [])) in
   case Symbol.look (ctxt, Symbol.symbol "main") of
     SOME x => ctxt
   | NONE => (ErrorMsg.error ((0, 0), "main function is not declared"); raise Type)
   end

 fun tc (fundecs : A.prog)  = 
  let val ctxt = build_global_context(fundecs) 
   in app (tc_fundec ctxt) fundecs
  end 
							     
end
