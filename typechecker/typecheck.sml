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
   else ErrorMsg.error (pos, "make a better error message than this!")

(* subtype join *)
 fun join complain (t1,t2) : A.tp = raise UNIMPLEMENTED

(* expression typing *)
 fun tc_exp ctxt pos e : A.tp =  raise UNIMPLEMENTED
   case e of
   (* 
    * -----------------
    * G |-- id : G (id) 
    *)
     A.ID id =>
       (case Symbol.look (ctxt, id) of
          SOME x => x
        | NONE => ErrorMsg.error (pos, "Unknown ID"))
   (* 
    * ---------------
    * G |-- num : int
    *)
   | A.Int i => A.Inttp
   | A.Op (op, exps) => 
       (case (op, map exp exps) of
          (A.Ref, []) => )

 fun tc_fundec ctxt ((pos, (f, x, tp1, tp2, exp)): A.fundec) =
 let val ctxt' = Symbol.enter(ctxt,x,tp1)
     val tp = tc_exp ctxt' pos exp
  in check_sub pos (tp, tp2)
 end 

 fun do_another_fun ((pos, fdec as (f_id, _, _, f_tp, _)), ctxt) = 
   Symbol.enter (ctxt, f_id, f_tp)

 fun build_global_context (fundecs) =
  foldl do_another_fun Symbol.empty fundecs

 fun tc (fundecs : A.prog)  = 
  let val ctxt = build_global_context(fundecs) 
   in app (tc_fundec ctxt) fundecs
  end 
							     
end
