structure Interp = struct

    structure S = SLP
    (* Use S.CompoundStm, S.Plus, S.IdExp, etc. to refer to
     * items defined in structure SLP (see slp.sml) *)
    
    type table = (S.id * int) list

    fun update(tbl, ident, newval) = (ident, newval)::tbl
    (*assume the first occurrence of any id takes precedence
      over any later occurrences in the table*)

    exception UndefinedVariable of string

    fun lookup (tbl:table, ident) = 
          case tbl of 
            nil => raise UndefinedVariable(ident)
          | (x, xval)::xs => if ident=x then xval
			     else lookup(xs, ident)

    exception InterpUnimplemented

    fun interpOp S.Plus  = Int.+
      | interpOp S.Minus = Int.-
      | interpOp S.Times = Int.*
      | interpOp S.Div   = Int.div

    (* interpStm returns table *)
    fun interpStm (s:S.stm, tbl:table) = 
          case s of 
            S.CompoundStm (s1, s2) => interpStm(s2, interpStm(s1, tbl))
          | S.AssignStm (i, e) => 
              let val (v, t) = interpExp(e, tbl) in
              update(t, i, v)
            end
          | S.PrintStm elist =>
              case elist of
                [] => (print("\n"); tbl)
              | head::rest =>
                  let val (v, t) = interpExp(head, tbl) in
                  (print(Int.toString v ^ " "); interpStm(S.PrintStm rest, t))
                end

    (* interExp returns int * table *)
    and interpExp (e:S.exp, tbl:table) = 
          case e of
            S.IdExp i => (lookup(tbl, i), tbl)
          | S.NumExp n => (n, tbl)
          | S.OpExp (e1, bo, e2) =>
              let val (v1, t1) = interpExp(e1, tbl)
                  val (v2, t2) = interpExp(e2, t1) in
              ((interpOp bo) (v1, v2), t2)
            end
          | S.EseqExp (s, e) => 
              let val t = interpStm(s, tbl) in
              interpExp(e, t)
            end

    fun interp s = 
          (interpStm(s, nil); ())
end
