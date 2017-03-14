structure TestProg = struct

    structure S = SLP
	val prog = 
	S.CompoundStm
           (S.AssignStm ("a", S.OpExp (S.NumExp 5, S.Plus, S.NumExp 3)),
	    S.CompoundStm
	         (S.AssignStm
		    ("b",
		     S.EseqExp
		       (S.PrintStm
			[S.IdExp "a",
			 S.OpExp (S.IdExp "a", S.Minus, S.NumExp 1)],
			S.OpExp (S.NumExp 10, S.Times, S.IdExp "a"))),
		    S.CompoundStm(S.PrintStm([]), S.PrintStm [S.IdExp "b"])))
end
