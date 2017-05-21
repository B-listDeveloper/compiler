signature REGALLOC = sig
(* alloc(f) returns mips code for function f that has all the temporaries
    replaced by mips regs and the load/store of spilled temps taken care of
*)
 val alloc : Mips.funcode ->  Mips.funcode 
end

structure RegAlloc :> REGALLOC =
struct
   structure M = Mips
   structure RS = Mips.RegSet
   structure IG = Liveness.IG

   fun spillCost x = 1

   fun getmove g (M.Move(r1,r2)) = IG.mk_edge g {from=r2,to=r1} 
     | getmove g _ = ()

   fun find_offset spill x =
     case List.find (fn (_, reg) => M.comparereg (x, reg) = EQUAL) spill of
       SOME (index, _) => M.immed (M.wordSize * index)
     | NONE => ErrorMsg.impossible "NOT REACHABLE"

   fun assign_offset spill =
     let val spillL = RS.listItems spill 
         fun assign l i result =
           case l of
             [] => result
           | h :: r =>
               assign r (i + 1) (result @ [(i, h)]) in
     assign spillL 0 []
     end

   fun general_spilling1 (instrL as ((lab, block) :: rest) : M.funcode) spill =
     let val count = RS.numItems spill
         val stack_alloc = M.Arithi (M.Addi, M.reg "$sp", M.reg "$sp", M.immed (~M.wordSize * count))
         val stack_dealloc = M.Arithi (M.Addi, M.reg "$sp", M.reg "$sp", M.immed (M.wordSize * count))
         val spill_offset = assign_offset spill
         fun step instrs checked = 
           case instrs of
             [] => checked
           | instr :: rest =>
               (case instr of
               (* move instruction *)
                 M.Move (rd, rs) => 
                   if RS.member (spill, rd) andalso RS.member (spill, rs) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset rs, M.reg "$sp")),
                                    M.Sw (M.reg "$t8", (find_offset spill_offset rd, M.reg "$sp"))] in
                     step rest (checked @ new) end
                   else if RS.member (spill, rd) then
                     let val new = [M.Move (M.reg "$t8", rs),
                                    M.Sw (M.reg "$t8", (find_offset spill_offset rd, M.reg "$sp"))] in
                     step rest (checked @ new) end
                   else if RS.member (spill, rs) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset rs, M.reg "$sp")),
                                    M.Move (rd, M.reg "$t8")] in
                     step rest (checked @ new) end
                   else step rest (checked @ [instr])
               (* nonmove instructions *)    
               | M.Arith2 (aop2, rd, rs) => 
                   if RS.member (spill, rd) andalso RS.member (spill, rs) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset rs, M.reg "$sp")),
                                    M.Arith2 (aop2, M.reg "$t9", M.reg "$t8"),
                                    M.Sw (M.reg "$t9", (find_offset spill_offset rd, M.reg "$sp"))] in
                     step rest (checked @ new) end
                   else if RS.member (spill, rd) then
                     let val new = [M.Arith2 (aop2, M.reg "$t8", rs),
                                    M.Sw (M.reg "$t8", (find_offset spill_offset rd, M.reg "$sp"))] in
                     step rest (checked @ new) end
                   else if RS.member (spill, rs) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset rs, M.reg "$sp")),
                                    M.Arith2 (aop2, rd, M.reg "$t8")] in
                     step rest (checked @ new) end
                   else step rest (checked @ [instr])
               | M.Arith3 (aop3, rd, rs, rt) => step rest (checked @ [instr])
               | M.Arithi (aopi, rt, rs, immed) => 
                   if RS.member (spill, rt) andalso RS.member (spill, rs) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset rs, M.reg "$sp")),
                                    M.Arithi (aopi, M.reg "$t9", M.reg "$t8", immed),
                                    M.Sw (M.reg "$t9", (find_offset spill_offset rt, M.reg "$sp"))] in
                     step rest (checked @ new) end
                   else if RS.member (spill, rt) then
                     let val new = [M.Arithi (aopi, M.reg "$t8", rs, immed),
                                    M.Sw (M.reg "$t8", (find_offset spill_offset rt, M.reg "$sp"))] in
                     step rest (checked @ new) end
                   else if RS.member (spill, rs) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset rs, M.reg "$sp")),
                                    M.Arithi (aopi, rt, M.reg "$t8", immed)] in
                     step rest (checked @ new) end
                   else step rest (checked @ [instr])
               | M.Li (r, immed) => 
                   if RS.member (spill, r) then
                     let val new = [M.Li (M.reg "$t8", immed), 
                                    M.Sw (M.reg "$t8", (find_offset spill_offset r, M.reg "$sp"))] in
                     step rest (checked @ new) end
                   else step rest (checked @ [instr])
               | M.La (r, lab) => 
                   if RS.member (spill, r) then
                     let val new = [M.La (M.reg "$t8", lab), 
                                    M.Sw (M.reg "$t8", (find_offset spill_offset r, M.reg "$sp"))] in
                     step rest (checked @ new) end
                   else step rest (checked @ [instr])
               | M.Lw (r, (immed, ra)) => step rest (checked @ [instr])
               | M.Sw (r, (immed, ra)) => step rest (checked @ [instr])
               | M.Branchz (comp1, r, lab) => 
                   if RS.member (spill, r) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset r, M.reg "$sp")),
                                    M.Branchz (comp1, M.reg "$t8", lab)] in
                     step rest (checked @ new) end
                   else step rest (checked @ [instr])
               | M.Branchu (comp2, r1, r2, lab) => 
                   if RS.member (spill, r1) andalso RS.member (spill, r2) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset r1, M.reg "$sp")),
                                    M.Lw (M.reg "$t9", (find_offset spill_offset r2, M.reg "$sp")),
                                    M.Branchu (comp2, M.reg "$t8", M.reg "$t9", lab)] in
                     step rest (checked @ new) end
                   else if RS.member (spill, r1) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset r1, M.reg "$sp")),
                                    M.Branchu (comp2, M.reg "$t8", r2, lab)] in
                     step rest (checked @ new) end
                   else if RS.member (spill, r2) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset r2, M.reg "$sp")),
                                    M.Branchu (comp2, r1, M.reg "$t8", lab)] in
                     step rest (checked @ new) end
                   else step rest (checked @ [instr])
               | M.Branch (comp1, r1, r2, lab) => 
                   if RS.member (spill, r1) andalso RS.member (spill, r2) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset r1, M.reg "$sp")),
                                    M.Lw (M.reg "$t9", (find_offset spill_offset r2, M.reg "$sp")),
                                    M.Branch (comp1, M.reg "$t8", M.reg "$t9", lab)] in
                     step rest (checked @ new) end
                   else if RS.member (spill, r1) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset r1, M.reg "$sp")),
                                    M.Branch (comp1, M.reg "$t8", r2, lab)] in
                     step rest (checked @ new) end
                   else if RS.member (spill, r2) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset r2, M.reg "$sp")),
                                    M.Branch (comp1, r1, M.reg "$t8", lab)] in
                     step rest (checked @ new) end
                   else step rest (checked @ [instr])
               | M.Jr (r, also) => 
                   if RS.member (spill, r) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset r, M.reg "$sp")),
                                    M.Jr (M.reg "$t8", also)] in
                     step rest (checked @ new) end
                   else step rest (checked @ [instr])
               | M.Jalr (r1, r2, use, def) => 
                   if RS.member (spill, r1) andalso RS.member (spill, r2) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset r2, M.reg "$sp")),
                                    M.Jalr (M.reg "$t9", M.reg "$t8", use, def),
                                    M.Sw (M.reg "$t9", (find_offset spill_offset r1, M.reg "$sp"))] in
                     step rest (checked @ new) end
                   else if RS.member (spill, r1) then
                     let val new = [M.Jalr (M.reg "$t8", r2, use, def),
                                    M.Sw (M.reg "$t8", (find_offset spill_offset r1, M.reg "$sp"))] in
                     step rest (checked @ new) end
                   else if RS.member (spill, r2) then
                     let val new = [M.Lw (M.reg "$t8", (find_offset spill_offset r2, M.reg "$sp")),
                                    M.Jalr (r1, M.reg "$t8", use, def)] in
                     step rest (checked @ new) end
                   else step rest (checked @ [instr])
               | _ => step rest (checked @ [instr]))
         fun eachblock l codes = 
           case l of
             [] => codes
           | (lab, instrs) :: r =>
               if String.isSubstring ".epilog" (M.lab2string lab) 
               then eachblock r (codes @ [(lab, step (stack_dealloc :: instrs) [])])
               else eachblock r (codes @ [(lab, step instrs [])]) in
     eachblock ((lab, stack_alloc :: block) :: rest) []
     end

   fun alloc(instrL as ((funlab,block)::rest) : M.funcode) = 
   let val ig = Liveness.interference_graph instrL
       val movegraph = IG.newGraph()
       val _ = app (fn (_,l) => app (getmove movegraph) l) instrL
       val _ = print "###### Move graph\n"
       val _ = Liveness.printgraph print movegraph
       val takenaway = M.list2set [M.reg "$t8", M.reg "$t9"] (* take 2 registers *)
       val palette = RS.difference (M.list2set (M.reg"$ra"::M.callerSaved @ M.calleeSaved), takenaway)
       val coloring = Color.color {interference = ig, moves=movegraph, 
	                  spillCost = spillCost, palette=palette}
       val _ = Color.verify{complain=ErrorMsg.impossible, func=instrL, 
                            spillCost=spillCost, palette=palette, 
                            coloring=coloring}
       val _ = print "Register Allocation verified.\n"
       val {alloc,spills} = coloring
       val _ = (print "Spills: "; 
                RS.app (fn r => (print (M.reg2name r); print " ")) spills;
	        print "\n") 
       val instrL' = general_spilling1 instrL spills in
  List.map (fn (l,instrs) => (l,List.map (M.rename_regs alloc) instrs)) instrL'
  end

end