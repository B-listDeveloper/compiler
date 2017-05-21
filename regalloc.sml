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

   val must_not_spill = ref RS.empty

   fun spillCost x = 
     if RS.member (!must_not_spill, x) then valOf Int.maxInt
     else 1

   fun getmove g (M.Move(r1,r2)) = IG.mk_edge g {from=r2,to=r1} 
     | getmove g _ = ()

   fun contain_virtual_reg instrs checked =
     case instrs of
       [] => checked
     | instr :: rest => 
         (case instr of
           M.Arith2 (aop2, rd, rs) => 
             if M.isvirtual rd orelse M.isvirtual rs then
             let val ta = M.newReg ()
                 val tb = M.newReg () 
                 val split = [M.Move (ta, rs), 
                              M.Arith2 (aop2, tb, ta),
                              M.Move (rd, tb)] in
             must_not_spill := RS.addList (!must_not_spill, [ta, tb]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.Arith3 (aop3, rd, rs, rt) => 
             if M.isvirtual rd orelse M.isvirtual rs orelse M.isvirtual rt then
             let val ta = M.newReg ()
                 val tb = M.newReg ()
                 val tc = M.newReg () 
                 val split = [M.Move (ta, rs),
                              M.Move (tb, rt),
                              M.Arith3 (aop3, tc, ta, tb),
                              M.Move (rd, tc)] in
             must_not_spill := RS.addList (!must_not_spill, [ta, tb, tc]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.Arithi (aopi, rt, rs, i) => 
             if M.isvirtual rt orelse M.isvirtual rs then
             let val ta = M.newReg ()
                 val tb = M.newReg () 
                 val split = [M.Move (ta, rs),
                              M.Arithi (aopi, tb, ta, i),
                              M.Move (rt, tb)] in
             must_not_spill := RS.addList (!must_not_spill, [ta, tb]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         (*| M.Li (r, i) => 
             if M.isvirtual r then
             let val ta = M.newReg () 
                 val split = [M.Move (ta, r),
                              M.Li (ta, i),
                              M.Move (r, ta)] in (* uncertain *)
             must_not_spill := RS.addList (!must_not_spill, [ta]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.La (r, lab) => 
             if M.isvirtual r then
             let val ta = M.newReg () 
                 val split = [M.Move (ta, r),
                              M.La (ta, lab),
                              M.Move (r, ta)] in (* uncertain *)
             must_not_spill := RS.addList (!must_not_spill, [ta]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.Lw (r, (i, ra)) => 
             if M.isvirtual r orelse M.isvirtual ra then
             let val ta = M.newReg () 
                 val tb = M.newReg ()
                 val split = [M.Move (ta, ra),
                              M.Lw (tb, (i, ta)),
                              M.Move (r, (tb))] in (* uncertain *)
             must_not_spill := RS.addList (!must_not_spill, [ta, tb]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.Sw (r, (i, ra)) => 
             if M.isvirtual r orelse M.isvirtual ra then
             let val ta = M.newReg () 
                 val tb = M.newReg ()
                 val split = [M.Move (ta, ra),
                              M.Sw (tb, (i, ta)),
                              M.Move (r, tb)] in (* uncertain *)
             must_not_spill := RS.addList (!must_not_spill, [ta, tb]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.Move (rd, rs) => 
             if M.isvirtual rd orelse M.isvirtual rs then
             let val ta = M.newReg () 
                 val tb = M.newReg ()
                 val split = [M.Move (ta, rs),
                              M.Move (tb, ta),
                              M.Move (rd, tb)] in (* uncertain *)
             must_not_spill := RS.addList (!must_not_spill, [ta, tb]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.Branchz (comp1, r, lab) => 
             if M.isvirtual r then
             let val ta = M.newReg () 
                 val split = [M.Move (ta, r),
                              M.Branchz (comp1, ta, lab)] in (* uncertain *)
             must_not_spill := RS.addList (!must_not_spill, [ta]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.Branchu (comp2, r1, r2, lab) => 
             if M.isvirtual r1 orelse M.isvirtual r2 then
             let val ta = M.newReg () 
                 val tb = M.newReg ()
                 val split = [M.Move (ta, r1),
                              M.Move (tb, r2),
                              M.Branchu (comp2, ta, tb, lab)] in (* uncertain *)
             must_not_spill := RS.addList (!must_not_spill, [ta, tb]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.Branch (comp1, r1, r2, lab) => 
             if M.isvirtual r1 orelse M.isvirtual r2 then
             let val ta = M.newReg () 
                 val tb = M.newReg ()
                 val split = [M.Move (ta, r1),
                              M.Move (tb, r2),
                              M.Branch (comp1, ta, tb, lab)] in (* uncertain *)
             must_not_spill := RS.addList (!must_not_spill, [ta, tb]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.Jr (r, use) => 
             if M.isvirtual r then
             let val ta = M.newReg ()
                 val split = [M.Move (ta, r),
                              M.Jr (ta, use)] in (* uncertain *)
             must_not_spill := RS.addList (!must_not_spill, [ta]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr])
         | M.Jalr (r1, r2, use, def) => 
             if M.isvirtual r1 orelse M.isvirtual r2 then
             let val ta = M.newReg () 
                 val tb = M.newReg ()
                 val split = [M.Move (ta, r1),
                              M.Move (tb, r2),
                              M.Jalr (ta, tb, use, def)] in (* uncertain *)
             must_not_spill := RS.addList (!must_not_spill, [ta]);
             contain_virtual_reg rest (checked @ split)
             end
             else contain_virtual_reg rest (checked @ [instr]) *)
         | _ => contain_virtual_reg rest (checked @ [instr]))
  
   fun general_spilling code stack =
     case code of 
       [] => stack
     | (lab, instrs) :: rest => 
         general_spilling rest (stack @ [(lab, contain_virtual_reg instrs [])])

   fun alloc(instrL as ((funlab,block)::rest) : M.funcode) = 
   let val ig = Liveness.interference_graph instrL
       val movegraph = IG.newGraph()
       val _ = app (fn (_,l) => app (getmove movegraph) l) instrL
       val _ = print "###### Move graph\n"
       val _ = Liveness.printgraph print movegraph
       val palette = M.list2set (M.reg"$ra"::M.callerSaved @ M.calleeSaved)
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
       val tmp = List.map (fn (l,instrs) => (l,List.map (M.rename_regs alloc) instrs)) instrL 
       val tmp2 = general_spilling tmp [] 
       val ig' = Liveness.interference_graph tmp2 
       val movegraph' = IG.newGraph () 
       val _ = app (fn (_,l) => app (getmove movegraph') l) tmp2 
       val coloring' = Color.color {interference = ig', moves=movegraph', 
	                  spillCost = spillCost, palette=palette}
       val _ = Color.verify{complain=ErrorMsg.impossible, func=tmp2, 
                            spillCost=spillCost, palette=palette, 
                            coloring=coloring'}
       val _ = print "Register Allocation verified.\n"
       val {alloc,spills} = coloring' in
  List.map (fn (l,instrs) => (l,List.map (M.rename_regs alloc) instrs)) tmp2
  end

end