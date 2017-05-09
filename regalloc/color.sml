structure Color : COLOR = 
struct

 structure IG = Liveness.IG
 structure M = Mips
 structure RS = M.RegSet

 val (SOME spillCostInfinity) = Int.maxInt

 type coloring = {alloc: M.allocation, spills: RS.set}

 fun verify {complain: string -> unit,
             func: M.funcode, 
             spillCost: Mips.reg -> int,
             palette: RS.set,
	     coloring={alloc: M.allocation, spills: RS.set}} : unit =
   ErrorMsg.impossible "Color.verify unimplemented"

 fun color ({interference = ig: IG.graph,
             moves: IG.graph,
             spillCost: M.reg ->int,
             palette: RS.set}) : coloring =
   ErrorMsg.impossible "Color.color unimplemented"

end

