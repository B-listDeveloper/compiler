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
   (*let fun interfere_verify r1 r2 =
         
       fun mention_verify r = 
         complain
         ()
       in
   Liveness.analyze {mention = mention_verify, interfere = interfere_verify} func
   end*)

 fun remove_node ig from edges =
   case edges of
     [] => ()
   | to :: r => 
       (IG.rm_edge ig {from = from, to = to};
       remove_node ig from r)

 fun simplify l ig =
   case l of
     [] => ErrorMsg.impossible "Not reachable"
   | (node, adjs) :: r =>
       (remove_node ig node (RS.listItems adjs);
       (node, adjs))
 
 fun spill l ig spills=
   case l of 
     [] => ErrorMsg.impossible "Not reachable"
   | (node, adjs) :: r =>
       (remove_node ig node (RS.listItems adjs);
       RS.add (spills, node))

 fun low_and_high ig deg = 
   let val nodes = RS.listItems (IG.nodes ig)
       fun f (ld, hd) l = 
         case l of 
           [] => (ld, hd)
         | h :: r => 
             if RS.numItems (IG.adj ig h) < deg then f ((h, IG.adj ig h) :: ld, hd) r
             else f (ld, (h, IG.adj ig h) :: hd) r in
   f ([], []) nodes
   end

 fun paint ig (node, adjs) palette allocated spilled = 
   
   

 fun color ({interference = ig: IG.graph,
             moves: IG.graph,
             spillCost: M.reg ->int,
             palette: RS.set}) : coloring =
   let val deg = RS.numItems palette 
       fun step g d (alloc, spills) = 
         let val (lowdegs, highdegs) = low_and_high g d in
         if List.length lowdegs = 0 then (* lowdegs is empty *)
           (if List.length highdegs = 0 then (alloc, spills)(* highdegs is also empty - return and coloring starts *)
           else step g d (alloc, spill highdegs g spills)) (* highdegs is not empty - apply Spill heuristic *)
         else (* lowdegs is not empty - apply Simplify heuristic *)
           let val (node, adjs) = simplify lowdegs g 
               val (alloc', spills') = step g d (alloc, spills) in 
           (paint g (node, RS.listItems adjs) palette alloc' spills', spills')
           end 
         end 
       val (alloc, spills) = step ig deg (M.RegTb.empty, RS.empty) in
   {alloc = alloc, spills = spills}
   end

    

end

