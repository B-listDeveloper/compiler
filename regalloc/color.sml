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
   ()
   (*ErrorMsg.impossible "Color.verify unimplemented"
   let fun interfere_verify r1 r2 =
         
       fun mention_verify r = 
         complain
         ()
       in
   Liveness.analyze {mention = mention_verify, interfere = interfere_verify} func
   end*)
 fun printnodes l =
   case l of 
     [] => (print "\n"; ())
   | h :: r => 
       (print ((M.reg2name h) ^ " ");
       printnodes r)

 fun printdegs l =
   case l of
     [] => (print "\n"; ())
   | (h, _) :: r =>
       (print ((M.reg2name h) ^ " ");
       printdegs r)

 fun remove_node ig from edges =
   case edges of
     [] => ()
   | to :: r => 
       (IG.rm_edge ig {from = from, to = to};
       IG.rm_edge ig {from = to, to = from};
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

 fun connected_nodes ig nodes conn = 
   case nodes of 
     [] => RS.listItems conn
   | h :: r => connected_nodes ig r (RS.union (RS.difference (IG.adj ig h, RS.singleton h), conn))

 fun low_and_high ig deg palette = 
   let val nodes = connected_nodes ig (RS.listItems (IG.nodes ig)) RS.empty
       fun f (ld, hd) l = 
         case l of 
           [] => (ld, hd)
         | h :: r => 
             if M.isvirtual h then (* nonprecolored register *)
               (if RS.numItems (IG.adj ig h) < deg then f ((h, IG.adj ig h) :: ld, hd) r
               else f (ld, (h, IG.adj ig h) :: hd) r)
             else f (ld, hd) r in (* precolored register *)
   f ([], []) nodes
   end

 fun paint ig (node, adjs) palette allocated = 
   let val cur_nodes = connected_nodes ig (RS.listItems (IG.nodes ig)) RS.empty
       fun restore l p = 
         case l of
           [] => p
         | h :: r =>
             let val removed = valOf (M.RegTb.look (allocated, h)) handle _ => h in
             IG.mk_edge ig {from = node, to = h}; IG.mk_edge ig {from = h, to = node};
             restore r (RS.delete (p, removed))
             end
       val palette' = restore cur_nodes palette in
   if RS.numItems palette' = 0 then ErrorMsg.impossible "Assumption violated - simplified node is not colorable"
   else M.RegTb.enter (allocated, node, List.hd (RS.listItems palette'))
   end

 fun color ({interference = ig: IG.graph,
             moves: IG.graph,
             spillCost: M.reg ->int,
             palette: RS.set}) : coloring =
   let val deg = RS.numItems palette 
       fun step g d (alloc, spills) = 
         let val (lowdegs, highdegs) = low_and_high g d palette in
         (*print "lowdegs: ";
         printdegs lowdegs;
         print "highdegs: ";
         printdegs highdegs;*)
         if List.length lowdegs = 0 then (* lowdegs is empty *)
           (if List.length highdegs = 0 then (alloc, spills) (* highdegs is also empty - return and coloring starts *)
           else step g d (alloc, spill highdegs g spills)) (* highdegs is not empty - apply Spill heuristic *)
         else (* lowdegs is not empty - apply Simplify heuristic *)
           let val (node, adjs) = simplify lowdegs g 
               val (alloc', spills') = step g d (alloc, spills) in 
           (paint g (node, adjs) palette alloc', spills')
           end 
         end 
       val (alloc, spills) = step ig deg (M.RegTb.empty, RS.empty) in
   {alloc = alloc, spills = spills}
   end

end

