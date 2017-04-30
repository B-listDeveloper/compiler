signature LIVENESS = sig
  structure IG: GRAPH where S=Mips.RegSet
  val analyze: {mention: Mips.reg -> unit, 
	        interfere: Mips.reg -> Mips.reg -> unit} ->
               Mips.funcode -> unit
  val interference_graph: Mips.funcode -> IG.graph
  val printgraph: (string->unit) -> IG.graph -> unit
end

structure Liveness : LIVENESS = struct
  structure IG = Graph(Mips.RegSet)
  structure M = Mips
  structure RS = Mips.RegSet

 fun create_live_at () : (string * RS.set) list ref = ref []

 val live_at = create_live_at ()

 fun init_live_at blocks =
    (*let fun f l acc =
          case l of
            [] => (live_at := acc; ())
          | (label, instrs) :: rest =>
              f rest (acc @ [("_" ^ M.lab2string label, RS.empty)]) in
    f blocks []
    end *)
    case blocks of
      [] => ()
    | (label, instrs) :: rest =>
        (live_at := !live_at @ [(M.lab2string label, RS.empty)];
        init_live_at rest) 

 fun live_at_lab lab =
    let fun f l = 
          (case l of
            [] => ErrorMsg.impossible "No such label found"
          | (x, live) :: rest => 
              if x = lab then live
              else f rest) in
    f (!live_at)
    end

 fun compute_live_in (instrs, live_at_end) = 
    case instrs of
      [] => live_at_end
    | M.Li(r, i) :: M.Syscall :: rest =>
        let val live_out = compute_live_in (rest, live_at_end) in
        if r = Mips.reg "$v0" then 
        (case M.syscall_def_use (M.immed2int i) of
          SOME {use,def} =>
            RS.union (use, RS.difference (live_out, def))
        | NONE => ErrorMsg.impossible "Unknown Syscall")
        else ErrorMsg.impossible "Syscall not preceded by li $v0" 
        end
    | instr :: rest => 
        let val live_out = compute_live_in (rest, live_at_end)
            val {use, def} = M.instr_def_use instr in
        case instr of
        (*  Branchz (_, _, lab) :: rest =>
            RS.union (use, RS.difference (RS.union (live_at lab, live_out), def))
        | Branchu (_, _, _, lab) :: rest =>
        | Branch (_, _, _, lab) :: rest =>
        | J (lab) :: rest => 
        |*) M.Jal (lab) =>
            RS.union (use, RS.difference (live_at_lab (M.lab2string lab), def))
        | M.Jr (r, also) => 
            RS.empty
        (*| Jalr (ra, r, use, def) :: rest =>*)
        | _ => RS.union (use, RS.difference (live_out, def))
        end

 fun nextlab lab =
    let fun f l =
          (case l of 
            [] => ErrorMsg.impossible "dddddd"
          | (label, instrs) :: rest =>
              if lab = label then label
              else f rest) in
    f (!live_at)
    end

 (* if new <> old then true else false *)
 fun comparelive new old changed =
    let val num = RS.numItems old
        val tmp = RS.union (old, new) in
    if RS.numItems tmp <> num then changed := true
    else ()
    end

 fun updatelive lab new = 
    let fun f l = 
          (case l of 
            [] => []
          | (entry as (label, live)) :: rest =>
              if lab = label then (lab, new) :: rest
              else entry :: (f rest)) in
    live_at := (f (!live_at)); ()
    end

 fun get_instrs lab blocks = 
    let fun f l =
          (case l of
            [] => ErrorMsg.impossible ("Instructions not found: " ^ lab)
          | (label, instrs) :: rest =>
              if lab = (M.lab2string label) then instrs
              else f rest) in
    print ("tries to find instruction on " ^ lab ^ "\n");
    f blocks
    end

 fun get_live (lab, live) = live

 fun outer_loop blocks =
    let val changed = ref false 
        fun inner_loop l changed =
          (case l of
            [] => ()
          | (lab, live) :: rest =>
              let val new = (compute_live_in (get_instrs lab blocks, get_live (List.hd rest))
                            handle Empty => compute_live_in (get_instrs lab blocks, RS.empty)) in
              comparelive new live changed;
              updatelive lab new;
              inner_loop rest changed
              end) in
    inner_loop (!live_at) changed;
    if !changed then outer_loop blocks
    else ()
    end

 fun printlist l = 
    case l of
      [] => ()
    | (lab, live) :: rest =>
        let val li = RS.listItems live 
            fun f ddd = 
              (case ddd of 
                [] => (print "\n"; ())
              | h :: left =>
                  (print ((M.reg2name h) ^ " ");
                  f left)) in
        print ("live_at (" ^ lab ^ "): ");
        f li;
        printlist rest
        end
 fun analyze {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
             (blocks: M.codeblock list) =
    (init_live_at blocks;
    outer_loop blocks;
    printlist (!live_at);
    ())
    

 fun printadj say g i = 
     (say (M.reg2name i); say ":";
      IG.S.app (fn j => (say " "; say (M.reg2name j))) (IG.adj g i);
      say "\n")

 fun printgraph say g = IG.S.app (printadj say g) (IG.nodes g);

 fun interference_graph(func: M.funcode) =
  let val _ = (print "################## LIVENESS: "; 
               print (Symbol.name(#1(List.nth(func,0)))); print "\n")
      val g = IG.newGraph()
      fun mention (r: M.reg) = (IG.succ g r; ())
      fun interfere r1 r2 = IG.mk_edge g {from=r1,to=r2}
   in analyze {mention=mention,interfere=interfere} func;
      print "################## INTERFERENCE GRAPH \n";
      printgraph print g;
      g
  end

end
