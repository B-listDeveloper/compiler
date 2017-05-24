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

  type livetable = RS.set Symbol.table

  fun look live_at lab = 
    case Symbol.look (live_at, lab) of
      SOME s => s
    | NONE => (ErrorMsg.impossible ("There is no label " 
                ^ (Symbol.name lab) ^ "\n") ; RS.empty)

  fun regList2String (reg::regList) = (M.reg2name reg) ^ " " ^ (regList2String regList)
    | regList2String (nil) = ""

  fun printRegSet regs = print (regList2String (RS.listItems (regs)) ^ "\n")

  fun interfere_sets {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit} 
      def live_out =
    (RS.app (fn reg => RS.app (interfere reg) live_out) def;
    RS.app (fn reg => RS.app (interfere reg) def) live_out)

  fun compute_live_in {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
      live_at (M.Li(r, i) :: M.Syscall :: rest, live_at_end) =
    let val live_out = compute_live_in {mention=mention, interfere=interfere} 
                        live_at (rest, live_at_end)
    in
      if r = M.reg "$v0"
      then (case M.syscall_def_use (M.immed2int i) of
          SOME {def, use} => 
            let val live_out' = RS.union (use, RS.difference(live_out, def))
                val rsingle = M.list2set[r]
            in
              (mention r; RS.app mention def; RS.app mention use;
              interfere_sets {mention=mention, interfere=interfere} def live_out;
              interfere_sets {mention=mention, interfere=interfere} rsingle live_out';
              RS.difference (live_out', rsingle))
            end
        | NONE => ErrorMsg.impossible "Unknown Syscall")
      else (ErrorMsg.impossible "Syscall not preceded by li $v0"; RS.empty)
    end

    | compute_live_in {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
      live_at (i::rest, live_at_end) =
    let val live_out = compute_live_in {mention=mention, interfere=interfere} 
                        live_at (rest, live_at_end)
        val {def, use} = M.instr_def_use i 
    in (case i of
        M.J (lab) => look live_at lab
      | M.Jal (lab) => 
          (RS.app mention def; RS.app mention use;
          interfere_sets {mention=mention, interfere=interfere} def live_out;
          RS.union (use, RS.difference (live_out, def)))
      | M.Jr (r, _) => 
          (mention r; mention (M.reg "$v0"); List.app mention M.calleeSaved;
          RS.union (M.list2set (r :: (M.reg "$v0") :: M.calleeSaved), live_out))
      | M.Jalr(r1, r2, _, _) =>
          let val def = M.list2set (r1 :: (M.reg "$v0") :: M.callerSaved)         
          in 
            (RS.app mention def; mention r2; mention (M.reg "$a0");
            interfere_sets {mention=mention, interfere=interfere} def live_out;
            RS.union (M.list2set [r2, M.reg "$a0"], RS.difference (live_out, def)))
          end
      | M.Branchz (_, r, lab) => 
          (mention r; RS.add (RS.union (look live_at lab, live_out), r))
      | M.Branchu (_, r1, r2, lab) =>
          (mention r1; mention r2;
          RS.union (RS.union (look live_at lab, live_out), M.list2set[r1, r2]))
      | M.Branch (_, r1, r2, lab) =>
          (mention r1; mention r2;
          RS.union (RS.union (look live_at lab, live_out), M.list2set[r1, r2]))
      | M.Move (r1, r2) => (
          let val interfere_live_out = RS.difference (live_out, use)
          in
            (mention r1; mention r2;
            interfere_sets {mention=mention, interfere=interfere} 
              def interfere_live_out;
            RS.union (use, RS.difference (live_out, def)))
          end)
      | _ => 
          (RS.app mention def; RS.app mention use;
          interfere_sets {mention=mention, interfere=interfere} def live_out;
          RS.union (use, RS.difference (live_out, def)))
    )
    end

    | compute_live_in {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
      live_at (nil, live_at_end) = live_at_end

  fun update {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
      (block, (live_at, nextblock)) =
    let 
      val (next_lab, next_insts) = nextblock
      val (lab, insts) = block
      val new = compute_live_in {mention=mention, interfere=interfere} 
                  live_at (insts, look live_at next_lab)
      val live_at' = Symbol.enter (live_at, lab, new)
    in (live_at', block)
    end

  fun update_loop {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
      live_at (head, tail) =
    let 
      val (live_at', _) = List.foldl (update {mention=mention, interfere=interfere}) 
                            (live_at, head) tail
      fun not_changed (lab, insts) = RS.equal (look live_at lab, look live_at' lab)
      val changed = not (List.all not_changed tail)
    in 
      if changed 
      then update_loop {mention=mention, interfere=interfere} live_at' (head, tail) 
      else live_at
    end

  fun start_update_loop {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
      live_at_init ((lab, insts), tl) =
    let 
      val live_in = compute_live_in {mention=mention, interfere=interfere}
                      live_at_init (insts, RS.empty)
      val live_at = Symbol.enter (live_at_init, lab, live_in)
    in 
      update_loop {mention=mention, interfere=interfere}
        live_at ((lab, insts), tl)
    end 

  fun compute_live_at {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
      live_at_init blocks_rev = 
    case blocks_rev of
      nil => live_at_init
    | hd :: tl => start_update_loop {mention=mention, interfere=interfere} 
                    live_at_init (hd, tl)

  fun debug_live_at live_at (lab, insts) =
    print ((Symbol.name lab) ^ ": " 
            ^ regList2String (RS.listItems(look live_at lab)) ^ "\n")

  fun analyze {mention: M.reg -> unit, interfere: M.reg -> M.reg -> unit}
    (blocks: M.codeblock list) =
    let 
      fun init_live ((lab, insts), tbl) = Symbol.enter(tbl, lab, RS.empty)
      val live_at_init = List.foldl init_live Symbol.empty blocks
      val live_at = compute_live_at {mention=mention, interfere=interfere} 
                      live_at_init (List.rev blocks)
      val _ = List.app (debug_live_at live_at) blocks
    in () (* TO IMPLEMENT !!! *)
    end

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
