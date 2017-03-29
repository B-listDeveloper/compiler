structure Compile : sig val compile: string -> unit end =
 
struct 

  exception Stop
fun still_ok () = 
  if (!ErrorMsg.anyErrors) then raise Stop else ()

fun compile filename = 
    let 
        val absyn = Parse.parse (filename, TextIO.openIn filename)
        val () = still_ok()
        val () = TypeCheck.tc absyn
        val () = still_ok()
    in print "Program successfully typechecked\n"
    end
 handle ErrorMsg.Error => print "\nCompiler bug.\n\n" 
      | Stop => print "\nCompilation Failed.\n\n" 
end
