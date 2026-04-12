(* Load bytecode from path in env SV0B; run main; print vm_exit:<code> for shell tests. *)

use "src/main.sml";

val path =
  case OS.Process.getEnv "SV0B" of
    SOME p => p
  | NONE =>
      ( TextIO.output (TextIO.stdErr, "sv0vm: set SV0B to a .sv0b file path\n")
      ; OS.Process.exit OS.Process.failure
      );

val () = print "SV0VM_RUN_BEGIN\n";
val exitCode = Interpreter.runFile path;
val () = print ("vm_exit:" ^ Int.toString exitCode ^ "\n");
OS.Process.exit (if exitCode = 0 then OS.Process.success else OS.Process.failure);
