(* Load bytecode from path in env SV0B (and its coverage binding from
   SV0B_COVERAGE_BINDING, if set); run main; print vm_exit:<code> for shell tests. *)

use "src/main.sml";

val path =
  case OS.Process.getEnv "SV0B" of
    SOME p => p
  | NONE =>
      ( TextIO.output (TextIO.stdErr, "sv0vm: set SV0B to a .sv0b file path\n")
      ; OS.Process.exit OS.Process.failure
      );

val () = print "SV0VM_RUN_BEGIN\n";
(* sv0cov CV-119/CV-120: a coverage rejection happens at load, before any
   instruction runs; report it with its registry code and fail. *)
(* sv0cov CV-120: SV0B_COVERAGE_BINDING names the program's .sv0covbind.json
   companion explicitly (a file next to the bytecode confers nothing). Read
   inline: a new top-level val would be echoed into the program's output,
   which callers parse after SV0VM_RUN_BEGIN. *)
val exitCode = Interpreter.runFileBound (path, OS.Process.getEnv "SV0B_COVERAGE_BINDING")
  handle Coverage.Reject (code, detail) =>
    ( TextIO.output (TextIO.stdErr, "sv0vm: error[" ^ code ^ "]: " ^ detail ^ "\n")
    ; OS.Process.exit OS.Process.failure );
val () = print ("vm_exit:" ^ Int.toString exitCode ^ "\n");
OS.Process.exit (if exitCode = 0 then OS.Process.success else OS.Process.failure);
