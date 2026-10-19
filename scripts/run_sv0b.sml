(* Load bytecode from path in env SV0B (and its coverage binding from
   SV0B_COVERAGE_BINDING, if set); run main; print vm_exit:<code> for shell tests. *)

(* A terminating signal must end the run at once with a failed status and
   publish nothing. Installed before the VM sources load (which can take
   seconds on a loaded machine) so an early signal is not ignored: `sv0
   vm-run` execs this runtime (no wrapper process), and the SML/NJ runtime
   does not die on SIGTERM by default. Posix.Process.exit skips at-exit
   hooks, so no coverage profile is written. *)
val () =
  List.app
    (fn (sg, code) =>
       ignore (Signals.setHandler (sg, Signals.HANDLER (fn _ => Posix.Process.exit code))))
    [(UnixSignals.sigTERM, 0w143), (UnixSignals.sigINT, 0w130)];

use "src/main.sml";

val path =
  case OS.Process.getEnv "SV0B" of
    SOME p => p
  | NONE =>
      ( TextIO.output (TextIO.stdErr, "sv0vm: set SV0B to a .sv0b file path\n")
      ; OS.Process.exit OS.Process.failure
      );

(* sv0cov CV-211: the raw-profile tier, from the launcher (`sv0 vm-run
   --raw-profile-tier`): both ceilings or neither. Not part of the SV0COV_*
   transport. Unset means the standard tier. *)
val () =
  (case (OS.Process.getEnv "SV0B_COVERAGE_MAX_COUNTERS", OS.Process.getEnv "SV0B_COVERAGE_MAX_BYTES") of
     (NONE, NONE) => ()
   | (SOME c, SOME b) =>
       (case (Int.fromString c, Int.fromString b) of
          (SOME c', SOME b') => Coverage.setTier (c', b')
        | _ => raise Coverage.Reject ("COV6001", "resource limit exceeded: the raw-profile ceilings are not integers"))
   | _ => raise Coverage.Reject ("COV6001", "resource limit exceeded: a raw-profile tier needs both ceilings"))
  handle Coverage.Reject (code, detail) =>
           ( TextIO.output (TextIO.stdErr, "sv0vm: error[" ^ code ^ "]: " ^ detail ^ "\n")
           ; OS.Process.exit OS.Process.failure )
       | Overflow =>
           ( TextIO.output (TextIO.stdErr, "sv0vm: error[COV6001]: resource limit exceeded: a raw-profile ceiling is out of range\n")
           ; OS.Process.exit OS.Process.failure );

val () = print "SV0VM_RUN_BEGIN\n";
(* sv0cov CV-119/CV-120: a coverage rejection happens at load, before any
   instruction runs; report it with its registry code and fail. *)
(* sv0cov CV-120: SV0B_COVERAGE_BINDING names the program's .sv0covbind.json
   companion explicitly (a file next to the bytecode confers nothing). Read
   inline: a new top-level val would be echoed into the program's output,
   which callers parse after SV0VM_RUN_BEGIN. *)
(* sv0cov CV-212: the process status matches the native executable. A
   program that ends in an orderly way -- a return, a contract failure or a
   runtime panic (both exit code 1) -- gives its exit code's low 8 bits, as a
   native process does; the profile has been published by then. Any other
   exception is a fatal VM failure: one diagnostic, status 134 (what a shell
   reports for an aborted process), no vm_exit line and no profile, so the
   run is incomplete. `sml < script` would otherwise carry on and exit 0. *)
val exitCode = Interpreter.runFileBound (path, OS.Process.getEnv "SV0B_COVERAGE_BINDING")
  handle Coverage.Reject (code, detail) =>
    ( TextIO.output (TextIO.stdErr, "sv0vm: error[" ^ code ^ "]: " ^ detail ^ "\n")
    ; OS.Process.exit OS.Process.failure )
       | e =>
    ( TextIO.flushOut TextIO.stdOut
    ; TextIO.output (TextIO.stdErr, "sv0vm: fatal: " ^ exnMessage e ^ "\n")
    ; TextIO.flushOut TextIO.stdErr
    ; Posix.Process.exit 0w134 );
val () = print ("vm_exit:" ^ Int.toString exitCode ^ "\n");
val () = TextIO.flushOut TextIO.stdOut;
Posix.Process.exit (Word8.fromInt (exitCode mod 256));
