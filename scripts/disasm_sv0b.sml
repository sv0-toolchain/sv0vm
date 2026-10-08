(* Print the disassembly of the .sv0b at env SV0B (Bytecode.disassemble; the
   same format as sv0c's bytecode.sv0 disasm_file), between
   SV0VM_DISASM_BEGIN and SV0VM_DISASM_END lines. *)

use "src/main.sml";

val path =
  case OS.Process.getEnv "SV0B" of
    SOME p => p
  | NONE =>
      ( TextIO.output (TextIO.stdErr, "sv0vm: set SV0B to a .sv0b file path\n")
      ; OS.Process.exit OS.Process.failure
      );

val bytes = let val ins = BinIO.openIn path in BinIO.inputAll ins before BinIO.closeIn ins end;
val () = print ("SV0VM_DISASM_BEGIN\n" ^ Bytecode.disassemble bytes ^ "SV0VM_DISASM_END\n");
OS.Process.exit OS.Process.success;
