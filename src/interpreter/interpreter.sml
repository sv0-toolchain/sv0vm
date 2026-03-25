(* Stack-machine interpreter: dispatch loop to be implemented in sv0vm-interpreter task. *)

structure Interpreter = struct

  type program = Bytecode.program

  fun run (_ : program) : unit =
    raise Fail "Interpreter.run: not implemented (milestone 2 phase 2)"

end
