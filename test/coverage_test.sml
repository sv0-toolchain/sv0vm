(* COVER_HIT (opcode 119) tests: sv0cov CV-119, sv0doc bytecode/coverage.md.
   Run after src/main.sml (make test). *)

local
  open Bytecode
  val nfail = ref 0
  fun fail (name : string) (why : string) =
    (nfail := !nfail + 1; print ("coverage test FAIL: " ^ name ^ ": " ^ why ^ "\n"))
  fun expect (name : string, cond : bool) = if cond then () else fail name "condition false"
  fun bytesOf (v : Word8Vector.vector) : int list =
    Word8Vector.foldr (fn (b, acc) => Word8.toInt b :: acc) [] v

  (* Run `body` and report the registry code of a Coverage.Reject, if any. *)
  fun rejects (f : unit -> unit) : string option =
    (f (); NONE) handle Coverage.Reject (code, _) => SOME code

  fun prog (code : insn list) : program =
    {strings = ["main"], funcs = [{nameIdx = 0, arity = 0, localCount = 1, code = code}]}
in
  (* 1. Encoding: five bytes, opcode then u32le; the full u32 range round-trips. *)
  val () = expect ("encode-layout", bytesOf (encodeInsn (COVER_HIT 4194303)) = [119, 255, 255, 63, 0])
  val () =
    app (fn k =>
      case decodeInsnVec (encodeInsn (COVER_HIT k)) 0 of
        (COVER_HIT k', 5) => expect ("roundtrip-" ^ Int.toString k, k' = k)
      | _ => fail ("roundtrip-" ^ Int.toString k) "decoded something else")
      [0, 1, 70000, 4194303, 2147483647, 2147483648, 4294967295]
  val () =
    app (fn k =>
      (encodeInsn (COVER_HIT k); fail ("encode-range-" ^ Int.toString k) "accepted")
      handle Fail _ => ())
      [~1, 4294967296]
  (* Instructions after a hit decode at the right boundary. *)
  val () =
    case decodeAll (encodeAll [COVER_HIT 3, PUSH_I32 (Int32.fromInt 7), COVER_HIT 0, RETURN]) of
      [COVER_HIT 3, PUSH_I32 x, COVER_HIT 0, RETURN] => expect ("decode-stream", Int32.toInt x = 7)
    | _ => fail "decode-stream" "wrong instruction stream"
  (* Other unassigned opcodes are still unknown. *)
  val () =
    (decodeInsnVec (Word8Vector.fromList [0w120]) 0; fail "unknown-120" "decoded")
    handle Fail msg => expect ("unknown-120", String.isSubstring "unknown opcode 120" msg)

  (* 2. Disassembly: the same text sv0c's bytecode.sv0 disasm_file prints for
     this program (its test_cover_hit), with a forward and a backward jump
     across hits. *)
  val want =
    "sv0b v1: 1 functions, 4 string bytes\n"
    ^ "fn 0 (name #0, arity 0, locals 1, 26 bytes):\n"
    ^ "      0  COVER_HIT 0\n"
    ^ "      5  JUMP_IF_NOT 10 -> 20\n"
    ^ "     10  COVER_HIT 4194303\n"
    ^ "     15  COVER_HIT 70000\n"
    ^ "     20  JUMP -25 -> 0\n"
    ^ "     25  RETURN\n"
  val file0 = encodeFile {strings = [], funcs =
    [{nameIdx = 0, arity = 0, localCount = 1,
      code = [COVER_HIT 0, JUMP_IF_NOT 10, COVER_HIT 4194303, COVER_HIT 70000, JUMP ~25, RETURN]}]}
  val () = expect ("disassemble", disassemble file0 = want)
  val () = if disassemble file0 = want then () else print (disassemble file0)
  (* An unknown opcode ends the function's listing with "?? <opcode>". *)
  val retFile = encodeFile {strings = [], funcs = [{nameIdx = 0, arity = 0, localCount = 0, code = [RETURN]}]}
  val badFile = Word8Vector.tabulate (Word8Vector.length retFile, fn j =>
    if j = Word8Vector.length retFile - 1 then 0w120 else Word8Vector.sub (retFile, j))
  val () = expect ("disassemble-unknown", String.isSubstring "      0  ?? 120\n" (disassemble badFile))

  (* 3. Load-time checks: before any instruction runs. *)
  val hit2 = prog [COVER_HIT 0, PUSH_I32 (Int32.fromInt 1), COVER_HIT 1, RETURN]
  val none = prog [PUSH_I32 (Int32.fromInt 1), RETURN]
  val () = expect ("unbound-rejected", rejects (fn () => Coverage.check (hit2, NONE)) = SOME "COV2201")
  val () = expect ("uninstrumented-unbound-ok", rejects (fn () => Coverage.check (none, NONE)) = NONE)
  val () = expect ("bound-ok", rejects (fn () => Coverage.check (hit2, SOME 2)) = NONE)
  val () = expect ("out-of-range", rejects (fn () => Coverage.check (hit2, SOME 1)) = SOME "COV2201")
  val () = expect ("zero-count-with-hit", rejects (fn () => Coverage.check (hit2, SOME 0)) = SOME "COV2201")
  val () = expect ("count-without-hit", rejects (fn () => Coverage.check (none, SOME 3)) = SOME "COV2201")
  val () = expect ("zero-count-no-hit", rejects (fn () => Coverage.check (none, SOME 0)) = NONE)
  (* The interpreter refuses an unbound instrumented program at load. *)
  val () = Coverage.reset ()
  val () = expect ("interpreter-rejects-unbound",
    ((Interpreter.runProgram hit2; false) handle Coverage.Reject ("COV2201", _) => true))
  (* A hit in a non-main function is found too. *)
  val twoFns : program =
    {strings = ["main", "f"], funcs =
      [{nameIdx = 0, arity = 0, localCount = 0, code = [PUSH_I32 (Int32.fromInt 0), RETURN]},
       {nameIdx = 1, arity = 0, localCount = 0, code = [COVER_HIT 0, PUSH_UNIT, RETURN]}]}
  val () = expect ("hit-in-other-fn", rejects (fn () => Coverage.check (twoFns, NONE)) = SOME "COV2201")

  (* 4. Execution with an arena: COVER_HIT is stack-neutral (the result is
     the uninstrumented one) and counts each execution. *)
  val counted = prog [COVER_HIT 0, PUSH_I32 (Int32.fromInt 5), COVER_HIT 1, PUSH_I32 (Int32.fromInt 6),
                      COVER_HIT 1, ADD_I32, RETURN]
  val () = Coverage.reset ()
  val () = Coverage.bound := SOME 2
  val r = Interpreter.runProgram counted
  val () = expect ("stack-neutral", r = 11)
  val () = expect ("counts", Array.sub (!Coverage.counters, 0) = 0w1 andalso Array.sub (!Coverage.counters, 1) = 0w2)
  (* Saturation: a counter at 2^64 - 1 stays there and records overflow. *)
  val () = Array.update (!Coverage.counters, 0, Coverage.maxCount - 0w1)
  val () = Coverage.hit 0
  val () = expect ("reaches-max", Array.sub (!Coverage.counters, 0) = Coverage.maxCount
                                  andalso not (Array.sub (!Coverage.overflow, 0)))
  val () = Coverage.hit 0
  val () = expect ("saturates", Array.sub (!Coverage.counters, 0) = Coverage.maxCount
                                andalso Array.sub (!Coverage.overflow, 0))
  val () = Coverage.reset ()

  val () =
    if !nfail = 0 then print "coverage tests: OK\n"
    else raise Fail ("coverage tests: " ^ Int.toString (!nfail) ^ " failure(s)")
end
