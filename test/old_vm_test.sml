(* sv0cov CV-122: a VM without coverage support rejects COVER_HIT (opcode
   119) as an unknown opcode while loading, before any instruction runs, and
   never misdecodes it (sv0doc bytecode/coverage.md 3.1, COV-VM-003, AC-019).

   Runs in its own sml session after src/main.sml: the bytecode is built
   with today's encoder, then the pinned pre-coverage decoder
   (test/fixtures/pre-coverage/bytecode.sml, sv0vm 5c52484) replaces the
   Bytecode structure and has to refuse it. *)

val oldFails = ref 0
fun oldFail (name : string) (why : string) =
  (oldFails := !oldFails + 1; print ("old-VM test FAIL: " ^ name ^ ": " ^ why ^ "\n"))
fun readAll (path : string) = let val i = BinIO.openIn path in BinIO.inputAll i before BinIO.closeIn i end

(* The pin itself must not drift. *)
val () =
  if Sha256.hexOf (readAll "test/fixtures/pre-coverage/bytecode.sml")
     = "f85c9c66b4ecc166ed84534adc59003834d42ce2e6b96a18124ee329b88010e7"
  then () else oldFail "pin" "test/fixtures/pre-coverage/bytecode.sml is not sv0vm 5c52484's decoder"
val () =
  if Sha256.hexOf (readAll "test/fixtures/f0-instrumented.sv0b")
     = "b8e1cb10358836410d4905f1092bf39be574d0e970bd78284e477712fc6d456a"
  then () else oldFail "fixture" "test/fixtures/f0-instrumented.sv0b changed"

(* Bytecode from today's encoder. *)
fun one (code : Bytecode.insn list) : Bytecode.program =
  {strings = ["main"], funcs = [{nameIdx = 0, arity = 0, localCount = 0, code = code}]}
val p32 = Bytecode.PUSH_I32 (Int32.fromInt 3)
val instrumented =
  [ ("hit-first-in-main", Bytecode.encodeFile (one [Bytecode.COVER_HIT 0, p32, Bytecode.RETURN]))
  , ("hit-last", Bytecode.encodeFile (one [p32, Bytecode.RETURN, Bytecode.COVER_HIT 4294967295]))
  , ("hit-in-other-fn", Bytecode.encodeFile
      {strings = ["main", "f"], funcs =
        [{nameIdx = 0, arity = 0, localCount = 0, code = [p32, Bytecode.RETURN]},
         {nameIdx = 1, arity = 0, localCount = 0, code = [Bytecode.PUSH_UNIT, Bytecode.COVER_HIT 7, Bytecode.RETURN]}]})
  , ("f0-instrumented.sv0b", readAll "test/fixtures/f0-instrumented.sv0b") ]
val plain = Bytecode.encodeFile (one [p32, Bytecode.PUSH_I32 (Int32.fromInt 4), Bytecode.ADD_I32, Bytecode.RETURN])

(* From here on, Bytecode is the pre-coverage decoder. *)
val () = use "test/fixtures/pre-coverage/bytecode.sml";

val () =
  app (fn (name, bytes) =>
    (Bytecode.decodeFile bytes; oldFail name "the pre-coverage decoder accepted it")
    handle Fail msg =>
      if msg = "unknown opcode 119" then () else oldFail name ("failed for another reason: " ^ msg))
    instrumented
(* The bare instruction decoder refuses it too, at any operand. *)
val () =
  (Bytecode.decodeInsnVec (Word8Vector.fromList [0w119, 0w0, 0w0, 0w0, 0w0]) 0; oldFail "insn" "decoded")
  handle Fail msg => if msg = "unknown opcode 119" then () else oldFail "insn" msg
(* Plain bytecode still decodes. *)
val () =
  (case Bytecode.decodeFile plain of
     {funcs = [f], ...} => if length (#code f) = 4 then () else oldFail "plain" "wrong instruction count"
   | _ => oldFail "plain" "wrong function count")
  handle Fail msg => oldFail "plain" msg

val () =
  if !oldFails = 0 then print "old-VM rejection tests: OK\n"
  else raise Fail ("old-VM rejection tests: " ^ Int.toString (!oldFails) ^ " failure(s)")
