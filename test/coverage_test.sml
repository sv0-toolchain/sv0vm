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

  (* 5. SHA-256 (FIPS 180-4 vectors). *)
  fun sha (s : string) = Sha256.hexOf (Byte.stringToBytes s)
  val () = expect ("sha-empty", sha "" = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  val () = expect ("sha-abc", sha "abc" = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  val () = expect ("sha-two-blocks",
    sha "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    = "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
  (* Padding boundaries: 55 bytes fits one block, 56 needs two. *)
  val () = expect ("sha-55-bytes", sha (CharVector.tabulate (55, fn _ => #"x")) = "d5e285683cd4efc02d021a5c62014694958901005d6f71e89e0989fac77e4072")
  val () = expect ("sha-56-bytes", sha (CharVector.tabulate (56, fn _ => #"x")) = "04c26261370ee7541549d16dee320c723e3fd14671e66a099afe0a377c16888e")
  val () = expect ("sha-63-bytes", sha (CharVector.tabulate (63, fn _ => #"x")) = "75220b47218278e656f2013bb8f0c455a25eaf01e86c64924e9d48d89776d6f2")
  val () = expect ("sha-64-bytes", sha (CharVector.tabulate (64, fn _ => #"x")) = "7ce100971f64e7001e8fe5a51973ecdfe1ced42befe7ee8d5fd6219506b5393c")
  val () = expect ("sha-65-bytes", sha (CharVector.tabulate (65, fn _ => #"x")) = "9537c5fdf120482f7d58d25e9ed583f52c02b4e304ea814db1633ad565aed7e9")

  (* 6. The companion binding: sv0cov's own golden (tests/test_vmbinding.py)
     reads back; every non-canonical variant is COV2201. *)
  val bc = Word8Vector.concat [Byte.stringToBytes "SV0B\001\000", Word8Vector.tabulate (40, fn i => Word8.fromInt i)]
  val ident = "sv0c@" ^ "0123456789abcdef0123456789abcdef012"
  val golden =
    "{\"bytecode_length\":46,\"bytecode_sha256\":\"" ^ Sha256.hexOf bc ^ "\","
    ^ "\"capabilities\":[\"sv0cov.coverage.v1\"],\"compiler_identity\":\"" ^ ident ^ "\","
    ^ "\"map_id\":\"" ^ CharVector.tabulate (64, fn i => if i mod 2 = 0 then #"a" else #"b")
    ^ "\",\"plan_capability\":\"sv0cov.plan.v1\",\"profile\":\"sv0vm-v1-coverage\","
    ^ "\"program_counter_count\":3,\"raw_profile_version\":\"1.0\",\"schema\":\"sv0cov.vm-binding\",\"version\":\"1.0\"}\n"
  val () =
    let val b = Coverage.readBinding golden
    in
      expect ("binding-golden", #programCounterCount b = 3 andalso #compilerIdentity b = ident
                                andalso #bytecodeLength b = 46);
      expect ("binding-binds", rejects (fn () => Coverage.bindTo (b, bc)) = NONE);
      expect ("binding-other-bytes",
        rejects (fn () => Coverage.bindTo (b, Word8Vector.update (bc, 45, 0w99))) = SOME "COV2202");
      expect ("binding-other-length",
        rejects (fn () => Coverage.bindTo (b, Word8Vector.concat [bc, Word8Vector.fromList [0w0]])) = SOME "COV2202")
    end
    handle Coverage.Reject (_, d) => fail "binding-golden" d
  fun replace (s : string, a : string, b : string) : string =
      let
        val n = size a
        fun go i = if i + n > size s then NONE
                   else if String.substring (s, i, n) = a then SOME i else go (i + 1)
      in
        case go 0 of
          NONE => raise Fail ("replace: " ^ a ^ " not found")
        | SOME i => String.substring (s, 0, i) ^ b ^ String.extract (s, i + n, NONE)
      end
  val variants =
    [ ("no-final-lf", String.substring (golden, 0, size golden - 1))
    , ("crlf", String.substring (golden, 0, size golden - 1) ^ "\r\n")
    , ("trailing", golden ^ " ")
    , ("space", replace (golden, "\"version\":", "\"version\": "))
    , ("reordered", replace (golden, "{\"bytecode_length\":46,\"bytecode_sha256\":", "{\"bytecode_sha256\":"))
    , ("extra-key", replace (golden, "\"version\":\"1.0\"}", "\"version\":\"1.0\",\"x\":1}"))
    , ("profile", replace (golden, "sv0vm-v1-coverage", "sv0vm-v1-core"))
    , ("schema", replace (golden, "\"sv0cov.vm-binding\"", "\"sv0cov.vm-binding-semantic\""))
    , ("capability", replace (golden, "sv0cov.coverage.v1", "sv0cov.coverage.v2"))
    , ("raw-version", replace (golden, "\"raw_profile_version\":\"1.0\"", "\"raw_profile_version\":\"1.1\""))
    , ("uppercase-map", replace (golden, "abababab", "ABABABAB"))
    , ("short-sha", replace (golden, "\"bytecode_sha256\":\"", "\"bytecode_sha256\":\"0"))
    , ("leading-zero", replace (golden, "\"program_counter_count\":3", "\"program_counter_count\":03"))
    , ("negative", replace (golden, "\"program_counter_count\":3", "\"program_counter_count\":-3"))
    , ("count-over-u32", replace (golden, "\"program_counter_count\":3", "\"program_counter_count\":4294967296"))
    , ("zero-length", replace (golden, "\"bytecode_length\":46", "\"bytecode_length\":0"))
    , ("identity-space", replace (golden, ident, "sv0c test"))
    , ("identity-empty", replace (golden, ident, ""))
    , ("identity-129", replace (golden, ident, CharVector.tabulate (129, fn _ => #"x")))
    , ("identity-unicode-escape", replace (golden, ident, "sv0c\\u0041"))
    , ("too-large", golden ^ CharVector.tabulate (4096, fn _ => #" "))
    , ("not-json", "")
    ]
  val () =
    app (fn (name, text) =>
      expect ("binding-" ^ name, rejects (fn () => ignore (Coverage.readBinding text)) = SOME "COV2201"))
      variants
  (* Escaped quote and backslash are valid identity bytes. *)
  val () = expect ("identity-escapes",
    (#compilerIdentity (Coverage.readBinding (replace (golden, ident, "a\\\"b\\\\c"))) = "a\"b\\c")
    handle Coverage.Reject _ => false)
  val () = expect ("count-u32-max",
    (#programCounterCount (Coverage.readBinding
       (replace (golden, "\"program_counter_count\":3", "\"program_counter_count\":4294967295"))) = 4294967295)
    handle Coverage.Reject _ => false)

  (* 7. load: binding, then bytes, then operands; nothing runs on failure. *)
  val () = Coverage.reset ()
  val () = expect ("load-unbound-instrumented",
    rejects (fn () => Coverage.load (hit2, bc, NONE)) = SOME "COV2201")
  val () = expect ("load-unbound-plain", rejects (fn () => Coverage.load (none, bc, NONE)) = NONE)
  val () = expect ("load-wrong-bytes",
    rejects (fn () => Coverage.load (hit2, Word8Vector.update (bc, 7, 0w7), SOME golden)) = SOME "COV2202")
  val () = expect ("load-count-mismatch",
    rejects (fn () => Coverage.load (hit2, bc, SOME (replace (golden, "\"program_counter_count\":3",
                                                              "\"program_counter_count\":1")))) = SOME "COV2201")
  val () = expect ("load-ok", rejects (fn () => Coverage.load (prog [COVER_HIT 2, RETURN], bc, SOME golden)) = NONE
                             andalso !Coverage.bound = SOME 3)
  val () = Coverage.reset ()

  (* 8. CRC32C (SPEC 16.4 check value) and raw profile byte parity with the
     sv0cov CV-024 goldens (copied to test/fixtures/rawprofile; the root
     test checks the copies match sv0cov's). *)
  val () = expect ("crc32c-check", RawProfile.crc32c (Byte.stringToBytes "123456789") = 0wxe3069283)
  val () = expect ("crc32c-empty", RawProfile.crc32c (Word8Vector.fromList []) = 0w0)
  fun readBytes (path : string) = let val i = BinIO.openIn path in BinIO.inputAll i before BinIO.closeIn i end
  fun rep (b : int) (n : int) = Word8Vector.tabulate (n, fn _ => Word8.fromInt b)
  val runFix = valOf (Coverage.hexBytes "0102030405060708090a0b0c0d0e0f10")
  val profFix = valOf (Coverage.hexBytes "a0a1a2a3a4a5a6a7a8a9aaabacadaeaf")
  val goldens : (string * Word32.word * int * string option * (int * Word64.word) list * int) list =
    [ ("native-basic", 0wx04, 0x10, NONE, [(0, 0w1), (2, 0w7)], 4)
    , ("vm-v1-empty-context", 0wx08, 0x11, SOME "", [(1, 0w1)], 3)
    , ("vm-v2-context-saturated", 0wx10, 0x12, SOME "ctx/\195\169", [(0, 0w5), (64, RawProfile.maxCount)], 65)
    , ("native-zero-counter", 0wx04, 0x13, NONE, [], 0)
    , ("native-no-hits", 0wx04, 0x14, NONE, [], 3) ]
  val () =
    app (fn (name, flag, mapByte, ctx, counts, n) =>
      let
        val got = RawProfile.encode {flags = flag, mapId = rep mapByte 32, runId = runFix, profileId = profFix,
                                     context = ctx, counts = counts, n = n}
        val want = readBytes ("test/fixtures/rawprofile/" ^ name ^ ".sv0profraw")
      in
        expect ("golden-" ^ name, got = want)
      end) goldens

  (* 9. flush: publishes exactly those bytes, mode 0600, under
     <run>-<profile>.sv0profraw; a name that exists is COV2112 and is kept. *)
  fun freshDir () =
    let val d = OS.FileSys.tmpName () in (OS.FileSys.remove d handle OS.SysErr _ => ()); OS.FileSys.mkDir d; d end
  fun prepare (dir : string) =
    ( Coverage.reset ()
    ; Coverage.activate 3
    ; Coverage.hit 1
    ; Coverage.profileDir := dir
    ; Coverage.runId := runFix
    ; Coverage.profileId := profFix
    ; Coverage.mapIdBytes := rep 0x11 32
    ; Coverage.context := SOME ""
    ; Coverage.publish := true )
  val dir = freshDir ()
  val () = prepare dir
  val () = expect ("flush-ok", Coverage.flush () = NONE)
  val name = "0102030405060708090a0b0c0d0e0f10-a0a1a2a3a4a5a6a7a8a9aaabacadaeaf.sv0profraw"
  val path = dir ^ "/" ^ name
  val () = expect ("flush-golden-bytes", (readBytes path = readBytes "test/fixtures/rawprofile/vm-v1-empty-context.sv0profraw")
                                         handle IO.Io _ => false)
  val () = expect ("flush-mode-0600",
    (Posix.FileSys.ST.mode (Posix.FileSys.stat path) = Posix.FileSys.S.flags [Posix.FileSys.S.irusr, Posix.FileSys.S.iwusr])
    handle OS.SysErr _ => false)
  val () = expect ("flush-once", Coverage.flush () = NONE)
  (* Collision: the same IDs again; the existing file is left alone. *)
  val () = prepare dir
  val () = Coverage.hit 0
  val () = Coverage.required := true
  val () = expect ("flush-collision-required", Coverage.flush () = SOME 1)
  val () = expect ("collision-kept", (readBytes path = readBytes "test/fixtures/rawprofile/vm-v1-empty-context.sv0profraw")
                                     handle IO.Io _ => false)
  val () = expect ("no-temporaries-left", (let val d = OS.FileSys.openDir dir
                                               fun all acc = case OS.FileSys.readDir d of NONE => acc | SOME f => all (f :: acc)
                                           in all [] before OS.FileSys.closeDir d end) = [name])
  (* A vanished directory: COV2010, status 1 only when required. *)
  val gone = freshDir ()
  val () = OS.FileSys.rmDir gone
  val () = prepare gone
  val () = expect ("flush-missing-dir-not-required", Coverage.flush () = NONE)
  val () = prepare gone
  val () = Coverage.required := true
  val () = expect ("flush-missing-dir-required", Coverage.flush () = SOME 1)
  val () = OS.FileSys.remove path
  val () = OS.FileSys.rmDir dir
  val () = Coverage.reset ()

  val () =
    if !nfail = 0 then print "coverage tests: OK\n"
    else raise Fail ("coverage tests: " ^ Int.toString (!nfail) ^ " failure(s)")
end
