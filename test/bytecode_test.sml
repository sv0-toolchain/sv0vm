(* Bytecode encode/decode and .sv0b container tests. *)

use "src/bytecode/bytecode.sml";

fun sameInsn (a : Bytecode.insn, b : Bytecode.insn) : bool =
  case (a, b) of
    (Bytecode.HALT, Bytecode.HALT) => true
  | (Bytecode.POP, Bytecode.POP) => true
  | (Bytecode.DUP, Bytecode.DUP) => true
  | (Bytecode.PUSH_UNIT, Bytecode.PUSH_UNIT) => true
  | (Bytecode.PUSH_I32 x, Bytecode.PUSH_I32 y) => x = y
  | (Bytecode.PUSH_I64 x, Bytecode.PUSH_I64 y) => x = y
  | (Bytecode.PUSH_F64 x, Bytecode.PUSH_F64 y) => Real.== (x, y)
  | (Bytecode.PUSH_BOOL x, Bytecode.PUSH_BOOL y) => x = y
  | (Bytecode.PUSH_STRING x, Bytecode.PUSH_STRING y) => x = y
  | (Bytecode.ADD_I32, Bytecode.ADD_I32) => true
  | (Bytecode.ADD_F64, Bytecode.ADD_F64) => true
  | (Bytecode.LOAD_LOCAL x, Bytecode.LOAD_LOCAL y) => x = y
  | (Bytecode.STORE_LOCAL x, Bytecode.STORE_LOCAL y) => x = y
  | (Bytecode.CALL (f, n), Bytecode.CALL (g, m)) => f = g andalso n = m
  | (Bytecode.CAST (a1, b1), Bytecode.CAST (a2, b2)) => a1 = a2 andalso b1 = b2
  | _ => false

fun sameInsns (xs : Bytecode.insn list) (ys : Bytecode.insn list) : bool =
  case (xs, ys) of
    ([], []) => true
  | (x :: xt, y :: yt) => sameInsn (x, y) andalso sameInsns xt yt
  | _ => false

val sampleInsns : Bytecode.insn list =
  [ Bytecode.PUSH_I32 (Int32.fromInt 3)
  , Bytecode.PUSH_I32 (Int32.fromInt 4)
  , Bytecode.ADD_I32
  , Bytecode.PUSH_F64 1.5
  , Bytecode.PUSH_BOOL true
  , Bytecode.PUSH_STRING 0
  , Bytecode.HALT
  ]

val buf = Bytecode.encodeAll sampleInsns
val decoded = Bytecode.decodeAll buf
val () =
  if sameInsns sampleInsns decoded then () else raise Fail "instruction round-trip mismatch"

val prog : Bytecode.program =
  { strings = ["main"]
  , funcs =
      [ { nameIdx = 0
        , arity = 0
        , localCount = 2
        , code =
            [ Bytecode.PUSH_I32 (Int32.fromInt 0)
            , Bytecode.STORE_LOCAL 0
            , Bytecode.LOAD_LOCAL 0
            , Bytecode.HALT
            ]
        }
      ]
  }

val fileVec = Bytecode.encodeFile prog
val prog2 = Bytecode.decodeFile fileVec
val () =
  if #strings prog = #strings prog2
     andalso length (#funcs prog) = length (#funcs prog2)
  then
    case (#funcs prog, #funcs prog2) of
      (f :: _, g :: _) =>
        if #nameIdx f = #nameIdx g andalso #arity f = #arity g
           andalso #localCount f = #localCount g
           andalso sameInsns (#code f) (#code g)
        then ()
        else raise Fail "file round-trip func mismatch"
    | _ => raise Fail "file round-trip func list"
  else
    raise Fail "file round-trip strings/funcs"

(* i32 arithmetic semantics (BH-2): wraparound on overflow + truncate-toward-zero
   div/mod, matching the C backend / Rust — NOT SML's wider-int / floor semantics. *)
local
  (* Round-trip through the .sv0b byte encoding so the program crosses into the
     interpreter as bytes (avoids SML `use`-load structure-identity issues that a
     direct Bytecode.program value hits). *)
  fun runI (insns : Bytecode.insn list) : int =
    let
      val prog : Bytecode.program =
        { strings = ["main"]
        , funcs = [ { nameIdx = 0, arity = 0, localCount = 0, code = insns } ] }
      val vec = Bytecode.encodeFile prog
      val path = OS.FileSys.tmpName ()
      val os = BinIO.openOut path
      val () = BinIO.output (os, vec)
      val () = BinIO.closeOut os
      val r = Interpreter.runFile path
      val () = OS.FileSys.remove path
    in
      r
    end
  fun p (i : int) = Bytecode.PUSH_I32 (Int32.fromInt i)
  fun expect (label, got, want) =
    if got = want then ()
    else raise Fail ("arith " ^ label ^ ": got " ^ Int.toString got
                     ^ " want " ^ Int.toString want)
in
  (* 2e9 + 2e9 = 4e9 wraps to -294967296 (not the SML-wide 4000000000) *)
  val () = expect ("overflow-add",
    runI [p 2000000000, p 2000000000, Bytecode.ADD_I32, Bytecode.RETURN], ~294967296)
  (* 46341*46341 = 2147488281 overflows i32 to negative *)
  val () = expect ("overflow-mul",
    runI [p 46341, p 46341, Bytecode.MUL_I32, Bytecode.RETURN], ~2147479015)
  (* (-100)/3 = -33 (trunc toward zero, not -34 floor) *)
  val () = expect ("neg-div",
    runI [p (~100), p 3, Bytecode.DIV_I32, Bytecode.RETURN], ~33)
  (* (-7) % 3 = -1 (sign of dividend, not +2 floor-mod) *)
  val () = expect ("neg-mod",
    runI [p (~7), p 3, Bytecode.MOD_I32, Bytecode.RETURN], ~1)
  (* (-1) % 100 = -1 (not +99) *)
  val () = expect ("neg-mod2",
    runI [p (~1), p 100, Bytecode.MOD_I32, Bytecode.RETURN], ~1)
  (* INT_MIN / -1 wraps to INT_MIN (would raise Overflow without the Word32 wrap) *)
  val () = expect ("intmin-div",
    runI [p (~2147483648), p (~1), Bytecode.DIV_I32, Bytecode.RETURN], ~2147483648)
end

(* ---- PUSH_F64 bit-pattern round-trip (sv0-mathlib per-fixture check /
   BUGS.md): the old Unsafe.cast real<->Word64 mis-decoded large-exponent
   values such as 2^52 under SML/NJ 110.99.9, aborting frac_floor_of_nonneg's
   ensures. The field-math codec must round-trip every normal magnitude and
   the special values exactly, and lay the 64 IEEE bits down little-endian. ---- *)
local
  fun rt (r : real) : real =
    case Bytecode.decodeInsnVec (Bytecode.encodeInsn (Bytecode.PUSH_F64 r)) 0 of
      (Bytecode.PUSH_F64 x, _) => x
    | _ => raise Fail "f64 round-trip: not PUSH_F64"
  fun same (a, b) =
    (Real.isNan a andalso Real.isNan b)
    orelse (Real.== (a, b) andalso (Real.signBit a = Real.signBit b
                                    orelse not (Real.== (a, 0.0))))
  fun chk (name, r) =
    if same (r, rt r) then ()
    else raise Fail ("f64 round-trip " ^ name ^ ": "
                     ^ Real.fmt (StringCvt.GEN (SOME 17)) r ^ " -> "
                     ^ Real.fmt (StringCvt.GEN (SOME 17)) (rt r))
  (* on-disk bytes must be the IEEE-754 pattern, little-endian (byte 0 = bits 7:0) *)
  fun bytesLe (r : real) =
    let val v = Bytecode.encodeInsn (Bytecode.PUSH_F64 r)
    in String.concatWith "" (List.tabulate (8, fn i =>
         StringCvt.padLeft #"0" 2
           (Word8.fmt StringCvt.HEX (Word8Vector.sub (v, i + 1))))) end
  fun chkBytes (name, r, hex) =
    if bytesLe r = hex then ()
    else raise Fail ("f64 bytes " ^ name ^ ": got " ^ bytesLe r ^ " want " ^ hex)
in
  val () = List.app chk
    [ ("+0.0", 0.0), ("-0.0", ~0.0), ("1.0", 1.0), ("-1.0", ~1.0), ("2.0", 2.0),
      ("0.5", 0.5), ("2.5", 2.5), ("-2.5", ~2.5), ("3.4", 3.4), ("-3.4", ~3.4),
      ("2^52", 4503599627370496.0), ("2^53", 9007199254740992.0),
      ("2^52-1", 4503599627370495.0), ("pi/2", 1.5707963267948966),
      ("just below 2", 1.9999999999999998), ("just above 2", 2.0000000000000004),
      ("12345678901234.0", 12345678901234.0), ("1e10", 1e10), ("1e~10", 1e~10),
      ("1e300", 1e300), ("1e~300", 1e~300), ("0.1", 0.1), ("0.3", 0.3),
      ("+inf", Real.posInf), ("-inf", Real.negInf), ("nan", 0.0 / 0.0) ]
  val () = List.app chkBytes
    [ ("2^52", 4503599627370496.0, "0000000000003043"),
      ("1.0",  1.0,                "000000000000F03F"),
      ("2.5",  2.5,                "0000000000000440"),
      ("-0.0", ~0.0,               "0000000000000080"),
      ("+inf", Real.posInf,        "000000000000F07F") ]
end

val () = print "bytecode tests: OK\n"

(* ===================================================================== *)
(* Interpreter execution semantics — expanded suite.                     *)
(* Builds single-fn programs, encodes to .sv0b, runs via runFile, and    *)
(* asserts the returned i32. Covers arithmetic boundaries, bitwise,      *)
(* logic, locals, stack ops, and control flow (relative jumps).          *)
(* ===================================================================== *)
local
  open Bytecode
  fun runL (localCount : int) (insns : insn list) : int =
    let
      val prog : program =
        { strings = ["main"]
        , funcs = [ { nameIdx = 0, arity = 0, localCount = localCount, code = insns } ] }
      val vec = encodeFile prog
      val path = OS.FileSys.tmpName ()
      val os = BinIO.openOut path
      val () = BinIO.output (os, vec)
      val () = BinIO.closeOut os
      val r = Interpreter.runFile path
      val () = OS.FileSys.remove path
    in r end
  fun run insns = runL 0 insns
  fun p (i : int) = PUSH_I32 (Int32.fromInt i)
  val nfail = ref 0
  fun expect (label, got, want) =
    if got = want then ()
    else (nfail := !nfail + 1;
          print ("FAIL " ^ label ^ ": got " ^ Int.toString got
                 ^ " want " ^ Int.toString want ^ "\n"))
in
  (* ---- arithmetic: basic + identities ---- *)
  val () = expect ("add", run [p 3, p 4, ADD_I32, RETURN], 7)
  val () = expect ("sub", run [p 10, p 3, SUB_I32, RETURN], 7)
  val () = expect ("sub-neg", run [p 3, p 10, SUB_I32, RETURN], ~7)
  val () = expect ("mul", run [p 6, p 7, MUL_I32, RETURN], 42)
  val () = expect ("mul-neg", run [p (~6), p 7, MUL_I32, RETURN], ~42)
  val () = expect ("mul-negneg", run [p (~6), p (~7), MUL_I32, RETURN], 42)
  val () = expect ("mul-zero", run [p 123, p 0, MUL_I32, RETURN], 0)
  val () = expect ("div-exact", run [p 20, p 4, DIV_I32, RETURN], 5)
  val () = expect ("div-trunc", run [p 7, p 2, DIV_I32, RETURN], 3)
  val () = expect ("neg", run [p 5, NEG_I32, RETURN], ~5)
  val () = expect ("neg-neg", run [p (~5), NEG_I32, RETURN], 5)
  val () = expect ("neg-zero", run [p 0, NEG_I32, RETURN], 0)
  val () = expect ("add-identity", run [p 42, p 0, ADD_I32, RETURN], 42)
  val () = expect ("chain", run [p 1, p 2, ADD_I32, p 3, ADD_I32, p 4, ADD_I32, RETURN], 10)
  val () = expect ("mixed-prec", run [p 2, p 3, MUL_I32, p 4, ADD_I32, RETURN], 10)
  (* ---- arithmetic: i32 boundaries + sign rules ---- *)
  val () = expect ("max-plus-zero", run [p 2147483647, p 0, ADD_I32, RETURN], 2147483647)
  val () = expect ("max-plus-one-wraps", run [p 2147483647, p 1, ADD_I32, RETURN], ~2147483648)
  val () = expect ("min-minus-one-wraps", run [p (~2147483648), p 1, SUB_I32, RETURN], 2147483647)
  val () = expect ("neg-min-wraps", run [p (~2147483648), NEG_I32, RETURN], ~2147483648)
  val () = expect ("div-pos-neg", run [p 100, p (~3), DIV_I32, RETURN], ~33)
  val () = expect ("div-neg-neg", run [p (~100), p (~3), DIV_I32, RETURN], 33)
  val () = expect ("mod-pos", run [p 7, p 3, MOD_I32, RETURN], 1)
  val () = expect ("mod-neg-dividend", run [p (~7), p 3, MOD_I32, RETURN], ~1)
  val () = expect ("mod-neg-divisor", run [p 7, p (~3), MOD_I32, RETURN], 1)
  val () = expect ("mod-exact-zero", run [p 12, p 4, MOD_I32, RETURN], 0)
  (* ---- bitwise ---- *)
  val () = expect ("bit-and", run [p 12, p 10, BIT_AND, RETURN], 8)
  val () = expect ("bit-or", run [p 12, p 10, BIT_OR, RETURN], 14)
  val () = expect ("bit-xor", run [p 12, p 10, BIT_XOR, RETURN], 6)
  val () = expect ("bit-not", run [p 0, BIT_NOT, RETURN], ~1)
  val () = expect ("bit-not-neg1", run [p (~1), BIT_NOT, RETURN], 0)
  val () = expect ("shl", run [p 1, p 4, SHL, RETURN], 16)
  val () = expect ("shl-3", run [p 3, p 2, SHL, RETURN], 12)
  val () = expect ("shr", run [p 256, p 4, SHR, RETURN], 16)
  val () = expect ("shr-odd", run [p 13, p 1, SHR, RETURN], 6)
  (* ---- logic (truthy of resulting bool via JUMP_IF_NOT select) ---- *)
  (* return (if (a AND b) then 1 else 0) style, but AND/OR/NOT push bools;
     feed through JUMP_IF_NOT to a numeric result to avoid bool->int coupling. *)
  fun sel (cond : insn list) (t : int) (f : int) : insn list =
    cond @ [ JUMP_IF_NOT 2, p t, RETURN, p f, RETURN ]
  val () = expect ("and-tt", run (sel [PUSH_BOOL true, PUSH_BOOL true, AND] 1 0), 1)
  val () = expect ("and-tf", run (sel [PUSH_BOOL true, PUSH_BOOL false, AND] 1 0), 0)
  val () = expect ("or-tf", run (sel [PUSH_BOOL true, PUSH_BOOL false, OR] 1 0), 1)
  val () = expect ("or-ff", run (sel [PUSH_BOOL false, PUSH_BOOL false, OR] 1 0), 0)
  val () = expect ("not-t", run (sel [PUSH_BOOL true, NOT] 1 0), 0)
  val () = expect ("not-f", run (sel [PUSH_BOOL false, NOT] 1 0), 1)
  (* ---- comparisons (via select) ---- *)
  val () = expect ("lt-true", run (sel [p 3, p 5, LT] 1 0), 1)
  val () = expect ("lt-false", run (sel [p 5, p 3, LT] 1 0), 0)
  val () = expect ("lt-eq", run (sel [p 5, p 5, LT] 1 0), 0)
  val () = expect ("gt-true", run (sel [p 5, p 3, GT] 1 0), 1)
  val () = expect ("lte-eq", run (sel [p 5, p 5, LTE] 1 0), 1)
  val () = expect ("gte-eq", run (sel [p 5, p 5, GTE] 1 0), 1)
  val () = expect ("eq-true", run (sel [p 7, p 7, EQ] 1 0), 1)
  val () = expect ("eq-false", run (sel [p 7, p 8, EQ] 1 0), 0)
  val () = expect ("neq-true", run (sel [p 7, p 8, NEQ] 1 0), 1)
  val () = expect ("cmp-neg", run (sel [p (~5), p 3, LT] 1 0), 1)
  (* ---- stack ops ---- *)
  val () = expect ("dup-add", run [p 21, DUP, ADD_I32, RETURN], 42)
  val () = expect ("pop", run [p 99, p 42, POP, RETURN], 99)
  (* ---- locals ---- *)
  val () = expect ("local-roundtrip", runL 1 [p 42, STORE_LOCAL 0, LOAD_LOCAL 0, RETURN], 42)
  val () = expect ("local-two",
    runL 2 [p 10, STORE_LOCAL 0, p 32, STORE_LOCAL 1,
            LOAD_LOCAL 0, LOAD_LOCAL 1, ADD_I32, RETURN], 42)
  val () = expect ("local-overwrite",
    runL 1 [p 1, STORE_LOCAL 0, p 2, STORE_LOCAL 0, LOAD_LOCAL 0, RETURN], 2)
  (* Control flow (branches, loops) uses BYTE-relative jump offsets, which are
     brittle to hand-encode; it is covered end-to-end by the sv0c integration +
     vm-parity suites (real programs compiled to bytecode and run here). This unit
     block deliberately stays offset-free. *)

  (* ---- VMF-011/012: f64 + i64 stack ops (sv0c-vm-float-parity). `sel` turns a
     bool result into a 1/0 exit code. ---- *)
  fun pf (r : real) = PUSH_F64 r
  fun pl (i : Int64.int) = PUSH_I64 i
  (* f64 arithmetic *)
  val () = expect ("f64-add", run (sel [pf 1.5, pf 2.5, ADD_F64, pf 3.9, GT] 1 0), 1)
  val () = expect ("f64-sub", run (sel [pf 5.0, pf 1.5, SUB_F64, pf 3.5, EQ] 1 0), 1)
  val () = expect ("f64-mul", run (sel [pf 2.0, pf 3.0, MUL_F64, pf 6.0, EQ] 1 0), 1)
  val () = expect ("f64-div", run (sel [pf 7.0, pf 2.0, DIV_F64, pf 3.5, EQ] 1 0), 1)
  val () = expect ("f64-neg", run (sel [pf 4.0, NEG_F64, pf 0.0, LT] 1 0), 1)
  val () = expect ("f64-lt", run (sel [pf 1.1, pf 2.2, LT] 1 0), 1)
  val () = expect ("f64-gte-eq", run (sel [pf 2.5, pf 2.5, GTE] 1 0), 1)
  val () = expect ("f64-neq", run (sel [pf 1.0, pf 2.0, NEQ] 1 0), 1)
  (* i64 arithmetic — values exceed i32 range, so these are wrong under CInt *)
  val () = expect ("i64-add",
    run (sel [pl 5000000000, pl 3000000000, ADD_I64, pl 8000000000, EQ] 1 0), 1)
  val () = expect ("i64-sub",
    run (sel [pl 10000000000, pl 1, SUB_I64, pl 9999999999, EQ] 1 0), 1)
  val () = expect ("i64-mul-wrap",
    run (sel [pl 4294967296, pl 4294967296, MUL_I64, pl 0, EQ] 1 0), 1)
  val () = expect ("i64-div",
    run (sel [pl 10000000000, pl 4, DIV_I64, pl 2500000000, EQ] 1 0), 1)
  val () = expect ("i64-neg",
    run (sel [pl 5000000000, NEG_I64, pl 0, LT] 1 0), 1)
  val () = expect ("i64-gt-wide",
    run (sel [pl 5000000000, pl 3000000000, GT] 1 0), 1)

  val () =
    if !nfail = 0 then print "interpreter exec tests: OK\n"
    else raise Fail ("interpreter exec tests: " ^ Int.toString (!nfail) ^ " failure(s)")
end
