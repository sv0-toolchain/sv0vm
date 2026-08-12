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

val () = print "bytecode tests: OK\n"
