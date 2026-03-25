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

val () = print "bytecode tests: OK\n"
