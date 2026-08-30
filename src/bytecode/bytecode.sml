(* sv0 bytecode: instruction set and .sv0b container. See sv0doc/bytecode/*.md *)

structure Bytecode = struct

  val magicBytes : Word8Vector.vector =
    let
      open Word8
      val a = Word8Array.array (4, 0w0)
      val () = Word8Array.update (a, 0, fromInt 83) (* S *)
      val () = Word8Array.update (a, 1, fromInt 86) (* V *)
      val () = Word8Array.update (a, 2, fromInt 48) (* 0 *)
      val () = Word8Array.update (a, 3, fromInt 66) (* B *)
    in
      Word8Array.vector a
    end

  val formatVersion : int = 1

  fun w8 (i : int) : Word8.word = Word8.fromInt i

  fun bytes1 (b : Word8.word) : Word8Vector.vector =
    let val a = Word8Array.array (1, b) in Word8Array.vector a end

  fun u16Le (i : int) : Word8Vector.vector =
    let
      val w = Word.fromInt i
      val a = Word8Array.array (2, 0w0)
      val () = Word8Array.update (a, 0, Word8.fromLargeWord (Word.toLargeWord (Word.andb (w, 0wxFF))))
      val () =
        Word8Array.update (a, 1,
          Word8.fromLargeWord (Word.toLargeWord (Word.>> (w, 0w8))))
    in
      Word8Array.vector a
    end

  fun u32FromWord32 (w : Word32.word) : Word8Vector.vector =
    let
      val lw = Word32.toLargeWord w
      val a = Word8Array.array (4, 0w0)
      fun set j =
        Word8Array.update (a, j,
          Word8.fromLargeWord
            (LargeWord.andb (LargeWord.>> (lw, Word.fromInt (j * 8)), 0wxFF)))
      val () = List.app set [0, 1, 2, 3]
    in
      Word8Array.vector a
    end

  fun u32Le (i : int) : Word8Vector.vector =
    u32FromWord32 (Word32.fromLargeInt (Int.toLarge i))

  fun i32Le (i : Int32.int) : Word8Vector.vector =
    u32FromWord32 (Word32.fromLargeInt (Int32.toLarge i))

  fun i64Le (i : Int64.int) : Word8Vector.vector =
    let
      val w = Word64.fromLargeInt (Int64.toLarge i)
      val lw = Word64.toLarge w
      val a = Word8Array.array (8, 0w0)
      fun set j =
        Word8Array.update (a, j,
          Word8.fromLargeWord
            (LargeWord.andb (LargeWord.>> (lw, Word.fromInt (j * 8)), 0wxFF)))
      val () = List.app set [0, 1, 2, 3, 4, 5, 6, 7]
    in
      Word8Array.vector a
    end

  local
    (* SML/NJ: IEEE f64 via word bit pattern (little-endian load/store). *)
    fun word64Le (w : Word64.word) : Word8Vector.vector =
      let
        val lw = Word64.toLarge w
        val a = Word8Array.array (8, 0w0)
        fun set j =
          Word8Array.update (a, j,
            Word8.fromLargeWord
              (LargeWord.andb (LargeWord.>> (lw, Word.fromInt (j * 8)), 0wxFF)))
        val () = List.app set [0, 1, 2, 3, 4, 5, 6, 7]
      in
        Word8Array.vector a
      end

    fun word64FromVecLe (v : Word8Vector.vector) (i : int) : Word64.word =
      let
        val lw =
          List.foldl
            (fn (k, acc) =>
              LargeWord.orb (acc,
                LargeWord.<< (Word8.toLargeWord (Word8Vector.sub (v, i + k)), Word.fromInt (k * 8)))) 0w0
            (List.tabulate (8, fn x => x))
      in
        Word64.fromLarge lw
      end
  in
    fun f64Le (r : real) : Word8Vector.vector =
      let val w : Word64.word = Unsafe.cast r in word64Le w end

    fun f64AtVec (v : Word8Vector.vector) (i : int) : real * int =
      let val w = word64FromVecLe v i
          val r : real = Unsafe.cast w
      in
        (r, i + 8)
      end
  end

  fun cat (chunks : Word8Vector.vector list) : Word8Vector.vector =
    Word8Vector.concat chunks

  fun u16At (v : Word8Vector.vector) (i : int) : int * int =
    let
      val b0 = Word8.toLargeWord (Word8Vector.sub (v, i))
      val b1 = Word8.toLargeWord (Word8Vector.sub (v, i + 1))
      val w = Word.orb (Word.fromLargeWord b0, Word.<< (Word.fromLargeWord b1, 0w8))
    in
      (Word.toInt w, i + 2)
    end

  fun u32ToWord32 (v : Word8Vector.vector) (i : int) : Word32.word =
    let
      val w0 = Word32.fromLarge (Word8.toLargeWord (Word8Vector.sub (v, i)))
      val w1 = Word32.<< (Word32.fromLarge (Word8.toLargeWord (Word8Vector.sub (v, i + 1))), 0w8)
      val w2 = Word32.<< (Word32.fromLarge (Word8.toLargeWord (Word8Vector.sub (v, i + 2))), 0w16)
      val w3 = Word32.<< (Word32.fromLarge (Word8.toLargeWord (Word8Vector.sub (v, i + 3))), 0w24)
    in
      Word32.orb (Word32.orb (w0, w1), Word32.orb (w2, w3))
    end

  fun u32At (v : Word8Vector.vector) (i : int) : int * int =
    let val w = u32ToWord32 v i in (Word32.toIntX w, i + 4) end

  fun i32At (v : Word8Vector.vector) (i : int) : Int32.int * int =
    let
      val w = u32ToWord32 v i
    in
      (Int32.fromLarge (Word32.toLargeIntX w), i + 4)
    end

  fun i64At (v : Word8Vector.vector) (i : int) : Int64.int * int =
    let
      val lw =
        List.foldl
          (fn (k, acc) =>
            LargeWord.orb (acc,
              LargeWord.<< (Word8.toLargeWord (Word8Vector.sub (v, i + k)), Word.fromInt (k * 8)))) 0w0
          (List.tabulate (8, fn x => x))
    in
      (Int64.fromLarge (LargeWord.toLargeIntX lw), i + 8)
    end

  datatype insn =
      HALT
    | POP
    | DUP
    | PUSH_UNIT
    | PUSH_I32 of Int32.int
    | PUSH_I64 of Int64.int
    | PUSH_F64 of real
    | PUSH_BOOL of bool
    | PUSH_STRING of int
    | ADD_I32 | SUB_I32 | MUL_I32 | DIV_I32 | MOD_I32 | NEG_I32
    | ADD_I64 | SUB_I64 | MUL_I64 | DIV_I64 | MOD_I64 | NEG_I64
    | ADD_F64 | SUB_F64 | MUL_F64 | DIV_F64 | NEG_F64
    | EQ | NEQ | LT | GT | LTE | GTE
    | AND | OR | NOT
    | BIT_AND | BIT_OR | BIT_XOR | BIT_NOT | SHL | SHR
    | LOAD_LOCAL of int
    | STORE_LOCAL of int
    | JUMP of int
    | JUMP_IF of int
    | JUMP_IF_NOT of int
    | CALL of int * int
    | RETURN
    | RETURN_SLOTS of int (* u8: pop n cells as return values (0 = void); replaces plain RETURN when n<>1 *)
    | CALL_BUILTIN of int
    | ALLOC_STRUCT of int
    | GET_FIELD of int
    | SET_FIELD of int
    | ALLOC_ARRAY of int
    | GET_INDEX
    | SET_INDEX
    | CONSTRUCT_VARIANT of int * int * int
    | GET_TAG
    | GET_VARIANT_FIELD of int
    | CONTRACT_CHECK of int
    | CAST of int * int

  fun encodeInsn insn =
    case insn of
      HALT => bytes1 (w8 0)
    | POP => bytes1 (w8 1)
    | DUP => bytes1 (w8 2)
    | PUSH_UNIT => bytes1 (w8 3)
    | PUSH_I32 x => cat [bytes1 (w8 4), i32Le x]
    | PUSH_I64 x => cat [bytes1 (w8 5), i64Le x]
    | PUSH_F64 x => cat [bytes1 (w8 6), f64Le x]
    | PUSH_BOOL true => cat [bytes1 (w8 7), bytes1 (w8 1)]
    | PUSH_BOOL false => cat [bytes1 (w8 7), bytes1 (w8 0)]
    | PUSH_STRING idx => cat [bytes1 (w8 8), u32Le idx]
    | ADD_I32 => bytes1 (w8 16)
    | SUB_I32 => bytes1 (w8 17)
    | MUL_I32 => bytes1 (w8 18)
    | DIV_I32 => bytes1 (w8 19)
    | MOD_I32 => bytes1 (w8 20)
    | NEG_I32 => bytes1 (w8 21)
    | ADD_I64 => bytes1 (w8 32)
    | SUB_I64 => bytes1 (w8 33)
    | MUL_I64 => bytes1 (w8 34)
    | DIV_I64 => bytes1 (w8 35)
    | MOD_I64 => bytes1 (w8 36)
    | NEG_I64 => bytes1 (w8 37)
    | ADD_F64 => bytes1 (w8 48)
    | SUB_F64 => bytes1 (w8 49)
    | MUL_F64 => bytes1 (w8 50)
    | DIV_F64 => bytes1 (w8 51)
    | NEG_F64 => bytes1 (w8 52)
    | EQ => bytes1 (w8 64)
    | NEQ => bytes1 (w8 65)
    | LT => bytes1 (w8 66)
    | GT => bytes1 (w8 67)
    | LTE => bytes1 (w8 68)
    | GTE => bytes1 (w8 69)
    | AND => bytes1 (w8 80)
    | OR => bytes1 (w8 81)
    | NOT => bytes1 (w8 82)
    | BIT_AND => bytes1 (w8 88)
    | BIT_OR => bytes1 (w8 89)
    | BIT_XOR => bytes1 (w8 90)
    | BIT_NOT => bytes1 (w8 91)
    | SHL => bytes1 (w8 92)
    | SHR => bytes1 (w8 93)
    | LOAD_LOCAL s => cat [bytes1 (w8 96), u32Le s]
    | STORE_LOCAL s => cat [bytes1 (w8 97), u32Le s]
    | JUMP off => cat [bytes1 (w8 112), i32Le (Int32.fromInt off)]
    | JUMP_IF off => cat [bytes1 (w8 113), i32Le (Int32.fromInt off)]
    | JUMP_IF_NOT off => cat [bytes1 (w8 114), i32Le (Int32.fromInt off)]
    | CALL (f, n) =>
        cat [bytes1 (w8 115), u32Le f, u32Le n]
    | RETURN => bytes1 (w8 116)
    | RETURN_SLOTS n =>
        if n < 0 orelse n > 255 then raise Fail "RETURN_SLOTS count out of range"
        else cat [bytes1 (w8 118), bytes1 (w8 n)]
    | CALL_BUILTIN id => cat [bytes1 (w8 117), u32Le id]
    | ALLOC_STRUCT t => cat [bytes1 (w8 128), u32Le t]
    | GET_FIELD f => cat [bytes1 (w8 129), u32Le f]
    | SET_FIELD f => cat [bytes1 (w8 130), u32Le f]
    | ALLOC_ARRAY n => cat [bytes1 (w8 131), u32Le n]
    | GET_INDEX => bytes1 (w8 132)
    | SET_INDEX => bytes1 (w8 133)
    | CONSTRUCT_VARIANT (t, vv, fc) =>
        cat [bytes1 (w8 144), u32Le t, u32Le vv, u32Le fc]
    | GET_TAG => bytes1 (w8 145)
    | GET_VARIANT_FIELD ix => cat [bytes1 (w8 146), u32Le ix]
    | CONTRACT_CHECK m => cat [bytes1 (w8 160), u32Le m]
    | CAST (a, b) => cat [bytes1 (w8 161), u16Le a, u16Le b]

  fun decodeInsnVec (v : Word8Vector.vector) (i : int) : insn * int =
    let val opc = Word8.toInt (Word8Vector.sub (v, i))
    in
      case opc of
        0 => (HALT, i + 1)
      | 1 => (POP, i + 1)
      | 2 => (DUP, i + 1)
      | 3 => (PUSH_UNIT, i + 1)
      | 4 =>
          let val (x, j) = i32At v (i + 1) in (PUSH_I32 x, j) end
      | 5 =>
          let val (x, j) = i64At v (i + 1) in (PUSH_I64 x, j) end
      | 6 =>
          let val (x, j) = f64AtVec v (i + 1) in (PUSH_F64 x, j) end
      | 7 =>
          let val b = Word8.toInt (Word8Vector.sub (v, i + 1))
          in (PUSH_BOOL (b <> 0), i + 2) end
      | 8 =>
          let val (x, j) = u32At v (i + 1) in (PUSH_STRING x, j) end
      | 16 => (ADD_I32, i + 1)
      | 17 => (SUB_I32, i + 1)
      | 18 => (MUL_I32, i + 1)
      | 19 => (DIV_I32, i + 1)
      | 20 => (MOD_I32, i + 1)
      | 21 => (NEG_I32, i + 1)
      | 32 => (ADD_I64, i + 1)
      | 33 => (SUB_I64, i + 1)
      | 34 => (MUL_I64, i + 1)
      | 35 => (DIV_I64, i + 1)
      | 36 => (MOD_I64, i + 1)
      | 37 => (NEG_I64, i + 1)
      | 48 => (ADD_F64, i + 1)
      | 49 => (SUB_F64, i + 1)
      | 50 => (MUL_F64, i + 1)
      | 51 => (DIV_F64, i + 1)
      | 52 => (NEG_F64, i + 1)
      | 64 => (EQ, i + 1)
      | 65 => (NEQ, i + 1)
      | 66 => (LT, i + 1)
      | 67 => (GT, i + 1)
      | 68 => (LTE, i + 1)
      | 69 => (GTE, i + 1)
      | 80 => (AND, i + 1)
      | 81 => (OR, i + 1)
      | 82 => (NOT, i + 1)
      | 88 => (BIT_AND, i + 1)
      | 89 => (BIT_OR, i + 1)
      | 90 => (BIT_XOR, i + 1)
      | 91 => (BIT_NOT, i + 1)
      | 92 => (SHL, i + 1)
      | 93 => (SHR, i + 1)
      | 96 =>
          let val (s, j) = u32At v (i + 1) in (LOAD_LOCAL s, j) end
      | 97 =>
          let val (s, j) = u32At v (i + 1) in (STORE_LOCAL s, j) end
      | 112 =>
          let val (x, j) = i32At v (i + 1) in (JUMP (Int32.toInt x), j) end
      | 113 =>
          let val (x, j) = i32At v (i + 1) in (JUMP_IF (Int32.toInt x), j) end
      | 114 =>
          let val (x, j) = i32At v (i + 1) in (JUMP_IF_NOT (Int32.toInt x), j) end
      | 115 =>
          let
            val (f, j1) = u32At v (i + 1)
            val (n, j2) = u32At v j1
          in
            (CALL (f, n), j2)
          end
      | 116 => (RETURN, i + 1)
      | 117 =>
          let val (id, j) = u32At v (i + 1) in (CALL_BUILTIN id, j) end
      | 118 =>
          let val n = Word8.toInt (Word8Vector.sub (v, i + 1))
          in (RETURN_SLOTS n, i + 2) end
      | 128 =>
          let val (t, j) = u32At v (i + 1) in (ALLOC_STRUCT t, j) end
      | 129 =>
          let val (f, j) = u32At v (i + 1) in (GET_FIELD f, j) end
      | 130 =>
          let val (f, j) = u32At v (i + 1) in (SET_FIELD f, j) end
      | 131 =>
          let val (n, j) = u32At v (i + 1) in (ALLOC_ARRAY n, j) end
      | 132 => (GET_INDEX, i + 1)
      | 133 => (SET_INDEX, i + 1)
      | 144 =>
          let
            val (t, j1) = u32At v (i + 1)
            val (vv, j2) = u32At v j1
            val (fc, j3) = u32At v j2
          in
            (CONSTRUCT_VARIANT (t, vv, fc), j3)
          end
      | 145 => (GET_TAG, i + 1)
      | 146 =>
          let val (ix, j) = u32At v (i + 1) in (GET_VARIANT_FIELD ix, j) end
      | 160 =>
          let val (m, j) = u32At v (i + 1) in (CONTRACT_CHECK m, j) end
      | 161 =>
          let
            val (a, j1) = u16At v (i + 1)
            val (b, j2) = u16At v j1
          in
            (CAST (a, b), j2)
          end
      | _ => raise Fail ("unknown opcode " ^ Int.toString opc)
    end

  fun decodeAll (v : Word8Vector.vector) : insn list =
    let
      val n = Word8Vector.length v
      fun loop i acc =
        if i >= n then List.rev acc
        else
          let val (insn, j) = decodeInsnVec v i in loop j (insn :: acc) end
    in
      loop 0 []
    end

  fun encodeAll (insns : insn list) : Word8Vector.vector =
    Word8Vector.concat (map encodeInsn insns)

  type func_entry =
    { nameIdx : int
    , arity : int
    , localCount : int
    , code : insn list
    }

  type program =
    { strings : string list
    , funcs : func_entry list
    }

  fun encodeStrings (ss : string list) : Word8Vector.vector =
    let
      val count = length ss
      val parts =
        map (fn s =>
          let
            val bytes = Byte.stringToBytes s
            val len = Word8Vector.length bytes
          in
            cat [u32Le len, bytes]
          end) ss
    in
      cat (u32Le count :: parts)
    end

  fun decodeStrings (v : Word8Vector.vector) (i : int) : string list * int =
    let
      val (cnt, p0) = u32At v i
      fun one k pos acc =
        if k >= cnt then (List.rev acc, pos)
        else
          let val (len, p1) = u32At v pos
              val s = Byte.bytesToString (Word8VectorSlice.vector
                (Word8VectorSlice.slice (v, p1, SOME len)))
          in
            one (k + 1) (p1 + len) (s :: acc)
          end
    in
      one 0 p0 []
    end

  fun encodeFile (p : program) : Word8Vector.vector =
    let
      val strSec = encodeStrings (#strings p)
      val codeParts =
        map (fn f => encodeAll (#code f)) (#funcs p)
      val codeBlob = Word8Vector.concat codeParts
      val (_, funcRecs) =
        List.foldl
          (fn (f, (off, recs)) =>
            let
              val c = encodeAll (#code f)
              val len = Word8Vector.length c
              val recBytes =
                cat
                  [ u32Le (#nameIdx f)
                  , u32Le (#arity f)
                  , u32Le (#localCount f)
                  , u32Le off
                  , u32Le len
                  ]
            in
              (off + len, recBytes :: recs)
            end)
          (0, []) (#funcs p)
      val funcCount = length (#funcs p)
      val funcBody = cat (u32Le funcCount :: rev funcRecs)
      val funcSec = cat [u32Le (Word8Vector.length funcBody), funcBody]
      val strBody = strSec
      val strSecWrapped = cat [u32Le (Word8Vector.length strBody), strBody]
      val codeSec = cat [u32Le (Word8Vector.length codeBlob), codeBlob]
      val header = cat [magicBytes, u16Le formatVersion]
    in
      cat [header, strSecWrapped, funcSec, codeSec]
    end

  fun decodeFile (v : Word8Vector.vector) : program =
    let
      val mlen = Word8Vector.length magicBytes
      val mag = Word8VectorSlice.vector (Word8VectorSlice.slice (v, 0, SOME mlen))
      val () =
        if not (Word8Vector.collate Word8.compare (mag, magicBytes) = EQUAL) then
          raise Fail "bad magic"
        else ()
      val (ver, p1) = u16At v mlen
      val () =
        if ver <> formatVersion then raise Fail "unsupported version" else ()
      val (strLen, p2) = u32At v p1
      val strPayload = Word8VectorSlice.vector (Word8VectorSlice.slice (v, p2, SOME strLen))
      val (strings, _) = decodeStrings strPayload 0
      val p3 = p2 + strLen
      val (ftLen, p4) = u32At v p3
      val ftEnd = p4 + ftLen
      val (codeLen, codeStart) = u32At v ftEnd
      val codeBlob = Word8VectorSlice.vector
        (Word8VectorSlice.slice (v, codeStart, SOME codeLen))
      val (funcCount, q0) = u32At v p4
      fun readFunc k q acc =
        if k >= funcCount then List.rev acc
        else
          let
            val (nameIdx, q1) = u32At v q
            val (arity, q2) = u32At v q1
            val (localCount, q3) = u32At v q2
            val (off, q4) = u32At v q3
            val (len, q5) = u32At v q4
            val codeVec =
              Word8VectorSlice.vector
                (Word8VectorSlice.slice (codeBlob, off, SOME len))
            val insns = decodeAll codeVec
          in
            readFunc (k + 1) q5
              ({ nameIdx = nameIdx
               , arity = arity
               , localCount = localCount
               , code = insns
               } :: acc)
          end
      val funcs = readFunc 0 q0 []
    in
      {strings = strings, funcs = funcs}
    end

end
