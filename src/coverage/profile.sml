(* sv0cov raw profile 1.0 encoder (sv0cov CV-121; SPEC 16.4).

   CRC32C (Castagnoli, reflected 0x82f63b78, init and final xor 0xffffffff)
   and the little-endian sparse container:

     magic "SV0PRF\0\0", u16 major 1, u16 minor 0, u32 flags,
     map_id[32], run_id[16], profile_id[16],
     u32 context_length, context bytes,
     u32 pair_count, (u32 index, u64 count) per nonzero counter in index order,
     u32 overflow_word_count, u64 words (only when a count is 2^64 - 1),
     "SV0DONE!", u32 crc32c of everything before it.

   Flags: 0x01 CONTEXT_PRESENT, 0x02 OVERFLOW_PRESENT, 0x04 BACKEND_NATIVE,
   0x08 BACKEND_VM_V1, 0x10 BACKEND_VM_V2. The overflow bitmap marks exactly
   the counts equal to 2^64 - 1 (a lower bound from then on), which is what
   the format requires of a set bit. *)

structure RawProfile =
struct
  val contextPresent : Word32.word = 0wx01
  val overflowPresent : Word32.word = 0wx02
  val backendVmV1 : Word32.word = 0wx08

  val crcTable : Word32.word vector =
    Vector.tabulate (256, fn i =>
      let
        fun step (0, c) = c
          | step (k, c) =
              step (k - 1, if Word32.andb (c, 0w1) = 0w1 then Word32.xorb (Word32.>> (c, 0w1), 0wx82f63b78)
                           else Word32.>> (c, 0w1))
      in step (8, Word32.fromInt i) end)

  fun crc32c (v : Word8Vector.vector) : Word32.word =
    Word32.xorb (0wxffffffff,
      Word8Vector.foldl (fn (b, c) =>
        Word32.xorb (Word32.>> (c, 0w8),
          Vector.sub (crcTable, Word32.toInt (Word32.andb (Word32.xorb (c, Word32.fromLarge (Word8.toLarge b)), 0wxff)))))
        0wxffffffff v)

  fun le (bytes : int) (x : LargeWord.word) : Word8Vector.vector =
    Word8Vector.tabulate (bytes, fn i => Word8.fromLarge (LargeWord.>> (x, Word.fromInt (8 * i))))
  fun u32 (x : int) = le 4 (LargeWord.fromInt x)
  fun u64 (x : Word64.word) = le 8 (Word64.toLarge x)

  val maxCount : Word64.word = 0wxFFFFFFFFFFFFFFFF

  (* counts: the nonzero (index, count) pairs in increasing index order, for
     a map of n counters. *)
  fun encode {flags : Word32.word, mapId : Word8Vector.vector, runId : Word8Vector.vector,
              profileId : Word8Vector.vector, context : string option,
              counts : (int * Word64.word) list, n : int} : Word8Vector.vector =
    let
      val saturated = List.filter (fn (_, c) => c = maxCount) counts
      val words =
        if null saturated then []
        else
          let val a = Array.array ((n + 63) div 64, 0w0 : Word64.word)
          in
            app (fn (i, _) =>
              Array.update (a, i div 64, Word64.orb (Array.sub (a, i div 64), Word64.<< (0w1, Word.fromInt (i mod 64)))))
              saturated;
            Array.foldr (op ::) [] a
          end
      val flags = Word32.orb (flags, Word32.orb (if isSome context then contextPresent else 0w0,
                                                 if null saturated then 0w0 else overflowPresent))
      val ctx = Byte.stringToBytes (getOpt (context, ""))
      val body = Word8Vector.concat
        ([ Byte.stringToBytes "SV0PRF\000\000", le 2 0w1, le 2 0w0, le 4 (Word32.toLarge flags)
         , mapId, runId, profileId, u32 (Word8Vector.length ctx), ctx, u32 (length counts) ]
         @ List.concat (map (fn (i, c) => [u32 i, u64 c]) counts)
         @ [u32 (length words)] @ map u64 words
         @ [Byte.stringToBytes "SV0DONE!"])
    in
      Word8Vector.concat [body, le 4 (Word32.toLarge (crc32c body))]
    end
end
