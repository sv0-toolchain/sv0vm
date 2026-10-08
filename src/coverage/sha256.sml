(* SHA-256 (FIPS 180-4) over a Word8Vector, for the coverage binding's
   bytecode digest (sv0cov CV-120; sv0doc bytecode/coverage.md 4.2). *)

structure Sha256 =
struct
  val k : Word32.word vector = Vector.fromList
    [ 0wx428a2f98, 0wx71374491, 0wxb5c0fbcf, 0wxe9b5dba5, 0wx3956c25b, 0wx59f111f1, 0wx923f82a4, 0wxab1c5ed5
    , 0wxd807aa98, 0wx12835b01, 0wx243185be, 0wx550c7dc3, 0wx72be5d74, 0wx80deb1fe, 0wx9bdc06a7, 0wxc19bf174
    , 0wxe49b69c1, 0wxefbe4786, 0wx0fc19dc6, 0wx240ca1cc, 0wx2de92c6f, 0wx4a7484aa, 0wx5cb0a9dc, 0wx76f988da
    , 0wx983e5152, 0wxa831c66d, 0wxb00327c8, 0wxbf597fc7, 0wxc6e00bf3, 0wxd5a79147, 0wx06ca6351, 0wx14292967
    , 0wx27b70a85, 0wx2e1b2138, 0wx4d2c6dfc, 0wx53380d13, 0wx650a7354, 0wx766a0abb, 0wx81c2c92e, 0wx92722c85
    , 0wxa2bfe8a1, 0wxa81a664b, 0wxc24b8b70, 0wxc76c51a3, 0wxd192e819, 0wxd6990624, 0wxf40e3585, 0wx106aa070
    , 0wx19a4c116, 0wx1e376c08, 0wx2748774c, 0wx34b0bcb5, 0wx391c0cb3, 0wx4ed8aa4a, 0wx5b9cca4f, 0wx682e6ff3
    , 0wx748f82ee, 0wx78a5636f, 0wx84c87814, 0wx8cc70208, 0wx90befffa, 0wxa4506ceb, 0wxbef9a3f7, 0wxc67178f2 ]

  fun rotr (x : Word32.word, n : word) : Word32.word =
    Word32.orb (Word32.>> (x, n), Word32.<< (x, 0w32 - n))

  (* The message padded to a multiple of 64 bytes: 0x80, zeros, and the
     bit length as a big-endian u64. *)
  fun pad (m : Word8Vector.vector) : Word8Vector.vector =
    let
      val len = Word8Vector.length m
      val zeros = (55 - len) mod 64
      val bits = Word64.fromInt len * 0w8
      val total = len + 1 + zeros + 8
    in
      Word8Vector.tabulate (total, fn i =>
        if i < len then Word8Vector.sub (m, i)
        else if i = len then 0wx80
        else if i < len + 1 + zeros then 0w0
        else Word8.fromLarge (Word64.toLarge (Word64.>> (bits, Word.fromInt (8 * (total - 1 - i))))))
    end

  fun word (v : Word8Vector.vector, i : int) : Word32.word =
    List.foldl (fn (j, acc) => Word32.orb (Word32.<< (acc, 0w8), Word32.fromLarge (Word8.toLarge (Word8Vector.sub (v, i + j)))))
      0w0 [0, 1, 2, 3]

  fun hash (m : Word8Vector.vector) : Word8Vector.vector =
    let
      val p = pad m
      val h = Array.fromList [ 0wx6a09e667, 0wxbb67ae85, 0wx3c6ef372, 0wxa54ff53a
                             , 0wx510e527f, 0wx9b05688c, 0wx1f83d9ab, 0wx5be0cd19 ] : Word32.word array
      val w = Array.array (64, 0w0 : Word32.word)
      fun block b =
        let
          val () = Array.modifyi (fn (t, _) => if t < 16 then word (p, b + 4 * t) else 0w0) w
          val () =
            List.app (fn t =>
              let
                val w15 = Array.sub (w, t - 15)
                val w2 = Array.sub (w, t - 2)
                val s0 = Word32.xorb (Word32.xorb (rotr (w15, 0w7), rotr (w15, 0w18)), Word32.>> (w15, 0w3))
                val s1 = Word32.xorb (Word32.xorb (rotr (w2, 0w17), rotr (w2, 0w19)), Word32.>> (w2, 0w10))
              in
                Array.update (w, t, Array.sub (w, t - 16) + s0 + Array.sub (w, t - 7) + s1)
              end) (List.tabulate (48, fn t => t + 16))
          fun round (t, (a, b', c, d, e, f, g, hh)) =
            let
              val s1 = Word32.xorb (Word32.xorb (rotr (e, 0w6), rotr (e, 0w11)), rotr (e, 0w25))
              val ch = Word32.xorb (Word32.andb (e, f), Word32.andb (Word32.notb e, g))
              val t1 = hh + s1 + ch + Vector.sub (k, t) + Array.sub (w, t)
              val s0 = Word32.xorb (Word32.xorb (rotr (a, 0w2), rotr (a, 0w13)), rotr (a, 0w22))
              val maj = Word32.xorb (Word32.xorb (Word32.andb (a, b'), Word32.andb (a, c)), Word32.andb (b', c))
            in
              (t1 + s0 + maj, a, b', c, d + t1, e, f, g)
            end
          val (a, b', c, d, e, f, g, hh) =
            List.foldl round
              (Array.sub (h, 0), Array.sub (h, 1), Array.sub (h, 2), Array.sub (h, 3),
               Array.sub (h, 4), Array.sub (h, 5), Array.sub (h, 6), Array.sub (h, 7))
              (List.tabulate (64, fn t => t))
          val () = List.app (fn (i, x) => Array.update (h, i, Array.sub (h, i) + x))
            [(0, a), (1, b'), (2, c), (3, d), (4, e), (5, f), (6, g), (7, hh)]
        in
          ()
        end
      fun blocks b = if b >= Word8Vector.length p then () else (block b; blocks (b + 64))
      val () = blocks 0
    in
      Word8Vector.tabulate (32, fn i =>
        Word8.fromLarge (Word32.toLarge (Word32.>> (Array.sub (h, i div 4), Word.fromInt (24 - 8 * (i mod 4))))))
    end

  fun hex (v : Word8Vector.vector) : string =
    let val d = "0123456789abcdef"
    in
      String.concat (Word8Vector.foldr (fn (b, acc) =>
        let val x = Word8.toInt b
        in String.str (String.sub (d, x div 16)) :: String.str (String.sub (d, x mod 16)) :: acc end) [] v)
    end

  fun hexOf (v : Word8Vector.vector) : string = hex (hash v)
end
