(* sv0vm coverage: the v1 companion binding, load-time COVER_HIT checks, and
   the counter arena (sv0cov CV-119, CV-120; sv0doc bytecode/coverage.md 3-4,
   sv0cov SPEC 14.2, 15.2, 15.3).

   A program is instrumented exactly when its code contains a COVER_HIT.
   Before any instruction runs, `load` (Interpreter.runFileBound first binds
   the companion to the raw file bytes, steps 2-3, before decoding it):
   1. reads the companion only when one was named explicitly; a file next
      to the bytecode confers nothing;
   2. validates it (`readBinding`): at most 4096 bytes, then exactly the
      canonical sv0cov.vm-binding 1.0 object -- the eleven properties in
      key order, no whitespace, one final LF, the fixed constants, 64-hex
      digests, canonical integers, a 1..128-byte printable compiler
      identity, and program_counter_count <= 4294967295. A closed canonical
      object has exactly one byte form, so matching that form is the schema
      check. Anything else is COV2201;
   3. binds it to these exact bytecode bytes: bytecode_length and
      bytecode_sha256 must match, else COV2202 (tampered, transplanted, or
      wrong-neighbour bytecode);
   4. `check`s the COVER_HIT operands against the count, else COV2201: any
      hit without a binding, an operand >= the count, a positive count with
      no hit;
   5. allocates the arena: n saturating u64 counters plus an overflow flag
      each. sv0vm executes one instruction at a time, so plain (non-atomic)
      storage gives the same observable counts as the native runtime's
      atomics (SPEC 14.2).
   `bound` holds the validated count; tests may set it directly. *)

structure Coverage =
struct
  exception Reject of string * string (* registry code, detail *)

  val bound : int option ref = ref NONE

  val counters : Word64.word array ref = ref (Array.array (0, 0w0))
  val overflow : bool array ref = ref (Array.array (0, false))
  val active : bool ref = ref false

  val maxCount : Word64.word = 0wxFFFFFFFFFFFFFFFF

  fun hitsOf (p : Bytecode.program) : int list =
    List.concat
      (map (fn f => List.mapPartial (fn Bytecode.COVER_HIT k => SOME k | _ => NONE) (#code f)) (#funcs p))

  fun check (p : Bytecode.program, binding : int option) : unit =
    let val hits = hitsOf p
    in
      case binding of
        NONE =>
          if null hits then ()
          else raise Reject ("COV2201",
            "the bytecode contains COVER_HIT but no coverage binding was supplied "
            ^ "(an sv0vm-v1-coverage program runs only with its .sv0covbind.json companion)")
      | SOME n =>
          (case List.find (fn k => k >= n) hits of
             SOME k => raise Reject ("COV2201",
               "COVER_HIT " ^ Int.toString k ^ " is outside the binding's " ^ Int.toString n ^ " counters")
           | NONE =>
               if n > 0 andalso null hits then
                 raise Reject ("COV2201",
                   "the binding counts " ^ Int.toString n ^ " counters but the bytecode has no COVER_HIT")
               else ())
    end

  (* ── the companion binding ─────────────────────────────────────────── *)

  type binding = {bytecodeLength : IntInf.int, bytecodeSha256 : string, compilerIdentity : string,
                  mapId : string, programCounterCount : int}

  val maxBindingBytes = 4096

  fun bad (detail : string) = raise Reject ("COV2201", "invalid coverage binding: " ^ detail)

  (* Parse `text` as exactly the canonical binding. *)
  fun readBinding (text : string) : binding =
    let
      val n = size text
      val () = if n > maxBindingBytes then bad "larger than 4096 bytes" else ()
      val pos = ref 0
      fun lit (s : string) =
        if !pos + size s <= n andalso String.substring (text, !pos, size s) = s
        then pos := !pos + size s
        else bad ("expected " ^ s ^ " at byte " ^ Int.toString (!pos))
      fun isDigit c = c >= #"0" andalso c <= #"9"
      fun isHex c = isDigit c orelse (c >= #"a" andalso c <= #"f")
      (* A canonical non-negative integer: no sign, no leading zero. *)
      fun integer (what : string) : IntInf.int =
        let
          val start = !pos
          fun go () = if !pos < n andalso isDigit (String.sub (text, !pos)) then (pos := !pos + 1; go ()) else ()
          val () = go ()
          val digits = String.substring (text, start, !pos - start)
        in
          if digits = "" then bad (what ^ " is not an integer")
          else if size digits > 1 andalso String.sub (digits, 0) = #"0" then bad (what ^ " has a leading zero")
          else if size digits > 20 then bad (what ^ " is out of range")
          else valOf (IntInf.fromString digits)
        end
      fun hex64 (what : string) : string =
        if !pos + 64 <= n andalso CharVector.all isHex (String.substring (text, !pos, 64))
        then (String.substring (text, !pos, 64) before pos := !pos + 64)
        else bad (what ^ " is not 64 lowercase hex digits")
      (* The identity: 0x21..0x7e, with canonical JSON's escapes \" and \\. *)
      fun identity () : string =
        let
          fun go acc =
            if !pos >= n then bad "unterminated compiler_identity"
            else
              let val c = String.sub (text, !pos)
              in
                if c = #"\"" then String.implode (List.rev acc)
                else if c = #"\\" then
                  if !pos + 1 < n andalso (String.sub (text, !pos + 1) = #"\"" orelse String.sub (text, !pos + 1) = #"\\")
                  then (pos := !pos + 2; go (String.sub (text, !pos - 1) :: acc))
                  else bad "compiler_identity has an escape canonical JSON does not use"
                else if Char.ord c < 0x21 orelse Char.ord c > 0x7e then bad "compiler_identity is not printable ASCII"
                else (pos := !pos + 1; go (c :: acc))
              end
          val id = go []
        in
          if size id < 1 orelse size id > 128 then bad "compiler_identity is not 1..128 bytes" else id
        end
      val () = lit "{\"bytecode_length\":"
      val len = integer "bytecode_length"
      val () = lit ",\"bytecode_sha256\":\""
      val sha = hex64 "bytecode_sha256"
      val () = lit "\",\"capabilities\":[\"sv0cov.coverage.v1\"],\"compiler_identity\":\""
      val ident = identity ()
      val () = lit "\",\"map_id\":\""
      val mapId = hex64 "map_id"
      val () = lit "\",\"plan_capability\":\"sv0cov.plan.v1\",\"profile\":\"sv0vm-v1-coverage\",\"program_counter_count\":"
      val count = integer "program_counter_count"
      val () = lit ",\"raw_profile_version\":\"1.0\",\"schema\":\"sv0cov.vm-binding\",\"version\":\"1.0\"}\n"
      val () = if !pos <> n then bad "trailing bytes after the object" else ()
      val () = if len < 1 orelse len > 18446744073709551615 then bad "bytecode_length is out of range" else ()
      val () = if count > 4294967295 then bad "program_counter_count is out of range" else ()
    in
      {bytecodeLength = len, bytecodeSha256 = sha, compilerIdentity = ident, mapId = mapId,
       programCounterCount = IntInf.toInt count}
    end

  (* The binding must name these exact bytecode bytes. *)
  fun bindTo (b : binding, bytecode : Word8Vector.vector) : unit =
    if IntInf.fromInt (Word8Vector.length bytecode) <> #bytecodeLength b
       orelse Sha256.hexOf bytecode <> #bytecodeSha256 b
    then raise Reject ("COV2202",
      "the coverage binding does not match this bytecode (its length or SHA-256 differs): "
      ^ "it belongs to another build or the bytecode was changed")
    else ()

  fun activate (n : int) : unit =
    ( counters := Array.array (n, 0w0)
    ; overflow := Array.array (n, false)
    ; active := true )

  fun reset () : unit =
    (bound := NONE; counters := Array.array (0, 0w0); overflow := Array.array (0, false); active := false)

  (* Steps 1-5 for one program: `bytecode` is the .sv0b bytes, `bindingText`
     the explicitly named companion's contents, if any. On success the arena
     is ready (or coverage is off for an uninstrumented, unbound program). *)
  fun load (p : Bytecode.program, bytecode : Word8Vector.vector, bindingText : string option) : unit =
    case bindingText of
      NONE => (bound := NONE; check (p, NONE))
    | SOME t =>
        let val b = readBinding t
        in
          bindTo (b, bytecode);
          check (p, SOME (#programCounterCount b));
          bound := SOME (#programCounterCount b)
        end

  (* One COVER_HIT: saturate at 2^64 - 1 and record the overflow. *)
  fun hit (k : int) : unit =
    if not (!active) then raise Fail "sv0vm: COVER_HIT executed without a coverage arena"
    else
      let val c = Array.sub (!counters, k)
      in
        if c = maxCount then Array.update (!overflow, k, true)
        else Array.update (!counters, k, c + 0w1)
      end
end
