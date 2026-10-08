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

  (* collection state (CV-121, below) *)
  val publish : bool ref = ref false
  val required : bool ref = ref false
  val profileDir : string ref = ref ""
  val runId : Word8Vector.vector ref = ref (Word8Vector.fromList [])
  val profileId : Word8Vector.vector ref = ref (Word8Vector.fromList [])
  val context : string option ref = ref NONE
  val mapIdBytes : Word8Vector.vector ref = ref (Word8Vector.fromList [])
  (* Test-only fixtures (never set from the environment): a fixed profile ID
     and backend flag for byte parity with the sv0cov goldens. *)
  val testProfileId : Word8Vector.vector option ref = ref NONE
  val testBackendFlag : Word32.word option ref = ref NONE


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
    (bound := NONE; counters := Array.array (0, 0w0); overflow := Array.array (0, false); active := false;
     publish := false; required := false; context := NONE; testProfileId := NONE; testBackendFlag := NONE)

  (* ── collection: transport, profile ID, flush (CV-121) ──────────────────
     Mirrors the native runtime (sv0cov runtime/c/sv0cov_rt.c, SPEC 14.3,
     16.4, 23.4). With a binding, before any instruction runs:
     - the transport: exactly SV0COV_PROFILE_DIR (absolute, an existing
       directory), SV0COV_RUN_ID (exact lowercase 32-hex, nonzero),
       SV0COV_CONTEXT (optional, <= 256 bytes of strict UTF-8; empty is
       distinct from absent) and SV0COV_REQUIRED (0 or 1; anything else
       counts as required, failing closed). Any other SV0COV_* name, a
       duplicate, a malformed or missing value is COV2001;
     - the profile ID: 16 bytes from /dev/urandom, never all zero and never
       a fallback, else COV2002;
     - a map above the standard raw-profile tier (4194304 counters) is
       COV6001.
     A failure prints one diagnostic naming the variable, never a value. In
     required mode the program then does not run (the loader raises Reject,
     exit status 1); otherwise it runs and counts, but nothing is
     published. After the program returns -- normally or through a contract
     failure, both of which end runWithStack with an exit code -- `flush`
     writes <run_id>-<profile_id>.sv0profraw: a mode-0600 temporary with an
     unpredictable name, fsynced and closed, then committed with link() (an
     atomic no-replace) and the temporary removed; the directory is fsynced.
     A crash (an uncaught exception) publishes nothing. Flush failures are
     COV2010 (I/O) and COV2112 (the name exists; that file is kept); in
     required mode they make the exit status 1. *)

  val tierMaxCounters = 4194304

  fun diag (code : string, title : string, detail : string) =
    TextIO.output (TextIO.stdErr, "sv0vm: error[" ^ code ^ "]: " ^ title ^ ": " ^ detail ^ "\n")

  fun hexBytes (s : string) : Word8Vector.vector option =
    let
      fun v c = if c >= #"0" andalso c <= #"9" then SOME (Char.ord c - 48)
                else if c >= #"a" andalso c <= #"f" then SOME (Char.ord c - 87) else NONE
      val n = size s div 2
    in
      if size s mod 2 <> 0 then NONE
      else
        let
          val ds = List.tabulate (n, fn i => (v (String.sub (s, 2 * i)), v (String.sub (s, 2 * i + 1))))
        in
          if List.all (fn (SOME _, SOME _) => true | _ => false) ds
          then SOME (Word8Vector.fromList (map (fn (SOME a, SOME b) => Word8.fromInt (16 * a + b) | _ => 0w0) ds))
          else NONE
        end
    end

  fun allZero (v : Word8Vector.vector) = Word8Vector.all (fn b => b = 0w0) v

  (* Strict UTF-8: no overlong forms, surrogates, or code points above U+10FFFF. *)
  fun validUtf8 (s : string) : bool =
    let
      val n = size s
      fun b i = Char.ord (String.sub (s, i))
      fun cont i = i < n andalso b i >= 0x80 andalso b i < 0xc0
      fun go i =
        if i >= n then true
        else
          let val c = b i
              fun seq (len, min, init) =
                if i + len > n orelse not (List.all (fn k => cont (i + k)) (List.tabulate (len - 1, fn k => k + 1)))
                then false
                else
                  let val cp = List.foldl (fn (k, acc) => acc * 64 + (b (i + k) - 0x80)) init
                                 (List.tabulate (len - 1, fn k => k + 1))
                  in cp >= min andalso cp <= 0x10ffff andalso not (cp >= 0xd800 andalso cp <= 0xdfff)
                     andalso go (i + len) end
          in
            if c < 0x80 then go (i + 1)
            else if c >= 0xc2 andalso c <= 0xdf then seq (2, 0x80, c - 0xc0)
            else if c >= 0xe0 andalso c <= 0xef then seq (3, 0x800, c - 0xe0)
            else if c >= 0xf0 andalso c <= 0xf4 then seq (4, 0x10000, c - 0xf0)
            else false
          end
    in go 0 end

  val transportNames = ["SV0COV_PROFILE_DIR", "SV0COV_RUN_ID", "SV0COV_CONTEXT", "SV0COV_REQUIRED"]

  (* Read the transport into the refs above; returns NONE or SOME reason. *)
  fun readTransport () : string option =
    let
      val vars = List.filter (String.isPrefix "SV0COV_") (Posix.ProcEnv.environ ())
      fun split e = case CharVector.findi (fn (_, c) => c = #"=") e of
                      SOME (i, _) => (String.substring (e, 0, i), SOME (String.extract (e, i + 1, NONE)))
                    | NONE => (e, NONE)
      val pairs = map split vars
      fun value name = List.mapPartial (fn (k, v) => if k = name then v else NONE) pairs
      val unknown = List.exists (fn (k, v) => not (List.exists (fn n => n = k) transportNames) orelse not (isSome v)) pairs
      val dup = List.find (fn n => length (value n) > 1) transportNames
      val req = value "SV0COV_REQUIRED"
      val () = required := (case req of [] => false | ["0"] => false | _ => true)
      val reqBad = case req of [] => false | ["0"] => false | ["1"] => false | _ => true
    in
      if unknown then SOME "an unknown SV0COV_* variable is set (only SV0COV_PROFILE_DIR, SV0COV_RUN_ID, SV0COV_CONTEXT and SV0COV_REQUIRED are recognized)"
      else if isSome dup then SOME (valOf dup ^ " is set more than once")
      else if reqBad then SOME "SV0COV_REQUIRED is not 0 or 1"
      else
        case (value "SV0COV_PROFILE_DIR", value "SV0COV_RUN_ID") of
          ([], _) => SOME "SV0COV_PROFILE_DIR is not set"
        | ([dir], run) =>
            if not (String.isPrefix "/" dir)
               orelse not ((Posix.FileSys.ST.isDir (Posix.FileSys.stat dir)) handle OS.SysErr _ => false)
            then SOME "SV0COV_PROFILE_DIR is not an absolute path to an existing directory"
            else
              (case run of
                 [] => SOME "SV0COV_RUN_ID is not set"
               | [r] =>
                   (case (if size r = 32 then hexBytes r else NONE) of
                      NONE => SOME "SV0COV_RUN_ID is not 32 lowercase hex digits naming a nonzero run"
                    | SOME bytes =>
                        if allZero bytes then SOME "SV0COV_RUN_ID is not 32 lowercase hex digits naming a nonzero run"
                        else
                          (case value "SV0COV_CONTEXT" of
                             [c] =>
                               if size c > 256 orelse not (validUtf8 c)
                               then SOME "SV0COV_CONTEXT is longer than 256 bytes or not valid UTF-8"
                               else (profileDir := dir; runId := bytes; context := SOME c; NONE)
                           | _ => (profileDir := dir; runId := bytes; context := NONE; NONE)))
               | _ => SOME "SV0COV_RUN_ID is set more than once")
        | _ => SOME "SV0COV_PROFILE_DIR is set more than once"
    end

  fun entropy () : Word8Vector.vector option =
    case !testProfileId of
      SOME v => SOME v
    | NONE =>
        (let
           val ins = BinIO.openIn "/dev/urandom"
           val v = BinIO.inputN (ins, 16) before BinIO.closeIn ins
         in
           if Word8Vector.length v = 16 then SOME v else NONE
         end
         handle IO.Io _ => NONE)

  (* A failure before the program runs: required mode refuses to run it. *)
  fun startFailed (code : string, title : string, detail : string) =
    if !required then raise Reject (code, title ^ ": " ^ detail ^ " (SV0COV_REQUIRED=1: the program does not run)")
    else (diag (code, title, detail); publish := false)

  (* Called by `load` once a binding of n counters for map `mapId` is valid. *)
  fun startCollection (n : int, mapId : string) : unit =
    ( publish := false
    ; required := false
    ; if n > tierMaxCounters then
        raise Reject ("COV6001", "resource limit exceeded: the map has more counters than the standard raw-profile tier allows (4194304)")
      else ()
    ; mapIdBytes := valOf (hexBytes mapId)
    ; case readTransport () of
        SOME why => startFailed ("COV2001", "invalid runtime transport", why)
      | NONE =>
          (case entropy () of
             NONE => startFailed ("COV2002", "runtime entropy unavailable", "/dev/urandom could not supply a profile ID")
           | SOME id =>
               if allZero id then startFailed ("COV2002", "runtime entropy unavailable", "the profile ID came out all zero")
               else (profileId := id; publish := true)) )

  fun hexOf (v : Word8Vector.vector) : string =
    String.concat (Word8Vector.foldr (fn (b, acc) =>
      let val x = Word8.toInt b val d = "0123456789abcdef"
      in String.str (String.sub (d, x div 16)) :: String.str (String.sub (d, x mod 16)) :: acc end) [] v)

  (* The profile bytes for the current arena. *)
  fun profileBytes () : Word8Vector.vector =
    let
      val n = Array.length (!counters)
      val counts = Array.foldri (fn (i, c, acc) => if c = 0w0 then acc else (i, c) :: acc) [] (!counters)
    in
      RawProfile.encode {flags = getOpt (!testBackendFlag, RawProfile.backendVmV1), mapId = !mapIdBytes,
                         runId = !runId, profileId = !profileId, context = !context, counts = counts, n = n}
    end

  (* Publish the profile; NONE when done or not collecting, SOME 1 when
     required mode turns a failure into the exit status. *)
  fun flush () : int option =
    if not (!publish) then NONE
    else
      let
        val () = publish := false
        val name = hexOf (!runId) ^ "-" ^ hexOf (!profileId) ^ ".sv0profraw"
        val dst = !profileDir ^ "/" ^ name
        val tmp = !profileDir ^ "/." ^ name ^ ".tmp-"
                  ^ (case entropy () of SOME v => hexOf v | NONE => hexOf (!profileId))
        val bytes = profileBytes ()
        fun fail (code, title, detail) =
          (diag (code, title, detail); if !required then SOME 1 else NONE)
        open Posix.FileSys
      in
        (let
           val fd = createf (tmp, O_WRONLY, O.excl, S.flags [S.irusr, S.iwusr])
           fun write off =
             if off >= Word8Vector.length bytes then ()
             else write (off + Posix.IO.writeVec (fd, Word8VectorSlice.slice (bytes, off, NONE)))
           val () = (write 0; Posix.IO.fsync fd; Posix.IO.close fd)
                    handle e => (Posix.IO.close fd handle _ => (); unlink tmp handle _ => (); raise e)
         in
           (link {old = tmp, new = dst}; unlink tmp;
            (let val dfd = openf (!profileDir, O_RDONLY, O.flags [])
             in (Posix.IO.fsync dfd handle _ => ()); Posix.IO.close dfd end) handle _ => ();
            NONE)
           handle OS.SysErr (_, SOME e) =>
             (unlink tmp handle _ => ();
              if e = Posix.Error.exist
              then fail ("COV2112", "raw-profile identity collision", "a profile with this run and profile ID already exists")
              else fail ("COV2010", "raw profile unavailable", "committing the profile failed"))
         end)
        handle OS.SysErr _ => fail ("COV2010", "raw profile unavailable", "writing the profile into SV0COV_PROFILE_DIR failed")
      end

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
          bound := SOME (#programCounterCount b);
          startCollection (#programCounterCount b, #mapId b)
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
