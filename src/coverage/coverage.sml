(* sv0vm coverage: load-time COVER_HIT checks and the counter arena
   (sv0cov CV-119; sv0doc bytecode/coverage.md 3, sv0cov SPEC 14.2, 15.2).

   A program is instrumented exactly when its code contains a COVER_HIT.
   Before any instruction runs, `check` rejects (registry code COV2201):
   - an instrumented program with no coverage binding;
   - with a binding of n counters: any COVER_HIT operand >= n, and a
     positive n with no COVER_HIT at all (a zero n with a hit is covered by
     the operand rule).
   `bound` holds the binding's counter count; the --coverage-binding loader
   (CV-120) sets it, and tests set it directly.

   The arena is n saturating u64 counters plus an overflow flag each. sv0vm
   executes one instruction at a time, so plain (non-atomic) storage gives
   the same observable counts as the native runtime's atomics (SPEC 14.2). *)

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

  fun activate (n : int) : unit =
    ( counters := Array.array (n, 0w0)
    ; overflow := Array.array (n, false)
    ; active := true )

  fun reset () : unit =
    (bound := NONE; counters := Array.array (0, 0w0); overflow := Array.array (0, false); active := false)

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
