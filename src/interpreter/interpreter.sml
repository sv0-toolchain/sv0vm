(* Stack interpreter: byte IP; call stack stores return IP in caller frame. *)

structure Interpreter = struct

  structure B = Bytecode

  datatype cell = CInt of int | CBool of bool | CUnit | CStrIdx of int

  fun truthy c =
    case c of
      CBool b => b
    | CInt i => i <> 0
    | CUnit => false
    | CStrIdx _ => true

  type func_rec =
    { nameIdx : int
    , arity : int
    , localCount : int
    , bytes : Word8Vector.vector
    }

  type activ = {f : func_rec, ip : int, loc : cell array}

  type loaded =
    { strings : string vector
    , funcs : func_rec vector
    }

  fun loadProgram (p : B.program) : loaded =
    let
      val ss = Vector.fromList (#strings p)
      val fs =
        Vector.fromList
          (map
            (fn f =>
              { nameIdx = #nameIdx f
              , arity = #arity f
              , localCount = #localCount f
              , bytes = B.encodeAll (#code f)
              }) (#funcs p))
    in
      {strings = ss, funcs = fs}
    end

  fun loadFileVec (v : Word8Vector.vector) : loaded =
    loadProgram (B.decodeFile v)

  fun findMain (ld : loaded) : int =
    let
      val n = Vector.length (#funcs ld)
      fun go i =
        if i >= n then raise Fail "interpreter: no main in bytecode"
        else
          let val f = Vector.sub (#funcs ld, i)
              val name = Vector.sub (#strings ld, #nameIdx f)
          in
            if name = "main" then i else go (i + 1)
          end
    in
      go 0
    end

  fun pop stack =
    case !stack of
      h :: t => (stack := t; h)
    | [] => raise Fail "interpreter: stack underflow"

  fun push stack x = stack := x :: !stack

  fun popN stack n =
    let fun go 0 acc = acc
          | go k acc = go (k - 1) (pop stack :: acc)
    in
      go n []
    end

  fun arithII opFn stack =
    let val b = pop stack
        val a = pop stack
    in
      case (a, b) of
        (CInt x, CInt y) => push stack (CInt (opFn (x, y)))
      | _ => raise Fail "interpreter: arithmetic on non-int"
    end

  (* C-style: bool promotes to 0/1 for relational ops (forall/exists lowering uses pred == 0). *)
  fun cellAsIntForCmp c =
    case c of
      CInt i => i
    | CBool b => if b then 1 else 0
    | _ => raise Fail "interpreter: compare on non-int"

  fun cmp rel stack =
    let val b = pop stack
        val a = pop stack
    in
      case (a, b) of
        (CInt x, CInt y) => push stack (CBool (rel (x, y)))
      | (CBool _, _) =>
          push stack (CBool (rel (cellAsIntForCmp a, cellAsIntForCmp b)))
      | (_, CBool _) =>
          push stack (CBool (rel (cellAsIntForCmp a, cellAsIntForCmp b)))
      | _ => raise Fail "interpreter: compare on non-int"
    end

  fun asWord32 i = Word32.fromLargeInt (Int.toLarge i)

  fun runWithStack (ld : loaded) (mainIdx : int) (stack : cell list ref) : int =
    let
      val funcs = #funcs ld
      val strings = #strings ld
      val dynStrings = ref ([] : string list)
      val dynCount = ref 0
      fun lookupStr i =
        if i < Vector.length strings then Vector.sub (strings, i)
        else List.nth (!dynStrings, i - Vector.length strings)
      fun addStr s =
        let val idx = Vector.length strings + !dynCount
        in dynStrings := !dynStrings @ [s]; dynCount := !dynCount + 1; idx end

      val dynVecs : int list ref list ref = ref []
      val dynVecCount = ref 0
      fun vecNew () =
        let val idx = !dynVecCount
        in dynVecs := !dynVecs @ [ref []]; dynVecCount := !dynVecCount + 1; idx end
      fun vecRef h = List.nth (!dynVecs, h)

      val boxPool : int Array.array = Array.array (65536, 0)
      val boxNext = ref 0
      fun boxAlloc nwords =
        let val h = !boxNext
        in boxNext := h + nwords; h end
      fun boxStore h off v = Array.update (boxPool, h + off, v)
      fun boxLoad h off = Array.sub (boxPool, h + off)
      fun vecPush h v = let val r = vecRef h in r := !r @ [v] end
      fun vecLen h = length (!(vecRef h))
      fun vecGet h i = List.nth (!(vecRef h), i)
      fun vecSet h i v =
        let val r = vecRef h
            val old = !r
        in r := List.tabulate (length old,
             fn k => if k = i then v else List.nth (old, k))
        end

      val frames = ref ([] : activ list)

      fun setTopIp newIp =
        case !frames of
          {f, loc, ...} :: rest => frames := {f = f, ip = newIp, loc = loc} :: rest
        | [] => ()

      fun step () : bool =
        case !frames of
          [] => false
        | {f, ip, loc} :: _ =>
            if ip >= Word8Vector.length (#bytes f) then false
            else
              let
                val (insn, nextIp) = B.decodeInsnVec (#bytes f) ip
              in
                case insn of
                  B.HALT => (frames := []; false)
                | B.POP => (pop stack; setTopIp nextIp; true)
                | B.DUP =>
                    let val x = pop stack in push stack x; push stack x; setTopIp nextIp; true end
                | B.PUSH_UNIT => (push stack CUnit; setTopIp nextIp; true)
                | B.PUSH_I32 x =>
                    (push stack (CInt (Int32.toInt x)); setTopIp nextIp; true)
                | B.PUSH_BOOL b => (push stack (CBool b); setTopIp nextIp; true)
                | B.PUSH_STRING i => (push stack (CStrIdx i); setTopIp nextIp; true)
                | B.ADD_I32 => (arithII (fn (a, b) => a + b) stack; setTopIp nextIp; true)
                | B.SUB_I32 => (arithII (fn (a, b) => a - b) stack; setTopIp nextIp; true)
                | B.MUL_I32 => (arithII (fn (a, b) => a * b) stack; setTopIp nextIp; true)
                | B.DIV_I32 =>
                    ( case !stack of
                        CInt 0 :: _ => raise Fail "interpreter: division by zero"
                      | _ => ();
                      arithII (fn (a, b) => a div b) stack;
                      setTopIp nextIp;
                      true)
                | B.MOD_I32 => (arithII (fn (a, b) => a mod b) stack; setTopIp nextIp; true)
                | B.NEG_I32 =>
                    ( case pop stack of
                        CInt x => push stack (CInt (~x))
                      | _ => raise Fail "interpreter: NEG_I32";
                      setTopIp nextIp;
                      true)
                | B.EQ => (cmp (fn (a, b) => a = b) stack; setTopIp nextIp; true)
                | B.NEQ => (cmp (fn (a, b) => a <> b) stack; setTopIp nextIp; true)
                | B.LT => (cmp (fn (a, b) => a < b) stack; setTopIp nextIp; true)
                | B.GT => (cmp (fn (a, b) => a > b) stack; setTopIp nextIp; true)
                | B.LTE => (cmp (fn (a, b) => a <= b) stack; setTopIp nextIp; true)
                | B.GTE => (cmp (fn (a, b) => a >= b) stack; setTopIp nextIp; true)
                | B.AND =>
                    let val b = pop stack
                        val a = pop stack
                    in
                      push stack (CBool (truthy a andalso truthy b));
                      setTopIp nextIp;
                      true
                    end
                | B.OR =>
                    let val b = pop stack
                        val a = pop stack
                    in
                      push stack (CBool (truthy a orelse truthy b));
                      setTopIp nextIp;
                      true
                    end
                | B.NOT =>
                    ( case pop stack of
                        CBool b => push stack (CBool (not b))
                      | CInt i => push stack (CBool (i = 0))
                      | _ => raise Fail "interpreter: NOT";
                      setTopIp nextIp;
                      true)
                | B.BIT_AND =>
                    ( arithII (fn (a, b) => Word32.toIntX (Word32.andb (asWord32 a, asWord32 b))) stack;
                      setTopIp nextIp;
                      true)
                | B.BIT_OR =>
                    ( arithII (fn (a, b) => Word32.toIntX (Word32.orb (asWord32 a, asWord32 b))) stack;
                      setTopIp nextIp;
                      true)
                | B.BIT_XOR =>
                    ( arithII (fn (a, b) => Word32.toIntX (Word32.xorb (asWord32 a, asWord32 b))) stack;
                      setTopIp nextIp;
                      true)
                | B.BIT_NOT =>
                    ( case pop stack of
                        CInt x =>
                          push stack (CInt (Word32.toIntX (Word32.notb (asWord32 x))))
                      | _ => raise Fail "interpreter: BIT_NOT";
                      setTopIp nextIp;
                      true)
                | B.SHL =>
                    ( arithII (fn (a, b) =>
                        let val sh = Word.fromInt (b mod 32)
                        in Word32.toIntX (Word32.<< (asWord32 a, sh)) end) stack;
                      setTopIp nextIp;
                      true)
                | B.SHR =>
                    ( arithII (fn (a, b) =>
                        let val sh = Word.fromInt (b mod 32)
                        in Word32.toIntX (Word32.~>> (asWord32 a, sh)) end) stack;
                      setTopIp nextIp;
                      true)
                | B.LOAD_LOCAL s =>
                    (push stack (Array.sub (loc, s)); setTopIp nextIp; true)
                | B.STORE_LOCAL s =>
                    let val x = pop stack in Array.update (loc, s, x); setTopIp nextIp; true end
                | B.JUMP off => (setTopIp (nextIp + off); true)
                | B.JUMP_IF off =>
                    let val c = pop stack
                    in
                      if truthy c then setTopIp (nextIp + off) else setTopIp nextIp;
                      true
                    end
                | B.JUMP_IF_NOT off =>
                    let val c = pop stack
                    in
                      if truthy c then setTopIp nextIp else setTopIp (nextIp + off);
                      true
                    end
                | B.CALL (fidx, na) =>
                    let
                      (* popN returns args in call order (left-to-right): each pop takes TOS,
                         so after n pops the list is [first-arg, ..., last-arg]. Do not reverse:
                         callee param slot 0 must bind the first formal. *)
                      val args = popN stack na
                      val callee = Vector.sub (funcs, fidx)
                      val locN = Array.array (#localCount callee, CUnit)
                      fun fill i [] = ()
                        | fill i (a :: rest) =
                            ( Array.update (locN, i, a); fill (i + 1) rest)
                      val () = fill 0 args
                      val () =
                        case !frames of
                          cur :: rest =>
                            frames :=
                              {f = callee, ip = 0, loc = locN}
                              :: {f = #f cur, ip = nextIp, loc = #loc cur} :: rest
                        | [] => raise Fail "interpreter: CALL with no frame"
                    in
                      true
                    end
                | B.RETURN => (* legacy single return *)
                    let val vals = rev (popN stack 1)
                    in
                      case !frames of
                        [_] => (app (push stack) vals; frames := []; false)
                      | _ :: caller :: rest =>
                          (frames := caller :: rest; app (push stack) vals; true)
                      | [] => raise Fail "interpreter: RETURN"
                    end
                | B.RETURN_SLOTS n =>
                    let
                      (* Same order as CALL: popN yields cells from first-returned slot to last;
                         push them in that order so TOS is the last slot (matches STORE_LOCAL
                         sequence after multi-slot CALL). *)
                      val vals = popN stack n
                    in
                      case !frames of
                        [_] => (app (push stack) vals; frames := []; false)
                      | _ :: caller :: rest =>
                          (frames := caller :: rest; app (push stack) vals; true)
                      | [] => raise Fail "interpreter: RETURN_SLOTS"
                    end
                | B.CALL_BUILTIN bid =>
                    if bid = 0 then
                      case pop stack of
                        CStrIdx i =>
                          (TextIO.print (lookupStr i ^ "\n"); setTopIp nextIp; true)
                      | _ => raise Fail "interpreter: println expects string index"
                    else if bid = 1 then
                      (case (pop stack, pop stack) of
                        (CInt b, CInt a) =>
                          (push stack (CBool (a <> b)); setTopIp nextIp; true)
                      | _ => raise Fail "interpreter: builtin 1 (no_alias) expects two int cells")
                    else if bid = 2 then
                      case pop stack of
                        CStrIdx i =>
                          (push stack (CInt (size (lookupStr i))); setTopIp nextIp; true)
                      | _ => raise Fail "interpreter: string_len expects string index"
                    else if bid = 3 then
                      (case (pop stack, pop stack) of
                        (CStrIdx bi, CStrIdx ai) =>
                          (push stack (CBool (lookupStr ai = lookupStr bi)); setTopIp nextIp; true)
                      | _ => raise Fail "interpreter: string_eq expects two string indices")
                    else if bid = 4 then
                      (case (pop stack, pop stack) of
                        (CStrIdx bi, CStrIdx ai) =>
                          let val s = lookupStr ai ^ lookupStr bi
                              val idx = addStr s
                          in push stack (CStrIdx idx); setTopIp nextIp; true end
                      | _ => raise Fail "interpreter: string_concat expects two string indices")
                    else if bid = 5 then
                      (case (pop stack, pop stack) of
                        (CInt idx, CStrIdx si) =>
                          let val s = lookupStr si
                          in push stack (CInt (Char.ord (String.sub (s, idx)))); setTopIp nextIp; true end
                      | _ => raise Fail "interpreter: string_char_at expects string index and int")
                    else if bid = 6 then
                      (case (pop stack, pop stack, pop stack) of
                        (CInt len, CInt start, CStrIdx si) =>
                          let val s = String.substring (lookupStr si, start, len)
                              val idx = addStr s
                          in push stack (CStrIdx idx); setTopIp nextIp; true end
                      | _ => raise Fail "interpreter: string_substr expects string index and two ints")
                    else if bid = 7 then
                      (push stack (CInt (vecNew ())); setTopIp nextIp; true)
                    else if bid = 8 then
                      (case (pop stack, pop stack) of
                        (CInt elem, CInt h) =>
                          (vecPush h elem; setTopIp nextIp; true)
                      | _ => raise Fail "interpreter: vec_push expects handle and int")
                    else if bid = 9 then
                      case pop stack of
                        CInt h =>
                          (push stack (CInt (vecLen h)); setTopIp nextIp; true)
                      | _ => raise Fail "interpreter: vec_len expects handle"
                    else if bid = 10 then
                      (case (pop stack, pop stack) of
                        (CInt idx, CInt h) =>
                          (push stack (CInt (vecGet h idx)); setTopIp nextIp; true)
                      | _ => raise Fail "interpreter: vec_get expects handle and int")
                    else if bid = 11 then
                      (case (pop stack, pop stack, pop stack) of
                        (CInt v, CInt idx, CInt h) =>
                          (vecSet h idx v; setTopIp nextIp; true)
                      | _ => raise Fail "interpreter: vec_set expects handle, int, int")
                    else if bid = 12 then
                      (case pop stack of
                        CInt nwords =>
                          (push stack (CInt (boxAlloc nwords)); setTopIp nextIp; true)
                      | _ => raise Fail "interpreter: box_alloc expects int")
                    else if bid = 13 then
                      let val vv = pop stack
                          val vo = pop stack
                          val vh = pop stack
                          val iv = case vv of CInt n => n | CUnit => 0 | CBool b => (if b then 1 else 0) | _ => raise Fail "interpreter: box_store bad value"
                          val io = case vo of CInt n => n | _ => raise Fail "interpreter: box_store bad offset"
                          val ih = case vh of CInt n => n | _ => raise Fail "interpreter: box_store bad handle"
                      in
                        boxStore ih io iv; setTopIp nextIp; true
                      end
                    else if bid = 14 then
                      (case (pop stack, pop stack) of
                        (CInt off, CInt h) =>
                          (push stack (CInt (boxLoad h off)); setTopIp nextIp; true)
                      | _ => raise Fail "interpreter: box_load expects handle and offset")
                    else
                      raise Fail ("interpreter: unknown builtin " ^ Int.toString bid)
                | B.CONTRACT_CHECK midx =>
                    let val ok = truthy (pop stack)
                        val msg = Vector.sub (strings, midx)
                    in
                      if ok then (setTopIp nextIp; true)
                      else raise Fail ("contract: " ^ msg)
                    end
                | _ => raise Fail "interpreter: opcode not implemented in this slice"
              end

      val mainF = Vector.sub (funcs, mainIdx)
      val mainLoc = Array.array (#localCount mainF, CUnit)
      val () = frames := [{f = mainF, ip = 0, loc = mainLoc}]
      fun loop () =
        if step () then loop () else ()
      val () = loop ()
    in
      case !stack of
        CInt code :: _ => code
      | [] => 0
      | _ => 0
    end

  fun runFile (path : string) : int =
    let
      val ins = BinIO.openIn path
      val v = BinIO.inputAll ins
      val () = BinIO.closeIn ins
      val ld = loadFileVec v
      val st = ref ([] : cell list)
    in
      runWithStack ld (findMain ld) st
    end

  fun runProgram (p : B.program) : int =
    let val ld = loadProgram p
        val mi = findMain ld
        val st = ref ([] : cell list)
    in
      runWithStack ld mi st
    end

  fun run (p : B.program) : unit =
    let val code = runProgram p
    in
      TextIO.print ("exit " ^ Int.toString code ^ "\n")
    end

end
