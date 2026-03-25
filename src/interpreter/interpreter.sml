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

  fun cmp rel stack =
    let val b = pop stack
        val a = pop stack
    in
      case (a, b) of
        (CInt x, CInt y) => push stack (CBool (rel (x, y)))
      | _ => raise Fail "interpreter: compare on non-int"
    end

  fun asWord32 i = Word32.fromLargeInt (Int.toLarge i)

  fun runWithStack (ld : loaded) (mainIdx : int) (stack : cell list ref) : int =
    let
      val funcs = #funcs ld
      val strings = #strings ld

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
                      val rawArgs = popN stack na
                      val args = rev rawArgs
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
                    let val vals = rev (popN stack n)
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
                          let val s = Vector.sub (strings, i)
                          in
                            TextIO.print (s ^ "\n"); setTopIp nextIp; true
                          end
                      | _ => raise Fail "interpreter: println expects string index"
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
