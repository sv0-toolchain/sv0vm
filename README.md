# sv0vm -- sv0 bytecode virtual machine

the sv0 bytecode interpreter, initially implemented in Standard ML (SML/NJ), transitioning to sv0 when sv0c matures.

## purpose

sv0vm executes sv0 bytecode produced by sv0c's VM backend. it provides fast iteration during development (no external C compiler needed), powers the REPL (`sv0 repl`), and runs doc tests.

## architecture

```
sv0 bytecode (.sv0b)
     |
     v
  Bytecode Loader   src/bytecode/
     |
     v
  Interpreter       src/interpreter/
     |
     v
  Runtime            src/runtime/
  (stack, heap,
   contract checks,
   built-in functions)
```

## directory structure

| directory | description |
|---|---|
| `src/bytecode/` | bytecode format definition, encoding/decoding |
| `src/interpreter/` | instruction dispatch loop, stack machine |
| `src/runtime/` | memory management, built-in functions, contract runtime |
| `test/` | test suite |
| `build/` | build artifacts (gitignored) |

## building

```bash
# requires SML/NJ
sml src/main.sml
```

## specification

sv0vm implements the bytecode format defined by sv0c's IR and the runtime semantics in [sv0doc](../sv0doc/).

## transition plan

sv0vm starts in SML/NJ for consistency with sv0c. once sv0c can compile sv0 code with the necessary features (pattern matching, algebraic data types, basic I/O), sv0vm will be rewritten in sv0 itself.
