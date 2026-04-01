# sv0vm -- sv0 bytecode virtual machine

the sv0 bytecode interpreter, initially implemented in Standard ML (SML/NJ), transitioning to sv0 when sv0c matures.

## purpose

sv0vm executes sv0 bytecode produced by sv0c's VM backend. it provides fast iteration during development (no external C compiler needed), powers the REPL (`sv0 repl`), and runs doc tests. this matches the **bytecode VM backend** and **sv0vm** placement in the [sv0 compiler vision and design](http://development.sasankvishnubhatla.net/tcowmbh/task/sv0-compiler-vision-and-design.html) document (compiler architecture diagram and toolchain table).

## specification

- **Bytecode:** [sv0doc/bytecode/format.md](../sv0doc/bytecode/format.md), [sv0doc/bytecode/instructions.md](../sv0doc/bytecode/instructions.md) (submodule path when using the combined workspace: same relative layout from repo root).

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
make check          # compile-load smoke (or: sml < src/main.sml via use)
make test           # bytecode round-trip tests

# run a .sv0b from the parent toolchain (after sv0c --target=vm)
SV0B=../sv0c/build/vm/hello.sv0b sml < scripts/run_sv0b.sml
```

## wider toolchain context

In the vision document, sv0-IR feeds **C**, **LLVM**, and **bytecode** backends. **Milestone 1** ships the C path; this repository implements the **bytecode** decode/execute side for **Milestone 2** (complete as of `task/sv0vm-milestone-2.Rmd` in the parent **sv0-toolchain** repo): loader, interpreter, runtime, contract checks, and CI parity with sv0c integration scenarios via `make integration-vm` / `./scripts/sv0 integration-vm`.

## transition plan

sv0vm starts in SML/NJ for consistency with sv0c. once sv0c can compile sv0 code with the necessary features (pattern matching, algebraic data types, basic I/O), sv0vm will be rewritten in sv0 itself.
