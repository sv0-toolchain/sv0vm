# Pre-coverage decoder pin (sv0cov CV-122)

`bytecode.sml` is sv0vm's `src/bytecode/bytecode.sml` exactly as it was at
commit `5c52484`, before `COVER_HIT` existed (its last change was `0e5f426`).
`git show 5c52484:src/bytecode/bytecode.sml` reproduces it byte for byte;
`test/old_vm_test.sml` pins its SHA-256, so it cannot drift.

It stands in for a VM without coverage support. sv0doc
`bytecode/coverage.md` 3.1 promises that such a VM never misdecodes
`COVER_HIT`: it decodes every function while loading and fails with
`unknown opcode 119` before execution starts. `test/old_vm_test.sml` checks
that against bytecode from today's encoder (a hit in `main`, in another
function, and last in the code) and against `../f0-instrumented.sv0b`, which
sv0c emitted for the sv0cov `f0` fixture with `--coverage=instrument`
(SHA-256 `b8e1cb10358836410d4905f1092bf39be574d0e970bd78284e477712fc6d456a`).
Uninstrumented bytecode must still decode with it.

Do not edit `bytecode.sml`; it is evidence, not code.
