# sv0vm — progress (submodule)

**Meta-repo rollup:** when this tree is the `sv0vm/` submodule of **sv0-toolchain**, the parent copies this file’s **`%`** into `task/sv0-toolchain-progress.md`. **Standalone clone:** keep this file authoritative here; reconcile on the next meta-repo integration.

**Last updated:** 2026-04-27 (**`CALL_BUILTIN` 15–17:** **`read_file` / `write_file` / `read_dir`** in **`src/interpreter/interpreter.sml`** — TextIO/OS walk + sorted path blob per **`sv0doc/compiler/bootstrap-host-io.md`**)

## Checklist (local source of truth)

| ID | Item | Done (0/1) |
|----|------|------------|
| VM-1 | Bytecode interpreter behavior matches `sv0doc` for implemented opcodes / semantics | 1 |
| VM-2 | Test harness and milestone task (`task/sv0vm-milestone-2.Rmd`) describe current VM coverage | 1 |
| VM-3 | Integration points with `sv0c` VM backend / parity story documented | 1 |

## Completion

- **Done:** count rows with `Done = 1` above.
- **Total:** row count of the checklist.
- **%:** `Done / Total * 100`.

## Notes

- 2026-04-10: Reconciled with `task/sv0vm-milestone-2.Rmd` (`state: complete`, closure summary 2026-04-01). Bytecode format defined in `sv0doc/bytecode/`, VM backend emits and executes bytecode, contracts work at runtime, REPL evaluates expressions; vm-parity corpus size follows **`sv0c/test/vm-parity/manifest.txt`** (currently **97** programs vs SML goldens — see **`task/sv0-toolchain-roadmap-full.Rmd`** snapshot and **`sv0c/PROGRESS.md`**). `break`/`continue` including `for`+`break`, `VAddrOf`/`no_alias` via local slot tokens + `CALL_BUILTIN 1`.
- 2026-04-22: Stale **101** vm-parity count in this note corrected to track the live manifest (**97**) without changing milestone-2 closure (criteria remain met).
- VM-3 parity story: `sv0c/test/vm-parity/` manifest + SML goldens; allowlist policy documented in `task/sv0-toolchain-milestone-3-self-host.Rmd`.
- 2026-08-12: **i32 arithmetic fixed (bug-hunt #2 / BH-2).** `src/interpreter/interpreter.sml` `ADD/SUB/MUL/NEG_I32` now wrap mod 2³² via `Word32` and `DIV/MOD_I32` use `Int.quot`/`Int.rem` (truncate toward zero) — matching the C backend / Rust `i32`, where before overflow stayed wide (`2e9 + 2e9` positive vs C's INT_MIN) and negative div/mod floored (`(-100)/3 = -34` vs C's -33). `MOD`-by-zero now guarded like `DIV`. Regression test in `test/bytecode_test.sml`; `make test` green. Milestone-2 closure unchanged. See parent `task/sv0-toolchain-progress.md` ## Post-M3 hardening status.
