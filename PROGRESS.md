# sv0vm — progress

**Live status is tracked in one place:** the parent workspace's
[`task/sv0-toolchain-progress.md`](../task/sv0-toolchain-progress.md). When this
tree is the `sv0vm/` submodule of **sv0-toolchain**, that file is authoritative.

## current state

- **Milestone 2 complete** — the bytecode VM (loader, interpreter, runtime,
  contract checks, host I/O) executes sv0 bytecode from sv0c's `--target=vm`
  backend, with CI parity against the sv0c integration scenarios
  (`make integration-vm`). See [`README.md`](README.md).
- Implemented in SML/NJ; a future rewrite in sv0 is planned once sv0c supports
  the needed language surface (see README "transition plan").

_Historical note: the detailed run log lives in git history and the parent
progress rollup; it is not duplicated here to keep a single source of truth._
