#!/usr/bin/env bash
# Run sv0vm's SML test files and fail when any of them fails.
#
# Piping `use` lines into sml exits 0 even after a type error or an uncaught
# exception, so `make test` used to pass whatever happened. Each test file now
# runs in its own sml session (a file that re-`use`s src/bytecode/bytecode.sml
# would otherwise give later files a different Bytecode structure), and the
# run fails on any "Error:", "uncaught exception" or "FAIL" line, or when an
# expected "... OK" line is missing.
set -euo pipefail
cd "$(dirname "$0")/.."
SML="${SML:-sml}"
status=0

# <test file> <required OK line>...
run() {
  local file="$1"; shift
  local out
  out="$(printf 'use "src/main.sml";\nuse "%s";\n' "$file" | "$SML" 2>&1)" || true
  if grep -qE 'Error:|uncaught exception|FAIL' <<<"$out"; then
    echo "sv0vm tests: $file FAILED" >&2
    grep -E 'Error:|uncaught exception|FAIL|raised at' <<<"$out" | head -40 >&2
    status=1
    return
  fi
  local want
  for want in "$@"; do
    if ! grep -qF "$want" <<<"$out"; then
      echo "sv0vm tests: $file did not report '$want'" >&2
      tail -20 <<<"$out" >&2
      status=1
      return
    fi
    echo "$want"
  done
}

run test/bytecode_test.sml "bytecode tests: OK" "interpreter exec tests: OK"
run test/coverage_test.sml "coverage tests: OK"
exit "$status"
