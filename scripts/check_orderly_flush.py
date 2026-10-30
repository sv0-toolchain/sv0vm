#!/usr/bin/env python3
"""sv0vm publishes a raw profile only at the orderly flush (sv0cov CV-406).

sv0cov SPEC COV-FMT-011/012 and AC-032: through R1 the runtime performs no
periodic profile checkpoint and no memory-mapped counter writes; a profile
exists only after the orderly end-of-program flush. The native runtime is
proved by an I/O trace in sv0cov; this is sv0vm's proof by dependency and
source inspection (the SML Basis has no memory mapping, and sv0vm starts no
thread), over src/ and scripts/:

- no interval timer, ML signal handler, thread or CML facility is used,
  and nothing names mmap;
- in src/coverage/coverage.sml every call that creates, writes, names or
  removes a file sits inside ``fun flush``, and only ``flush`` serializes
  the counters (``profileBytes``);
- ``Coverage.flush`` is called exactly once, in
  ``Interpreter.runFileBound``, after ``runWithStack`` has returned the
  program's exit code.

    python3 scripts/check_orderly_flush.py [--root DIR]
    python3 scripts/check_orderly_flush.py --selftest
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

# Restoring a signal's default action (scripts/run_sv0b.sml) is allowed; an ML handler is not.
FORBIDDEN = re.compile(r"\b(IntervalTimer|Signals\.HANDLER|CML|Thread|mmap)\b")
FILE_CALL = re.compile(
    r"\b(createf|creat|openf|link|unlink|rename|writeVec|writeArr|BinIO\.openOut|BinIO\.openAppend|"
    r"TextIO\.openOut|TextIO\.openAppend|ftruncate)\b"
)
COMMENT = re.compile(r"\(\*.*?\*\)", re.S)


def strip(text: str) -> str:
    """SML comments blanked (newlines kept, so line numbers stay)."""
    while True:
        out = COMMENT.sub(lambda m: re.sub(r"[^\n]", " ", m.group()), text)
        if out == text:
            return out
        text = out


def top_level_funs(text: str) -> list[tuple[str, int, int]]:
    """(name, start, end) offsets of each two-space-indented ``fun`` in a structure body."""
    starts = [(m.group(1), m.start()) for m in re.finditer(r"^  fun (\w+)", text, re.M)]
    ends = [s for _, s in starts[1:]] + [len(text)]
    return [(n, s, e) for (n, s), e in zip(starts, ends)]


def check(files: dict[str, str]) -> list[str]:
    """Problems found in ``files`` (relative path -> source text)."""
    problems = []
    clean = {p: strip(t) for p, t in files.items()}
    for path, text in sorted(clean.items()):
        for m in FORBIDDEN.finditer(text):
            problems.append(f"{path}:{text.count(chr(10), 0, m.start()) + 1}: uses {m.group()}")

    cov = clean.get("src/coverage/coverage.sml")
    if cov is None:
        return problems + ["src/coverage/coverage.sml is missing"]
    funs = top_level_funs(cov)
    flush = [(s, e) for n, s, e in funs if n == "flush"]
    if len(flush) != 1:
        return problems + ["coverage.sml: expected exactly one top-level fun flush"]
    fs, fe = flush[0]
    for rx, what in ((FILE_CALL, "file call"), (re.compile(r"(?<!fun )\bprofileBytes\s*\(\)"), "profile serialization")):
        for m in rx.finditer(cov):
            if not fs <= m.start() < fe:
                problems.append(f"src/coverage/coverage.sml:{cov.count(chr(10), 0, m.start()) + 1}: "
                                f"{what} {m.group()!r} outside fun flush")

    calls = [(p, m.start()) for p, t in clean.items() for m in re.finditer(r"\bCoverage\.flush\b", t)]
    if len(calls) != 1:
        problems.append(f"Coverage.flush is called {len(calls)} times, expected once")
    else:
        path, at = calls[0]
        owner = [(n, s) for n, s, e in top_level_funs(clean[path]) if s <= at < e]
        body = clean[path][owner[0][1]:at] if owner else ""
        if path != "src/interpreter/interpreter.sml" or not owner or owner[0][0] != "runFileBound":
            problems.append(f"{path}: Coverage.flush is called outside Interpreter.runFileBound")
        elif not re.search(r"\bval code = runWithStack\b", body):
            problems.append(f"{path}: Coverage.flush runs before runWithStack returns the exit code")
    return problems


def load(root: Path) -> dict[str, str]:
    return {str(p.relative_to(root)): p.read_text() for d in ("src", "scripts") for p in sorted((root / d).rglob("*.sml"))}


def selftest(root: Path) -> None:
    base = load(root)
    assert check(base) == [], check(base)
    cov, interp = "src/coverage/coverage.sml", "src/interpreter/interpreter.sml"

    def mutated(path: str, old: str, new: str) -> list[str]:
        assert old in base[path], old
        return check({**base, path: base[path].replace(old, new, 1)})

    # A checkpoint writer elsewhere in coverage.sml.
    hit = "  fun hit (k : int) : unit ="
    assert any("outside fun flush" in p for p in mutated(cov, hit, "  fun checkpoint () = Posix.FileSys.unlink \"x\"\n" + hit))
    assert any("profile serialization" in p for p in mutated(cov, hit, "  fun snap () = profileBytes ()\n" + hit))
    # A timer, a second flush call, a flush before the program's exit code.
    assert any("IntervalTimer" in p for p in mutated(cov, hit, "  val t = SMLofNJ.IntervalTimer.tick\n" + hit))
    assert any("Signals.HANDLER" in p for p in mutated(cov, hit, "  val h = Signals.HANDLER (fn (_, _, k) => k)\n" + hit))
    assert any("called 2 times" in p for p in mutated(interp, "      val code = runWithStack", "      val _ = Coverage.flush ()\n      val code = runWithStack"))
    early = mutated(interp, "      val code = runWithStack ld (findMain ld) st",
                    "      val early = Coverage.flush ()\n      val code = runWithStack ld (findMain ld) st")
    assert any("called 2 times" in p for p in early)
    moved = base[interp].replace("      val code = runWithStack ld (findMain ld) st\n", "", 1)
    moved = moved.replace("case Coverage.flush () of SOME failed => failed | NONE => code",
                          "case Coverage.flush () of SOME failed => failed | NONE => runWithStack ld (findMain ld) st", 1)
    assert any("before runWithStack" in p for p in check({**base, interp: moved})), check({**base, interp: moved})
    # Comments are not code.
    assert check({**base, cov: base[cov].replace(hit, "  (* IntervalTimer, unlink *)\n" + hit, 1)}) == []
    print("check_orderly_flush selftest: OK")


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--root", type=Path, default=Path(__file__).resolve().parent.parent)
    ap.add_argument("--selftest", action="store_true")
    args = ap.parse_args(argv)
    if args.selftest:
        selftest(args.root)
        return 0
    problems = check(load(args.root))
    for p in problems:
        print(f"check_orderly_flush: {p}", file=sys.stderr)
    if not problems:
        print("check_orderly_flush: OK (no checkpoint, no mapping; one orderly flush)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
