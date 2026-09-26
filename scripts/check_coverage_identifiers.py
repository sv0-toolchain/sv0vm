#!/usr/bin/env python3
"""Check a repository against the coverage identifier registry (CV-102).

The registry is sv0doc ``bytecode/coverage-identifiers.json``. This script is
kept byte-identical in sv0doc, sv0c, and sv0vm (the sv0-toolchain root guard
compares the copies); each repository runs it from its own test suite
against its own copy of the registry.

Checks:

- **Opcode 119** belongs to ``COVER_HIT`` alone. ``--repo sv0c``: no
  ``fn OP_<NAME>() -> i32 { return 119; }`` other than ``OP_COVER_HIT``; if
  ``OP_COVER_HIT`` exists it returns 119 and ``insn_encoded_size`` gives 119
  five bytes. ``--repo sv0vm``: no encoder arm other than ``COVER_HIT`` emits
  ``w8 119`` and no decoder arm ``| 119 =>`` builds another instruction; if
  ``COVER_HIT`` exists it encodes as ``w8 119`` plus ``u32Le`` and decodes
  from 119.
- **Identifier spelling.** In the scanned sources, any token equal to a
  registry value ignoring case, or starting with the stem of a versioned
  registry capability (``sv0cov.plan.``, ``sv0cov.coverage.``),
  must be byte-identical to a registry value. Only string literals are
  scanned (identifiers are wire data; a class named ``Covr`` is not). This
  catches case variants and version drift such as ``sv0cov.plan.v2``.

    python3 scripts/check_coverage_identifiers.py --repo sv0c|sv0vm|sv0cov [--root DIR] [--registry FILE]
    python3 scripts/check_coverage_identifiers.py --selftest
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

SCAN = {
    "sv0c": ("lib", "runtime", "scripts", "sml-legacy/backend/vm"),
    "sv0vm": ("src", "scripts"),
    "sv0cov": ("src", "scripts"),
}
SUFFIXES = {".sv0", ".sml", ".sig", ".c", ".h", ".py", ".sh"}
RE_OP_DEF = re.compile(r"fn OP_([A-Z][A-Z0-9_]*)\(\)\s*->\s*i32\s*\{\s*return\s+(\d+)\s*;\s*\}")
RE_SIZE_119 = re.compile(r"if opc == 119 \{ return 5; \}")
RE_ENC_ARM = re.compile(r"\|\s*([A-Z][A-Z0-9_]*)\b[^|]*?bytes1 \(w8 (\d+)\)([^|]*)")
RE_DEC_ARM = re.compile(r"\|\s*(\d+)\s*=>[^|]*?\(([A-Z][A-Z0-9_]*)")


def check_opcodes_sv0c(text: str) -> list[str]:
    errors = []
    defs = RE_OP_DEF.findall(text)
    for name, num in defs:
        if int(num) == 119 and name != "COVER_HIT":
            errors.append(f"OP_{name} uses opcode 119, reserved for COVER_HIT")
        if name == "COVER_HIT" and int(num) != 119:
            errors.append(f"OP_COVER_HIT returns {num}, not 119")
    if any(name == "COVER_HIT" for name, _ in defs) and not RE_SIZE_119.search(text):
        errors.append("insn_encoded_size must give opcode 119 five bytes")
    return errors


def check_opcodes_sv0vm(text: str) -> list[str]:
    errors = []
    enc = RE_ENC_ARM.findall(text)
    for name, num, rest in enc:
        if int(num) == 119 and name != "COVER_HIT":
            errors.append(f"encoder arm {name} emits opcode 119, reserved for COVER_HIT")
        if name == "COVER_HIT" and (int(num) != 119 or "u32Le" not in rest):
            errors.append("encoder arm COVER_HIT must emit w8 119 followed by u32Le")
    for num, name in RE_DEC_ARM.findall(text):
        if int(num) == 119 and name != "COVER_HIT":
            errors.append(f"decoder arm 119 builds {name}, not COVER_HIT")
        if name == "COVER_HIT" and int(num) != 119:
            errors.append(f"decoder builds COVER_HIT from opcode {num}")
    has_ctor = any(name == "COVER_HIT" for name, _, _ in enc)
    has_dec = any(name == "COVER_HIT" for _, name in RE_DEC_ARM.findall(text))
    if has_ctor != has_dec:
        errors.append("COVER_HIT must be both encoded and decoded, or neither")
    return errors


RE_LITERAL = re.compile(r'"(?:[^"\\\n]|\\.)*"' + "|" + r"'(?:[^'\\\n]|\\.)*'")
RE_VERSIONED = re.compile(r"^(sv0cov\.[a-z0-9-]+\.)v\d+$")


def check_spelling(text: str, where: str, values: list[str]) -> list[str]:
    """Registry spellings inside string literals (wire data), not code identifiers."""
    errors = []
    folded = {v.lower(): v for v in values}
    stems = [m.group(1) for v in values if (m := RE_VERSIONED.match(v))]
    for lit in RE_LITERAL.finditer(text):
        for m in re.finditer(r"[A-Za-z0-9_][A-Za-z0-9_.\-]*[A-Za-z0-9_]", lit.group(0)):
            tok = m.group(0)
            low = tok.lower()
            suspicious = low in folded or any(low.startswith(stem) for stem in stems)
            if suspicious and tok not in values:
                line = text.count("\n", 0, lit.start()) + 1
                errors.append(f"{where}:{line}: {tok!r} is not a registry spelling")
    return errors


def check_repo(repo: str, root: Path, registry: Path) -> list[str]:
    reg = json.loads(registry.read_bytes())
    if reg.get("schema") != "sv0.coverage-identifiers" or reg.get("version") != "1.0":
        return [f"{registry}: not a coverage identifier registry 1.0"]
    op = next((o for o in reg["opcodes"] if o["name"] == "COVER_HIT"), None)
    if op is None or op["opcode"] != 119 or op["encoded_length"] != 5 or op["operand"] != "u32le":
        return [f"{registry}: COVER_HIT must be opcode 119, u32le, 5 bytes"]
    values = [i["value"] for i in reg["identifiers"]] + ["COVER_HIT"]
    errors = []
    for base in SCAN[repo]:
        for path in sorted((root / base).rglob("*")):
            if not path.is_file() or path.suffix not in SUFFIXES or "golden" in path.parts:
                continue
            text = path.read_text(encoding="utf-8", errors="replace")
            rel = path.relative_to(root).as_posix()
            if path.name == "check_coverage_identifiers.py":
                continue
            errors += check_spelling(text, rel, values)
            if repo == "sv0c" and rel == "lib/bytecode.sv0":
                errors += [f"{rel}: {e}" for e in check_opcodes_sv0c(text)]
            if repo == "sv0vm" and rel == "src/bytecode/bytecode.sml":
                errors += [f"{rel}: {e}" for e in check_opcodes_sv0vm(text)]
    return errors


def selftest() -> int:
    values = ["sv0cov.coverage.v1", "sv0cov.plan.v1", "sv0vm-v1-coverage", "sv0vm-v2-typed", "COVR", "sv0cov.vm-binding", "1.0", "COVER_HIT"]
    cases = [
        (check_opcodes_sv0c("fn OP_RETURN_SLOTS() -> i32 { return 118; }"), 0),
        (check_opcodes_sv0c("fn OP_NEW_THING() -> i32 { return 119; }"), 1),
        (check_opcodes_sv0c("fn OP_COVER_HIT() -> i32 { return 120; }"), 2),
        (check_opcodes_sv0c("fn OP_COVER_HIT() -> i32 { return 119; }\n    if opc == 119 { return 5; }"), 0),
        (check_opcodes_sv0vm("    | RETURN => bytes1 (w8 116)\n      | 116 => (RETURN, i + 1)"), 0),
        (check_opcodes_sv0vm("    | NEW_THING => bytes1 (w8 119)\n      | 119 => (NEW_THING, i + 1)"), 2),
        (check_opcodes_sv0vm("    | COVER_HIT c => cat [bytes1 (w8 119), u32Le c]\n      | 119 =>\n let val (c, j) = u32At v (i + 1) in (COVER_HIT c, j) end"), 0),
        (check_opcodes_sv0vm("    | COVER_HIT c => cat [bytes1 (w8 119), u32Le c]"), 1),
        (check_spelling('x = "sv0cov.plan.v1"; y = "sv0cov.map";', "f", values), 0),
        (check_spelling('x = "sv0cov.plan.v2"', "f", values), 1),
        (check_spelling('x = "SV0VM-V1-COVERAGE"', "f", values), 1),
        (check_spelling('x = "covr"', "f", values), 1),
        (check_spelling('x = "sv0vm-v1-core"', "f", values), 0),
        (check_spelling("class Covr:\n    pass", "f", values), 0),
        (check_spelling('s = "sv0cov.vm-binding-semantic"', "f", values), 0),
        (check_spelling("val tag = 'sv0cov.coverage.V1'", "f", values), 1),
    ]
    bad = [i for i, (errs, want) in enumerate(cases) if len(errs) != want]
    if bad:
        print(f"check_coverage_identifiers selftest: cases {bad} failed", file=sys.stderr)
        return 1
    print(f"check_coverage_identifiers selftest: {len(cases)} cases OK")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", choices=sorted(SCAN))
    ap.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    ap.add_argument("--registry", type=Path, help="default: <root>/test/coverage-identifiers.json")
    ap.add_argument("--selftest", action="store_true")
    args = ap.parse_args()
    if args.selftest:
        return selftest()
    if not args.repo:
        ap.error("--repo is required")
    registry = args.registry or args.root / "test" / "coverage-identifiers.json"
    errors = check_repo(args.repo, args.root, registry)
    for e in errors:
        print(f"coverage identifiers: {e}", file=sys.stderr)
    if errors:
        return 1
    print(f"coverage identifiers: {args.repo} agrees with the registry")
    return 0


if __name__ == "__main__":
    sys.exit(main())
