SML := sml

.PHONY: check test

check:
	@tmp=$$(mktemp); (cd "$(CURDIR)" && echo 'use "src/main.sml";' | $(SML) >$$tmp 2>&1); \
	  if grep -q 'Error:' $$tmp; then tail -40 $$tmp; rm -f $$tmp; exit 1; fi; \
	  rm -f $$tmp

test: coverage-identifiers
	@echo 'use "src/main.sml"; use "test/bytecode_test.sml";' | $(SML)

# CV-102: opcode 119 and coverage identifiers agree with the sv0doc registry copy.
.PHONY: coverage-identifiers
coverage-identifiers:
	python3 scripts/check_coverage_identifiers.py --selftest
	python3 scripts/check_coverage_identifiers.py --repo sv0vm
