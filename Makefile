SML := sml

.PHONY: check test

check:
	@tmp=$$(mktemp); (cd "$(CURDIR)" && echo 'use "src/main.sml";' | $(SML) >$$tmp 2>&1); \
	  if grep -q 'Error:' $$tmp; then tail -40 $$tmp; rm -f $$tmp; exit 1; fi; \
	  rm -f $$tmp

test:
	@echo 'use "src/main.sml"; use "test/bytecode_test.sml";' | $(SML)
