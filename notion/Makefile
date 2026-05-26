.PHONY: install test

install:
	@. ../script/lib.sh; \
	dest="$$HOME/.notion"; \
	maybe_mkdir "$$dest"; \
	(cd notion && for f in *; do \
	    copy_to "$$f" "$$dest"; \
	done)

test:
	@echo notion test not yet implemented.
