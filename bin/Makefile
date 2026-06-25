.PHONY: install test

install:
	@. ../script/lib.sh; \
	dest="$$HOME/bin/"; \
	maybe_mkdir "$$dest"; \
	(cd bin && for f in *; do \
	    if [ -f "$$f" ] && [ "$${f##*~}" = "$$f" ]; then \
	        copy_to "$$f" "$$dest"; \
	    fi; \
	done); \
	copy_to config/timezones "$$HOME/.timezones"

test:
	@$(MAKE) -C test test
