.PHONY: install test

install:
	@. ../script/lib.sh; \
	if [ "X$$HOSTNAME" = Xvogel ]; then \
	    dpi=108; \
	else \
	    dpi=96; \
	fi; \
	sed -e "s/{% dpi %}/$$dpi/;" Xdefaults > "$$HOME/.Xdefaults"

test:
	@:
