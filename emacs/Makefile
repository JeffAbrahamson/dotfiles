.PHONY: install test

install:
	@. ../script/lib.sh; \
	dest="$$HOME/.dotfiles/elisp/"; \
	maybe_mkdir "$$dest"; \
	(cd elisp && for f in *; do \
	    copy_to "$$f" "$$dest"; \
	done); \
	maybe_append .emacs .dotfiles/elisp/emacs.el emacs-include
	@# These need the network.  Warn rather than fail, so that an
	@# offline install still sets up everything else.
	@if command -v emacs > /dev/null; then \
	    emacs -Q --batch --load install-packages.el \
	        || echo "Warning: failed to install Emacs packages." >&2; \
	fi
	@./install-python-venv.sh \
	    || echo "Warning: failed to set up the Python venv for Emacs." >&2

test:
	@echo emacs tests not yet implemented.
