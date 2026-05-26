.PHONY: install test

install:
	@. ../script/lib.sh; \
	dest="$$HOME/.dotfiles/elisp/"; \
	maybe_mkdir "$$dest"; \
	(cd elisp && for f in *; do \
	    copy_to "$$f" "$$dest"; \
	done); \
	maybe_append .emacs .dotfiles/elisp/emacs.el emacs-include

test:
	@echo emacs test.sh not yet implemented.
