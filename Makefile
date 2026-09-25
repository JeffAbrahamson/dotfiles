.PHONY: install test python-checks black flake8 cflake8 pytest cleanup \
	$(addsuffix -install,$(INSTALL_SUBDIRS)) \
	$(addsuffix -test,$(TEST_SUBDIRS)) \
	i3-install-skipped i3-test-skipped ratpoison-test-skipped

INSTALL_SUBDIRS := firewall X11 bash bat bin emacs postgresql python \
	readline screen sway tmux misc
TEST_SUBDIRS := bash bat bin emacs git mutt notion sway

install: $(addsuffix -install,$(INSTALL_SUBDIRS)) i3-install-skipped cleanup

test: $(addsuffix -test,$(TEST_SUBDIRS)) i3-test-skipped ratpoison-test-skipped python-checks

i3-install-skipped i3-test-skipped:
	@echo "===> Skipping i3"

ratpoison-test-skipped:
	@echo "===> Skipping ratpoison"

python-checks:
	./script/python-checks.sh all

black:
	./script/python-checks.sh black

flake8:
	./script/python-checks.sh flake8

cflake8:
	./script/python-checks.sh cflake8

pytest:
	./script/python-checks.sh pytest

%-install:
	@echo $*
	@$(MAKE) -C $* install

%-test:
	@echo $*
	@$(MAKE) -C $* test

cleanup:
	./cleanup.sh
