.PHONY: install test python-checks black flake8 cflake8 pytest cleanup \
	$(addsuffix -install,$(INSTALL_SUBDIRS)) \
	$(addsuffix -test,$(TEST_SUBDIRS))

INSTALL_SUBDIRS := firewall X11 bash bin emacs i3 postgresql python \
	readline screen sway tmux misc
TEST_SUBDIRS := bash bin emacs git i3 mutt notion ratpoison sway

install: $(addsuffix -install,$(INSTALL_SUBDIRS)) cleanup

test: $(addsuffix -test,$(TEST_SUBDIRS)) python-checks

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
