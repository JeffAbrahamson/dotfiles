.PHONY: install test

install:
	@. ../script/lib.sh; \
	host_specific_config="i3/config.$$(hostname -s)"; \
	if [ -r "$$host_specific_config" ]; then \
	    cat i3/config_base "$$host_specific_config" > i3/config; \
	else \
	    cat i3/config_base > i3/config; \
	fi; \
	dest="$$HOME/.i3/"; \
	maybe_mkdir "$$dest"; \
	(cd i3 && copy_to i3status.conf "$$HOME/.i3status.conf"); \
	maybe_mkdir "$$(dirname "$$dest")"; \
	(cd i3 && copy_to config "$$dest"); \
	maybe_mkdir "$$HOME/.config/dunst"; \
	if [ "X$$HOSTNAME" = "Xbirdsong" ] || [ "X$$HOSTNAME" = "Xmorning" ]; then \
	    sed -e 's/{% fontsize %}/10/;' < i3/dunstrc > "$$HOME/.config/dunst/dunstrc"; \
	elif [ "X$$HOSTNAME" = "Xvogel" ]; then \
	    sed -e 's/{% fontsize %}/16/;' < i3/dunstrc > "$$HOME/.config/dunst/dunstrc"; \
	elif [ "X$$HOSTNAME" = "Xstarshine" ]; then \
	    sed -e 's/{% fontsize %}/18/;' < i3/dunstrc > "$$HOME/.config/dunst/dunstrc"; \
	elif [ "X$$HOSTNAME" = "Xtau-ceti" ]; then \
	    sed -e 's/{% fontsize %}/11/;' < i3/dunstrc > "$$HOME/.config/dunst/dunstrc"; \
	else \
	    echo "Unrecognized host, not copying dunstrc."; \
	fi; \
	dest="$$HOME/bin/"; \
	maybe_mkdir "$$dest"; \
	(cd i3/bin && for f in *; do \
	    copy_to "$$f" "$$dest"; \
	done)

test:
	@runtime_dir="$$(mktemp -d)"; \
	test_config="$$runtime_dir/config"; \
	host_specific_config="i3/config.$$(hostname -s)"; \
	trap 'rm -rf "$$runtime_dir"' EXIT; \
	if [ -r "$$host_specific_config" ]; then \
	    cat i3/config_base "$$host_specific_config" > "$$test_config"; \
	else \
	    cp i3/config_base "$$test_config"; \
	fi; \
	XDG_RUNTIME_DIR="$$runtime_dir" i3 -C -c "$$test_config"
