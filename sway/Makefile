.PHONY: install test
SHELL := /bin/bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

install:
	@. ../script/lib.sh
	hostname_short="$$(hostname -s)"
	host_specific_sway_config="sway/config.$$hostname_short"
	sway_config="sway/config"
	if [ -r "$$host_specific_sway_config" ]; then
	    cat sway/config_base "$$host_specific_sway_config" > "$$sway_config"
	else
	    echo "Warning: no host-specific sway config found at $$host_specific_sway_config; using sway/config_base only." >&2
	    cat sway/config_base > "$$sway_config"
	fi
	sway_config_dest_dir="$$HOME/.config/sway"
	maybe_mkdir "$$sway_config_dest_dir"
	sway_config_dest="$$sway_config_dest_dir/config"
	swaylock_dest_dir="$$HOME/.swaylock"
	maybe_mkdir "$$swaylock_dest_dir"
	(cd sway && copy_to swaylock_config "$$swaylock_dest_dir/config")
	systemd_make_dir="$$HOME/.config/systemd/user"
	maybe_mkdir "$$systemd_make_dir"
	(cd sway && copy_to systemd-mako-service "$$systemd_make_dir/mako.service")
	kitty_dest_dir="$$HOME/.config/kitty"
	maybe_mkdir "$$kitty_dest_dir"
	kitty_dest="$$kitty_dest_dir/kitty.conf"
	case "$$hostname_short" in
	    vogel)
	        kitty_font_size=13.0
	        window_title_font_size=10.0
	        bar_font_size=12
	        mako_font_size=16
	        ;;
	    tau-ceti)
	        kitty_font_size=11.0
	        window_title_font_size=8.0
	        bar_font_size=9
	        mako_font_size=11
	        ;;
	    morning)
	        kitty_font_size=11.0
	        window_title_font_size=8.0
	        bar_font_size=9
	        unset mako_font_size
	        ;;
	    *)
	        kitty_font_size=11.0
	        window_title_font_size=8.0
	        bar_font_size=9
	        unset mako_font_size
	        ;;
	esac
	sed -e "s/{% font_size %}/$$kitty_font_size/;" < sway/kitty.conf > "$$kitty_dest"
	sed -e "s/{% window_title_font_size %}/$$window_title_font_size/; s/{% bar_font_size %}/$$bar_font_size/;" < sway/config > "$$sway_config_dest"
	maybe_mkdir "$$HOME/.config/mako"
	if [ "$${mako_font_size+set}" = set ]; then
	    sed -e "s/{% fontsize %}/$$mako_font_size/;" < sway/mako-config > "$$HOME/.config/mako/config"
	else
	    echo "Unrecognized host '$$hostname_short', not copying mako-config."
	fi
	dest="$$HOME/bin/"
	maybe_mkdir "$$dest"
	(cd sway/bin && for f in *; do
	    copy_to "$$f" "$$dest"
	done)
	bash_dest="$$HOME/.dotfiles/bash/rc_post/"
	maybe_mkdir "$$bash_dest"
	rsync -av --exclude 'README' --exclude 'README.*' bash/ "$$bash_dest/"

test:
	@runtime_dir="$$(mktemp -d)"
	trap 'rm -rf "$$runtime_dir"' EXIT
	output="$$(XDG_RUNTIME_DIR="$$runtime_dir" sway -C -c sway/config 2>&1)" || {
	    case "$$output" in
	        *"Unable to create backend"*|*"Could not connect to remote display"*)
	            echo "Skipping sway config validation: no display backend available."
	            exit 0 ;;
	    esac
	    printf '%s\n' "$$output" >&2
	    exit 1
	}
