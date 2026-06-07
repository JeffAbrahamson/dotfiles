.PHONY: install test

install:
	@if [ "X$$HOSTNAME" != Xbirdsong ] && [ "X$$HOSTNAME" != Xstarshine ] && \
	    [ "X$$HOSTNAME" != Xmorning ] && [ "X$$HOSTNAME" != Xvogel ] && \
	    [ "X$$HOSTNAME" != Xtau-ceti ] && [ "X$$HOSTNAME" != Xxinghan ]; then \
	    echo "Not setting ufw firewall rules on this host."; \
	    exit 0; \
	fi
	@. ../script/lib.sh; \
	echo "Setting up firewall, will require sudo."; \
	have_sudo=1; \
	if sudo -n true 2>/dev/null; then \
	    :; \
	elif sudo -v; then \
	    :; \
	else \
	    echo "Warning: sudo not available; skipping firewall setup."; \
	    have_sudo=0; \
	fi; \
	if [ "$$have_sudo" -eq 1 ]; then \
	    sudo ufw default allow outgoing; \
	    sudo ufw default deny incoming; \
	    sudo ufw allow ssh/tcp; \
	    sudo ufw limit ssh; \
	    timeout 10 sudo ufw --force enable; \
	    ufw_enable_status=$$?; \
	    if [ $$ufw_enable_status -eq 124 ]; then \
	        echo "Error: timed out waiting 10 seconds for ufw enable; firewall setup aborted."; \
	        exit 1; \
	    elif [ $$ufw_enable_status -ne 0 ]; then \
	        exit $$ufw_enable_status; \
	    fi; \
	    if [ "X$$HOSTNAME" = "Xsiegfried" ]; then \
	        sudo ufw allow 80/tcp; \
	        sudo ufw allow 53/udp; \
	    fi; \
	    sudo ufw status verbose; \
	fi

test:
	@:
