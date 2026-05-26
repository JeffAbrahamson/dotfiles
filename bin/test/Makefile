.PHONY: test

test:
	@echo 'Running bin smoke tests.'; \
	for file in *; do \
	    if [ -f "$$file" ] && [ -x "$$file" ] && [ "$$file" != test.sh ] && \
	        [ "$${file##*~}" = "$$file" ]; then \
	        echo "  Running $$file."; \
	        ./$$file; \
	    fi; \
	done
