.PHONY: test

test:
	@echo 'Running bin smoke tests.'; \
	ok=0; \
	for file in *; do \
	    if [ -f "$$file" ] && [ -x "$$file" ] && [ "$${file##*~}" = "$$file" ]; then \
	        echo "  Running $$file."; \
	        ./$$file || ok=1; \
	    fi; \
	done; \
	exit "$$ok"
