EMACS ?= emacs
BATCH = $(EMACS) --batch -L . -L tests

TEST_FILES = $(wildcard tests/test-*.el)

.PHONY: test test-protocol clean

test: ## Run all tests
	$(BATCH) -l test-helper $(foreach f,$(TEST_FILES),-l $(f)) -f ert-run-tests-batch-and-exit

test-protocol: ## Run protocol tests only
	$(BATCH) -l test-helper -l tests/test-protocol.el -f ert-run-tests-batch-and-exit

lint: ## Byte-compile all files (warnings as errors)
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el

clean: ## Remove compiled files
	rm -f *.elc tests/*.elc

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
