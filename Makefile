EMACS ?= emacs
BATCH = $(EMACS) --batch -L . -L tests

TEST_FILES = $(wildcard tests/test-*.el)

.PHONY: test test-protocol test-handlers test-scram test-sts test-dcc test-socks clean

test: ## Run all tests
	$(BATCH) -l test-helper $(foreach f,$(TEST_FILES),-l $(f)) -f ert-run-tests-batch-and-exit

test-protocol: ## Run protocol tests only
	$(BATCH) -l test-helper -l tests/test-protocol.el -f ert-run-tests-batch-and-exit

test-handlers: ## Run handler dispatch tests
	$(BATCH) -l test-helper -l tests/test-handlers.el -f ert-run-tests-batch-and-exit

test-scram: ## Run SCRAM-SHA-256 tests
	$(BATCH) -l test-helper -l tests/test-sasl-scram.el -f ert-run-tests-batch-and-exit

test-sts: ## Run STS tests
	$(BATCH) -l test-helper -l tests/test-sts.el -f ert-run-tests-batch-and-exit

test-dcc: ## Run DCC tests
	$(BATCH) -l test-helper -l tests/test-dcc.el -f ert-run-tests-batch-and-exit

test-socks: ## Run SOCKS5 proxy tests
	$(BATCH) -l test-helper -l tests/test-socks.el -f ert-run-tests-batch-and-exit

lint: ## Byte-compile all files (warnings as errors)
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el

clean: ## Remove compiled files
	rm -f *.elc tests/*.elc

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
