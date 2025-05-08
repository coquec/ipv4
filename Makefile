EMACS ?= emacs

TEST_DIR=$(shell pwd)/test

# Run all tests by default.
MATCH ?= test-ipv4-*

.PHONY: test

test:
	$(EMACS) --batch -L . -L $(TEST_DIR) -l ipv4.el -l test.el -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'
