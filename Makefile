EMACS ?= emacs

.PHONY: test

test:
	$(EMACS) -Q --batch -L . -l test/test.el -eval '(ert-run-tests-batch-and-exit)'
