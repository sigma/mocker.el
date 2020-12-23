EMACS ?= emacs

.PHONY: build test clean

build:
	keg build

test:
	keg exec $(EMACS) --batch -l test/mocker-test.el -f ert-run-tests-batch-and-exit

clean:
	keg clean
