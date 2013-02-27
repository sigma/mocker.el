PROJECT=mocker
EMACS=emacs

ELS=$(wildcard *.el)
ELCS=$(ELS:.el=.elc)

EFLAGS=
BATCH=$(EMACS) $(EFLAGS) -batch -q -no-site-file -eval \
  "(setq load-path (cons (expand-file-name \".\") load-path))"

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

all: $(ELCS)

clean:
	rm -f $(ELCS)

test: all
	$(BATCH) -l $(PROJECT)-tests.el -f ert-run-tests-batch-and-exit
