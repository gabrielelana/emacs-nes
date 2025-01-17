# Run with $ EMACS=~/opt/emacs-latest/bin/emacs make foo

EMACS ?= emacs
CASK ?= cask

all: clean prepare compile test clean

prepare:
	${CASK} install

test:
	${CASK} exec ert-runner --load *.el test/*-test.el

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile *.el

clean:
	${CASK} clean-elc

.PHONY: prepare test compile clean
