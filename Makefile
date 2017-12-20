EMACS ?= emacs
EMACSFLAGS := -Q --batch

SITELISP = $(shell find site-lisp -name "*.el")
SITELISPDIR = $(PWD)/site-lisp

all: compile-site-lisp

.PHONY: compile-site-lisp
compile-site-lisp: $(SITELISP)
	$(EMACS) -Q --batch --eval "(byte-recompile-directory \"$(SITELISPDIR)\" 0)"


