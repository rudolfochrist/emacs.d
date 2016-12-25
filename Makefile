SRCS_LISP = $(wildcard lisp/*.el)
SRCS_THEME = $(wildcard themes/*.el)
SRCS_SITE_LISP = $(wildcard site-lisp/**/*.el)
SRCS = init.el $(SRCS_LISP) $(SRCS_THEME) $(SRCS_SITE_LISP)

EMACS ?= emacs
EMACSFLAGS := -Q --batch

all: byte-compile

.PHONY: compile
byte-compile: $(SRCS)
	$(EMACS) $(EMACSFLAGS) \
	--eval '(byte-recompile-directory "lisp" 0)' \
	--eval '(byte-recompile-directory "themes" 0)' \
	--eval '(byte-recompile-directory "site-lisp" 0)' \
	--eval '(byte-recompile-file "init.el" nil 0)' \

.PHONY: clean
clean:
	-rm $(SRCS:.el=.elc)


