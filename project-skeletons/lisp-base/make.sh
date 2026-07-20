#!/bin/sh

LISP="${LISP:-sbcl}"
CLFLAGS="${CLFLAGS:---non-interactive --no-userinit}"

clean () {
    rm -f -- *.fasl **/*.fasl
}

check () {
    # shellcheck disable=SC2086
    CL_SOURCE_REGISTRY="$PWD//" "$LISP" $CLFLAGS \
                      --eval "(require 'asdf)" \
                      --eval '(asdf:test-system "__PROJECT-NAME__")'
}

coverage () {
    if test -e coverage/coverage.lisp
    then
        # shellcheck disable=SC2086
        CL_SOURCE_REGISTRY="$PWD//" "$LISP" $CLFLAGS --load coverage/coverage.lisp
    else
        mkdir coverage
        cat <<EOF > coverage/coverage.lisp
(require :asdf)
(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))
(asdf:load-system "__PROJECT-NAME__" :force t)
(asdf:test-system "__PROJECT-NAME__")
(sb-cover:report "coverage/" :form-mode :car)
(uiop:quit)
EOF
        coverage
    fi
}

install_hooks () {
    git config core.hooksPath .githooks
}

documentation () {
    sbcl --noinform --non-interactive \
         --eval '(asdf:make "__PROJECT-NAME__/documentation")'
    makeinfo __PROJECT-NAME__.texi
    makeinfo --plaintext -o README __PROJECT-NAME__.texi
}

default () {
    if test -e build.lisp
    then
        # shellcheck disable=SC2086
        "$LISP" $CLFLAGS --load build.lisp
    else
        echo "No build.lisp -- This doens't look like an executable. Running tests..."
        check
    fi
}

"${@:-default}"
