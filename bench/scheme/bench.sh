#! /bin/sh

cat ./prelude.lisp ../../base/bootstrap.lisp ./gabriel/tak.lisp ./postlude.lisp \
    | ../../vesihiisi -

cat ./prelude.lisp ../../base/bootstrap.lisp ./gabriel/cpstak.lisp ./postlude.lisp \
    | ../../vesihiisi -
