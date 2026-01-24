#! /bin/sh

export VSHS_HOME=../..

cat ./prelude.lisp ./gabriel/cpstak.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/divrec.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/tak.lisp ./postlude.lisp | ../../vesihiisi -
