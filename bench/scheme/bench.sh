#! /bin/sh

export VSHS_HOME=../..

cat ./prelude.lisp ./gabriel/browse.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/cpstak.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/ctak.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/deriv.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/destruc.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/diviter.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/divrec.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/nboyer.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/puzzle.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/tak.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/takl.lisp ./postlude.lisp | ../../vesihiisi -

cat ./prelude.lisp ./gabriel/triangl.lisp ./postlude.lisp | ../../vesihiisi -
