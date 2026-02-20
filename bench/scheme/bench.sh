#! /bin/sh

export VSHS_HOME=../..

for benchmark in gabriel/*.lisp
do
  cat ./prelude.lisp $benchmark ./postlude.lisp | ../../vesihiisi -
done

for benchmark in kvw/*.lisp
do
  cat ./prelude.lisp $benchmark ./postlude.lisp | ../../vesihiisi -
done


for benchmark in numeric/*.lisp
do
  cat ./prelude.lisp $benchmark ./postlude.lisp | ../../vesihiisi -
done
