# Vesihiisi

> ...sihisi hississä.

Yet another Lisp.

## Philosophy

* Make the core as elegant as possible.

## Current State

* Rudimentary REPL
    - Simple literals (fixnums `23`, characters `#"c"`, booleans `#t` & `#f`, strings "foo")
    - Special forms `(def foo 23)`, `(if #t then what)` and `(quote (quoth he))`
    - Also debug printing from compilation (can't even be turned off)

## Immediate Goals

* Complete bytecode compiler
    - TCO
    - Safe for space
* Complete bytecode VM
* Add GC

## And Then

* Primitive procedures e.g. `fx+`, `new`/`make`
* Standard library
* Macroexpansion

## Lofty ambitions

* Multimethods
* Get to a JIT, even if it is just a simple method JIT.
* FFI
