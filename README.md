# Vesihiisi

> ...sihisi hississä.

Yet another Lisp.

## Philosophy

* Make the core as elegant as possible.

## Current State

* Rudimentary REPL
    - Constants work, including `(quote ...)`
    - Also debug printing from compilation (can't even be turne off)

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

