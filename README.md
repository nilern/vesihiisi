# Vesihiisi

> ...sihisi hississä.

Yet another Lisp.

## Philosophy

* Make the core as elegant as possible.

## Current State

* Rudimentary REPL
    - Simple literals (fixnums `23`, characters `#"c"`, booleans `#t` & `#f`, strings "foo")
    - Special forms `(def foo 23)`, `(if #t then what)` `(let ((ans 42)) ans)` and
      `(quote (quoth he))`
    - Global variable references `foo`
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

* Delimited continuations (with winders)
* Multimethods
* Non-blocking IO etc. (Concurrent ML)
* Get to a JIT, even if it is just a simple method JIT.
* FFI
* Native threads
