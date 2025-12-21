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
    - Functions `(def id (fn (x) x))` and calls `(id (quote ego))` including varargs
      `(fn (x y . zs) zs)`.
        * Full tail call optimization
    - Stop-and-copy GC using the elegant Cheney algorithm
    - Also debug printing from compilation (can't even be turned off)

## Immediate Goals

* Multimethods to underpin e.g. `+` but also for their own sake
    - The design requires first adding (dynamically checked) parameter types and in turn for those
      to have sensible scoping efficiently, "templated methods" (e.g.
      `(fn (<t>) (fn ((: x <t>)) x))` should work but create only one method per `<t>`).
* `letfn` for local recursion
* Safe for space

## And Then

* Primitive procedures e.g. `new`/`make`, `apply`
* Standard library
* Macroexpansion

## Lofty ambitions

* Delimited continuations (with winders)
* Non-blocking IO etc. (Concurrent ML)
* Get to a JIT, even if it is just a simple method JIT.
* FFI
* Native threads
