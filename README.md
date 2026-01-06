# Vesihiisi

> ...sihisi hississä.

Yet another Lisp.

## Philosophy

* Make the core as elegant as possible.
* If we must choose, prioritize speed over space. We anticipate slightly modern
  system requirements of a 64-bit processor and megabytes of RAM. If you must
  run Lisp on your IoT toaster, look elsewhere (you do have plenty of existing
  options even there).

## Current State

* Rudimentary REPL
    - Simple literals (fixnums `23`, characters `#"c"`, booleans `#t` & `#f`, strings "foo")
    - Special forms `(def foo 23)`, `(if #t then what)` `(let ((ans 42)) ans)` and
      `(quote (quoth he))`
    - Global variable references `foo`
    - Functions `(def id (fn (x) x))` and calls `(id (quote ego))` including varargs
      `(fn (x y . zs) zs)`.
        * Full tail call optimization
    - Also debug printing from compilation (with `-d`)
* Stop-and-copy GC using the elegant Cheney algorithm

## Immediate Goals

* Standard library (roughly to par with R4RS Scheme)
* Complete reader
    - Missing `'`, negative and hex literals char escapes etc.
* Macroexpansion and `quasiquote`
* Delimited continuations (with winders)
* Safe for space
    - Seems like the only thing remaining is not tracing dead roots in VM
      registers

## Lofty ambitions

* Non-blocking IO etc. (Concurrent ML)
* Get to a JIT, even if it is just a simple method JIT.
* Generational GC
* FFI
* Native threads
    - Parallel GC
* Concurrent (or just incremental) GC
