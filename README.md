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

! Highly experimental, not rounded out or even robust enough to be usable.

But what we do have going on in various states of completeness:

* NaN-tagged value representation
* Last record slot can be indexed i.e. arrays are not special
* Stop-and-copy GC using the elegant Cheney algorithm
* Register-based bytecode VM
    - A bytecode compiler that actually tries to do good register allocation
    - Compressed debug info on bytecode methods
* Lisp-1
* Full tail call optimization
* Multimethods
	- But with no overriding whatsoever (methods must have zero overlap)
* First-class multi-shot continuations
* Unhygienic macros
    - Including symbol/identifier macros
* UTF-8 ports and strings
* Self-hosting reader, macroexpander and CLI interface including the REPL

## Immediate Goals

* Exception handling (the REPL still crashes on your first mistake)
* Stack traces
* Standard library (roughly to par with R4RS Scheme)
* Complete reader
    - Missing e.g. reader macros for `quasiquote` and `unquote(-splicing)`
* Delimited continuations (with winders)
* Safe for space
    - Seems like the only thing remaining is not tracing dead roots in VM
      registers

## Lofty ambitions

* Namespace system
* Non-blocking IO etc. (Concurrent ML)
* Get to a JIT, even if it is just a simple method JIT.
* Generational GC
* FFI
* Native threads
    - Parallel GC
* Concurrent (or just incremental) GC
