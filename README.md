# Vesihiisi

> ...sihisi hississä.

Yet another Lisp.

## Philosophy

> Lauloi päivät pääksytysten, yhytysten yöt saneli<br/>
> muinaisia muisteloita, noita syntyjä syviä,<br/>
> joit' ei laula kaikki lapset, ymmärrä yhet urohot<br/>
> tällä inhalla iällä, katovalla kannikalla. 

* Make the core as elegant as possible.
* If we must choose, prioritize speed over space. We anticipate slightly modern
  system requirements of a 64-bit processor and megabytes of RAM. If you must
  run Lisp on your IoT toaster, look elsewhere (you do have plenty of existing
  options even there).

## Current State

Basically a proof-of-concept for:

* NaN-tagged value representation
* Last record slot can be indexed i.e. arrays are not special
* Generational GC
	- Very simple arrangement, basically plopped a nursery in front of a
	  semispace collector and started juggling the semispaces around as the
	  Cheney copying [actually only cares about the destination space layout](https://www.more-magic.net/posts/internals-gc.html).
	  Of course a write barrier also needed to be (somewhat painstakingly)
	  added.
* Register-based bytecode VM
    - With a bytecode compiler that actually tries to do good register
      allocation
    - Compressed debug info on bytecode methods
* Lisp-1
* Full tail call optimization
* Multimethods
	- But with no overriding whatsoever (method domains must have zero overlap)
* First-class multi-shot continuations
* Delimited continuations (with winders)
	- Exception handling and stack traces built on these
	    * Stack traces show some artefacts (frames of a trampoline continuation)
	      of the
	      [implementation strategy](https://okmij.org/ftp/continuations/implementations.html#delimcc-scheme)
* Self-hosting reader
    * Mostly consisting of an LL(1) parser combinator library (not supporting
      empty productions as supporting those would require FOLLOW sets which
      cannot be conveniently computed in the freely composable idiom of parser
      combinators).
    * Of course C++ reader is needed to parse the file containing this, but
      then gets replaced.
    * Both readers are very basic e.g. not even negative numeric literals are
      supported (actually `-1` gets read as a symbol...).
* Unhygienic macros via self-hosting macroexpander
    - Including symbol/identifier macros
    * No local macros, but would be easy to add
* Standard library
    * Initial goal is to be roughly on par with R4RS Scheme, not quite there yet
* UTF-8 ports and strings
* Self-hosting CLI interface including the REPL
* FFI
	* Very limited e.g. only supports up to two arguments, no null-terminated
	  strings etc. but extending to usefulness should be merely tedious. At
	  least it can do `getchar`, `putchar` and `sin`.
* Method JIT
	- Very straightforward "template" JIT but should at least get rid of the
	  terrible branch misprediction costs and other interpeter overhead.
	  (Using a computed goto for the bytecode interpreter gave a nice speedup
	  over a loop and `switch` but is still nasty for branch predictors.)
		* Using a Smalltalk/Java -style JIT like this is a big reason for this
		  project. Traditionally Lisp is JITed in the sense of runtime machine
		  code generation but it does not have any warmup period where a
		  bytecode interpreter runs.
		* Currently this architecture just avoids generating machine code for
		  initialization code and other cold functions but eventually it could
		  provide runtime type information and basic block entry conts to an
		  optimizing JIT.
	* Missing typed parameters and varargs and the FFI probably also needs
	  fixing.
	* But worst of all the VM crashes in optimized builds. Since the JITed
	  code is the same, probably the C++ code has some UB that interacts badly
	  with the JITed code. Guessing that e.g. JITed code accidentally assumes
	  something is at least zero-initialized when the C++ standard definitely
	  makes no such guarantees. So we get to combine debugging optimized C++
	  (annoying) with debugging JITed code (extremely inconvenient) for double,
	  no, quadruple the fun! Perhaps try UBSan instead?

## Next Goals

* Safe for space
    - Seems like the only thing remaining is not tracing dead roots in VM
      registers
* Namespace system

## Lofty ambitions

* Optimizing JIT, preferably self-hosted
* Non-blocking IO etc. (Concurrent ML)
* Native threads
    - Parallel GC
* Concurrent (or just incremental) GC

## Trying It Out

```sh
> make dev && VSHS_HOME=. ./vesihiisi-dev
```

also `make run-dev` is equivalent to

```sh
> make dev && VSHS_HOME=. ./vesihiisi-dev -d
```

and gives (a LOT) of debug info including reader and macroexpansion results and
more importantly both the bytecode and JIT compilers.
