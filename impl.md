# Language Implementation

## Heap Objects

The word **object** is used in the GC sense of a value allocated on the garbage-collected heap, not
in the sense of object-oriented programming.

A pointer to a heap object points at the start of its data. The data is preceded by the type pointer
(a heap object reference) and in the case of flex types (e.g. arrays and strings) the type ref is
preceded by the object length (in slots or bytes).

## Object References

Object references are 64 bits. Thus objects are at least 64-bit aligned (because even if the
object's fields could be less aligned, at least its type is an object reference). So the three LSB:s
of object would always be 0b000. Instead of wasting those precious bits so we use tagged pointers:

* ...000 61-bit fixnum (a tag of all zeros simplifies arithmetic somewhat)
* ...100 32-bit flonum
* ...010 32-bit (actually less, see Unicode) char
* ...110 bool
* ...(00)1 Heap object reference
    - Heap object type reference can also be (invisible in-lang):
        * ...11 GC relocation pointer (replaces type reference in a relocated
          object)
        * 0b001 alignment hole (just needs to be distinct from both a type /
    relocation pointer and a length fixnum for heap scanning)

## Allocation

* `tospace.free +=` 16 if `flex`, else 8.
* Align `tospace.free` to whatever `type` requires.
* `tospace.free -= 16`.
* `*tospace.free = length; ++tospace.free` if flex.
* `*tospace.free = type; ++tospace.free`.
* `oref = tospace.free | heap_tag`.
* Zero-initialize slots/bytes, incidentally advancing `tospace.free`.

## GC Scanning Objects

* Start at the start of tospace.
* While `tospace.scan < tospace.free`:
    - `++tospace.scan` while `*tospace.scan` is an alignment hole.
    - Scan object:
        * If `*tospace.scan` is a fixnum, `flex = true` and `++tospace.scan`.
        * Mark type, re-tagging with slots/bytes bit.
        * Find object `length` from length field (if `flex`) or type.
        * If object is bytes, skip ahead `length` bytes and oref-align forwards.
        * else scan `length` slots `*tospace.scan = mark(*tospace.scan)`.

## Bytecode ISA

### Goals

* Keep the set of operations small as inline primitives would have to be wrapped
  in functions anyway; at that point might as well wrap separate C functions
  into objects. A potential JIT can still inline those special functions. This
  is basically the Self approach (though they went as far as to have 3-bit
  opcodes, 5-bit operands and a baseline JIT instead of actually running the
  bytecode at all).
* Operations should do as much as possible. For bytecode VM:s CISC is better
  than RISC since every instruction is a potential branch misprediction
  (**instruction dispatch overhead** or simply/vaguely "interpreter overhead").
  Yes the various variations of "threaded code" and ultimately even the simplest
  JIT can reduce that overhead but the ISA definition should also do its part.
* Instruction encoding should be as small as possible to improve data cache
  utilization and reduce heap size. Maybe we won't be as brief as Self but e.g.
  there is no need for a full opcode byte when clearly we will not have anything
  near to 255 opcodes.
* Nevertheless try to keep it fairly strightforward.

## Bytecode Compiler

FIXME: `call`s should never have a join point as continuation => make `call`
a stmt instead of a transfer?
TODO: Avoid clover recording by splitting `IRLabel` from `IRName`
TODO: Is lambda lifting useful (since register allocation goes backwards)?
OPTIMIZE: Avoid redundant loads for consts (as we do for clovers)
OPTIMIZE: Constant and copy propagation if convenient

Four passes:

1. CPS conversion and global variable load/store detection
2. Liveness and abstract closure conversion + local lambda lifting
3. Clover loads and closure allocation
4. Bytecode generation with register allocation

### CPS conversion and global variable load/store detection

* Name all subexpressions and build CFG.
	- CFG will be a DAG (no loops at this level) and generated and stored
	  in reverse post-order.
* Keep environment of local bindings (linked list of frames containing maps
  from source symbols to `IRName`s
* Do alpha conversion using environment
* Globals are mutable and so should not be treated like normal bindings by
  later passes.
    - If a `(def ...)` is detected outside global scope, error.
    - Uses and `set!`s of globals turn into `(global ...)` and
      `(global-set! ...)`.
    		* `(set! ...)` of a local is also an error (even if said local
    		  shadows a global [which the compiler can't even detect...]).
	    		  
### Liveness and abstract closure conversion + local lambda lifting

* Analyses for the benefit of the clover loads and register allocation
  passes (respectively).
* Post-order CFG DAG traversal (= go backwards through blocks and their
  contents)
* Keep a set of live variables.
	- Each variable def removes the variable from the set (variable cannot
	  be live before its definition)
	- Each variable use adds the variable to the set (= variable becomes
	  live on last use)
* At transfer (block terminator)
    - `goto` (goes to join point):
        * Let additional params be params of target without first param
        * Live-ins is additional params with value arg (target does not
          escape)
    		* Append additional params to args
    - `if` (goes to split targets):
        * Compute live-outs as union of live-ins of targets.
        * Then add condition to it (targets do not escape)
	- `call`:
	    * Create initial set of live variables from args
	    * Add clovers of target to the set (as for `fn`)
	    * Record clovers
    - `tailcall`: just create initial set of live variables from args
    - `return`: just create initial set of live variables from args
* At each statement
    - Local `fn`:
	    * Process def normally
	    * Recursively run analysis.
	    * Process entry point clovers as uses in parent
	- Else just compute live-ins from live-outs
* At block entry
    - Account for the parameters as defs.
    - Then for:
		* Function entry points (main entry point, return points):
			- Abstract closure conversion: Record remaining live variables
			  (except the closure itself) as being in the closure.
		* Join points:
			- Local lambda lifting: Append remaining live variables to
			  parameter list of block.
			    * Yes this breaks the SSA property but this is not an
			      optimizing compiler so no need to maintain that (and
			      perhaps even if it was we would already be past the main
			      optimization phase).
		* Split (`if`) targets:
			- Record set of live variables into a side table for `if` to use
				
### Clover loads and closure allocation

* Pre-order CFG DAG traversal (= go forwards through blocks and their
  contents)
    - Allows loading as late as possible. Post-order would put all loads
      into entry point even if the values are not needed on all branches.
      Otherwise this could be integrated into codegen like register
      allocation is.
* Generates a new CFG (that may steal parts of the old one).
* Keep a mapping from variables to index in clover and `isLoaded`.
* Initialize a new mapping at each entry point.
* For a statement or transfer
	- Process each argument
		* If a clover and not yet loaded, emit load and add to mapping
			- Again the emitted `(let $x (clover ...))` breaks SSA but we
			  are past caring.
	- Emit statement or transfer
		
### Bytecode generation with register allocation

* Post-order CFG DAG traversal (= go backwards through blocks and their
  contents)
* Keep a mapping from variables to registers
* At transfer
	- `goto`
		* Get mapping from target
		* Replace first param of target with first arg, keeping register
		  i.e. an actual phi.
		* Emit `br ${displacement}` (unless displacement is 0)
	- `if`
		* Create mapping as union of mappings of targets
		* Process cond like stmt args (alloc arbitrary low reg if not yet
		  found in mapping)
		* Emit `brf rArg ${displacement}`
	- `call`
		* Emit `call ${arity}`
		* Create mapping that puts callee in `r0`, continuation in `r1` and
		  args in `r2` etc.
		* Find live-outs (clovers of continuation block).
		* Allocate live-outs not found in mapping after last arg and add
		  them to mapping
		* Emit `r1 = frame ${live-outs}`
	- `tailcall`:
		* Emit `tailcall ${arity}`
		* Create mapping that puts callee in `r0`, continuation in `r1` and
		  args in `r2` etc.
	- `return`:
		* Emit `ret`
		* Create mapping that puts dest in `r1` and arg in `r2`.
* At each statement
	- Remove definiends from mapping
	- Allocate registers for arguments missing in mapping (just take the
	  lowest free one, if all 255 are in use it is well about time to crash
	  and burn).
	- Emit operation
		* `closure` uses the same variable-width bitmask format as `frame`
* At block entry
	- Entry point blocks: Emit shuffle to match calling convention
	- Split targets
		* Like static branch prediction we assume that forward branches are
		  usually not taken i.e. the falsy branch is penalized:
		* Truthy branch: do nothing
		* Falsy branch:
			- Determine intersection of variables in mappings of this and
			  its sibling.
			- Generate shuffle to make the entry of this block match its
			  sibling (except that it can have extra variables, in registers
			  not used by the favorite sibling).
	- Join points: do nothing
	- Save resulting block entry mapping, except for function main entry
