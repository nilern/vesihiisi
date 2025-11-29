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

