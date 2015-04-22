(Nominal) Adapton in Rust
==========================

Based on [this OCaml implementation](https://github.com/plum-umd/adapton.ocaml).

Development Activities
-----------------------

 - Building Adapton in Rust "outwards" from a basic, core interface.

   The core library implements this interface.  I will use many baby
   steps (see below). To check this interface and run experiments, we
   need to implement user code.  Some ideas are below.

 - I am learning Rust in the process.  See detailed Q&A below.

*Next steps:*

 - Lazy Evaluation (simple, pure caching).

 - Function Caching:
   - Nominal/Classical caching, explicit eviction.
   - Names cannot be "double-used", cells cannot change.
   - Implements Bill Pugh's notion of IC, pure function caching.

 - Full Nominal Adapton:
   - changeable input cells,
   - Bidirectional DCG structure structure
   - dirtying traversals, repair traversals.


Future work
============

Basic Data Structures and Algorithms
-------------------------------------------
 - lazy, memoized lists
 - mergesort
 - generic fixed-point loop
 - balanced trees to represent sequences
 - balanced trees to represent sets
 - graph exploration algorithms (e.g., search)

Rust Q&A
---------

 - http://users.rust-lang.org/t/trait-objects-with-associated-types/746/16?u=matthewhammer
