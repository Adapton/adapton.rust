(Nominal) Adapton in Rust
==========================

Based on this OCaml implementation: 
 https://github.com/plum-umd/adapton.ocaml

Development Activities
-----------------------

 - Building "outwards" from a basic, core interface.

   - The core library implements this interface.
     It will proceed in three baby steps (see below).

   - To run experiments, we need to implement user code.
     I will proceed by creating simple apps, as in our recent papers 
     (PLDI 2014, Nominal Adapton draft).

     I will also implement simple small-step evaluators, towards
     better understanding and illustrate AAM + IC via Adapton.


TODO: Core Library
-----------------
 - ~~ Basic articulation interface ~~

 - Language understanding: How do I .invoke an Invoke object?

 - Baby step 1: Lazy evaluation (simple thunks).
                [need .invoke, then done].

 - Baby step 2: Nominal/Classical caching, explicit eviction.
                Names cannot be "double-used", cells cannot change.
                Implements Bill Pugh's notion of IC, pure function caching.

 - Baby step 3: Bidirectional DCG structure structure, changeable input cells, 
                dirtying traversals, repair traversals.


TODO: Basic Data Structures and Algorithms
-------------------------------------------
 - ~~ Implement mergesort. ~~
 - ~~ Implement lazy, memoized lists. ~~
 - Implement memoized tries.
 - Implement memoized sequences.

TODO: Basic Incremental Evaluators 
-----------------------------------------------------------
 - Arithmetic expressions, with small-step expression trace
