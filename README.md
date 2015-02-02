(Nominal) Adapton in Rust
==========================

Based on [this OCaml implementation](https://github.com/plum-umd/adapton.ocaml).

Development Activities
-----------------------

 - Building Adapton in Rust "outwards" from a basic, core interface.

   - The core library implements this interface.
     I will use several baby steps (see below).

   - To run experiments, we need to implement user code.
     I will also implement simple small-step evaluators, to better
     understand and illustrate AAM + IC via Adapton.

 - Learning Rust in the process.  See detailed Q&A below.

TODO: Core Library
-----------------
  - <del> Basic articulation interface </del>
  - <del> "Lazy recalc" (no caching). </del>

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


TODO: Basic Data Structures and Algorithms
-------------------------------------------
 - <del> lazy, memoized lists. </del>
 - <del> mergesort. </del>
 - <del> generic fixed-point loop. </del>

*Next steps*:

 - balanced trees to represent sequences
 - balanced trees to represent sets
 - graph exploration algorithms (e.g., search)

TODO: Incremental Program Evaluators
-----------------------------------------------------------

*Next steps:*

 - Arithmetic expressions, with small-step expression trace
 - Let-language (arithmetic and binding, no loops)
 - Loops (first-order)
 - Lambda abstractions, closures


Rust Q&A
---------

  - When are explicit lifetime paremters needed on:
    - Q: type delcarations.
      A: When they use traits instead of concrete types (?).

    - Q: function paramters/results.
      A: When they use traits instead of concrete types (??).
 
  - <del> Can I implement data structures with sharing? </del>
     - See `examples/exp.rs` for a baby version of a small-step interpreter.

  - <del> How do I .invoke an Invoke object? </del> 
     - See `examples/invoke.rs`
