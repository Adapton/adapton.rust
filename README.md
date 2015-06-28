Adapton in Rust  [![Travis](https://api.travis-ci.org/plum-umd/adapton.rust.svg?branch=master)](https://travis-ci.org/plum-umd/adapton.rust)
========================

A general-purpose **Incremental Computation** (IC) library for Rust.

**Based on**:

- The paper [_Incremental Computation with Names_, 2015](http://arxiv.org/abs/1503.07792).

- A prior [OCaml implementation](https://github.com/plum-umd/adapton.ocaml).

Development Activities
-----------------------
- The library exposes a small core interface.  
See the [`Adapton` trait](https://github.com/plum-umd/adapton.rust/blob/master/src/adapton_sigs.rs#L7).  

- The library implements this interface with an imperative data structure.  
See [`AdaptonState`](https://github.com/plum-umd/adapton.rust/blob/master/src/adapton_state.rs).  
See also: [_Incremental Computation with Names_, 2015](http://arxiv.org/abs/1503.07792).

- Rust macros provide syntactic sugar.  
See [`adapton_syntax`](https://github.com/plum-umd/adapton.rust/blob/master/src/adapton_syntax.rs).  

**Testing:**

- Lazy Evaluation (simple, pure caching).

- Function Caching:
  - Implements Bill Pugh's notion of IC, pure function caching.

- Structural Adapton:
  - changeable input cells
  - bidirectional DCG structure
  - dirtying traversal; repair traversal.

- Nominal Adapton:
  - first-class names
  - nominal memoization

- [**Incremental Data Structures and Algorithms**](https://github.com/plum-umd/adapton.rust/blob/master/src/structures.rs).
  - generic list and tree traits
  - balanced trees from lists ("unfold")
  - tree folds (left-to-right, right-to-left, bottom-up)
  - mergesort
  
Future work
============

Test Infrastructure
----------------------
- random interactions:
  - generate random inputs, edits
  - generate random observations

Data Structures and Algorithms
-------------------------------------------
- tries that represent sets, maps,
- generic fixed-point loop
- graphs, graph exploration algorithms (e.g., search)


Technical Debt
================

Design Issues:
---------------
- canonical balanced trees for sequences: Data only at leaves (B-Tree style).

Rust-Specific:
--------------------
- The concept of "object safety" seems to bite me a lot in naive designs.
I sidestepped this problem in `adapton_state.rs` twice: by writing `Producer::copy` and the `ShapeShifter` trait.  Both avoid returning a `Self`.  
Is there a better way?  
- Do I need really need `Rc<Box<Fn (_) -> _>>` instead of `Rc<Fn (_) -> _>`? (Why?)  
- Done:
  - http://users.rust-lang.org/t/trait-objects-with-associated-types/746/16?u=matthewhammer  

 
