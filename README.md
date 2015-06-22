Adapton in Rust
========================

A general-purpose **Incremental Computation** (IC) library for Rust.

**Based on**:

 - [the paper _Incremental Computation with Names_, 2015](http://arxiv.org/abs/1503.07792).

 - [an earlier OCaml implementation](https://github.com/plum-umd/adapton.ocaml).

Development Activities
-----------------------

 - Building Adapton in Rust around core interface, the
   [`Adapton trait`](https://github.com/plum-umd/adapton.rust/blob/master/src/adapton_sigs.rs#L7).
    
 - The library implements this interface with an imperative data structure,
   [`AdaptonState`](https://github.com/plum-umd/adapton.rust/blob/master/src/adapton_state.rs).

 - I am learning Rust in the process.  See detailed Q&A below.

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

 - Incremental Data Structures and Algorithms:
   - lazy, memoized lists
   - balanced trees to/from lists
   - tree folds (lr, rl, up)
   - mergesort

Future work
============

Data Structures and Algorithms
-------------------------------------------
 - tries that represent sets, maps,
 - generic fixed-point loop
 - graphs, graph exploration algorithms (e.g., search)


Technical Debt
================

Pending issues:
-----------------
 - Canonical balanced trees for sequences: Where should data go? Only at leaves (B-Tree style)?
 - List/Tree intro forms should take unboxed, references, or boxes?
   error: type annotations required: cannot resolve `<_ as structures::ListT<A, _>>::List == _` [E0284]
 - Implement `ArtId::Structural` for cell and thunk to do hashing internally, not externally.

Rust Q&A
---------

 - http://users.rust-lang.org/t/trait-objects-with-associated-types/746/16?u=matthewhammer

 - The concept of "object safety" seems to bite me a lot in naive designs:
 
```
src/adapton_impl.rs:653:51: 653:111 error: cannot convert to a trait object because trait `adapton_impl::Producer` is not object-safe [E0038]
...
src/adapton_impl.rs:653:51: 653:111 note: method `eq` references the `Self` type in its arguments or return type
```

 I sidestepped this problem in `adapton_state.rs` twice: by writing `Producer::copy` and the `ShapeShifter` trait.  Both avoid returning a `Self`.

 - Do I need really need `Rc<Box<Fn (_) -> _>>` instead of `Rc<Fn (_) -> _>`? (Why?)

 
