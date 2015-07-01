[Adapton](http://adapton.org) in Rust  [![Travis](https://api.travis-ci.org/plum-umd/adapton.rust.svg?branch=master)](https://travis-ci.org/plum-umd/adapton.rust)
====================================================================================

A general-purpose **Incremental Computation** (IC) library for Rust.  
[available on crates.io](https://crates.io/crates/adapton)  

**Based on**:  

- The paper [_Incremental Computation with Names_, 2015](http://arxiv.org/abs/1503.07792).  
- A prior [OCaml implementation](https://github.com/plum-umd/adapton.ocaml).  

Library Components:
-----------------------

- The library exposes a small **core interface**.  
See the [`Adapton` trait](https://github.com/plum-umd/adapton.rust/blob/master/src/adapton_sigs.rs#L7).  

- The library uses rust macros to provide **syntactic sugar**.  
See [`adapton_syntax`](https://github.com/plum-umd/adapton.rust/blob/master/src/adapton_syntax.rs).  

- The library implements this interface with an **imperative data structure**, and **without garbage collection**.  
See [`AdaptonState`](https://github.com/plum-umd/adapton.rust/blob/master/src/adapton_state.rs).  

- The library provides generic **incremental data structures and algorithms**.  
See also: [`structures.rs`](https://github.com/plum-umd/adapton.rust/blob/master/src/structures.rs):  
  - generic lists
  - generic binary trees
  - tree unfold: balanced trees from a step function
  - tree folds (left-to-right, right-to-left, bottom-up)
  - sorting: mergesort
- **Next**:  
  - tries that represent sets, maps,
  - generic fixed-point loop
  - graphs, graph exploration algorithms (e.g., search)


Supported IC Paradigms:
----------------------

- **Pure Function Caching**  
*Bill Pugh and Tim Teitelbaum, POPL 1989.*
  - hash-cons'd, purely-functional data structures
  - memoized function calls (to pure computations)

- **(Structural) Adapton**: Composable, Demand-Driven Incremental Computation.  
*Matthew A. Hammer, Yit Phang Khoo, Michael Hicks and Jeffrey S. Foster.*  
**PLDI 2014.**  
  - changeable input cells
  - bidirectional DCG structure
  - dirtying traversal; repair traversal.

- **Nominal Adapton:** Incremental Computation with Names  
*Matthew A. Hammer, Joshua Dunfield, Kyle Headley, Nicholas Labich, Jeffrey S. Foster, Michael Hicks, David Van Horn.*  
*March 2015.*  
  - first-class names
  - nominal memoization
  
Future work
============

Test Infrastructure
----------------------
- random interactions:
  - generate random inputs, edits
  - generate random observations

- [report memory statistics](http://stackoverflow.com/questions/30869007/how-to-benchmark-memory-usage-of-a-function)

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
See also: [0255-object-safety](https://github.com/rust-lang/rfcs/blob/master/text/0255-object-safety.md)  
- Do I need really need `Rc<Box<Fn (_) -> _>>` instead of `Rc<Fn (_) -> _>`?  
Why?  
- Done:
  - [trait-objects-with-associated-types](http://users.rust-lang.org/t/trait-objects-with-associated-types/746/16?u=matthewhammer)

 
