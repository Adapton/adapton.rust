[Adapton](http://adapton.org) in Rust  [![Travis](https://api.travis-ci.org/cuplv/adapton.rust.svg?branch=master)](https://travis-ci.org/cuplv/adapton.rust)
====================================================================================

A general-purpose **Incremental Computation** (IC) library for Rust.  
[available on crates.io](https://crates.io/crates/adapton)  

**Based on**:  

- The paper [_Incremental Computation with Names_, OOPSLA 2015](http://arxiv.org/abs/1503.07792).  
- A prior [OCaml implementation](https://github.com/plum-umd/adapton.ocaml).  

Library Components:
-----------------------

- The library exposes a small **core interface**.
For details, see the documentation of `engine` interface:
```
  cargo doc
  open target/doc/adapton/engine/index.html
```

- The library uses rust macros to provide **syntactic sugar**.  
See [`macros.rs`](https://github.com/cuplv/adapton.rust/blob/master/src/macros.rs).  

- The library implements this interface with an **imperative data structure**, and **without garbage collection**.  
See [`engine.rs`](https://github.com/cuplv/adapton.rust/blob/master/src/engine.rs).

- The library provides generic **incremental data structures and algorithms**.  
See also: [`collections.rs`](https://github.com/cuplv/adapton.rust/blob/master/src/collections.rs).
**This collections module is a work in progress.**
  
- **Next**:  
  - sequences as random access zippers (See also: https://arxiv.org/abs/1608.06009)
  - tries that represent sets, maps (See also: https://arxiv.org/abs/1503.07792, Section 3.2),
  - generic fixed-point loop
  - graphs, graph exploration algorithms (e.g., search)


Supported Incremental Computation Paradigms:
--------------------------------------------

- **Pure Function Caching**:  
[*Incremental computation via function caching*](http://dl.acm.org/citation.cfm?id=75305)  
*Bill Pugh and Tim Teitelbaum.*  
**POPL 1989.**  
  - hash-cons'd, purely-functional data structures
  - memoized function calls (to pure computations)

- **Structural Adapton**:  
[Adapton: Composable, Demand-Driven Incremental Computation.](https://www.cs.umd.edu/~hammer/adapton/)  
*Matthew A. Hammer, Yit Phang Khoo, Michael Hicks and Jeffrey S. Foster.*  
**PLDI 2014.**  
  - changeable input cells
  - bidirectional DCG structure
  - dirtying traversal; repair traversal.

- **Nominal Adapton:**  
[Incremental Computation with Names](http://arxiv.org/abs/1503.07792)  
*Matthew A. Hammer, Joshua Dunfield, Kyle Headley, Nicholas Labich, Jeffrey S. Foster, Michael Hicks, David Van Horn.*  
**OOPSLA 2015.**  
  - first-class names
  - nominal memoization
  
Future work
============

- Benchmarking based on tests:
  - report time statistics
  - [report memory statistics](http://stackoverflow.com/questions/30869007/how-to-benchmark-memory-usage-of-a-function)

Technical Debt
================

Rust-Specific:
--------------------
- In `engine.rs` I wrote `Producer::copy` and the `ShapeShifter` trait.  Both avoid returning a `Self`.  
Is there a better way?  
See also: [0255-object-safety](https://github.com/rust-lang/rfcs/blob/master/text/0255-object-safety.md)  
- Do I need really need `Rc<Box<Fn (_) -> _>>` instead of `Rc<Fn (_) -> _>`?  
Why?  
- Done:
  - [trait-objects-with-associated-types](http://users.rust-lang.org/t/trait-objects-with-associated-types/746/16?u=matthewhammer)


Play.Rust-lang.Org
=====================
 - http://is.gd/4czIEG
