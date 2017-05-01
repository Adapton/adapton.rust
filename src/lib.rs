/*! 

Adapton for Rust
================

This Rust implementation embodies the latest implementation
[Adapton](http://adapton.org), which offers a foundational,
language-based semantics for general-purpose incremental computation.

**Programming interface in Rust:**
For an overview of its Rust programming interface, see the
documentation for the `adapton::macros` module, which wraps the
`adapton::engine` module with some convenient macros.

**See also**:

- [Presentations and benchmark results](https://github.com/cuplv/adapton-talk#benchmark-results)
- [IODyn: Adapton collections, for algorithms with dynamic input and output](https://github.com/cuplv/iodyn.rust)
- [Adapton Lab: Evaluation and testing](https://github.com/cuplv/adapton-lab.rust)


Background:
-----------

Adapton proposes the _demanded computation graph_ (or **DCG**), and a
demand-driven _change propagation_ algorithm. Further, it proposes
first-class _names_ for identifying cached data structures and
computations. 

The following academic papers detail these technical proposals:

- **DCG, and change propagation**: [_Adapton: Composable, demand-driven incremental computation_, **PLDI 2014**](http://www.cs.umd.edu/~hammer/adapton/).  
- **Nominal memoization**: [_Incremental computation with names_, **OOPSLA 2015**](http://arxiv.org/abs/1503.07792).
- **Type and effect structures**: The draft [_Typed Adapton: Refinement types for incremental computation with precise names_](https://arxiv.org/abs/1610.00097).

Why Rust?
----------

Adapton's first implementations used Python and OCaml; The latest
implementation in Rust offers the best performance thus far, since (1)
Rust is fast, and (2) [traversal-based garbage collection presents
performance challenges for incremental
computation](http://dl.acm.org/citation.cfm?doid=1375634.1375642).  By
liberating Adapton from traversal-based collection, [our empirical
results](https://github.com/cuplv/adapton-talk#benchmark-results) are
both predictable and scalable.

*/

#![feature(associated_consts)]
#![feature(box_patterns)]
#![feature(box_syntax)]

#![crate_name = "adapton"]
#![crate_type = "lib"]
    
extern crate core;

#[macro_use]
pub mod macros ;
pub mod engine ;
pub mod catalog ;
pub mod parse_val;
pub mod reflect;


mod adapton {
    pub use super::*;
}
