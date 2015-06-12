(Nominal) Adapton in Rust
==========================

Based on [this OCaml implementation](https://github.com/plum-umd/adapton.ocaml).

Development Activities
-----------------------

 - Building Adapton in Rust around core interface, the `Adapton trait`.

   The library implements this interface with an imperative structure
   called `AdaptonState`.

 - I am learning Rust in the process.  See detailed Q&A below.

*Written:*

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

 - The concept of "object safety" seems to bite me a lot in naive designs:
 
 ```
 src/adapton_impl.rs:653:51: 653:111 error: cannot convert to a trait object because trait `adapton_impl::Producer` is not object-safe [E0038]
 src/adapton_impl.rs:653                 let producer : Box<Producer<T>> = Box::new(App{prog_pt:prog_pt,fn_box:fn_box,arg:arg.clone()}) ;
 ~~~~~~
 src/adapton_impl.rs:653:51: 653:111 note: method `eq` references the `Self` type in its arguments or return type
 src/adapton_impl.rs:653                 let producer : Box<Producer<T>> = Box::new(App{prog_pt:prog_pt,fn_box:fn_box,arg:arg.clone()}) ;
```
