(Nominal) Adapton in Rust
==========================

Based on [this OCaml implementation](https://github.com/plum-umd/adapton.ocaml).

Development Activities
-----------------------

 - Building Adapton in Rust around core interface, the
   [`Adapton trait`](https://github.com/plum-umd/adapton.rust/blob/master/src/adapton_sigs.rs#L7).
    
 - The library implements this interface with an imperative data structure,
   [`AdaptonState`](https://github.com/plum-umd/adapton.rust/blob/master/src/adapton_state.rs).

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

-
```
Compiling adapton v0.0.1 (file:///Users/hammer/homedir/work/umd/adapton.rust)
src/adapton_state.rs:750:50: 750:51 error: mismatched types:
expected `alloc::rc::Rc<Box<for<'r> core::ops::Fn(&'r mut adapton_state::AdaptonState, alloc::rc::Rc<_>) -> alloc::rc::Rc<_>>>`,
found `alloc::rc::Rc<Box<fn(&'r mut adapton_state::AdaptonState, alloc::rc::Rc<u64>) -> alloc::rc::Rc<u64> {adapton_state::fact}>>`
(expected trait core::ops::Fn,
found fn item) [E0308]
src/adapton_state.rs:750     let t = st.thunk(ArtId::Eager,prog_pt!(fact),f,Rc::new(6)) ;
```
