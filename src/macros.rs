/*! Macros to make using the `engine` module's interface more
    ergonomic.

*/

// Adapton uses memoization under the covers, which needs an efficient
// mechanism to search for function pointers and compare them for
// equality.
//
// Meanwhile, Rust does not provide Eq and Hash implementations for
// trait Fn.  So, to identify Rust functions as values that we can
// hash and compare, we need to bundle additional static information
// along with the function pointer as use this data as a proxy for the
// function itself.  The idea is that this information uniquely
// identifies the function pointer (i.e., two distinct functions will
// always have two distinct identities).
//

use std::cell::RefCell;
use std::fmt::{Formatter,Result,Debug};

#[doc(hidden)]
pub use std::rc::Rc;

thread_local!(static NAME_COUNTER: RefCell<usize> = RefCell::new(0));

#[doc(hidden)]
/// Program points: used by the Adapton engine to distinguish different memoized functions.
#[derive(PartialEq,Eq,Clone,Hash)]
pub struct ProgPt {
  // Symbolic identity, in Rust semantics:
  pub symbol:&'static str, // via stringify!(...)
  // module:Rc<String>, // via module!()

  // Location in local filesystem:
  //pub file:&'static str,   // via file!()
  //pub line:u32,        // via line!()
  //pub column:u32,      // via column!()
}

impl Debug for ProgPt {
  fn fmt(&self, f: &mut Formatter) -> Result { self.symbol.fmt(f) }
}

#[doc(hidden)]
/// Convenience function: A global counter for creating unique names,
/// e.g., in unit tests. Avoid using this outside of unit tests.
pub fn bump_name_counter() -> usize {
    NAME_COUNTER.with(|ctr|{let c = *ctr.borrow(); *ctr.borrow_mut() = c + 1; c})
}

#[doc(hidden)]
/// Generate a "program point", used as a unique ID for memoized functions.
#[macro_export]
macro_rules! prog_pt {
  ($symbol:expr) => {{
    ProgPt{
      symbol:$symbol,
      //file:file!(),
      //line:line!(),
      //column:column!(),
    }
  }}
}

/**
Convenience wrapper for `engine::force`

Example usage:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let c = cell!(123);
assert_eq!(get!(c), 123);
assert_eq!(get!(c), force(&c));
# }
```

*/
#[macro_export]
macro_rules! get {
  ($art:expr) => {{
      force(&($art))
  }}
 ;
  ($art:expr, $cycle_out:expr) => {{
      force_cycle(&($art), Some($cycle_out))
  }}
}

/**
Convenience wrappers for `engine::cell`.

## Optional-name version

In this verion, supply an expression `optional_name` of type
`Option<Name>` to specify the name for the cell, created by either
`cell` or `put`, in the case that `optional_name` is `Some(name)` or
`None`, respectively:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let n = name_of_str(stringify!(c));
let c = cell!([Some(n)]? 123);

assert_eq!(get!(c), 123);
assert_eq!(get!(c), force(&c));

let c = cell!([None]? 123);

assert_eq!(get!(c), 123);
assert_eq!(get!(c), force(&c));
# }
```

## Explicit-names version:

In this verion, use `[ name ]` to specify the cell's name is `name`:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let c = cell!([c] 123);

assert_eq!(get!(c), 123);
assert_eq!(get!(c), force(&c));
# }
```

## Global counter version:

Uses a global counter to choose a unique name. Important note: This
_may_ be appopriate for the Editor role, but is _never appropriate for
the Archivist role_.

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let c = cell!( 123 );

assert_eq!( get!(c), 123 );
assert_eq!( get!(c), force(&c) );
# }
```

*/

#[macro_export]
macro_rules! cell {
  ( $value:expr ) => {{
      cell(name_of_usize(bump_name_counter()), $value)
  }}
  ;
  ( [ $nm:expr ] ? $value:expr ) => {{
      match $nm { Some(n) => cell(n, $value), None => put($value) }
  }}
  ;
  ( [ $nm:ident ] $value:expr ) => {{
      cell(name_of_str(stringify!($nm)), $value)
  }}
}


/**  Wrappers for `engine::thunk`.

Thunks
=======================

The following form is preferred:

`thunk!( [ optional_name ]? fnexpr ; lab1 : arg1, ..., labk : argk )`

It accepts an optional name, of type `Option<Name>`, and a function
`fnexpr`, of type `Fn(A1,...,Ak) -> B`.  

The arguments `arg1,...,argk` have types `A1,...,Ak`.  Like the other
thunk and memoization forms, this form requires that the programmer
provide a label `labi` for each argument `argi`.

Example 1
----------

The programmer specifies the optional name `opnm`, function expression
`max`, and two labeled arguments `x` and `y`:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
fn max(a:usize,b:usize) -> usize { 
  if a > b { a } else { b } 
};
let opnm : Option<Name> = Some(name_unit());
let t : Art<usize> = thunk!([opnm]? max ; x:10, y:20 );
assert_eq!(get!(t), 20);
# }
```

Example 2
----------

The function expression need not be pre-declared:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let opnm : Option<Name> = Some(name_unit());
let t : Art<usize> = thunk!([opnm]? 
                            |x,y| if x > y { x } else { y }; 
                            x:10, y:20 );
assert_eq!(get!(t), 20);
# }
```

Example 3
----------
Sometimes thunks just consist of a body expression, without separated arguments:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let x : Art<usize> = cell!([x] 10);
let y : Art<usize> = cell!([y] 20);
let t : Art<usize> = thunk![[Some(name_unit())]? 
                            if get!(x) > get!(y) { get!(x) } else { get!(y) } ];
assert_eq!(get!(t), 20);
# }
```

Convenience forms, for examples
================================

**Example 4**:

We can use the Rust symbol `t` as the name to repeat Example 2
above, as follows:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let t : Art<usize> = thunk!([t]
                            |x,y| if x > y { x } else { y }; 
                            x:10, y:20 );
assert_eq!(get!(t), 20);
# }
```

**Example 5**

We can use the Rust symbol `t` as the name to repeat Example 3
above, as follows:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let x : Art<usize> = cell!([x] 10);
let y : Art<usize> = cell!([y] 20);
let t : Art<usize> = thunk![[t] if get!(x) > get!(y) { get!(x) } else { get!(y) } ];
assert_eq!(get!(t), 20);
# }
```

**Example 6**

Implicit name-counter version (not suitable for archivist role):

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let x : Art<usize> = cell!(10);
let y : Art<usize> = cell!(20);
let t : Art<usize> = thunk![ if get!(x) > get!(y) { get!(x) } else { get!(y) } ];
assert_eq!(get!(t), 20);
# }
```

Spurious arguments
======================

Sometimes, we need to pass arguments that do not admit the traits
required by Adapton thunks `Eq + Hash + Debug + Clone`.

For instance, suppose that we want to pass `Fn`s around, which are not
`Debug`, `Eq` or `Hash`.  Though generally unsound, we can use the
`;;` syntax below to append arguments to thunks that do not admit
these traits.  For soundness, it is critical that the name and/or
other arguments (before the `;;`) [functionally
determine](https://en.wikipedia.org/wiki/Functional_dependency) the
arguments that follow the `;;`, which are stored and never updated,
nor tested for changes.

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let t : Art<usize> = thunk!(
  [Some(name_unit())]?
    |x:usize,y:usize,
      choose:Rc<Fn(usize,usize)->bool>|{ 
        if choose(x,y) { x } else { y }
    };
    x:10, y:20 ;; 
    f:Rc::new(|x,y| if x > y { true } else { false })
  );

assert_eq!(get!(t), 20);
# }
```


*/
#[macro_export]
macro_rules! thunk {
  ([ $nmop:expr ] ? $fun:expr ; $( $lab:ident :$arg:expr ),* ) => {{
      thunk(
          match $nmop {
              None => { NameChoice::Eager },
              Some(n) => { NameChoice::Nominal(n) }},
          prog_pt!(stringify!($fun)),
          Rc::new(Box::new(
              move |($($lab),*,()),()| $fun ( $($lab),* )
          )),
          ( $( $arg ),*,()),
          () )
  }}
  ;
  ([ $nmop:expr ] ? $fun:expr ; $( $lab:ident :$arg:expr ),* ;; $( $lab2:ident :$arg2:expr ),* ) => {{
      thunk(
          match $nmop {
              None => { NameChoice::Eager },
              Some(n) => { NameChoice::Nominal(n) }},
          prog_pt!(stringify!($fun)),
          Rc::new(Box::new(
              move | arg1 , arg2 | {
              let ( $( $lab  ),*, () ) = arg1 ;
              let ( $( $lab2 ),*, () ) = arg2 ;
              $fun ( $( $lab ),*, $( $lab2 ),* )
              }
          )),
          ( $( $arg ),*, () ),
          ( $( $arg2 ),*, () )
      )
  }}
  ;
  ( [ $name:ident ] $fun:expr ; $( $lab:ident :$arg:expr ),* ) => {{
      thunk!([Some(name_of_str(stringify!($name)))]? 
             $fun ; 
             $( $lab:$arg ),* 
      )
  }}
  ;
  ( $nm:expr =>> $fun:expr , $( $lab:ident : $arg:expr ),* ) => {{
      thunk!([Some($nm)]? 
             $fun ; 
             $( $lab:$arg ),* 
      )
  }}
  ;
  ( $nm:expr =>> $fun:expr , $( $lab:ident : $arg:expr ),* ;; $( $lab2:ident : $arg2:expr ),* ) => {{
      thunk!([Some($nm)]? 
             $fun ; 
             $( $lab:$arg ),* ;;
             $( $lab2:$arg2 ),* 
      )
  }}
  ;
  [ [ $nmop:expr ] ? $body:expr ] => {{
      thunk(
          match $nmop {
              None => { NameChoice::Eager },
              Some(n) => { NameChoice::Nominal(n) }},
          prog_pt!(stringify!($body)),
          Rc::new(Box::new( move |(),()| { $body } )),
          () ,
          () )
  }}
  ;
  [ [ $name:ident ] $body:expr ] => {{
      thunk(
          NameChoice::Nominal(name_of_str(stringify!($name))),
          prog_pt!(stringify!($fun)),
          Rc::new(Box::new( move |(),()| { $body } )),
          () ,
          () )
  }}
  ;
  [ $body:expr ] => {{
      thunk![ [Some(name_of_usize(bump_name_counter()))]? 
               $body ]
  }}
}

/** Wrappers for `engine::fork_name`.

Name forking
-----------------

Sometimes one has a single name but wants more _that are determined by
it, deterministically_:

```
# #[macro_use] extern crate adapton;
# fn main() {
use adapton::macros::*;
use adapton::engine::*;
manage::init_dcg();

// Run 1, start with `name_unit()`:
let u : Name = name_unit();
let (u1,u2) = fork!( u );
let (u3,u4) = fork!( u1 );

// Run 2, start with the same name, `name_unit()`:
let w : Name = name_unit();
let (w1,w2) = fork!( w );
let (w3,w4) = fork!( w1 );

// The name forks consist of the same names between runs 1 and 2:
assert_eq!(u2, w2);
assert_eq!(u3, w3);
assert_eq!(u4, w4);
# }
```

In the context of incremental computation, the archivist names the
output of their computation by the names of its input.


*/
#[macro_export]
macro_rules! fork {
    ( $nm:expr ) => {{ 
        name_fork($nm)
    }}
}


/** Optional name forking.

Name forking
=============

Sometimes one has a single name but wants more _that are determined by
it, deterministically_:

```
# #[macro_use] extern crate adapton;
# fn main() {
use adapton::macros::*;
use adapton::engine::*;
manage::init_dcg();

// Run 1, start with `name_unit()`:
let u : Name = name_unit();
let (u1,u2) = fork!( u );
let (u3,u4) = fork!( u1 );

// Run 2, start with the same name, `name_unit()`:
let w : Name = name_unit();
let (w1,w2) = fork!( w );
let (w3,w4) = fork!( w1 );

// The name forks consist of the same names between runs 1 and 2:
assert_eq!(u2, w2);
assert_eq!(u3, w3);
assert_eq!(u4, w4);
# }
```

In the context of incremental computation, the archivist names the
output of their computation by the names of its input.


Optional-name forking
------------------------

Sometimes it's natural to work with _optional_ names, and in these
contexts, one may want to fork optional names:

```
# #[macro_use] extern crate adapton;
# fn main() {
use adapton::macros::*;
use adapton::engine::*;
manage::init_dcg();

let u : Option<Name> = Some(name_unit());
let (u1,u2) = forko!(u);
let (w1,w2) = forko!(u1);
# }
```

When the name is `None`, one gets more `None`s, as expected:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
# 
let u : Option<Name> = None;
let (u1,u2) = forko!(u);
let (w1,w2) = forko!(u1);

assert_eq!(w1, None);
assert_eq!(w2, None);
# }
```

*/
#[macro_export]
macro_rules! forko {
    ( $nmop:expr ) => {{ 
        match $nmop { 
            None => (None,None),
            Some(n) => { 
                let (l,r) = name_fork(n);
                (Some(l),Some(r))
            }
        } 
    }}
}


/** Wrappers for creating and forcing thunks (`engine::thunk` and `engine::force`).

Memoization
============

Memoization provides a mechanism for caching the results of
subcomputations; it is a crtical feature of Adapton's approach to
incremental computation.

In Adapton, each _memoization point_ has three ingredients:

- A function expression (of type `Fn`)

- Zero or more arguments.  Each argument type must have an
  implementation for the traits `Eq + Clone + Hash + Debug`.  The
  traits `Eq` and `Clone` are both critical to Adapton's caching and
  change propagation engine.  The trait `Hash` is required when
  Adapton's naming strategy is _structural_ (e.g., where function
  names are based on the hashes of their arguments).  The trait
  `Debug` is useful for debugging, and reflection.

- An optional _name_, which identifies the function call for reuse later. 

    - When this optional name is `None`, the memoization point may be
      treated in one of two ways: either as just an ordinary, uncached
      function call, or as a cached function call that is identified
      _structurally_, by its function pointer and arguments.  Adapton
      permits structural subcomputations via the engine's
      [structural](https://docs.rs/adapton/0/adapton/engine/fn.structural.html)
      function.

    - When this is `Some(name)`, the memoization point uses `name` to
      identify the work performed by the function call, and its
      result.  Critically, in future incremental runs, it is possible
      for `name` to associate with different functions and/or argument
      values.


Optional name version
----------------------

The following form is preferred:

`memo!( [ optional_name ]? fnexp ; lab1 : arg1, ..., labk : argk )`

It accepts an optional name, of type `Option<Name>`, and an arbitrary
function expression `fnexp` (closure or function pointer).  Like the
other forms, it requires that the programmer label each argument.

Example 1
---------

**Optional name:**

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
// Choose an optional name:
let opnm : Option<Name> = Some(name_unit());

let (t,z) : (Art<usize>, usize) = 
  memo!([opnm]?
    |x:usize,y:usize|{ if x > y { x } else { y }};
     x:10,   y:20   );

assert_eq!(z, 20);
assert_eq!(force(&t), 20);
# }
```

Example 2
---------------

**Function pointers as arguments:**

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
fn max(x:usize,y:usize) -> bool { 
  if x > y { true } else { false } 
};

let (t,z) : (Art<usize>, usize) = 
  memo!([Some(name_unit())]?
    |x:usize,y:usize,choose:fn(usize,usize)->bool|{ 
       if choose(x,y) { x } else { y }
    }; x:10, y:20, f:max );

assert_eq!(z, 20);
assert_eq!(force(&t), 20);
# }
```

Spurious arguments
===================

Sometimes, we need to pass arguments that do not admit the traits
required by Adapton thunks `Eq + Hash + Debug + Clone`.

For instance, suppose that we want to pass `Fn`s around, which are not
`Debug`, `Eq` or `Hash`.  Though generally unsound, we can use the
`;;` syntax below to append arguments to thunks that do not admit
these traits.  For soundness, it is critical that the name and/or
other arguments (before the `;;`) [functionally
determine](https://en.wikipedia.org/wiki/Functional_dependency) the
arguments that follow the `;;`, which are stored and never updated,
nor tested for changes.

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let (t,z) : (Art<usize>, usize) = memo!(
  [Some(name_unit())]?
    |x:usize,y:usize,
      choose:Rc<Fn(usize,usize)->bool>|{ 
        if choose(x,y) { x } else { y }
    };
    x:10, y:20 ;; 
    f:Rc::new(|x,y| if x > y { true } else { false })
  );

assert_eq!(z, 20);
assert_eq!(get!(t), 20);
# }
```


*/
#[macro_export]
macro_rules! memo {
  ( [ $nmop:expr ] ? $fun:expr ; $( $lab:ident :$arg:expr ),* ) => {{
      { let t = thunk!( [$nmop]? $fun ; $( $lab:$arg ),* ); let x = get!(t); (t, x) }
  }}
  ;
  ( [ $nmop:expr ] ? $fun:expr ; $( $lab:ident :$arg:expr ),* ;; $( $lab2:ident : $arg2:expr ),* ) => {{
      { let t = thunk!( [$nmop]? $fun ; $( $lab:$arg ),* ;; $( $lab2:$arg2 ),* ); let x = get!(t); (t, x) }
  }}
  ;
  ( $nm:expr =>> $fun:expr , $( $lab:ident : $arg:expr ),* ) => {{ 
      get!(thunk!( [Some($nm)]? $fun ; $( $lab:$arg ),* ))
  }}
  ;
  ( $nm:expr =>> $fun:expr , $( $lab1:ident : $arg1:expr ),* ;; $( $lab2:ident : $arg2:expr ),* ) => {{
      get!(thunk!( [Some($nm)]? $fun ; $( $lab1:$arg1 ),* ;; $( $lab2:$arg2 ),* ))
  }}
}


/// Similar to `memo!`, except return both the thunk and its observed (`force`d) value.
#[macro_export]
#[doc(hidden)]
macro_rules! eager {
  ( $nm:expr =>> $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
    let t = thunk
      (NameChoice::Nominal($nm),
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*) = args ;
           $f :: < $( $ty ),* >( $( $lab ),* )
         })),
       ( $( $arg ),*, ),
       ()
       );
    let res = force(&t) ;
    (t, res)
  }}
  ;
  ( $nm:expr =>> $f:path , $( $lab:ident : $arg:expr ),* ) => {{
    let t = thunk
      (NameChoice::Nominal($nm),
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*) = args ;
           $f ( $( $lab ),* )
         })),
       ( $( $arg ),* ),
       ()
       );
    let res = force(&t) ;
    (t, res)
  }}
  ;
  ( $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
    let t = thunk
      (NameChoice::Structural,
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*) = args ;
           $f :: < $( $ty ),* >( $( $lab ),* )
         })),
       ( $( $arg ),* ),
       ()
       );
    let res = force(&t) ;
    (t, res)
  }}
  ;
  ( $f:path , $( $lab:ident : $arg:expr ),* ) => {{
    let t = thunk
      (NameChoice::Structural,
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*, _) = args ;
           $f ( $( $lab ),* )
         })),
       ( $( $arg ),*, () ),
       ()
       );
    let res = force(&t) ;
    (t, res)
  }}
  ;
  ( $nm:expr =>> $f:ident =>> < $( $ty:ty ),* > , $( $lab1:ident : $arg1:expr ),* ;; $( $lab2:ident : $arg2:expr ),* ) => {{
    let t = thunk
      (NameChoice::Nominal($nm),
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args1, args2|{
           let ($( $lab1 ),*, _) = args1 ;
           let ($( $lab2 ),*, _) = args2 ;
           $f :: < $( $ty ),* > ( $( $lab1 ),* , $( $lab2 ),* )
         })),
       ( $( $arg1 ),*, () ),
       ( $( $arg2 ),*, () ),
       );
    let res = force(&t) ;
    (t, res)
  }}
  ;
}

/// Convenience wrapper: Call a function and place the result into an `engine::cell`.
#[doc(hidden)]
#[macro_export]
macro_rules! cell_call {
  ( $nm:expr =>> $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
    let res = {
      $f :: < $( $ty ),* >( $( $arg ),*, )
    } ;
    let cell = cell($nm, res) ;
    cell
  }}
  ;
  ( $nm:expr =>> $f:ident , $( $lab:ident : $arg:expr ),* ) => {{
    let res = {
      $f ( $( $arg ),*, )
    } ;
    let cell = cell($nm, res) ;
    cell
  }}
}


/**
Let-bind a nominal ref cell via `cell`, using the let-bound variable identifier as its name.  Permits sequences of bindings.

Example usage: [Adapton Example: Nominal firewalls](https://docs.rs/adapton/0/adapton/index.html#nominal-firewalls).
*/
#[macro_export]
macro_rules! let_cell {
  { $var:ident = $rhs:expr; $body:expr } => {{ {
    let name = name_of_str(stringify!($var));
    let value = $rhs;
    let $var = cell(name, value); $body }
  }};
  { $var1:ident = $rhs1:expr ; $( $var2:ident = $rhs2:expr ),+ ; $body:expr} => {{
    let_cell!($var1 = $rhs1;
              let_cell!( $( $var2 = $rhs2 ),+ ; $body ))
  }};
}

/**
Let-bind a nominal thunk via `thunk!`, without forcing it.  Permits sequences of bindings.

Example usage: [Adapton Example: Nominal firewalls](https://docs.rs/adapton/0/adapton/index.html#nominal-firewalls).
*/
#[macro_export]
macro_rules! let_thunk {
  { $var:ident = $rhs:expr; $body:expr } => {{
    let name = name_of_str(stringify!($var));
    let $var = thunk!([Some(name)]?{$rhs}); $body
  }};
  { $var1:ident = $rhs1:expr ; $( $var2:ident = $rhs2:expr ),+ ; $body:expr} => {{
    let_thunk!($var1 = $rhs1;
               let_thunk!( $( $var2 = $rhs2 ),+ ; $body ))
  }};
}


#[test]
fn test_let_cell_let_thunk_macros() {
    use adapton::macros::*;
    use adapton::engine::*;
    
    fn demand_graph(a: Art<i32>) -> Art<i32> {
        let c : Art<i32> = get!(let_thunk!{f = {
            let a = a.clone();
            let b : Art<i32> = get!(let_thunk!{g = {let x = get!(a); let_cell!{b = x * x; b}}; g});
            let c : Art<i32> = get!(let_thunk!{h = {let x = get!(b); let_cell!{c = if x < 100 { x } else { 100 }; c}}; h});
            c}; f});
        return c
    };
    
    manage::init_dcg();
    
    // 1. Initialize input cell "a" to hold 2, and do the computation illustrated above:
    let _ = demand_graph(cell(name_of_str("a"), 2));
    
    // 2. Change input cell "a" to hold -2, and do the computation illustrated above:
    let _ = demand_graph(cell(name_of_str("a"), -2));
    
    // 3. Change input cell "a" to hold 3, and do the computation illustrated above:
    let _ = demand_graph(cell(name_of_str("a"), 3));
}

/**
Let-bind a nominal thunk, force it, and let-bind its result.  Permits sequences of bindings.

Example usage and expansion:

- [Nominal firewall, example usage](https://docs.rs/adapton/0/adapton/index.html#example-nominal-firewall).
- [Nominal firewall, expanded](https://docs.rs/adapton/0/adapton/index.html#let_memo-example).
*/
#[macro_export]
macro_rules! let_memo {
  { $var:ident = ( $thkvar1:ident ) = $rhs:expr; $body:expr } => {{
    let $thkvar1 = thunk!([$thkvar1]{$rhs});
    let $var = get!($thkvar1);
    $body
  }};
  { $var1:ident = ( $thkvar1:ident ) = $rhs1:expr ; $( $var2:ident = ( $thkvar2:ident ) = $rhs2:expr ),+ ; $body:expr} => {{
    let_memo!($var1 = ( $thkvar1 ) = $rhs1;
     let_memo!( $( $var2 = ( $thkvar2 ) = $rhs2 ),+ ; $body ))
  }};
}


#[test]
fn test_memo_macros() {
    use adapton::macros::*;
    use adapton::engine::*;
    
    fn demand_graph(a: Art<i32>) -> Art<i32> {
        let_memo!{c =(f)= {
            let a = a.clone();
            let_memo!{b =(g)= {let x = get!(a); let_cell!{b = x * x; b}};
                      c =(h)= {let x = get!(b); let_cell!{c = if x < 100 { x } else { 100 }; c}};
                      c}};
                  c}
    }
    
    manage::init_dcg();
    
    // 1. Initialize input cell "a" to hold 2, and do the computation illustrated above:
    let _ = demand_graph(cell(name_of_str("a"), 2));
    
    // 2. Change input cell "a" to hold -2, and do the computation illustrated above:
    let _ = demand_graph(cell(name_of_str("a"), -2));
    
    // 3. Change input cell "a" to hold 3, and do the computation illustrated above:
    let _ = demand_graph(cell(name_of_str("a"), 3));
}

