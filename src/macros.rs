/*! Macros to make using the `engine` module's interface more
 ergonomic.

Nominal memoization: Toy Examples
------------------------------------

Below, we memoize several function calls to `sum` with different names
and arguments.  In real applications, the memoized function typically
performs more work than summing two machine words. :)

```
# #[macro_use] extern crate adapton;
# fn main() {
use adapton::macros::*;
use adapton::engine::*;

// create an empty DCG (demanded computation graph)
manage::init_dcg();

// a simple function (memoized below for illustration purposes;
// probably actually not worth it!)
fn sum(x:usize, y:usize) -> usize {
    x + y
}

// Optional: Traces what the engine does below (for diagnostics, testing, illustration)
reflect::dcg_reflect_begin();

let nm_a_0 : Name = name_of_str("a"); // name "a"
let nm_a_1 : Name = name_of_str("a"); // name "a" (another copy)
let nm_b_0 : Name = name_of_str("b"); // name "b"
let nm_b_1 : Name = name_of_str("b"); // name "b" (another copy)

// create a memo entry, named "a", that remembers that `sum(42,43) = 85`
let res1 : usize = memo!(nm_a_0 =>> sum, x:42, y:43);

// same name "a", same arguments (42, 43) => reuses the memo entry above for `res1`
let res2 : usize = memo!(nm_a_1 =>> sum, x:42, y:43);

// different name "b", same arguments (42, 43) => will *not* match `res1`; creates a new entry
let res3 : usize = memo!(nm_b_0 =>> sum, x:42, y:43);

// same name "b", different arguments; will *overwrite* entry named "b" with new args & result
let res4 : usize = memo!(nm_b_1 =>> sum, x:55, y:66);

// Optional: Assert what happened above, in terms of analytical counts
let traces = reflect::dcg_reflect_end();
let counts = reflect::trace::trace_count(&traces, None);

// Editor allocated two thunks ("a" and "b")
assert_eq!(counts.alloc_fresh.0, 2);

// Editor allocated one thunk without changing it ("a", with same args)
assert_eq!(counts.alloc_nochange.0, 1);

// Editor allocated one thunk by changing it ("b", different args)
assert_eq!(counts.alloc_change.0, 1);

// Archivist allocated nothing
assert_eq!(counts.alloc_fresh.1, 0);
# drop((res1,res2,res3,res4));
# }
```
Some notes about the code above:

 - **Callsite argument names**: The macro `memo!` relies on
   programmer-supplied variable names in its macro expansion of these
   call sites, shown as `x` and `y` in the uses above.  These can be
   chosen arbitrarily: So long as these symbols are distinct from one
   another, they can be _any_ symbols, and need not actually match the
   formal argument names.

 - **Type arguments**: If the function call expects type arguments,
   `memo!` accomodates these calls with alternative syntax.

 - **Spurious arguments**: If the function call expects some later
   arguments that do not implement `Eq`, but are _functionally
   determined_ by earlier ones that do (including the supplied
   `Name`), `memo!` accomodates these calls with alternative syntax.
   We call these arguments "spurious", since the Adapton engine does
   _not check_ their identity when performing change
   propagation. Common examples include function values (e.g.,
   anonymous closures).

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

pub use std::rc::Rc;

thread_local!(static NAME_COUNTER: RefCell<usize> = RefCell::new(0));

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

/// Convenience function: A global counter for creating unique names,
/// e.g., in unit tests. Avoid using this outside of unit tests.
pub fn bump_name_counter() -> usize {
    NAME_COUNTER.with(|ctr|{let c = *ctr.borrow(); *ctr.borrow_mut() = c + 1; c})
}

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

/// Convenience wrapper for `engine::force`
#[macro_export]
macro_rules! get {
  ($art:expr) => {{
      force(&($art))
  }}
}

/// Convenience wrapper for `engine::cell`
///
/// Warning: Uses a global counter to choose a unique name. This _may_
/// be appopriate for the Editor role, but is never appropriate for
/// the Archivist role.
#[macro_export]
macro_rules! cell {
  ($value:expr) => {{
      cell(name_of_usize(bump_name_counter()), $value)
  }}
}

/// Convenience wrapper for `engine::thunk`
///
/// Warning: When not given a name, this macro uses a global counter
/// to choose a unique name. This _may_ be appopriate for the Editor
/// role, but is never appropriate for the Archivist role.
#[macro_export]
macro_rules! thunk {
  [ $suspended_body:expr ] => {{
    thunk
      (ArtIdChoice::Nominal(name_of_usize(bump_name_counter())),
       prog_pt!(stringify!("anonymous")),
       Rc::new(Box::new(
         move |(),()|{
           $suspended_body
         })),
       (), 
       ()
      )
  }}
  ;
  [ $nm:expr =>> $suspended_body:expr ] => {{
    thunk
      (ArtIdChoice::Nominal($nm),
       prog_pt!(stringify!("anonymous")),
       Rc::new(Box::new(
         move |(),()|{
           $suspended_body
         })),
       (), 
       ()
      )
  }}
  ;
  ( $nm:expr =>> $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
    thunk
      (ArtIdChoice::Nominal($nm),
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*, _) = args ;
           $f :: < $( $ty ),* >( $( $lab ),* )
         })),
       ( $( $arg ),*, ()),
       ()
       )
  }}
  ;
  ( $nm:expr =>> $f:path , $( $lab:ident : $arg:expr ),* ) => {{
    thunk
      (ArtIdChoice::Nominal($nm),
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*, _) = args ;
           $f ( $( $lab ),* )
         })),
       ( $( $arg ),*, () ),
       ()
       )
  }}
  ;
  ( $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
    thunk
      (ArtIdChoice::Structural,
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*, _) = args ;
           $f :: < $( $ty ),* >( $( $lab ),* )
         })),
       ( $( $arg ),*, () ),
       ()
       )
  }}
  ;
  ( $f:path , $( $lab:ident : $arg:expr ),* ) => {{
    thunk
      (ArtIdChoice::Structural,
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*, _) = args ;
           $f ( $( $lab ),* )
         })),
       ( $( $arg ),*, () ),
       ()
       )        
  }}
  ;
  ( $nm:expr =>> $f:ident =>> < $( $ty:ty ),* > , $( $lab1:ident : $arg1:expr ),* ;; $( $lab2:ident : $arg2:expr ),* ) => {{
    let t = thunk
      (ArtIdChoice::Nominal($nm),
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
    t
  }}
  ;
}

/// Convenience wrapper for `engine::thunk` and `engine::force`:
/// creates a thunk and immediately forces it.
#[macro_export]
macro_rules! memo {
  ( $nm:expr =>> $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
    let t = thunk
      (ArtIdChoice::Nominal($nm),
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*) = args ;
           $f :: < $( $ty ),* >( $( $lab ),* )
         })),
       ( $( $arg ),*, ),
       ()
       );
    force(&t)
  }}
  ;
  ( $nm:expr =>> $f:path , $( $lab:ident : $arg:expr ),* ) => {{
    let t = thunk
      (ArtIdChoice::Nominal($nm),
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*) = args ;
           $f ( $( $lab ),* )
         })),
       ( $( $arg ),* ),
       ()
       );
    force(&t)
  }}
  ;
  ( $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
    let t = thunk
      (ArtIdChoice::Structural,
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*) = args ;
           $f :: < $( $ty ),* >( $( $lab ),* )
         })),
       ( $( $arg ),* ),
       ()
       );
    force(&t)
  }}
  ;
  ( $f:path , $( $lab:ident : $arg:expr ),* ) => {{
    let t = thunk
      (ArtIdChoice::Structural,
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args, _|{
           let ($( $lab ),*, _) = args ;
           $f ( $( $lab ),* )
         })),
       ( $( $arg ),*, () ),
       ()
       );
    force(&t)
  }}
  ;
  ( $nm:expr =>> $f:path , $( $lab1:ident : $arg1:expr ),* ;; $( $lab2:ident : $arg2:expr ),* ) => {{
    let t = thunk
      (ArtIdChoice::Nominal($nm),
       prog_pt!(stringify!($f)),
       Rc::new(Box::new(
         |args1, args2|{
           let ($( $lab1 ),*, _) = args1 ;
           let ($( $lab2 ),*, _) = args2 ;
           $f ( $( $lab1 ),* , $( $lab2 ),* )
         })),
       ( $( $arg1 ),*, () ),
       ( $( $arg2 ),*, () ),
       );
    force(&t)
  }}
  ;
  ( $nm:expr =>> $f:ident =>> < $( $ty:ty ),* > , $( $lab1:ident : $arg1:expr ),* ;; $( $lab2:ident : $arg2:expr ),* ) => {{
    let t = thunk
      (ArtIdChoice::Nominal($nm),
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
    force(&t)
  }}
  ;
}

/// Similar to `memo!`, except return both the thunk and its observed (`force`d) value.
#[macro_export]
macro_rules! eager {
  ( $nm:expr =>> $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
    let t = thunk
      (ArtIdChoice::Nominal($nm),
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
      (ArtIdChoice::Nominal($nm),
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
      (ArtIdChoice::Structural,
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
      (ArtIdChoice::Structural,
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
      (ArtIdChoice::Nominal($nm),
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
