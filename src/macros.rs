/*! Macros to make using the `engine` module's interface more
 ergonomic.

Demand-driven change propagation
---------------------------------

The example below demonstrates _demand-driven change propagation_,
which is unique to Adapton's approach to incremental computation.  The
example constructs two mutable inputs, `nom` and `den`, an
intermediate subcomputation `div` that divides the numerator in `nom`
by the denominator in `den`, and a thunk `check` that first checks
whether the denominator is zero (returning zero if so) and if
non-zero, returns the value of the division.

```
# #[macro_use] extern crate adapton;
# fn main() {
use adapton::macros::*;
use adapton::engine::*;
manage::init_dcg();

// Two mutable inputs, for numerator and denominator of division
let num  = cell!(42);
let den  = cell!(2);

// In Rust, cloning is explicit:
let den2 = den.clone(); // clone _global reference_ to cell.
let den3 = den.clone(); // clone _global reference_ to cell, again.

// Two subcomputations: The division, and a check thunk with a conditional expression
let div   = thunk![ get!(num) / get!(den) ];
let check = thunk![ if get!(den2) == 0 { None } else { Some(get!(div)) } ];

// Observe output of `check` while we change the input `den`
// Step 1: (Explained in detail, below)
assert_eq!(get!(check), Some(21));

// Step 2: (Explained in detail, below)
set(&den3, 0);
assert_eq!(get!(check), None);

// Step 3: (Explained in detail, below)
set(&den3, 2);
assert_eq!(get!(check), Some(21));  // division is reused
# }
```

The programmer's changes and observations in the last lines induce the
following change propagation behavior:

1. When the `check` is demanded the first time, it executes the
   condition, and `den` holds `2`, which is non-zero.  Hence, the
   `else` branch executes `get!(div)`, which demands the output of the
   division, `21`.

2. After this first observation of `check`, the programmer changes
   `den` to `0`, and re-demands the output of `check`.  In response,
   change propagation first re-executes the condition (not the
   division), and the condition branches to the `then` branch,
   resulting in `None`; in particular, it does _not_ re-demand the `div`
   node, though this node still exists in the DCG.

3. Next, the programmer changes `den` back to its original value, `2`,
   and re-demands the output of `check`.  In response, change
   propagation re-executes the condition, which re-demands the output
   of `div`.  Change propagation attempts to "clean" the `div` node
   before re-executing it.  To do so, it compares its _last
   observations_ of `num` and `den` to their current values, of `42`
   and `2`, respectively.  In so doing, it finds that these earlier
   observations match the current values.  Consequently, it _reuses_
   the output of the division (`21`) _without_ having to re-execute
   the division.

For a graphical illustration of this behavior, see [these slides](https://github.com/cuplv/adapton-talk/blob/master/2017-05-06--dcg-example--div-by-zero/Adapton_DCG--Avoid_div-by-zero.pdf).

In the academic literature on Adapton, we refer to this three-step
pattern as _switching_: The demand of `div` switches from being
present (in step 1) to absent (in step 2) to present (in step 3).

Past work on self-adjusting computation does not support this
switching pattern directly: Because of its change propagation
semantics, it would "forget" the division in step 2, and rerun it
_from-scratch_ in step 3.

Furthermore, some other change propagation algorithms base their
re-execution schedule on "node height" (of the graph's topological
ordering).  These algorithms may also have undesirable behavior.  In
particular, they may re-execute the division in step 2, though it is
not presently in demand. For an example, see 
[this gist](https://gist.github.com/khooyp/98abc0e64dc296deaa48).

Use `force_map` for finer-grained dependence tracking
----------------------------------------------------

Below, we show that using `force_map` prunes the dirtying phase of
change propagation; this test traces the engine, counts the number of
dirtying steps, and ensures that this count is zero, as expected.

```
# #[macro_use] extern crate adapton;
# fn main() {
use adapton::macros::*;
use adapton::engine::*;
use adapton::reflect;
manage::init_dcg();    

// Trace the behavior of change propagation; ensure dirtying works as expected
reflect::dcg_reflect_begin();

let pair  = cell!((1234, 5678));
let pair1 = pair.clone();

let t = thunk![{
  // Project the first component of pair:
  let fst = force_map(&pair, |_,x| x.0); 
  fst + 100
}];

// The output is `1234 + 100` = `1334`
assert_eq!(force(&t), 1334);

// Update the second component of the pair; the first is still 1234
set(&pair1, (1234, 8765));

// The output is still `1234 + 100` = `1334`
assert_eq!(force(&t), 1334);

// Assert that nothing was dirtied (due to using `force_map`)
let traces = reflect::dcg_reflect_end();
let counts = reflect::trace::trace_count(&traces, None);
assert_eq!(counts.dirty.0, 0);
assert_eq!(counts.dirty.1, 0);
# }
```

Nominal memoization: Toy Examples
------------------------------------

Adapton offers nominal memoization, which uses first-class _names_
(each of type `Name`) to identify cached computations and data. Behind
the scenes, these names control how and when the engine _overwrites_
cached data and computations.  As such, they permit patterns of
programmatic _cache eviction_.

For a simple illustration, we memoize several function calls to `sum`
with different names and arguments.  In real applications, the
memoized function typically performs more work than summing two
machine words. :)

```
# #[macro_use] extern crate adapton;
# fn main() {
use adapton::macros::*;
use adapton::engine::*;
use adapton::reflect;

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
