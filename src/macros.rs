/*! Macros to make using the `engine` module's interface more
 ergonomic.

Demand-driven change propagation
=================================

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
//let num  = cell!(42);
//let den  = cell!(2);
let_cell!{num = 42; den = 2; {

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
}}
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

For a graphical illustration of this behavior, see [these slides](https://github.com/cuplv/adapton-talk/blob/master/adapton-example--div-by-zero/).

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
======================================================

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
===================================

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


Nominal Firewalls
===================

This example demonstrates how nominal allocation mixes dirtying and
cleaning behind the scenes: when the input changes, dirtying proceeds
incrementally through the edges of the DCG, _during cleaning_.  In
some situations (Run 2, below), nominal allocation prevents dirtying
from cascading, leading to finer-grained dependency tracking, and more
incremental reuse.  One might call this design pattern _"nominal
firewalls"_ (thanks to @nikomatsakis for suggesting the term
"firewall" in this context).

First, consider this DCG:

```                                 
//   cell                           +---- Legend ------------------+
//   a                              | [ 2 ]   ref cell holding 2   |
//   [ 2 ]                          |  (g)    thunk named 'g'      |
//     ^                            | ---->   force/observe edge   |
//     | force                      | --->>   allocation edge      |              
//     | 2                          +------------------------------+
//     |
//     |                 cell                                   cell
//     |    alloc 4       b      force 4           alloc 4       c
//    (g)------------->>[ 4 ]<--------------(h)-------------->>[ 4 ]
//     ^                                     ^
//     | force                               | force h,
//     | returns b                           | returns c
//     |                                     |
//    (f)------------------------------------+
//     ^
//     | force f,
//     | returns cell c
//     |
//  (root of demand)
```

In this graph, the ref cell `b` acts as the "firewall".

Below, we show a particular input change for cell `a` where a
subcomputation `h` is never dirtied nor cleaned by change propagation
(input change 2 to -2). We show another change to the same input where
this subcomputation `h` *is* _eventually_ dirtied and cleaned by
Adapton, though not immediately (input change -2 to 3).

Here's the Rust code for generating this DCG, and these changes to its
input cell, named `"a"`:

```
# #[macro_use] extern crate adapton;
# fn main() {
use adapton::macros::*;
use adapton::engine::*;

fn demand_graph(a: Art<i32>) -> Art<i32> {
    let_memo!{
      c =(f)= { let a = a.clone();
        let_memo!{
          b =(g)={ let x = get!(a);
                   let_cell!{b = x * x; 
                             b }};
          c =(h)={ let x = get!(b); 
                   let_cell!{c = if x < 100 { x } else { 100 }; 
                             c }};
          c }};
      c }
}

manage::init_dcg();

// 1. Initialize input cell "a" to hold 2, and do the computation illustrated above:
let _ = demand_graph(let_cell!{a = 2; a});

// 2. Change input cell "a" to hold -2, and do the computation illustrated above:
let _ = demand_graph(let_cell!{a = -2; a});

// 3. Change input cell "a" to hold 3, and do the computation illustrated above:
let _ = demand_graph(let_cell!{a = 3; a});
# }
```

The `let_memo!` macro above expands as follows:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
fn demand_graph__post_macro_expansion(a: Art<i32>) -> Art<i32> {
    let f = let_thunk!{f = {
             let a = a.clone();
             let g = let_thunk!{g = {let x = get!(a);
                                     let_cell!{b = x * x; 
                                               b }};
                                g };
             let b = force(&g);
             let h = let_thunk!{h = {let x = get!(b); 
                                     let_cell!{c = if x < 100 { x } 
                                                   else { 100 };
                                               c }};
                                h };
             let c = force(&h);
        c};
       f};
    let c = force(&f);
    c
};
# }
```


In this example DCG, thunk `f` allocates and forces two
sub-computations, thunks `g` and `h`.  The first observes the input
`a` and produces an intermediate result (ref cell `b`); the second
observes this intermediate result and produces a final result (ref
cell `c`), which both thunks `h` and `f` return as their final result.

**Run 1.** In the first computation, the input cell `a` holds 2, and
the final resulting cell `c` holds `4`.

**Run 2.** When the input cell `a` changes, e.g., from 2 to -2, thunks
`f` and `g` are dirtied.  Thunk `g` is dirty because it observes the
changed input.  Thunk `f` is dirty because it demanded (observed) the
output of thunk `g` in the extent of its own computation.

_Importantly, thunk `h` is *not* immediately dirtied when cell `a`
changes._ In a sense, cell `a` is an indirect ("transitive") input to
thunk `h`.  This fact may suggest that when cell `a` is changed from 2
to -2, we should dirty thunk `h` immediately.  However, thunk `h` is
related to this input only by reading a *different* ref cell (ref cell
b) that depends, indirectly, on cell `a`, via the behavior of thunk
`g`, on which thunk `h` does *not* directly depend: thunk `h` does not
force thunk `g`.

Rather, when thunk `f` is re-demanded, Adapton will necessarily
perform a cleaning process (aka, "change propagation"), re-executing
`g`, its immediate dependent, which is dirty.  Since thunk `g` merely
squares its input, and 2 and -2 both square to 4, the output of thunk
`g` will not change in this case.  Consequently, the observers of cell
`b`, which holds this output, will not be dirtied or re-executed.  In
this case, thunk `h` is this observer.  In situations like these,
Adapton's dirtying + cleaning algorithms do not dirty nor clean thunk
`h`.

In sum, under this change, after `f` is re-demanded, the cleaning
process will first re-execute `g`, the immediate observer of cell `a`.
Thunk `g` will again allocate cell `b` to hold 4, the same value as
before.  It also yields this same cell pointer (to cell `b`).
Consequently, thunk `f` is not re-executed, and is cleaned.
Meanwhile, the outgoing (dependency) edges thunk of `h` are never
dirtied.

**Run 3.** For some other change, e.g., from 2 to 3, thunk `h` would
_eventually_ be dirtied and cleaned.  

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
  ;
  ($nm:ident =>>> $value:expr) => {{
      cell(name_of_str(stringify($ident)), $value)
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
  [ $nm:ident =>>> $suspended_body:expr ] => {{
      thunk!(name_of_str(stringify!($nm)), $suspended_body)
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

/**
Let-bind a nominal ref cell via `cell`, using the let-bound variable identifier as its name.  Permits sequences of bindings.

Example usage: [Adapton Example: Nominal firewalls](https://docs.rs/adapton/0/adapton/macros/index.html#nominal-firewalls).
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

Example usage: [Adapton Example: Nominal firewalls](https://docs.rs/adapton/0/adapton/macros/index.html#nominal-firewalls).
*/
#[macro_export]
macro_rules! let_thunk {
  { $var:ident = $rhs:expr; $body:expr } => {{
    let name = name_of_str(stringify!($var));
    let $var = thunk![name =>> $rhs]; $body
  }};
  { $var1:ident = $rhs1:expr ; $( $var2:ident = $rhs2:expr ),+ ; $body:expr} => {{
    let_thunk!($var1 = $rhs1;
               let_thunk!( $( $var2 = $rhs2 ),+ ; $body ))
  }};
}

/**
Let-bind a nominal thunk, force it, and let-bind its result.  Permits sequences of bindings.

Example usage: [Adapton Example: Nominal firewalls](https://docs.rs/adapton/0/adapton/macros/index.html#nominal-firewalls).
*/
#[macro_export]
macro_rules! let_memo {
  { $var:ident = ( $thkvar1:ident ) = $rhs:expr; $body:expr } => {{
    let name = name_of_str(stringify!($thkvar1));
    let $thkvar1 = thunk![name =>> $rhs];
    let $var = get!($thkvar1);
    $body
  }};
  { $var1:ident = ( $thkvar1:ident ) = $rhs1:expr ; $( $var2:ident = ( $thkvar2:ident ) = $rhs2:expr ),+ ; $body:expr} => {{
    let_memo!($var1 = ( $thkvar1 ) = $rhs1;
     let_memo!( $( $var2 = ( $thkvar2 ) = $rhs2 ),+ ; $body ))
  }};
}

#[test]
fn test_let_macros() {
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
