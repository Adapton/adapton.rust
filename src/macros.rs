/*! Macros to make using the `engine` module's interface more
 ergonomic.

# Adapton programming model

**Adapton roles**: Adapton proposes _editor_ and _achivist roles_:  

 - The **Editor role** _creates_ and _mutates_ input, and _demands_ the
   output of incremental computations in the **Archivist role**.

 - The **Archivist role** consists of **Adapton thunks**, where each is
   cached computation that consumes incremental input and produces
   incremental output.

**Examples:** The examples below illustrate these roles, in increasing complexity:

 - [Start the DCG engine](#start-the-dcg-engine)
 - [Create incremental cells](#create-incremental-cells)
 - [Observe `Art`s](#observe-arts)
 - [Mutate input cells](#mutate-input-cells)
 - [Demand-driven change propagation](#demand-driven-change-propagation)
 - [Memoization](#memoization)
 - [Create thunks](#thunks)
 - [Use `force_map` for more precise dependencies](#use-force_map-for-more-precise-dependencies)
 - [Nominal memoization](#nominal-memoization)
 - [Nominal firewalls](#nominal-firewalls)

**Programming primitives:** The following list of primitives covers
the core features of the Adapton engine.  Each primitive below is
meaningful in each of the two, editor and archivist, roles:  

 - **Ref cell allocation**: Mutable input (editor role), and cached data structures that change across runs (archivist role).
   - [**`cell!`**](https://docs.rs/adapton/0/adapton/macro.cell.html) -- Preferred version  
   - [`let_cell!`](https://docs.rs/adapton/0/adapton/macro.let_cell.html)  -- Useful in simple examples  
   - [`engine::cell`](https://docs.rs/adapton/0/adapton/engine/fn.cell.html) -- Engine's raw interface  
 - **Observation** and **demand**: Both editor and archivist role.  
   - [**`get!`**](https://docs.rs/adapton/0/adapton/macro.get.html) -- Preferred version  
   - [`engine::force`](https://docs.rs/adapton/0/adapton/engine/fn.force.html) -- Engine's raw interface  
   - [`engine::force_map`](https://docs.rs/adapton/0/adapton/engine/fn.force_map.html) -- A variant for observations that compose before projections  
 - **Thunk Allocation**: Both editor and archivist role.  
   - Thunk allocation, **_without_ demand**:  
     - [**`thunk!`**](https://docs.rs/adapton/0/adapton/macro.thunk.html) -- Preferred version  
     - [`let_thunk!`](https://docs.rs/adapton/0/adapton/macro.let_thunk.html) -- Useful in simple examples  
     - [`engine::thunk`](https://docs.rs/adapton/0/adapton/engine/fn.thunk.html) -- Engine's raw interface (can be cumbersome)  
   - Thunk allocation, **_with_ demand**:  
     - [**`memo!`**](https://docs.rs/adapton/0/adapton/macro.memo.html) -- Preferred version  
     - [`let_memo!`](https://docs.rs/adapton/0/adapton/macro.let_memo.html) -- Useful in simple examples  

## Start the DCG engine

The call `init_dcg()` below initializes a DCG-based engine, replacing
the `Naive` default engine.

```
#[macro_use] extern crate adapton;
use adapton::macros::*;
use adapton::engine::*;

fn main() {
    manage::init_dcg();

    // Put example code below here
# let c : Art<usize> = cell!( 123 );
# assert_eq!( get!(c), 123 );
}
```

Create incremental cells
========================

Commonly, the input and intermediate data of Adapton computations
consists of named reference `cell`s.  A reference `cell` is one
variety of `Art`s; another are [`thunk`s](#create-thunks).

## Implicit counter for naming `cell`s

`cell!(123)` uses a global counter to choose a unique name to hold
`123`. Important note: This _may_ be appopriate for the Editor role,
but is _never appropriate for the Archivist role_.

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let c : Art<usize> = cell!( 123 );

assert_eq!( get!(c), 123 );
# }
```

## Explicitly-named `cell`s

Sometimes we name a cell using a Rust identifier.  We specify this
case using the notation `[ name ]`, which specifies that the cell's
name is a string, constructed from the Rust identifer `name`:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let c : Art<usize> = cell!([c] 123);

assert_eq!(get!(c), 123);
# }
```

## Optionally-named `cell`s

Most generally, we supply an expression `optional_name` of type
`Option<Name>` to specify the name for the `Art`.  This `Art` is
created by either `cell` or `put`, in the case that `optional_name` is
`Some(name)` or `None`, respectively:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let n : Name = name_of_str(stringify!(c));
let c : Art<usize> = cell!([Some(n)]? 123);

assert_eq!(get!(c), 123);

let c = cell!([None]? 123);

assert_eq!(get!(c), 123);
# }
```
Observe `Art`s
======================

The macro `get!` is sugar for `engine::force!`, with reference
introduction operation `&`:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let c : Art<usize> = cell!(123);

assert_eq!( get!(c), force(&c) ); 
```

Since the type `Art<T>` classifies both `cell`s and
[`thunk`s](#create-thunks), the operations `force` and `get!` can be
used interchangeably on `Art<T>`s that arise as `cell`s or `thunk`s.

Mutate input cells
=========================

One may mutate cells explicitly, or _implicitly_, which is common in Nominal Adapton.

The editor (implicitly or explicitly) mutates cells that hold input
and they re-demand the output of the archivist's computations.  During
change propagation, the archivist mutates cells with implicit
mutation.

**Implicit mutation uses nominal allocation**: By allocating a cell
with the same name, one may _overwrite_ cells with new content:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let n : Name = name_of_str(stringify!(c));
let c : Art<usize> = cell!([Some(n.clone())]? 123);

assert_eq!(get!(c), 123);

// Implicit mutation (re-use cell by name `n`):
let d : Art<usize> = cell!([Some(n)]? 321);

assert_eq!(d, c);
assert_eq!(get!(c), 321);
assert_eq!(get!(d), 321);
# }
```

**No names implies no effects**: Using `None` to allocate cells always
**gives distinct cells, with no overwriting:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();

let c = cell!([None]? 123);
let d = cell!([None]? 321);

assert_eq!(get!(c), 123);
assert_eq!(get!(d), 321);
# }
```

**Explicit mutation, via `set`**: If one wants mutation to be totally
explicit, one may use `set`:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let n : Name = name_of_str(stringify!(c));
let c : Art<usize> = cell!([Some(n)]? 123);

assert_eq!(get!(c), 123);

// Explicit mutation (overwrites cell `c`):
set(&c, 321);

assert_eq!(get!(c), 321);
# }
```


Demand-driven change propagation
=================================

The example below demonstrates _demand-driven change propagation_,
which is unique to Adapton's approach to incremental computation.  The
example constructs two mutable inputs, `num` and `den`, an
intermediate subcomputation `div` that divides the numerator in `num`
by the denominator in `den`, and a thunk `check` that first checks
whether the denominator is zero (returning zero if so) and if
non-zero, returns the value of the division.

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
# 
// Two mutable inputs, for numerator and denominator of division
let num = cell!(42); 
let den = cell!(2);

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

[Slides with illustrations](https://github.com/cuplv/adapton-talk/blob/master/adapton-example--div-by-zero/)
of the graph structure and the code side-by-side may help:

**Step 1**

<img src="https://raw.githubusercontent.com/cuplv/adapton-talk/master/adapton-example--div-by-zero/Adapton_Avoiddivbyzero_10.png" 
   alt="Slide-10" style="width: 800px;"/>

**Steps 2 and 3**

<img src="https://raw.githubusercontent.com/cuplv/adapton-talk/master/adapton-example--div-by-zero/Adapton_Avoiddivbyzero_12.png" 
   alt="Slide_12" style="width: 200px;"/>
<img src="https://raw.githubusercontent.com/cuplv/adapton-talk/master/adapton-example--div-by-zero/Adapton_Avoiddivbyzero_16.png" 
   alt="Slide_16" style="width: 200px;"/>
<img src="https://raw.githubusercontent.com/cuplv/adapton-talk/master/adapton-example--div-by-zero/Adapton_Avoiddivbyzero_17.png" 
   alt="Slide-17" style="width: 200px;"/>
<img src="https://raw.githubusercontent.com/cuplv/adapton-talk/master/adapton-example--div-by-zero/Adapton_Avoiddivbyzero_23.png" 
   alt="Slide-23" style="width: 200px;"/>

In the academic literature on Adapton, we refer to this three-step
pattern as _switching_:  

- The demand of `div` switches from being present (in step 1),
- to absent (in step 2),
- to present (in step 3).

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

A memoization yields two results:

- An articulation, of type `Art<Res>`, where `Res` is the result type
  of the function expression.

- A result value of type `Res`, which is also cached at the articulation.


Optional name version
----------------------

The following form is preferred:

`memo!( [ optional_name ]? fnexp ; lab1 : arg1, ..., labk : argk )`

It accepts an optional name, of type `Option<Name>`, and an arbitrary
function expression `fnexp` (closure or function pointer).  Like the
other forms, it requires that the programmer label each argument.

Example
-------

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let (t,z) : (Art<usize>, usize) = 
  memo!([Some(name_unit())]?
    |x:usize,y:usize|{ if x > y { x } else { y }};
     x:10,   y:20   );

assert_eq!(z, 20);
# }
```

[More examples of `memo!` macro](https://docs.rs/adapton/0/adapton/macro.memo.html#memoization)

Create thunks
===============

Each [_memoization point_](#memoization) is merely a _forced thunk_.
We can also create thunks without demanding them.

The following form is preferred:

`thunk!( [ optional_name ]? fnexp ; lab1 : arg1, ..., labk : argk )`

It accepts an optional name, of type `Option<Name>`, and an arbitrary
function expression `fnexp` (closure or function pointer).  Like the
other forms, it requires that the programmer label each argument.

Each thunk is an `Art<Res>`, where `Res` is the return type of
function expression `fnexp`.

Example
-------

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# manage::init_dcg();
let t : Art<usize> =
  thunk!([ Some(name_unit()) ]?
    |x:usize,y:usize|{ if x > y { x } else { y }};
     x:10,   y:20   );

assert_eq!(get!(t), 20);
# }
```

[More examples of `thunk!` macro](https://docs.rs/adapton/0/adapton/macro.thunk.html#thunks)

Use `force_map` for more precise dependencies
==============================================

Suppose that we want to project only one field of type `A` from a pair
within an `Art<(A,B)>`.  If the field of type `B` changes, our
observation of the `A` field will not be affected.

Below, we show that using `force_map` prunes the dirtying phase of
change propagation.  Doing so means that computations that would
otherwise be dirty and cleaned via re-execution are never diritied in
the first place.  We show a simple example of projecting a pair.

To observe this fact, this test traces the engine, counts the number
of dirtying steps, and ensures that this count is zero, as expected.

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
# use adapton::reflect;
# manage::init_dcg();
# 
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
assert_eq!(get!(t), 1334);

// Update the second component of the pair; the first is still 1234
set(&pair1, (1234, 8765));

// The output is still `1234 + 100` = `1334`
assert_eq!(get!(t), 1334);

// Assert that nothing was dirtied (due to using `force_map`)
let traces = reflect::dcg_reflect_end();
let counts = reflect::trace::trace_count(&traces, None);
assert_eq!(counts.dirty.0, 0);
assert_eq!(counts.dirty.1, 0);
# }
```


Nominal memoization
=========================

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
# use adapton::macros::*;
# use adapton::engine::*;
# use adapton::reflect;
# 
# // create an empty DCG (demanded computation graph)
# manage::init_dcg();
# 
// a simple function (memoized below for illustration purposes;
//                    probably actually not worth it!)
fn sum(x:usize, y:usize) -> usize {
    x + y
}

// Optional: Traces what the engine does below (for diagnostics, testing, illustration)
reflect::dcg_reflect_begin();

// create a memo entry, named `a`, that remembers that `sum(42,43) = 85`
let res1 : usize = get!(thunk!([a] sum; x:42, y:43));

// same name `a`, same arguments (42, 43) => reuse cached result
let res2 : usize = get!(thunk!([a] sum; x:42, y:43));

// different name `b`, same arguments (42, 43) => recomputes `sum` for `b`
let res3 : usize = get!(thunk!([b] sum; x:42, y:43));

// same name `b`, different arguments; *overwrite* `b` with new args & result
let res4 : usize = get!(thunk!([b] sum; x:55, y:66));

// Optional: Assert what happened above, in terms of analytical counts
let traces = reflect::dcg_reflect_end();
let counts = reflect::trace::trace_count(&traces, None);

// Editor allocated two thunks (`a` and `b`)
assert_eq!(counts.alloc_fresh.0, 2);

// Editor allocated one thunk without changing it (`a`, with same args)
assert_eq!(counts.alloc_nochange.0, 1);

// Editor allocated one thunk by changing it (`b`, different args)
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
                   cell!([b] x * x) };
          c =(h)={ let x = get!(b); 
                   cell!([c] if x < 100 { x } else { 100 }) };
          c }};
      c }
}

manage::init_dcg();

// 1. Initialize input cell "a" to hold 2, and do the computation illustrated above:
let c = demand_graph(let_cell!{a = 2; a});

// 2. Change input cell "a" to hold -2, and do the computation illustrated above:
let c = demand_graph(let_cell!{a = -2; a});

// 3. Change input cell "a" to hold 3, and do the computation illustrated above:
let c = demand_graph(let_cell!{a = 3; a});

# drop(c)
# }
```

The `let_memo!` macro above expands as follows:

```
# #[macro_use] extern crate adapton;
# fn main() {
# use adapton::macros::*;
# use adapton::engine::*;
fn demand_graph__mid_macro_expansion(a: Art<i32>) -> Art<i32> {
    let f = let_thunk!{f = {
              let a = a.clone();
              let g = thunk!([g]{ let x = get!(a);
                                  cell!([b] x * x) });
              let b = force(&g);
              let h = thunk!([h]{ let x = get!(b); 
                                  cell!([c] if x < 100 { x } else { 100 })});
              let c = force(&h);
              c };
            f };
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
              None => { ArtIdChoice::Eager },
              Some(n) => { ArtIdChoice::Nominal(n) }},
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
              None => { ArtIdChoice::Eager },
              Some(n) => { ArtIdChoice::Nominal(n) }},
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
              None => { ArtIdChoice::Eager },
              Some(n) => { ArtIdChoice::Nominal(n) }},
          prog_pt!(stringify!($body)),
          Rc::new(Box::new( move |(),()| { $body } )),
          () ,
          () )
  }}
  ;
  [ [ $name:ident ] $body:expr ] => {{
      thunk(
          ArtIdChoice::Nominal(name_of_str(stringify!($name))),
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

Example usage: [Adapton Example: Nominal firewalls](https://docs.rs/adapton/0/adapton/macros/index.html#nominal-firewalls).
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

