
Blog Series on Adapton in Rust
==============================

**Incremental computing** is ubiquitous in today's computer systems:

> A computation is incremental if repeating it with a changed input is
> faster than from-scratch recomputation.

[Adapton](http://adapton.org) is a programming languages (PL) approach
for general-purpose incremental computing. As this blog series will
show in detail, Adapton provides a concise set of abstractions for
encoding a large class of incremental computations.  Under the hood,
Adapton uses a combination of **memoization (aka, function caching)**
and **dynamic dependency graphs** to cache computations and adapt them
when inputs change.

The remainder of this initial post will introduce Adapton in Rust with
some simple examples. Future posts will demonstrate Adapton in Rust
with successive complexity.  Though Adapton is nominally given as a
library for Rust, using it often requires rethinking the structure of
one's programs.  This blog series will help explore this tension, and
hopefully through comments and feedback, this series will lead to
improvements in the library's documentation and design.

The desired outcomes for this blog series are:

- motivate Rust as the implementation and host language for Adapton.

- document basic toy examples of the Adapton Rust library, so Rust
  users can get started and begin playing with it.

- document larger examples, and motivate Adapton to programmers, generally.

- in doing the above, I also hope to document the Adapton programming
  model for outsiders who are unfamiliar with it, especially
  programmers accustomed to ordinary Rust programming.

- get feedback from the Rust community, and the PL research community.


Caching improves asymptotic complexity
--------------------------------------

To start as simple as possible,
consider the following implementation of `fib` in Rust:
```rust
fn fib (n:u64) -> u64 {
 match n {
  0 => 0,
  1 => 1,
  n => fib(n-1) + fib(n-2)
  }
}
```

Other than speaking of `u64` instead of `Nat`, this code *is* the
[mathematical definition of the `n`th Fibonacci number](https://en.wikipedia.org/wiki/Fibonacci_number).  Unfortunately,
this *pure specification* is not efficient: As written above, it is
exponential in argument `n`.

The classic solution to this problem is to introduce **memoization**.
Memoization is implemented with *memo tables*, which consists of
global memory that persists across different calls to `fib`, and which
records its arguments and their corresponding results.  Each recursive
call consults the memo table before running from-scratch.

Adapton in Rust expresses this memoization solution as follows:
```rust
fn fib<A:Adapton> (st:&mut A, n:u64 ) -> u64 {
 match n {
  0 => 0,
  1 => 1,
  n => { memo!(st, fib, n:n-1) + memo!(st, fib, n:n-2) }
  }
}
```

The key difference is that the recursive calls are performed by the
`memo!` macro. Behind the scenes, this macro mutates an abstract
`Adapton` state `st` that remembers all of the argument-result pairs
for each memoized invocation of `fib`.  The `memo!` macro consults
this state before running the call from-scratch.

Using Adapton here has a drastic performance impact: Compared to the
version above which lacks memoization, this variation of the algorithm
runs *exponentially* faster: in *O(n)* time, instead of *O(φ ⁿ)* time
(where the
[golden ratio φ](https://en.wikipedia.org/wiki/Golden_ratio) is about
`1.6`).  The improvement from exponential-time to linear-time is
extremely dramatic, and pleasently, the code above accomplishes this
feat without loosing the structrure of the original program.  In fact,
if we substitute an implementation `A` for the `Adapton` trait that
runs all invocations from-scratch, we will exactly recover the
original program's behavior.  This is pleasent too, since it means
that our implementation can be verified (by human inspection) against
a closely-related specification.

Computing `fib` is one of the simplest examples of using memoization
to *reinterpret* a simple specification program and improve its
asymptotic complexity.  However, compared with general incremental
programs that employ memoization, in `fib` there is no "changing
input". Indeed, the structure of the numbers given to `fib` are
constants, and each number's valuation is fixed over time. (A three is
always a three).

By contrast, in many **interactive software systems**, the input to
repeated computations tends to *change over time*.  For instance,
**spreadsheets** calculate over *changing formula and data*, **web
browsers** layout and render a *changing DOM tree*, and **software
editors** perform static checking, compiling and testing over
*changing software*.

With an eye towards these more complex applications of incremental
computing, we are currently in the process of building a library of
algorithms and data structure abstractions that can be used to do two
things at once:

1. Concisely specify **functional correctness**, the desired input-output
   behavior of each algorithm.

2. Concisely specify **efficient incremental behavior**, through clever
   uses of Adapton's primitives.

In particular, we seek to design libraries of algorithms and data
structures that can be written once, and re-interpreted in at least
two ways:

1. As **purely-functional specifications**.

2. As **efficient, incremental implementations**.

The current design of library takes inspiration from the two current
research papers on Adapton, and from functional programming.

Adapton Encourages Functional Programming
-------------------------------------------

Though Adapton is implemented in Rust, it exposes and encourages a
programming model that is closer to traditional pure functional
programming, where *data structure mutation is eschewed* in favor of
*data structure sharing, which is highly encouraged*.

The desirability of pure functional patterns stems from the fact that
Adapton uses memoization under the hood as its chief mechanism to
reuse prior computations.  For instance, our incremental collections
library takes inspiration from purely-functional data structures
designed by
[Bill Pugh and Tim Teitelbaum in the late 1980's](http://dl.acm.org/citation.cfm?id=75305).
In these structures, sharing is the key to efficiently storing and
updating multiple versions of a data structure, i.e., before and after
a change.

Though Adapton can also reason about a limited class of **mutation**
(e.g., input changes that overwrite previous values), *general-purpose
function caching is most applicable when side-effects are avoided*,
sometimes by wisely encoding these effects into a functional pattern.
For instance, our OOPSLA 2015 paper
([*Incremental Computation with Names*](http://arxiv.org/abs/1503.07792))
describes an [incremental interpreter for IMP](https://github.com/plum-umd/inc-imp), an [imperative
programming language used for education](https://www21.in.tum.de/~nipkow/pubs/fac98.html).  While IMP is imperative, its interpreter's
implementation of imperative state uses Adapton, and in particular,
its (purely-functional) collections library.

Topics for Future Posts
-----------------------

Future posts will cover more uses of the Adapton library, as well as
descriptions of its design rationale.  In particular, future posts
will cover the following topics:

- **Adapton and Garbage Collection**: Programs that cache and reuse
  dynamic program state pose an complex challenge of automatic memory
  management.  These posts will cover the inherent complexity of
  interfacing with an automatic garbage collector, and the exciting
  potential for Rust to provide an alternative path forward.

- **Nominal Matching in Adapton**:
  [Traditional hash-consing](https://en.wikipedia.org/wiki/Hash_consing)
  is extremely value, but can also be insufficient in many
  situations. These posts will cover the use of
  [*first-class names*](http://arxiv.org/abs/1503.07792) to improve
  the incremental efficiency of algorithms that run repeatedly over
  slowly changing input structures.

- **Adapton Collections Library** These posts will describe the use
  and design of Adapton's collections library.


-------------------------------------------------

**Feedback from:**  
Lars,  
Kyle,  
