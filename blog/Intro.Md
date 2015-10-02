
Blog Series on Adapton in Rust
==============================

**Incremental computing** is ubiquitous in today's computer systems:

> A computation is incremental if repeating it with a changed input is
> faster than from-scratch recomputation.

Adapton is a programming languages (PL) approach for general-purpose
incremental computing. As this blog series will show in detail,
Adapton provides a concise set of abstractions for encoding a large
class of incremental computations.  Under the hood, Adapton uses a
combination of **memoization (aka, function caching)** and **dynamic
dependency graphs** to cache computations and adapt them when inputs
change.

The remainder of this initial post will discuss general issues that
arise when writing libraries that abstract incremental computing, and
why Rust was chosen as an exciting and promising alternative to other
languages that have we tried in the past. Future posts will
demonstrate Adapton's interface in Rust, with successive complexity.
Though Adapton is nominally given as a library for Rust, its usage
often requires rethinking the structure of the programs that use it.
This blog series will help explore this tension, and hopefully will
lead to improvements in the library's documentation and design.

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
mathematical definition of the `n`th Fibonacci number.  Unfortunately,
this *pure specification* is not efficient: As written above, it is
exponential in argument `n`.

The classic solution to this problem is to introduce `memoization`,
which consists of a global memory that persists across different calls
to `fib`.  Each recursive call consults the memoization state before
running from-scratch.

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
`memo!` macro, and that this macro mutates an abstract `Adapton` state
`st` that remembers all of the invocations of `fib`, including their
results. Compared to the version above which lacks memoization, this
variation of the algorithm runs *exponentially* faster: in *O(n)*
time, instead of *O(φ ⁿ)* time, where the golden ratio φ is about `1.6`.

Computing `fib` is one of the simplest examples of using memoization
to transform a simple specification program and improve its asymptotic
complexity.  Compared with general incremental programs that employ
memoization, there is no "changing input", since the structure of the
numbers given to `fib` are constants, each number's valuation does not
vary over time. (A three is always a three).  By contrast, in many
software systems the input to repeated computations tends to change
over time.  For instance, spreadsheets calculate over changing formula
and data, web browsers layout and render a changing DOM, and software
editors perform static checking, compiling and testing over changing
software.

With an eye towards these more complex applications of incremental
computing, we are currently in the process of building a library of
algorithms and data structure abstractions that can be used to do two
things at once:

1. Concisely specify functional correctness, the desired input-output
   behavior of each algorithm.

2. Concisely specify efficient incremental behavior, through clever
   uses of Adapton's primitives.

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
designed by Bill Pugh and Tim Teitelbaum in the late 1980's.  In these
structures, sharing is the key to efficiently storing and updating
multiple versions of a data structure, i.e., before and after a
change.

Though Adapton can also reason about a limited class of **mutation**
(e.g., input changes that overwrite previous values), *general-purpose
function caching is most applicable when side-effects are avoided*,
sometimes by wisely encoding these effects into a functional pattern.
For instance, our OOPSLA 2015 paper
([*Incremental Computation with Names*](http://arxiv.org/abs/1503.07792))
describes an incremental interpreter for IMP, an imperative
programming language.  While IMP is imperative, in its interpreter's
implementation of imperative state uses Adapton, and in particular,
its (purely-functional) collections library.

For Rust programmers that are accustomed to thinking about ownership
and memory management, this talk of function caching and data
structure sharing may raise the question:

> How and when are these caches and data structures garbaged collected?

This question has a complex answer.  In short, Adapton currently uses
a special form of reference counting to manage memory.  Before
addressing the question in further detail, it is worth considering the
challenges for implementing Adapton in typical garbage-collected
functional languages.  The rest of this post will give the reader an
overview, which both highlights the key ingredients in Adapton's
implementation, and the challenges for integrating these features with
general-purpose "automatic" memory management.

Caching vs Garbage Collection
------------------------------

When we implement Adapton without reference counting, in
garbage-collected languages such as OCaml, there is a serious issue:
*traversal-based garbage collectors typically equate reachability with
liveness*.  This means that cached memoization data is always
considered live, and consequently, it is never collected by the
garbage collector, leading to memory requirements that grow over time.
To solve this problem, we can use an escape hatch that many collectors
provide: **weak references**.  The idea of a weak reference is to point at
an object until that object is collected, upon which the reference
becomes null.  For instance, OCaml implements weak references with the
[Weak module](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Weak.html).

Weak references are attractive for caches because they seem to solve
the reachability problem: The referent cached object requires an
ordinary ("strong") pointer to maintain its reachable status, and
otherwise the weak references that form the cache can be collected.
Indeed, this weak reference strategy is the one that we used in our
[PLDI 2014](http://www.cs.umd.edu/~hammer/adapton/)
[implementation of Adapton in OCaml](https://bitbucket.org/khooyp/adapton.ocaml).
However, there was a lingering soundness problem that only manifested
later, when we extended the system.  As it turns out, the problem
also affects the original system as well.

As we eventually learned, the incompatibility of weak references and
Adapton is fundemental. It is rooted in how Adapton provides both a
function cache as well as a dependency graph that we call a *demanded
computation graph (DCG)*.  This graph, which forms a DAG, connects
cached nodes recording function invocations with their dynamic
dependencies, which consist of other invocations and special mutable
input cells that these invocations may read.  To ensure that these
extra connections do not lead to "extra reachability" in the heap, and
thus to memory leaks, we make the backward direction of the DAG edges
weak, meaning that each node only weakly points at the nodes that
depend on it, while it strongly points at the nodes on which it
depends.  Periodically, when the external user issues input changes,
we use the weak, backward pointers to walk over the graph and mark
dependencies as "dirty".

The latent problem we discovered after the PLDI 2014 paper arises in
the unique combination of a dependency graph with weak references
overlayed upon a memoization table with weak references.  As we found
by accident, it is possible to generate DCGs and input changes that
will interact with the garbage collector in undesireable ways, leading
to errors in Adapton's internal change propagation algorithm, and
incorrect results in the updated computation.  Exhibiting this
behavior requires several steps:

1. During change propagation, some nodes in the DCG are re-executed,
   replacing their outgoing (strong) edges with different ones.

2. As a result, a sub-graph G temporarily becomes strongly disconnected;
  it is only reachable from a memo table, whose pointers are weak.

3. The garbage collector begins to collect weak references in the
  disconnected sub-graph G, invalidating its structural invariants.

4. After a time, change propagation uses the memo table to match and
  reuse some portion of the sub-graph G, making it "live" again.

5. Upon the next cycle of change propagation, sub-graph G is missing
  edges from dependencies to dependents, and fails to correctly mark
  dependent nodes as dirty.

This soundness issue was latent until we extended the system with
first-class names, leading to more aggressive memo table-based reuse
in step 4 (See [this paper](http://arxiv.org/abs/1503.07792) for
more).  After finding the problem, we fixed both systems.
Fortunately, the problem can be understood and solved independently
from that recent extension.

One Solution: An Awkward and Wasteful Dance
---------------------------------------------

We fixed the unsound design described above by using fewer weak
references: Instead of the memo table cache storing only weak
references, we made the references held by the memo table strong.
This ensures that the garbage collector will not spuriously collect
the weak references that represent the DAG edges between the cached
nodes, since every cached node has, by definition, at least one strong
pointer that comes from the memo table cache.

Of course, this fix just raises the question: How do we collect the
memo table cache? To address this issue, the OCaml Adapton library now
employs **reference counting for each cache node**.  The library
adjusts reference counts as cached invocations call other cached
invocations, as these calls form the changing edges of the graph.
When the user chooses to free up memory, they invoke a special "flush"
operation and the Adapton library collects all nodes with reference
count of zero.

Unfortunately, maintaining these counts is not nearly as simple as one
might hope.  First, nodes from the graph are intermixed with data
structures used in the incremental computation, especially lazy
structures.  To interface to the user's OCaml code, which is garbage
collected without reference counts, the library wraps data structures
that it maintains with special **finalizers**, so that when and if
these pointers are collected, they will decrement the shared reference
count of the cached node (see `finalise` in the
[Gc module](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html)).

Next, since Adapton programs often consist of both user code that
calls Adapton library calls as well as Adapton library routines that
call back into user code, things become even more complex.  While it
is vital that user code wraps pointers to cached nodes with
finalizers, it is equally important that Adapton's internal state
**not** include wrapped versions with finalizers, since this internal
state may not ever become unreachable, and thus never finalized.  To
avoid these finalizers in its internal state, Adapton data structures
all implement a wasteful but necessary **"sanitize" operation**, which
copies the prefix of the structure, up to and including pointers to
any cached nodes wrapped with finalizers. Each time the library
accepts data structures as arguments from user code, it avoids storing
data with finalizers by ensuring that this data is always sanitized
(shallowly copied).

Thus, an awkward and wasteful dance ensues as the program runs:

- When the Adapton library returns data structures to the user's code,
  it must ensure that these versions are **wrapped in finalizers**,
  lest the reference count that it maintains be inaccurate when that
  outside code either keeps or drops pointers to cached nodes.

- When the Adapton library accepts data structures from the user's
  code, it must **sanitize** these data structures
  before storing their values, lest they contain finalizers that will
  never finalize.

If the description above seems hard to understand or inefficient, I
agree completely.

Recall that this dance is all motivated by the desire for the garbage
collector and Adapton to work together to avoid leaking memory, and
also avoid forgetting dependency information that is critical for
sound incremental updates.  The fundemental problem is the
incapability of traversal-based garbage collection and the invariants
required by Adapton's internal algorithms.  One can hope that in a
language with more control over memory management, Adapton could have
a more direct, more efficient sound implementation.

Another Solution: Use Rust
---------------------------

The heart of the memory management solution above is
reference-counting: Each cached node has a reference count, and when
it reaches zero, the user can safely flush the node from the
cache. The rest of the complexity, including the introduction and
careful management of finalizers, stems from having to interface
library state that is reference-counted with code that assumes a
traversal-based garbage collector.

In Rust, the assumption of a traversal-based collector is lifted,
simplifying the library considerably: No finalizers or sanitization
steps are required.  Instead, the library simply uses existing Rust
abstractions for reference-counted objects (see the
[Rc module](https://doc.rust-lang.org/std/rc/struct.Rc.html)).

--------------------------------------

TODO: Wrap up the end of the post somehow:  
Conclusions and remaining questions?  
Reiterate next steps in the blog series (also mentioned at top, above)  

TODO: Include simple figures for the description above?  
Illustrate: Nodes, Memo table, Edges (Strong vs Weak pointers), Ref counts  
