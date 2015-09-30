
Rust and Adapton Blog Series
============================

This blog series will assume a basic understanding of Rust and ML, and
will explain how Adapton combines with these language features to
expose support for general-purpose incremental computing.  Though
Adapton is nominally a library for Rust, its usage often requires
rethinking the structure of the programs that use it.  This blog
series will help explore this tension, and hopefully will lead to
improvements in the library's documentation and design.

The desired outcomes for this blog series are:

- document basic toy examples of the Adapton Rust library, so Rust
  users can get started and begin playing with it.

- document the Adapton programming model for outsiders who are
  unfamiliar with it, especially programmers accustomed to ordinary
  Rust programming.

- get feedback from the Rust community, and the PL research community.

**Adapton for General-Purpose Incremental Computing.** As this series
will show in detail, Adapton provides a small set of primitives for
encoding a large class of incremental computations.  In short, Adapton
uses a combination of **memoization (aka, function caching) and
dynamic dependency graphs** to cache computations and adapt them when
inputs change.  The remainder of this post will discuss general issues
that arise when writing libraries with these features, and why Rust
was chosen as an exciting and promising alternative to other languages
that have we tried in the past.  Future posts will demonstrate
Adapton's interface in Rust, with successive complexity.


Adapton is Functional Programming
---------------------------------

Though Adapton is implemented in Rust, it exposes and encourages a
programming model that is closer to traditional pure functional
programming, where *data structure mutation is eschewed* in favor of
*data structure sharing, which is highly encouraged*.  The
desirability of pure functional patterns stems from the fact that
Adapton uses memoization under the hood as its chief mechanism to
reuse prior computations.  Though Adapton can reason about a limited
level of mutation (e.g., input changes), general-purpose function
caching is most applicable when side-effects are either avoided, or
wisely encoded into a functional pattern.  For instance, our
incremental collections library takes inspiration from
purely-functional data structures designed by Bill Pugh and Tim
Teitelbaum in the late 1980's, where sharing is the key to efficiently
storing and updating multiple versions of a data structure (i.e.,
before and after a change).  Due to these encodings, we can isolate
the effects of input mutations to the data structure to only those
computations that depend on them.

For Rust programmers that are accustomed to thinking about ownership
and memory management, this talk of function caching and data
structure sharing may raise the question: How and when are these
caches and data structures garbaged collected?

Caching vs Garbage Collection
------------------------------

In short, Adapton currently uses a special form of reference counting.
Before addressing this question in further detail, let's consider the
challenges for implementing Adapton in typical garbage-collected
functional languages.

When we implement Adapton without reference counting, in
garbage-collected languages, there is a serious issue:
*traversal-based garbage collectors typically equate reachability with
liveness*.  This means that the caching performed by memoization is
considered always live, and consequently is never collected by the
garbage collector.  To solve this problem, we can use an escape hatch
that many collectors provide: weak references.  The idea of a weak
reference is to point at something until that something is collected,
upon which the reference becomes null.

Weak references are attractive for caches because they seem to solve
the reachability problem: The referent something requires an ordinary
("strong") pointer to maintain its reachable status, and otherwise the
weak references that form the cache can be collected.  Indeed, this
weak reference strategy is the one that we used in our PLDI 2014
implementation of Adapton in OCaml.  However, there was a lingering
soundness problem that only manifested later, when we extended the
system.

The incompatibility of weak references and Adapton is rooted in how
Adapton not only provides a function cache, it also tracks mutable
references with a dependency graph that we call a *demanded
computation graph (DCG)*.  This graph, which forms a DAG, connects
cached function invocations with their dynamic dependencies, which
consist of other invocations and special mutable input cells.  To
ensure that these extra connections do not lead to "extra
reachability" in the heap, and thus to memory leaks, we made the
backward direction of the DAG edges weak, meaning that each node only
weakly points at the nodes that depend on it.  We use these weak
pointers to walk over the graph and mark dependencies as "dirty".

The latent problem we discovered after the PLDI 2014 paper arises the
combination of a dependency graph with weak references, and a
memoization table with weak references (aka, the function cache).  As
it turns out, it is possible to generate DCGs and input changes that
will interact with the garbage collector in undesireable ways, leading
to errors in Adapton's internal change propagation algorithm, and
incorrect results in the updated computation.  The problem requires
several steps:

1. some nodes in the DCG are re-executed, replacing their outgoing
   (strong) edges with different ones.

2. as a result, a sub-graph G becomes strongly disconnected temporarily
  (only reachable from a memo table).

3. the garbage collector begins to collect weak references in the
  disconnected sub-graph G.

4. after a time, re-evaluation uses the memo table to match some
  portion of the sub-graph G.

5. upon the next cycle of change propagation, sub-graph G is missing
  edges from dependencies to dependents, and fails to correctly mark
  dependent nodes as dirty.

This soundness issue was latent until we extended the system with
first-class names, leading to more aggressive memo table-based reuse
in step 4.  Fortunately, the problem can be understood and solved
independently from that extension.

One Solution: An Awkward and Wasteful Dance
---------------------------------------------









Another Solution: Use Rust
---------------------------







