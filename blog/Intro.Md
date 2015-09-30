
Blog Series on Adapton in Rust
==============================

Adapton is a programming languages (PL) approach for general-purpose
incremental computing. As this blog series will show in detail,
Adapton provides a concise set of abstractions for encoding a large
class of incremental computations.  Under the hood, Adapton uses a
combination of **memoization (aka, function caching) and dynamic
dependency graphs** to cache computations and adapt them when inputs
change.

The remainder of this initial post will discuss general issues that
arise when writing libraries with these features, and why Rust was
chosen as an exciting and promising alternative to other languages
that have we tried in the past. Future posts will demonstrate
Adapton's interface in Rust, with successive complexity.  Though
Adapton is nominally given as a library for Rust, its usage often
requires rethinking the structure of the programs that use it.  This
blog series will help explore this tension, and hopefully will lead to
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


Adapton is Functional Programming
---------------------------------

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
(e.g., input changes that overwrite previous values),
**general-purpose function caching is most applicable when
side-effects are avoided**, sometimes by wisely encoding these effects
into a functional pattern.  For instance, our OOPSLA 2015 paper
(*Incremental Computation with Names*) describes an incremental
interpreter for IMP, an imperative programming language.  While IMP is
imperative, in its interpreter's implementation of imperative state
uses Adapton, and in particular, its (purely-functional) collections
library.

For Rust programmers that are accustomed to thinking about ownership
and memory management, this talk of function caching and data
structure sharing may raise the question:

> How and when are these caches and data structures garbaged collected?

This question has a complex answer.  In short, Adapton currently uses
a special form of reference counting.  Before addressing the question
of garbage collection in further detail, it is worth considering the
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
provide: weak references.  The idea of a weak reference is to point at
something until that something is collected, upon which the reference
becomes null.

Weak references are attractive for caches because they seem to solve
the reachability problem: The referent cached object requires an
ordinary ("strong") pointer to maintain its reachable status, and
otherwise the weak references that form the cache can be collected.
Indeed, this weak reference strategy is the one that we used in our
PLDI 2014 implementation of Adapton in OCaml.  However, there was a
lingering soundness problem that only manifested later, when we
extended the system.

The incompatibility of weak references and Adapton is fundemental, and
is rooted in how Adapton not only provides a function cache, but also
tracks mutable references with a dependency graph that we call a
*demanded computation graph (DCG)*.  This graph, which forms a DAG,
connects cached function invocations with their dynamic dependencies,
which consist of other invocations and special mutable input cells.
To ensure that these extra connections do not lead to "extra
reachability" in the heap, and thus to memory leaks, we make the
backward direction of the DAG edges weak, meaning that each node only
weakly points at the nodes that depend on it.  Periodically, when the
external user issues input changes, we use these weak pointers to walk
over the graph and mark dependencies as "dirty".

The latent problem we discovered after the PLDI 2014 paper arises in
the unique combination of a dependency graph with weak references
overlayed upon a memoization table with weak references (aka, the
function cache).  As it turns out, it is possible to generate DCGs and
input changes that will interact with the garbage collector in
undesireable ways, leading to errors in Adapton's internal change
propagation algorithm, and incorrect results in the updated
computation.  Exhibiting this behavior requires several steps:

1. During change propagation, some nodes in the DCG are re-executed,
   replacing their outgoing (strong) edges with different ones.

2. As a result, a sub-graph G becomes strongly disconnected temporarily
  (only reachable from a memo table).

3. The garbage collector begins to collect weak references in the
  disconnected sub-graph G.

4. After a time, change propagation uses the memo table to match some
  portion of the sub-graph G.

5. Upon the next cycle of change propagation, sub-graph G is missing
  edges from dependencies to dependents, and fails to correctly mark
  dependent nodes as dirty.

This soundness issue was latent until we extended the system with
first-class names, leading to more aggressive memo table-based reuse
in step 4.  Fortunately, the problem can be understood and solved
independently from that extension.

One Solution: An Awkward and Wasteful Dance
---------------------------------------------

We fixed the unsound design described above by using fewer weak
references: Instead of the memo table cache storing only weak
references, we made these references strong.  This ensures that the
garbage collector will not collect any weak references between the
cached nodes (every cached node has, by definition, at least one
strong pointer, coming from the cache).

Of course, this fix just raises the question: How do we collect the
memo table cache? To address this issue, the OCaml Adapton library now
employs **reference counting for each cache node**.  The library
adjusts reference counts as cached invocations call other cached
invocations (these calls form the changing edges of the graph).
Often, nodes from the graph are intermixed with data structures
(especially lazy structures).  To interface to the user's OCaml code,
which is garbage collected without reference counts, the library wraps
data structures that it maintains with special **finalizers**, so that
when and if these pointers are collected, they will decrement the
shared reference count of the cached node.

Finally, since Adapton programs often consist of both user code that
calls Adapton library calls as well as Adapton library routines that
call back into user code, things become even more complex.  While it
is vital that user code wraps pointers to cached nodes with
finalizers, it is equally important that Adapton's internal state
**not** include wrapped versions with finalizers, since this internal
state may not ever become unreachable.  To avoid these finalizers,
Adapton data structures all implement a wasteful but necessary
**"sanitize" operation**, which copies the prefix of the structure, up
to and including pointers to any cached nodes (but not further, to the
pointers stored at those nodes).

Thus, an awkward and wasteful dance ensues as the program runs:

- When the Adapton library returns data structures to the user's code,
  it must ensure that these versions are **wrapped in finalizers**, lest
  the reference count be inaccurate when that outside code either
  keeps or drops pointers to cached nodes.

- When the Adapton library accepts data structures from the user's
  code, it must **sanitize* these data structures before storing their
  values, lest they contain finalizers that will never finalize.

If the description above seems hard to understand or inefficient, I
agree completely.

Recall that this dance is all motivated by the desire for the garbage
collector and Adapton to work together to avoid leaking memory, and
also avoid forgetting dependency information that is critical for
sound incremental updates.  The fundemental problem is the
incapability of traversal-based GC assumptions and the invariants
required by Adapton's internal algorithms.  One can hope that in a
language with more control over memory management, Adapton could have
a more direct, more efficient sound implementation.

Another Solution: Use Rust
---------------------------







