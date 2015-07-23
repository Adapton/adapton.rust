Why Zippers?
-------------

Zippers provide a simple, yet expressive functional model for
incremental editing.  We consider a list editor below.  

Capturing the notion of "incremental editing" is essential for
effective incremental computation.  Specifically, the editor
metaphor is the right input-output interface to the "outside user"
of the incremental computation, since it directly captures the
process by which the user edits, and it gives a language to these
edits, viz., the primitive operations supported by the zipper.  

Zippers efficiently implement functional editors for many common
data structures.  This efficiency comes from avoiding unnecessary
traversals.  In functional programming, traversals consist of both
allocation and garbage collection, e.g., to reverse pointers along
the path to the focused sublist or subtree.  The design of zippers
ammortize the work of focusing and unfocusing the structure, over a
large number of edits, rather than pay the linear cost for each
edit individually.  Just like other functional data structures, and
unlike imperative editors, zippers enjoy **referential
transparency**, meaning that new states of the editor do not arise
by destructively updating the old states.  Instead, new and old
states co-exist, and they typically share internal structure
wherever the sub-structure before and after the edit are unchanged,
and thus common.  This sharing is key to their efficiency.  

Unfortunately, zippers appear to have a fundemental limitation:
They implement efficient single-cursor editing, but do not
efficiently implement multi-cursor editing.  To perform an edit
sequence that involves multiple cursors, a naive zipper user would
focus and unfocus each time the "current cursor" changes between
consecutive edits. The alternative to repeated focusing and
unfocusing are designs for specialized multi-cursor zippers (e.g.,
a two-cursor zipper, a three-cursor zipper, etc.).  These
multi-cursor designs are supported by comparatively little
research, and unfortunately, they also appear to be significantly
more complex than their single-cursor counterparts, as the number
of structural cases to consider for each operation explodes as
cursors are added.  Hence, the presence of multiple cursors seems
to present a fundamental, insurmountable limitation for the
zipper, which otherwise works simply, and beautifully.

**Claim 1**: Using nominal IC, the single-focus list zipper can refocus
in amortized O(log n) time, for list structures of length n.  

**Key idea**: Choose a canonical form for the unfocused structure based
on probabilistically-balanced trees, and use nominal IC to memoize
the transformations between this canonical form and a single-focus
zipper at the desired cursor.  See `tree_of_2lists` below for the
specific algorithm I'm currently trying out.  

Questions for future designs:
-------------------------------
To what extent can we exploit laziness and function inverses?  For
instance, we want to avoid building zipperized views for parts of
the structure that are not edited.  When we canonicalize these
parts of the zipper structure, it'd be nice to just apply the
identity function, i.e., do nothing.  

