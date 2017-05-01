/*!
Reflects the DCG engine, including both the effects of the
programs running in it, and the internal effects of the engine
cleaning and dirtying the DCG.  For the latter effects, see the
`trace` module.

**Reflected Values**.  Notably, the values in the engine
(including the values of mutable and compute nodes, and the values
stored on edges between them) are reflected here into a special `Val`
type.  Primarily, the distinction between actual Rust values and
this reflected `Val` type is what makes the DCG engine "reflected"
by the definitions in this module, and not identical to them.

This module provides an interface used by Adapton Lab to produce
HTML visualizations of these internal structures, for
experimentation and debugging (namely, the `dcg_reflect_begin` and
`dcg_reflect_end` functions).  For the purposes of debugging,
visualization and design/exploration, we exploit the reflected
version of values to "walk" them, finding their articulations, and
walking their values, recursively.
*/

use engine::Name;
use macros::ProgPt;
use std::fmt::Debug;
use std::rc::Rc;
use std::collections::HashMap;

pub use engine::reflect_dcg::*;

/// This trait consists of the ability for a reference to `Self` to
/// produce a `T`.  Conceptually, that value of type T is the
/// "reflection" of `Self`.  A large set of types in `engine`
/// implement this trait for a particular type in this module, which
/// represents its reflection.  The documentation of this module makes
/// this correspondance clear.
pub trait Reflect<T> : Debug {
  fn reflect (&self) -> T;
}

impl<S,T:Reflect<S>+Debug> Reflect<Option<S>> for Option<T> {
  fn reflect (&self) -> Option<S> {
    match *self { 
      None => None, 
      Some(ref x) => Some(x.reflect())
    }
  }
}

impl<'a,S,T:Reflect<S>+Debug> Reflect<S> for &'a Rc<T> {
  fn reflect (&self) -> S {
    (**self).reflect()
  }
}

impl<'a,S,T:Reflect<S>+Debug> Reflect<S> for Rc<T> {
  fn reflect (&self) -> S {
    (**self).reflect()
  }
}

/// Reflected value; Gives a syntax for inductive data type
/// constructors (`Constr`), named articulations (`Art`) and primitive
/// data (`Data`).  All values in the engine (including the values of
/// nodes, and the values stored on edges) are represented with this
/// reflected `Val` type.  Primarily, this distinction between actual
/// Rust values and this type is what makes the DCG engine "reflected"
/// by the definitions in this module, and not identical to them.
#[derive(Debug,Clone,Hash,Eq,PartialEq)]
pub enum Val {
  /// Constructor, with a sequence of value parameters.
  Constr(Name,Vec<Val>),

  /// A tuple of values (like a constructor, but without a constructor
  /// name). Can be seen as a special case of `Constr`.
  Tuple(Vec<Val>),

  /// A list of values (like a tuple, but parsed and printed
  /// differently). Can be seen as a special case of `Constr`.
  Vec(Vec<Val>),
  
  /// Constructor with a sequence of fields (name-value pairs) as parameters.
  Struct(Name,Vec<(Name,Val)>),

  /// Named articulation, and its content (an `Art` is either a named value, or a named computation).
  Art(Loc,ArtContent),

  /// First-class `Name` value.
  Name(Name),
  
  /// Primitive, immutable data.
  Const(Const),
  
  /// Temporary; for marking places in code where we should produce a
  /// value, but don't yet have a good way to do so.
  ValTODO,
}

/// Primitive constants
#[derive(Debug,Clone,Hash,Eq,PartialEq)]
pub enum Const {
  /// Integers
  Num(isize),
  /// Natural numbers
  Nat(usize),
  /// Strings
  String(String),
}

/// The content of an articulation: Either a cell holding a value, or
/// a thunk that has optionally produced a value.
#[derive(Debug,Clone,Hash,Eq,PartialEq)]
pub enum ArtContent {
  /// The `Art` consists of a ref cell holding a value
  Val(Rc<Val>),
  /// The `Art` consists of a thunk that, when forced, computes a value
  Comp(Option<Rc<Val>>),
  /// `Unknown` content means that it has not yet dereferenced by any
  /// reflective process.  A reflective process dereferences an `Art`
  /// by using a reflected `DCG` to map this `Art`'s `Loc` to a
  /// `Node`.  This node gives one of the two known cases (`Val` or
  /// `Comp`), above, depending on whether it is a `RefNode` or a
  /// `CompNode`.
  Unknown,
}

/// Reflected version of `engine::Loc` A `Loc` is a particular
/// template for a `Name`: It is a path (a possibly-empty list of
/// `Name`s), followed by a distinguished `Name`.  A `Loc` can be
/// thought of roughly like a file path in UNIX (but Adapton has
/// nothing to do with files, or with UNIX, directly).
#[derive(PartialEq,Eq,Debug,Hash,Clone)]
pub struct Loc {
  /// The path of the `Loc` is a list of `Name`s.
  pub path: Path,
  /// The distinguished `Name` of the `Loc` (must be unique in the path).
  pub name: Name,
}

/// A `Path` here is just a `Vec` of `Name`s
pub type Path = Vec<Name>;

/// Reflected version of `engine::Effect`
#[derive(Debug,Clone,Eq,PartialEq,Hash)]
pub enum Effect {
  /// The effect consists of a thunk observing the value of another
  /// thunk or reference cell.  That is, the effect consists of
  /// **consuming** a value, by demanding its production.
  Force,
  /// The effect consists of a thunk allocating a value or thunk at a
  /// particular name.  That is, the effect consists of **producing**
  /// a value or computation.  If this content differs, then the
  /// allocation is a **reallocation**, and the engine dirties the old
  /// observers and allocators of the preceding content.
  Alloc,
}

/// Reflected version of `engine::Succ`.  Unlike the real engine's
/// `Succ` type, this version stores a reflected value (of type
/// `Val`).
#[derive(Debug,Clone,Eq,PartialEq,Hash)]
pub struct Succ {
  /// Dirty invariant: If this edge is dirty, then all predecessors of
  /// the edge are dirty too.
  pub dirty:  bool,
  /// The target of the outgoing `Effect`
  pub loc:    Loc,
  /// The effect: either producing or consuming articulated content
  pub effect: Effect,
  /// The value either produced or consumed by this `Effect`
  pub value:  Val,
  /// Duplicate edge invariant: If this edge is a duplicate according
  /// to this flag, then it is preceded by an edge with the same
  /// effect `effect`, targeting the same location `loc`.
  /// Furthermore, because of the (precise naming) semantics of
  /// Adapton and its programs, the observed value `value` and `dirty`
  /// statuses are always the same across duplicated edges.  Hence,
  /// the engine does not store these duplicate edges: They are
  /// completely redundant.
  pub is_dup: bool,
}

// Note: Can't implement Reflect<T> for T, since that conflicts with
// impl Reflect<Option<S>> for Option<T>.  I don't understand why,
// since T != S in the latter case, making it applicable when
// Reflect<T> for T is not.
impl Reflect<Succ> for Succ {
  fn reflect (&self) -> Succ {
    self.clone()
  }
}

/// Reflected version of `engine::Pred`
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Pred {
  /// The predecessor of the node in question
  pub loc:    Loc,
  /// The effect that the predecessor has done to the node in question
  pub effect: Effect,
}

/// Reflected version of `engine::CompNode`.  Stores a reflected value
/// of type `Option<Val>`, which is `None` when the node has not yet
/// been executed, and `Some(_)` otherwise.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CompNode {
  pub preds:   Vec<Pred>,
  pub succs:   Vec<Succ>,
  pub prog_pt: ProgPt,
  pub value:   Option<Val>,
}

/// Reflected version of `engine::MutNode`.  Stores a reflected value of type `Val`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct RefNode {  
  pub preds: Vec<Pred>,
  pub value: Val,
}

/// Reflected version of `engine::PureNode`.  Stores a reflected value of type `Val`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PureNode {  
  pub value: Val,
}

/// Reflected version of `engine::Node`.  Unlike the real engine,
/// these nodes are not parameterized by a value type.  Instead, their
/// values are all reflected into type `Val`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Node {
  Comp(CompNode),
  Ref(RefNode),
  Pure(PureNode),
}

/// Get the `Succ`s of a `Node`, if they are defined.
pub fn succs_of_node (nd:&Node) -> Option<&Vec<Succ>> {
  match *nd {
    Node::Comp(ref nd) => Some(&nd.succs),
    Node::Ref(ref _nd) => None,
    Node::Pure(ref _nd) => None,
  }
}

/// Get the `Pred`s of a `Node`, if they are defined.
pub fn preds_of_node (nd:&Node) -> Option<&Vec<Pred>> {
  match *nd {
    Node::Comp(ref nd) => Some(&nd.preds),
    Node::Ref(ref nd) => Some(&nd.preds),
    Node::Pure(ref _nd) => None,
  }
}

/// Reflected version of `engine::Frame`.
#[derive(Debug, Clone)]
pub struct Frame {
  pub loc:   Loc,
  pub succs: Vec<Succ>,
}

/// Reflected version of `engine::DCG`.
#[derive(Debug, Clone)]
pub struct DCG {
  /// The current memo table, mapping `Loc`s to `Node`s.
  pub table: HashMap<Loc, Node>,
  /// A stack of `Frame`s, which store the currently-executing nodes, and their outgoing edges thus far.
  pub stack: Vec<Frame>,
  /// A list of `Name`s, which is extended for nested regions of code
  /// by the `ns` (namespace) combinator.  This path variable
  /// determines the path for each allocated `Loc`.
  pub path:  Vec<Name>,
}


/// Wrapper for `parse_val::parse_val`. Transforms most Rust data
/// that derives `Debug` into a reflected `Val`.
pub fn reflect_val <V:Debug> (v:&V) -> Val { 
    use parse_val::parse_val;
    parse_val(v)
}

/// Gives effects and traces for cleaning and dirtying, the engine's
/// internal DCG traversal/processing.  By contrast, the enclosing
/// module (`reflect`) only gives reflected versions of the DCG
/// itself, not changes that the engine makes to it.
pub mod trace {
  use std::fmt;

  /// Distinguish fresh allocations from those that reuse an existing location.
  #[derive(Clone,Debug)]
  pub enum AllocCase {
    /// The allocation was **created** fresh; it was **not** reused.
    LocFresh,
    /// The allocation matched the location of a prior allocation. its
    /// content may or may not also match.  The `bool` indicates the two
    /// cases: `true` means same content, `false` means changed content.
    LocExists(ChangeFlag),      
  }

  #[derive(Clone,Debug)]
  pub enum ChangeFlag {
      ContentDiff,
      ContentSame,
  }

  /// Distinguish ref cell allocations from thunk allocations
  #[derive(Clone,Debug)]
  pub enum AllocKind {
    RefCell,
    Thunk,
  }

  /// When the program `force`s a computation, either the cache is
  /// either empty (`CacheMiss`) or non-empty (`CacheHit`).  The cached
  /// value may not be consistent without a cleaning.  When the program
  /// `force`s a reference cell, it simply gets the current value.
  #[derive(Clone,Debug)]
  pub enum ForceCase {
    /// The DCG has no cached value for this computation; no prior
    /// computation will be reused.
    CompCacheMiss,
    /// The DCG has a cached value for this computation; it may not be
    /// consistent without a cleaning first.
    CompCacheHit,
    /// The forced node is a ref cell with a (mutable) value, and hence,
    /// no computation was necessary.  The `force` simply gets the
    /// current value.
    RefGet,
  }

  /// The effects of the DCG (including cleaning and dirtying) on one of
  /// its edges.
  #[derive(Clone,Debug)]
  pub enum Effect { 
    /// Wrapper for Effect::Alloc; transition to DCG after the alloc.
    Alloc (AllocCase, AllocKind), 
    
    /// Wrapper for Effect::Force; transition to DCG after the force.
    Force (ForceCase),

    /// Transition to this edge as **dirty** (potentially inconsistent).
    /// This transition may consist of marking other edges dirty.  The
    /// DCG's invariant about dirty edges is simple: If an edge is
    /// dirty, then all edges that force (demand/observe) the source of
    /// that edge must also be dirty.  This transitive closure property
    /// ensures that we do not accidentally reuse stale cached values by
    /// mistake (a dirty edge will always witness a potentially stale
    /// cached value).
    Dirty, 
    
    /// Clean this edge, marking it **not dirty**, or equivalently,
    /// **definitely consistent**. Cleaning an edge consists of
    /// processing the edge's dirty transitive dependencies, if any.
    /// Recursively, this processing may consist of marking other edges
    /// clean (`CleanEdge`), and/or, removing edges (`Remove`) and
    /// replacing them via reevaluation under the current DCG state
    /// (`CleanEval`).  
    ///
    /// This process finishes in one of two mutually-exclusive
    /// situations: Either the edge is cleaned, meaning that its target
    /// need not be recomputed (`CleanEdge`). Or else, the value on this
    /// edge has changed, and this means that the edge's source is
    /// potentially affected by the change. Consequently, this change
    /// requires that the edge source be reevaluated (`CleanEval`).
    CleanRec,
    
    /// Transition to this edge as **clean** (definitely consistent),
    /// after doing a recursive cleaning of its dependencies and finding
    /// that they are clean.  This effect is mutually-exclusive with
    /// `CleanEval`, which occurs when `CleanEdge` **cannot** occur on
    /// an edge that is being recursively cleaned (via `CleanRec`).
    CleanEdge,
    
    /// Re-evaluate the previously-forced thunk that is the target of
    /// this edge, to clean it.  This effect is mutually-exclusive with
    /// `CleanEdge`.  It occurs when `CleanEdge` cannot occur.
    CleanEval,
    
    /// Transition to the DCG without this edge.  Perhaps it will be
    /// replaced via re-execution, sometime later.
    Remove,
  }
  
  /// An edge in the DCG, representing an effect of the incremental program.
  #[derive(Clone,Debug)]
  pub struct Edge {
    /// The source of the directed edge; it is actively _doing_ the
    /// effect of `succ.effect` to `succ.loc`.  `None` means the doer is
    /// the **editor**, who is not identified by any location. (The editor
    /// is not a node in the DCG, but rather, an actor operating outside
    /// of it).
    pub loc: Option<super::Loc>,
    /// The effect and target of the directed edge.
    pub succ: super::Succ,
  }
  
  /// `DCGTrace`: A Rose-tree of DCG edge-effects.  This tree structure
  /// allows the effects to have a a "time interval" that nests around
  /// and within the time intervals of other effects.
  #[derive(Clone,Debug)]
  pub struct Trace { 
    /// The DCG effect (e.g., Alloc(MatchDiff), Dirty, Clean, etc.)
    pub effect:Effect,
    
    /// The DCG edge on which this DCG effect takes place
    pub edge: Edge,
    
    /// The DCG effects that occur subordinately as a result of this
    /// effect. (They begin after this effect begins, and the end before
    /// this effect ends).
    pub extent:Box<Vec<Trace>>,
  }

    
    #[derive(Clone,Copy)]
    pub enum Role { Editor, Archivist }
    
    /// Count of effects and effect-patterns in trace.  For fields
    /// with two numbers, the first and second projection give the
    /// `Role::Editor` and `Role::Archivist` counts, respectively.
    #[derive(Clone)]
    pub struct TraceCount {
        pub total_updates:   usize,
        pub total_archivist: usize,
        pub reeval_nochange: usize,
        pub reeval_change:   usize,
        pub clean_rec:       usize,
        pub dirty:           (usize, usize),
        pub alloc_fresh:     (usize, usize),
        pub alloc_nochange:  (usize, usize),
        pub alloc_change:    (usize, usize),
    }
    
    pub fn trace_count_zero() -> TraceCount {
        TraceCount{ alloc_fresh:(0,0), 
                    alloc_change:(0,0), 
                    alloc_nochange:(0,0),
                    reeval_change:0, 
                    reeval_nochange:0, 
                    dirty:(0,0), 
                    clean_rec:0,
                    total_updates:0,
                    total_archivist:0,
        }
    }

    impl fmt::Debug for TraceCount {
        fn fmt(&self, f:&mut fmt::Formatter) -> fmt::Result {  
            write!(f,"\
                Trace counts:        {:>12} {:>12}
------------------------------------------------
editor:
  alloc_fresh:       {:>12} {:>12.2}
  alloc_nochange:    {:>12} {:>12.2}
  alloc_change:      {:>12} {:>12.2}
  dirty:             {:>12} {:>12.2} 
archivist:
  alloc_fresh:       {:>12} {:>12.2}
  alloc_nochange:    {:>12} {:>12.2}
  alloc_change:      {:>12} {:>12.2}
  dirty:             {:>12} {:>12.2}
  reeval:
    clean_rec:       {:>12} {:>12.2}
    reeval_nochange: {:>12} {:>12.2}
    reeval_change:   {:>12} {:>12.2}
",
                   format!("sum"),        format!("ave"),
                   // Editor
                   self.alloc_fresh.0,    self.alloc_fresh.0    as f32 / self.total_updates as f32,
                   self.alloc_nochange.0, self.alloc_nochange.0 as f32 / self.total_updates as f32,
                   self.alloc_change.0,   self.alloc_change.0   as f32 / self.total_updates as f32,
                   self.dirty.0,          self.dirty.0          as f32 / self.total_updates as f32,
                   // Archivist
                   self.alloc_fresh.1,    self.alloc_fresh.1    as f32 / self.total_updates as f32,
                   self.alloc_nochange.1, self.alloc_nochange.1 as f32 / self.total_updates as f32,
                   self.alloc_change.1,   self.alloc_change.1   as f32 / self.total_updates as f32,
                   self.dirty.1,          self.dirty.1          as f32 / self.total_updates as f32,
                   self.clean_rec,        self.clean_rec        as f32 / self.total_updates as f32,
                   self.reeval_nochange,  self.reeval_nochange  as f32 / self.total_updates as f32, 
                   self.reeval_change,    self.reeval_change    as f32 / self.total_updates as f32,
            )
        }
    }

    
    pub fn trace_count(trs:&Vec<Trace>, total_updates:Option<usize>) -> TraceCount {
        let mut trace_count = trace_count_zero();
        match total_updates {
            Some(x) => trace_count.total_updates = x,
            None    => trace_count.total_updates = 1,
        }
        for tr in trs {
            trace_count_rec(Role::Editor, tr, &mut trace_count)
        }
        return trace_count
    }

    fn trace_count_dirty(role:Role, tr:&Trace, c:&mut TraceCount) {
        match tr.effect {
            Effect::Dirty => match role { 
                Role::Editor    => c.dirty.0 += 1,
                Role::Archivist => c.dirty.1 += 1,
            },
            _ => panic!("unexpected effect: {:?}", tr),
        }
        for sub_tr in tr.extent.iter() {
            trace_count_dirty(role, sub_tr, c)
        }
    }

    pub fn trace_count_rec(role:Role, tr:&Trace, c:&mut TraceCount) {
        match role {
            Role::Editor =>
                match tr.effect {
                    Effect::Dirty => trace_count_dirty(role, tr, c),
                    Effect::Remove => unreachable!(),
                    Effect::CleanEdge => unreachable!(),
                    Effect::CleanEval => unreachable!(),
                    Effect::CleanRec => unreachable!(),
                    Effect::Alloc(AllocCase::LocFresh, _) => { 
                        c.alloc_fresh.0 += 1; 
                        assert!(tr.extent.len() == 0)  
                    }
                    Effect::Alloc(AllocCase::LocExists(ChangeFlag::ContentSame), _) => { 
                        c.alloc_nochange.0 += 1;
                        for sub_tr in tr.extent.iter() { trace_count_dirty(role, sub_tr, c) }
                    }
                    Effect::Alloc(AllocCase::LocExists(ChangeFlag::ContentDiff), _) => {
                        c.alloc_change.0 += 1;
                        for sub_tr in tr.extent.iter() { trace_count_dirty(role, sub_tr, c) }
                    }
                    Effect::Force(ForceCase::RefGet) => assert!(tr.extent.len() == 0),
                    Effect::Force(ForceCase::CompCacheHit)  |
                    Effect::Force(ForceCase::CompCacheMiss) => {
                        c.total_archivist += 1;
                        for sub_tr in tr.extent.iter() { 
                            trace_count_rec(Role::Archivist, sub_tr, c);
                        }
                    }
                },
            Role::Archivist =>
                match tr.effect {
                    Effect::Dirty => trace_count_dirty(role, tr, c),
                    Effect::Remove => assert!(tr.extent.len() == 0),
                    Effect::CleanEdge => assert!(tr.extent.len() == 0),
                    Effect::CleanEval => {
                        let mut change = false;
                        for subtr in tr.extent.iter() {
                            match subtr.effect {
                                Effect::Remove => (),
                                Effect::CleanEdge => (),
                                Effect::CleanEval => (),
                                Effect::CleanRec => (),
                                Effect::Alloc(AllocCase::LocExists(ChangeFlag::ContentSame), _) => (),
                                Effect::Alloc(AllocCase::LocFresh, _)                           => change = true,
                                Effect::Alloc(AllocCase::LocExists(ChangeFlag::ContentDiff), _) => change = true,
                                Effect::Force(ForceCase::RefGet)        => (), /* XXX/TODO: This is undercounting?; do we need to know if gotten value was different? */
                                Effect::Force(ForceCase::CompCacheHit)  => (),
                                Effect::Force(ForceCase::CompCacheMiss) => change = true,
                                Effect::Dirty => change = true,
                            }
                        }
                        if change { c.reeval_change   += 1 }
                        else      { c.reeval_nochange += 1 };
                        for sub_tr in tr.extent.iter() { 
                            trace_count_rec(Role::Archivist, sub_tr, c);
                        }     
                    },
                    Effect::CleanRec => { 
                        c.clean_rec += 1;
                        for sub_tr in tr.extent.iter() { 
                            trace_count_rec(Role::Archivist, sub_tr, c);
                        }
                    }
                    Effect::Alloc(AllocCase::LocFresh, _) => {
                        c.alloc_fresh.1 += 1;
                        assert!(tr.extent.len() == 0);
                    }
                    Effect::Alloc(AllocCase::LocExists(ChangeFlag::ContentSame), _) => {
                        c.alloc_nochange.1 += 1;
                        for sub_tr in tr.extent.iter() { 
                            trace_count_rec(Role::Archivist, sub_tr, c);
                        }
                    }
                    Effect::Alloc(AllocCase::LocExists(ChangeFlag::ContentDiff), _) => {
                        c.alloc_change.1 += 1;
                        for sub_tr in tr.extent.iter() { 
                            trace_count_rec(Role::Archivist, sub_tr, c);
                        }                  
                    }
                    Effect::Force(ForceCase::RefGet) => {
                        assert!(tr.extent.len() == 0);
                    },
                    Effect::Force(ForceCase::CompCacheHit) => {
                        for sub_tr in tr.extent.iter() { 
                            trace_count_rec(Role::Archivist, sub_tr, c);
                        }
                    }
                    Effect::Force(ForceCase::CompCacheMiss) => {
                        for sub_tr in tr.extent.iter() { 
                            trace_count_rec(Role::Archivist, sub_tr, c);
                        }                  
                    }
                }
        }
    }
}
