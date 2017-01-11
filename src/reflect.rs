use engine::Name;
use macros::ProgPt;
use std::fmt::Debug;
//use std::hash::Hash;
use std::rc::Rc;
use std::collections::HashMap;

/// Reflected primitive data
pub trait Data : Debug { }

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
/// data (`Data).
#[derive(Debug,Clone)]
pub enum Val {
  /// Constructor, with value parameters.
  Constr(Name,Vec<Val>),
  /// Named articulation, and its content (an `Art` is either a named value, or a named computation).
  Art(Name,ArtContent),
  /// Primitive, immutable data.
  Data(Rc<Data>),
  
  /// Temporary; for marking places in code where we should produce a
  /// value, but don't yet have a good way to do so.
  ValTODO,
}

/// The content of an articulation: Either a cell holding a value, or
/// a thunk that has optionally produced a value.
#[derive(Debug,Clone)]
pub enum ArtContent {
  /// A cell holding a value
  Val(Rc<Val>),
  /// A thunk that, when forced, computes a value
  Comp(Option<Rc<Val>>),
}

/// `Reflected version of engine::Loc` A `Loc` is a particular
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

/// `Reflected version of engine::Effect`
#[derive(Debug,Clone)]
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

/// Reflected version of `engine::Succ`
#[derive(Debug,Clone)]
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
}

impl Reflect<Succ> for Succ {
  fn reflect (&self) -> Succ {
    self.clone()
  }
}

/// Reflected version of `engine::Pred`
#[derive(Debug)]
pub struct Pred {
  /// The predecessor of the node in question
  pub loc:    Loc,
  /// The effect that the predecessor has done to the node in question
  pub effect: Effect,
}

/// Reflected version of `engine::CompNode`
#[derive(Debug)]
pub struct CompNode {
  pub preds:   Vec<Pred>,
  pub succs:   Vec<Succ>,
  pub prog_pt: ProgPt,
  pub value:   Option<Val>,
}

/// Reflected version of `engine::MutNode`
#[derive(Debug)]
pub struct RefNode {  
  pub preds: Vec<Pred>,
  pub value: Val,
}

/// Reflected version of `engine::PureNode`
#[derive(Debug)]
pub struct PureNode {  
  pub value: Val,
}

/// Reflected version of `engine::GraphNode`
#[derive(Debug)]
pub enum GraphNode {
  Comp(CompNode),
  Ref(RefNode),
  Pure(PureNode),
}

/// Reflected version of `engine::Frame`
#[derive(Debug)]
pub struct Frame {
  pub loc:   Loc,
  pub succs: Vec<Succ>,
}

/// Reflected version of `engine::DCG`
#[derive(Debug)]
pub struct DCG {
  /// The current memo table, mapping `Loc`s to `GraphNode`s.

  pub table: HashMap<Loc, Box<GraphNode>>,
  pub stack: Vec<Frame>,
  pub path:  Vec<Name>,
}

#[derive(Debug)]
pub enum DCGAlloc {
  /// The allocation was **created** fresh; it was **not** reused.
  LocFresh,
  /// The allocation matched the location of a prior allocation. its
  /// content may or may not also match.  The `bool` indicates the two
  /// cases: `true` means same content, `false` means changed content.
  LocMatch(bool),
}

/// When the program performs a `force`, either the cache is either
/// empty (`CacheMiss`) or non-empth (`CacheHit`).  The cached value
/// may not be consistent without a cleaning.
#[derive(Debug)]
pub enum DCGForce {
  /// The DCG has no cached value for this computation; no prior computation will be reused.
  CacheMiss,
  /// The DCG has a cached value for this computation; it may not be consistent without a cleaning.
  CacheHit,
}

/// The effects of the DCG (including cleaning and dirtying) on one of
/// its edges.
#[derive(Debug)]
pub enum DCGEffect { 
  /// Wrapper for Effect::Alloc; transition to DCG after the alloc.
  Alloc (DCGAlloc), 

  /// Wrapper for Effect::Force; transition to DCG after the force.
  Force (DCGForce),

  /// Transition to this edge as **dirty** (potentially inconsistent).
  /// This transition may consist of marking other edges dirty.  The
  /// DCG's invariant about dirty edges is simple: If an edge is
  /// dirty, then all edges that force (demand/observe) the source of
  /// that edge must also be dirty.  This transitive closure property
  /// ensures that we do not accidentally reuse stale cached values by
  /// mistake (a dirty edge will always witness a potentially stale
  /// cached value)
  Dirty, 

  /// Transition to this edge as **clean** (definitely consistent).
  /// This transition may consist of marking other edges clean,
  /// and/or, removing edges and replacing them via reexecution under
  /// the current DCG state.
  Clean,

  /// Transition to the DCG without this edge.  Perhaps it will be
  /// replaced via re-execution, sometime later.
  Remove,
}

/// An adge in the DCG, representing an effect of the incremental program.
#[derive(Debug)]
pub struct DCGEdge {
  /// The source of the directed edge; it is actively _doing_ the
  /// effect of `succ.effect` to `succ.loc`.  `None` means the doer is
  /// the **editor**, who is not identified by any location. (The editor
  /// is not a node in the DCG, but rather, an actor operating outside
  /// of it).
  pub loc: Option<Loc>,
  /// The effect and target of the directed edge.
  pub succ: Succ,
}

/// `DCGTrace`: A Rose-tree of DCG edge-effects.  This tree structure
/// allows the effects to have a a "time interval" that nests around
/// and within the time intervals of other effects.
#[derive(Debug)]
pub struct DCGTrace { 
  /// The DCG effect (e.g., Alloc(MatchDiff), Dirty, Clean, etc.)
  pub effect:DCGEffect,

  /// The DCG edge on which this DCG effect takes place
  pub edge: DCGEdge,

  /// The DCG effects that occur subordinately as a result of this
  /// effect. (They begin after this effect begins, and the end before
  /// this effect ends).
  pub extent:DCGTraces,
}
/// A (boxed) vector of `DCGTrace`s.
pub type DCGTraces = Box<Vec<DCGTrace>>;
