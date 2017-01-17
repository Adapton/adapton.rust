use engine::Name;
use macros::ProgPt;
use std::fmt::Debug;
//use std::hash::Hash;
use std::rc::Rc;
use std::collections::HashMap;

// /// Reflected primitive data
//pub trait Data : Debug { }

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

// impl<T> Reflect<T> for T {
//  fn reflect(&self) -> T {
//    self.clone()
//  }
// }

/// Reflected value; Gives a syntax for inductive data type
/// constructors (`Constr`), named articulations (`Art`) and primitive
/// data (`Data`).  All values in the engine (including the values of
/// nodes, and the values stored on edges) are represented with this
/// reflected `Val` type.  Primarily, this distinction between actual
/// Rust values and this type is what makes the DCG engine "reflected"
/// by the definitions in this module, and not identical to them.
#[derive(Debug,Clone)]
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
  Art(Name,ArtContent),
  
  /// Primitive, immutable data.
  Data(Rc<Debug>),
  
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

/// Reflected version of `engine::Succ`.  Unlike the real engine's
/// `Succ` type, this version stores a reflected value (of type
/// `Val`).
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
#[derive(Debug, Clone)]
pub struct Pred {
  /// The predecessor of the node in question
  pub loc:    Loc,
  /// The effect that the predecessor has done to the node in question
  pub effect: Effect,
}

/// Reflected version of `engine::CompNode`.  Stores a reflected value
/// of type `Option<Val>`, which is `None` when the node has not yet
/// been executed, and `Some(_)` otherwise.
#[derive(Debug, Clone)]
pub struct CompNode {
  pub preds:   Vec<Pred>,
  pub succs:   Vec<Succ>,
  pub prog_pt: ProgPt,
  pub value:   Option<Val>,
}

/// Reflected version of `engine::MutNode`.  Stores a reflected value of type `Val`.
#[derive(Debug, Clone)]
pub struct RefNode {  
  pub preds: Vec<Pred>,
  pub value: Val,
}

/// Reflected version of `engine::PureNode`.  Stores a reflected value of type `Val`.
#[derive(Debug, Clone)]
pub struct PureNode {  
  pub value: Val,
}

/// Reflected version of `engine::Node`.  Unlike the real engine,
/// these nodes are not parameterized by a value type.  Instead, their
/// values are all reflected into type `Val`.
#[derive(Debug, Clone)]
pub enum Node {
  Comp(CompNode),
  Ref(RefNode),
  Pure(PureNode),
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

/// Gives effects and traces for cleaning and dirtying, the engine's
/// internal DCG traversal/processing.  By contrast, the enclosing
/// module (`reflect`) only gives reflected versions of the DCG
/// itself, not changes that the engine makes to it.
pub mod trace {

  /// Distinguish fresh allocations from those that reuse an existing location.
  #[derive(Clone,Debug)]
  pub enum AllocCase {
    /// The allocation was **created** fresh; it was **not** reused.
    LocFresh,
    /// The allocation matched the location of a prior allocation. its
    /// content may or may not also match.  The `bool` indicates the two
    /// cases: `true` means same content, `false` means changed content.
    LocExists,
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
}
