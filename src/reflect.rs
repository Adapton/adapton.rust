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

/// Reflected value; Gives a syntax for inductive data type
/// constructors (`Constr`), named articulations (`Art`) and primitive
/// data (`Data).
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(PartialEq,Eq,Debug,Hash)]
pub struct Loc {
  /// The path of the `Loc` is a list of `Name`s.
  pub path: Path,
  /// The distinguished `Name` of the `Loc` (must be unique in the path).
  pub name: Name,
}

/// A `Path` here is just a `Vec` of `Name`s
pub type Path = Vec<Name>;

/// `Reflected version of engine::Effect`
#[derive(Debug)]
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
#[derive(Debug)]
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
