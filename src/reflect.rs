use engine::Name;
use macros::ProgPt;
use std::fmt::Debug;
//use std::hash::Hash;
use std::rc::Rc;
use std::collections::HashMap;

/// Reflected data
pub trait Data : Debug { }

/// Reflected value; Gives a syntax for inductive data type
/// constructors (`Constr`), named articulations (`Art`) and primitive
/// data (`Data).
#[derive(Debug)]
pub enum Val {
  Constr(Name,Vec<Val>),
  Art(Name,ArtContent),
  Data(Box<Data>),
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

/// `Reflected version of engine::Loc`
#[derive(PartialEq,Eq,Debug,Hash)]
pub struct Loc {
  pub path: Vec<Name>,
  pub name: Name,
}

/// `Reflected version of engine::Effect`
#[derive(Debug)]
pub enum Effect {
  Observe,
  Allocate,
}

/// Reflected version of `engine::Succ`
#[derive(Debug)]
pub struct Succ {
  pub dirty:  bool,
  pub loc:    Rc<Loc>,
  pub effect: Effect,
  pub value:  Rc<Val>,
}

/// Reflected version of `engine::Pred`
#[derive(Debug)]
pub struct Pred {
  pub loc:    Loc,
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

/// Reflected version of `engine::GraphNode`
#[derive(Debug)]
pub enum GraphNode {
  Comp(CompNode),
  Ref(RefNode),
}

/// Reflected version of `engine::Frame`
#[derive(Debug)]
pub struct Frame {
  pub loc:   Rc<Loc>,
  pub succs: Vec<Succ>,
}

/// Reflected version of `engine::DCG`
#[derive(Debug)]
pub struct DCG {
  pub table: HashMap<Rc<Loc>, Box<GraphNode>>,
  pub stack: Vec<Frame>,
  pub path:  Vec<Name>,
}
