//! Adapton's core calculus, implemented as a runtime library.  We
//! implement two versions of this interface, which we refer to as
//! _engines_: The **naive engine** and the **DCG engine**,
//! implemented based on the algorithms from the Adapton papers.
//!
//! To program algorithms that use this interface, consider the
//! following functions and macros:
//! 
//!  - `fn force`, which observes/consumes/demands the value of an
//!     `Art`.
//!
//!  - `fn cell`, which allocates/produces an `Art` to hold a given
//!     value.  See also, the macro `cell_call!`, which places the
//!     result of a function call into a (named) cell.
//!
//!  - `fn thunk`, and the macros `thunk!` and `eager!`, which
//!     introduce `Art`s that hold suspended computations and produce
//!     their results, when `force`d.  The macro `eager!` forces this
//!     suspended computation eagerly.
//! 
//!  - `fn ns`, which manages a monadic _naming policy_ for
//!     allocations by `cell` and `thunk` (and macros that use these).
//!     This function extends the current path (a sequence of names)
//!     by one name, which we refer to as a _namespace_.  This
//!     namespace concept is analogous to a directory in the UNIX
//!     filesystem.

use core::any::TypeId;
use core::marker::PhantomData;

use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fmt::Debug;
use std::fmt::{Formatter,Result};
use std::fmt;
use std::hash::{Hash,Hasher};
use std::collections::hash_map::DefaultHasher;
use std::mem::replace;
use std::mem::transmute;
use std::rc::Rc;
use std::fmt::Write;

use macros::{ProgPt};
use reflect;

thread_local!(static GLOBALS: RefCell<Globals> = RefCell::new(Globals{engine:Engine::Naive}));
thread_local!(static UNIT_NAME: Name = Name{ hash:0, symbol: Rc::new(NameSym::Unit) });

struct TraceSt { stack:Vec<Box<Vec<reflect::trace::Trace>>>, }

/// When this option is set to some, the engine will record a trace of its DCG effects.
thread_local!(static TRACES: RefCell<Option<TraceSt>> = RefCell::new( None ));

fn my_hash<T>(obj: T) -> u64
  where T: Hash
{
  let mut hasher = DefaultHasher::new();
  obj.hash(&mut hasher);
  hasher.finish()
}

/// Reflects the DCG engine, including both the effects of the
/// programs running in it, and the internal effects of the engine
/// cleaning and dirtying the DCG.  For the latter effects, see the
/// `trace` module.
///
/// **Reflected Values**.  Notably, the values in the engine
/// (including the values of mutable and compute nodes, and the values
/// stored on edges between them) are reflected here into a special `Val`
/// type.  Primarily, the distinction between actual Rust values and
/// this reflected `Val` type is what makes the DCG engine "reflected"
/// by the definitions in this module, and not identical to them.
/// 
/// This module provides an interface used by Adapton Lab to produce
/// HTML visualizations of these internal structures, for
/// experimentation and debugging (namely, the `dcg_reflect_begin` and
/// `dcg_reflect_end` functions).  For the purposes of debugging,
/// visualization and design/exploration, we exploit the reflected
/// version of values to "walk" them, finding their articulations, and
/// walking their values, recursively.

pub mod reflect_dcg {
  use reflect::*;
  pub use parse_val;

  use std::fmt::{Write};
  use super::{TraceSt,TRACES,GLOBALS,Engine};
  use adapton::engine::Name;

  /// See doc for `write_name`. Returns this output as a string.
  pub fn string_of_name (n:&Name) -> String {
    let mut output = String::from("");  write_name(&mut output, n);  output
  }
  /// See doc for `write_path`. Returns this output as a string.
  pub fn string_of_path (p:&Path) -> String {
    let mut output = String::from("");  write_path(&mut output, &p); output
  }
  /// See doc for `write_loc`. Returns this output as a string.
  pub fn string_of_loc (l:&Loc) -> String {
    let mut output = String::from("");  write_loc (&mut output, &l); output
  }

  /// Write a concise human-readable version of the name (not the
  /// verbose, machine-parsable `Debug` version).
  pub fn write_name<W:Write> (w:&mut W, n:&Name) {
    super::write_namesym(w, &n.symbol).unwrap();
  }

  /// Write a concise human-readable version of the path (not the
  /// verbose, machine-parsable `Debug` version).
  pub fn write_path<W:Write> (w:&mut W, p:&Path) {
    write!(w, "__").unwrap(); // Underscores are valid in CSS class names
    for n in p.iter() {
      write_name(w, n);
      write!(w, "__").unwrap(); // Underscores are valid in CSS class names
    }
  }

  /// Write a concise human-readable version of the location (not the
  /// verbose, machine-parsable `Debug` version).  
  pub fn write_loc<W:Write> (w:&mut W, l:&Loc)  {
    write_path(w, &l.path);
    write!(w, "_").unwrap(); // Underscores are valid in CSS class names
    write_name(w, &l.name);
  }
  
  /// Reflect the DCG's internal structure now.  Does not reflect any
  /// engine effects over this DCG (e.g., no cleaning or dirtying),
  /// just the _program effects_ recorded by the DCG's structure.
  /// Returns None if the engine is `Naive` and thus has no reflected
  /// state whatsoever.
  pub fn dcg_reflect_now() -> Option<DCG> {
    GLOBALS.with(|g| {
      match g.borrow().engine {
        Engine::DCG(ref dcg) => Some((*dcg.borrow()).reflect()),
        Engine::Naive => None,
      }
    })
  }

  /// Begin recording (reflections of) DCG effects.  See `dcg_reflect_end()`.
  pub fn dcg_reflect_begin() {
    TRACES.with(|tr| { 
      let check = match *tr.borrow() {
        None => true,
        Some(_) => false };
      if check { 
        *tr.borrow_mut() = Some(TraceSt{stack:vec![Box::new(vec![])]})
      } else { 
        panic!("cannot currently nest calls to dcg_reflect_begin().")
      }
    })
  }
  
  /// Stop recording (reflections of) DCG effects, and return them as a
  /// forrest (of DCG traces).  See `dcg_reflect_begin()`.
  pub fn dcg_reflect_end() -> Vec<trace::Trace> {
    TRACES.with(|tr| { 
      let traces = match *tr.borrow_mut() {
        None => panic!("dcg_reflect_end() without a corresponding dcg_reflect_begin()."),
        Some(ref mut tr) => { 
          // Assert that dcg_effect_(begin/end) are not mismatched.
          assert_eq!(tr.stack.len(), 1);
          tr.stack.pop()
        }
      };
      match traces {
        None => unreachable!(),
        Some(traces) => {
          *tr.borrow_mut() = None;
          *traces
        }
      }
    })
  }
}
use reflect::Reflect;


//#[macro_export]
macro_rules! dcg_effect_begin {
  ( $eff:expr, $loc:expr, $succ:expr, $has_extent:expr ) => {{ 
    /// The beginning of an effect, with an option extent (nested effects)
    TRACES.with(|tr| {
      match *tr.borrow_mut() {
        None => (),
        // Some ==> We are building a trace
        Some(ref mut ts) => {            
          match ts.stack.last_mut() {
            None => unreachable!(),
            Some(ref mut ts) => { 
              ts.push( reflect::trace::Trace{
                extent: Box::new(vec![]),
                effect:$eff,
                edge: reflect::trace::Edge{
                  loc:  $loc.reflect(),
                  succ: $succ.reflect(),
                }})}};
          if $has_extent {
            ts.stack.push(Box::new(vec![]))
          } else { }
        }
      }})
  }}
  ;
  ( $eff:expr, $loc:expr, $succ:expr ) => {{
    dcg_effect_begin!($eff, $loc, $succ, true)
  }}
}

//#[macro_export]
macro_rules! dcg_effect_end {
  () => {{ 
    /// The end of an effects' extent. Operationally, the traces at
    /// the top of the stack are popped; they become the extent of the
    /// trace at the end (top) of the second top-most sequence of
    /// traces.
    TRACES.with(|tr| {
      match *tr.borrow_mut() {
        None => (),
        Some(ref mut tr) => {
          let trs = tr.stack.pop(); 
          match (trs, tr.stack.last_mut()) {
            (None, _) => unreachable!(),
            (_, None) => unreachable!(),
            (Some(parent_extent), Some(trs)) => 
              match trs.last_mut() {
                None => unreachable!(),
                Some(parent) => { assert_eq!(parent.extent.len(), 0);
                                  parent.extent = parent_extent }
              }
          }
        }
      }
    })
  }}
}

macro_rules! dcg_effect {
  ( $eff:expr, $loc:expr, $succ:expr ) => {{ 
    /// An effect without an extent (without nested effects)
    dcg_effect_begin!($eff, $loc, $succ, false)
  }}
}

macro_rules! current_loc {
  ( $st:expr ) => {{ 
    match ($st).stack.last() { 
      None => None,        
      Some(frame) => Some(&frame.loc), 
    }
  }}
}

/// *Names*: First-class data that identifies a mutable cell (see
/// `cell`) or a thunk (see `thunk`).  When a name identifies
/// different content over time, it describes *where* incremental
/// changing is occurring, relative to other (unaffected) parts of
/// data structures or computations.
#[derive(PartialEq,Eq,Clone)]
pub struct Name {
  hash : u64, // hash of symbol
  symbol : Rc<NameSym>,
}
impl Debug for Name {
  fn fmt(&self, f:&mut Formatter) -> Result { self.symbol.fmt(f) }
}
impl Hash for Name {
  fn hash<H>(&self, state: &mut H) where H: Hasher {
    self.hash.hash(state)
  }
}

// Each location identifies a node in the DCG.
#[derive(PartialEq,Eq,Clone)]
struct Loc {
  hash : u64, // hash of (path,id)
  path : Rc<Path>,
  id   : Rc<ArtId>,
}
impl Debug for Loc {
  fn fmt(&self, f:&mut Formatter) -> Result {
    //write!(f,"{:?}*{:?}",self.path,self.id)
    write!(f,"Loc {{ path:[{:?}], id:{:?} }}", self.path, self.id)
  }
}
impl Hash for Loc {
  fn hash<H>(&self, state: &mut H) where H: Hasher {
    self.hash.hash(state)
  }
}
impl reflect::Reflect<reflect::Loc> for Loc {
  fn reflect(&self) -> reflect::Loc {
    reflect::Loc {
      path:self.path.reflect(),
      name:match *self.id { 
        ArtId::Structural(ref hash) => name_of_hash64(*hash),
        ArtId::Nominal(ref name) => name.clone(),
      }
    }
  }
}

#[derive(Hash,PartialEq,Eq,Clone)]
enum ArtId {
  /// Identifies an `Art` structurally, based on hashing content.
  Structural(u64), 
  /// Identifies an `Art` nominally, based on a programmer-chosen `Name`.
  Nominal(Name),
}

impl Debug for ArtId {
  fn fmt(&self, f:&mut Formatter) -> Result {
    match *self {
      ArtId::Structural(ref hash) => write!(f, "{}", hash),
      ArtId::Nominal(ref name) => write!(f, "{:?}", name),
    }
  }
}

/// Flags control runtime behavior of the DCG.
#[derive(Debug)]
pub struct Flags {
  pub use_purity_optimization : bool,
  /// Ignore the `Nominal` `ArtIdChoice`, and use `Structural` behavior instead
  pub ignore_nominal_use_structural : bool, 
  /// After each Adapton operation, check that the DCG is well-formed
  pub check_dcg_is_wf : bool, 
  /// Within each well-formedness check, write the DCG to the local filesystem
  pub write_dcg : bool, 
  /// Deprecated: At certain points in the Engine's code, write state changes as graph-movie output
  /// TODO: To be replaced with DCG reflection, and reflection-to-filesystem logic.
  pub gmlog_dcg : bool,
}

struct Globals {
  engine: Engine,
}

/// The engine API works in two modes: `Naive` and `DCG`. A `Naive` engine is stateless, whereas the `DCG` is stateful.
#[derive(Debug,Clone)]
pub enum Engine {
  DCG(RefCell<DCG>),
  Naive
}

/// *(DCG) Demanded Computation Graph*: The cache of past computation.
///
/// The DCG consists of private state (a memo table of DCG nodes, a
/// stack of DCG nodes, edges among these nodes, the current
/// namespace, etc.).
#[derive(Debug)]
pub struct DCG {
  pub flags : Flags, // public because I dont want to write / design abstract accessors
  table : HashMap<Rc<Loc>, Box<GraphNode>>,
  stack : Vec<Frame>,
  path  : Rc<Path>,
  //cnt   : Cnt,
  dcg_count : usize,
  dcg_hash  : u64,  
}

impl reflect::Reflect<reflect::DCG> for DCG {
  fn reflect(&self) -> reflect::DCG {
    reflect::DCG{
      table:{
        let mut table = HashMap::new();
        for (loc, gn) in self.table.iter() {
          let _ = table.insert(loc.reflect(), gn.reflect());
        }; table
      },
      stack:self.stack.iter()
        .map(|ref frame| frame.reflect() )
        .collect::<Vec<_>>(),
      path:self.path.reflect(),
    }
  }
}

impl Hash  for     DCG { fn hash<H>(&self, _state: &mut H) where H: Hasher { unimplemented!() }}
impl Eq    for     DCG { }
impl PartialEq for DCG { fn eq(&self, _other:&Self) -> bool { unimplemented!() } }
impl Clone for     DCG { fn clone(&self) -> Self { unimplemented!() } }

/// Name symbols.
/// 
/// For a core-calculus of names in this context, see this document:
/// https://arxiv.org/abs/1610.00097 (Typed Adapton: Refinement types
/// for nominal memoization).
/// 
/// For a general semantics of symbols, see Chapter 31 of PFPL 2nd
/// Edition. Harper 2016: http://www.cs.cmu.edu/~rwh/pfpl
#[derive(Hash,PartialEq,Eq,Clone,Debug)]
enum NameSym {
  Unit,           // Unit value for name symbols
  Hash64,        // Hashes (for structural names); hash stored in name struct
  String(String), // Strings encode globally-unique symbols.
  Usize(usize),   // USizes encode globally-unique symbols.
  Isize(isize),   // USizes encode globally-unique symbols.
  Pair(Rc<NameSym>,Rc<NameSym>), // A pair of unique symbols, interpeted as a symbol, is unique
  ForkL(Rc<NameSym>), // Left projection of a unique symbol is unique
  ForkR(Rc<NameSym>), // Right projection of a unique symbol is unique
}

fn write_namesym<W:Write>(w:&mut W, n:&NameSym) -> Result {
  match *n {
    NameSym::Unit => write!(w, "▲"),
    NameSym::Hash64 => write!(w, "(Hash64)"),
    NameSym::String(ref s) => write!(w, "{}", s),
    NameSym::Usize(ref n) => write!(w, "{}", n),
    NameSym::Isize(ref n) => write!(w, "{}", n),
    NameSym::Pair(ref l, ref r) => { write_namesym(w, l).unwrap(); write!(w, "-").unwrap(); write_namesym(w, r) },
    NameSym::ForkL(ref s) => { write_namesym(w, s).unwrap(); write!(w, "-l") },
    NameSym::ForkR(ref s) => { write_namesym(w, s).unwrap(); write!(w, "-r") },
  }
}

// Paths are built implicitly via the Adapton::ns command.
#[derive(Hash,PartialEq,Eq,Clone)]
enum Path {
  Empty,
  Child(Rc<Path>,Name),
}
impl reflect::Reflect<reflect::Path> for Path {
  fn reflect(&self) -> reflect::Path {
    match *self {
      Path::Empty => vec![],
      Path::Child(ref path, ref name) => {
        let mut p = path.reflect();
        p.push(name.clone());
        p
      }
    }
  }
}

impl Debug for Path {
  fn fmt(&self, f:&mut Formatter) -> Result {
    match *self {
      Path::Empty => write!(f, ""),
      Path::Child(ref p, ref n) => write!(f, "{:?},{:?}", p, n),
    }
  }
}

// The DCG structure consists of `GraphNode`s:
trait GraphNode : Debug + reflect::Reflect<reflect::Node> {
  fn res_typeid      (self:&Self) -> TypeId ;
  fn preds_alloc<'r> (self:&Self) -> Vec<Rc<Loc>> ;
  fn preds_obs<'r>   (self:&Self) -> Vec<(Rc<Loc>, Option<Rc<Box<DCGDep>>>)> ;
  fn preds_insert<'r>(self:&'r mut Self, Effect, &Rc<Loc>, Option<Rc<Box<DCGDep>>>) -> () ;
  fn preds_remove<'r>(self:&'r mut Self, &Rc<Loc>) -> () ;
  fn succs_def<'r>   (self:&Self) -> bool ;
  fn succs_mut<'r>   (self:&'r mut Self) -> &'r mut Vec<Succ> ;
  fn succs<'r>       (self:&'r Self) -> &'r Vec<Succ> ;
  fn hash_seeded     (self:&Self, u64) -> u64 ;
}

#[derive(Debug,Clone)]
struct Frame {
  loc   : Rc<Loc>,    // The currently-executing node
  succs : Vec<(Succ, Option<Rc<Box<DCGDep>>>)>,  // The currently-executing node's effects (viz., the nodes it demands)
}

impl reflect::Reflect<reflect::Frame> for Frame {
  fn reflect(&self) -> reflect::Frame {
    reflect::Frame{
      loc:self.loc.reflect(),
      succs:self.succs.reflect(),
    }
  }
}

#[derive(Debug,Clone)]
struct Succ {
  dirty  : bool,    // mutated to dirty when loc changes, or any of its successors change
  loc    : Rc<Loc>, // Target of the effect, aka, the successor, by this edge
  effect : Effect,
  dep    : Rc<Box<DCGDep>>, // Abstracted dependency information (e.g., for Observe Effect, the prior observed value)
}

#[derive(Debug,Clone)]
struct Pred {
  loc    : Rc<Loc>, // Source of the effect, aka, the predecessor, by this edge
  effect : Effect,
  /// This `dep` field is None when the predecessor (loc field) is
  /// observing a thunk, and None when the predecessor is _fully_
  /// observing a mutable cell.  When the predecessor partially
  /// observes a mutable cell, this field is Some(dep), where
  /// `dep.dirty(...)` permits the dirtying algorithm of the engine to
  /// check whether the observed value has changed, and whether
  /// dirtying should continue to the predecessors of `loc` .
  dep    : Option<Rc<Box<DCGDep>>>,
}

impl reflect::Reflect<reflect::Succ> for Succ {
  fn reflect(&self) -> reflect::Succ {
    reflect::Succ {
      dirty:self.dirty,
      loc:self.loc.reflect(),
      effect:self.effect.reflect(),
      value:reflect::Val::ValTODO,
      is_dup:false, // XXX -- Actually: Not checked here.
    }
  }
}

impl reflect::Reflect<Vec<reflect::Succ>> for Vec<Succ> {
  fn reflect(&self) -> Vec<reflect::Succ> {
    self.iter().map(|ref x| x.reflect()).collect::<Vec<_>>()
  }
}

impl reflect::Reflect<Vec<reflect::Succ>> for Vec<(Succ, Option<Rc<Box<DCGDep>>>)> {
  fn reflect(&self) -> Vec<reflect::Succ> {
    self.iter().map(|ref x| x.0.reflect()).collect::<Vec<_>>()
  }
}

#[derive(PartialEq,Eq,Debug,Clone,Hash)]
enum Effect {
  Observe,
  Allocate,
}
impl reflect::Reflect<reflect::Effect> for Effect {
  fn reflect(&self) -> reflect::Effect {
    match *self {
      // TODO-Someday: Too many names for the same thing
      // (force/observe/consume, and allocate/alloc/produce).
      Effect::Observe  => reflect::Effect::Force,
      Effect::Allocate => reflect::Effect::Alloc,
    }
  }
}


struct DCGRes {
    // It is always "sound" to set changed=true.  However, when
    // changed=false, dirtying or cleaning can short-circuit further
    // dirtying or reevaluation, respectively.
    changed : bool,
}
// DCGDep abstracts over the value produced by a dependency, as
// well as mechanisms to update and/or re-produce it.
trait DCGDep : Debug {
  fn dirty (self:&Self, g:&mut DCG,      loc:&Rc<Loc>) -> DCGRes ;
  fn clean (self:&Self, g:&RefCell<DCG>, loc:&Rc<Loc>) -> DCGRes ;
}

impl Hash for Succ {
  fn hash<H>(&self, hasher: &mut H) where H: Hasher {
    self.dirty.hash( hasher );
    self.loc.hash( hasher );
    self.effect.hash( hasher );
  }
}

impl Hash for Pred {
  fn hash<H>(&self, hasher: &mut H) where H: Hasher {
    self.loc.hash( hasher );
    self.effect.hash( hasher );
  }
}


// Structureful (Non-opaque) nodes:
#[allow(dead_code)] // Pure case: not introduced currently.
#[derive(Debug,Hash)]
enum Node<Res> {
  Comp(CompNode<Res>),
  Pure(PureNode<Res>),
  Mut(MutNode<Res>),
  Unused,
}
impl<X:Debug> reflect::Reflect<reflect::Node> for Node<X> {
  fn reflect(&self) -> reflect::Node {
    use parse_val::parse_val;
    match *self {
      Node::Comp(ref n) => {
        reflect::Node::Comp(
          reflect::CompNode{
            preds:n.preds.reflect(),
            succs:n.succs.reflect(),
            prog_pt:n.producer.prog_pt().clone(),
            value:match n.res { 
              Some(ref v) => Some( parse_val(v) ),
              None => None
            }
          })
      },
      Node::Pure(ref n) => {
        reflect::Node::Pure(
          reflect::PureNode {
            value:parse_val( &n.val ),
          })
      },
      Node::Mut(ref n) => {
        reflect::Node::Ref(
          reflect::RefNode {
            preds:n.preds.reflect(),
            value:parse_val( &n.val ),
          })        
      },
      Node::Unused => panic!(""),
    }
  }
}

// PureNode<T> for pure hash-consing of T's.
// Location in table never changes value.
#[derive(Debug,Hash)]
struct PureNode<T> {
  val : T,
}

// MutNode<T> for mutable content of type T.
// The set operation mutates a MutNode; set may only be called by *outer* Rust environment.
// Its notable that the CompNodes' producers do not directly change the value of MutNodes with set.
// They may indirectly mutate these nodes by performing nominal allocation; mutation is limited to "one-shot" changes.
#[derive(Debug,Hash)]
struct MutNode<T> {
  preds : Vec<Pred>,
  val   : T,
}

// CompNode<Res> for a suspended computation whose resulting value of
// type T.  The result of the CompNode is affected in two ways: the
// (1) producer may change, which may affect the result and (2) the
// values produced by the successors may change, indirectly
// influencing how the producer produces its resulting value.
struct CompNode<Res> {
  preds    : Vec<Pred>,
  succs    : Vec<Succ>,
  producer : Box<Producer<Res>>, // Producer can be App<Arg,Res>, where type Arg is hidden.
  res      : Option<Res>,
}

impl reflect::Reflect<Vec<reflect::Pred>> for Vec<Pred> {
  fn reflect(&self) -> Vec<reflect::Pred> {
    self.iter().map(|pred| 
                    reflect::Pred{
                        effect:pred.effect.reflect(),
                        loc:pred.loc.reflect()
                    }).collect::<Vec<_>>()
  }
} 

impl reflect::Reflect<Vec<reflect::Loc>> for Vec<Rc<Loc>> {
    fn reflect(&self) -> Vec<reflect::Loc> {
        self.iter().map(|loc| loc.reflect()).collect::<Vec<_>>()
    }
}


/// An `ArtIdChoice` choses between `Eager`, `Structural` and
/// `Nominal` identities for articulation points introduced by
/// `thunk`.
/// 
/// An `Eager` identity is special, and it means "do not introduce any
/// laziness/memoization overhead here"; when Eager is used, no thunk
/// is created; rather, the computation eagerly produces an immutable
/// value held in an `Rc`.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum ArtIdChoice {
  /// Eagerly produces an `Art` that merely consists of an `Rc`; no additional indirection is needed/used.
  Eager,
  /// Identifies an `Art` based on hashing content (e.g., `prog_pt` for code and argument(s)).
  Structural,
  /// Identifies an `Art` based on a programmer-chosen name.
  Nominal(Name),
}

// Produce a value of type Res.
trait Producer<Res> : Debug {
//  fn produce(self:&Self, st:&mut DCG) -> Res;
  fn produce(self:&Self) -> Res;
  fn copy(self:&Self) -> Box<Producer<Res>>;
  fn eq(self:&Self, other:&Producer<Res>) -> bool;
  fn prog_pt<'r>(self:&'r Self) -> &'r ProgPt;
}
// Consume a value of type Arg.
trait Consumer<Arg> : Debug {
  fn consume(self:&mut Self, Arg);
  fn get_arg(self:&mut Self) -> Arg;
}
// struct App is hidden by traits Comp<Res> and CompWithArg<Res>, below.
#[derive(Clone)]
struct App<Arg:Debug,Spurious,Res> {
  prog_pt: ProgPt,
  fn_box:   Rc<Box<Fn(Arg, Spurious) -> Res>>,
  arg:      Arg,
  spurious: Spurious,
}

impl<Arg:Debug,Spurious,Res> 
  Debug for 
  App<Arg,Spurious,Res> 
{
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f,"App({:?} {:?})", self.prog_pt, self.arg)
  }
}

impl<Arg:Hash+Debug,Spurious,Res> 
  Hash for 
  App<Arg,Spurious,Res> 
{
  fn hash<H>(&self, state: &mut H) where H: Hasher { (&self.prog_pt,&self.arg).hash(state) }
}

impl<Arg:'static+PartialEq+Eq+Clone+Debug,Spurious:'static+Clone,Res:'static+Debug+Hash> 
  Producer<Res> for 
  App<Arg,Spurious,Res>
{
  fn produce(self:&Self) -> Res {
    let f = self.fn_box.clone() ;
    let res = f (self.arg.clone(),self.spurious.clone()) ;
    res
  }
  fn copy(self:&Self) -> Box<Producer<Res>> {
    Box::new(App{
      prog_pt:self.prog_pt.clone(),
      fn_box:self.fn_box.clone(),
      arg:self.arg.clone(),
      spurious:self.spurious.clone(),
    })
  }
  fn prog_pt<'r>(self:&'r Self) -> &'r ProgPt {
    & self.prog_pt
  }
  fn eq (&self, other:&Producer<Res>) -> bool {
    if &self.prog_pt == other.prog_pt() {
      let other = Box::new(other) ;
      // This is safe if the prog_pt implies unique Arg and Res types.
      // TODO-Soon: Program points should store argument + result types; we should check these dynamically here
      let other : &Box<App<Arg,Spurious,Res>> = unsafe { transmute::<_,_>( other ) } ;
      self.arg == other.arg
    } else {
      false
    }
  }
}
impl<Arg:Clone+PartialEq+Eq+Debug,Spurious,Res> 
  Consumer<Arg> for 
  App<Arg,Spurious,Res> 
{
  fn consume(self:&mut Self, arg:Arg) { self.arg = arg; }
  fn get_arg(self:&mut Self) -> Arg   { self.arg.clone() }
}

// ----------- Location resolution:

fn lookup_abs<'r>(st:&'r mut DCG, loc:&Rc<Loc>) -> &'r mut Box<GraphNode> {
  match st.table.get_mut( loc ) {
    None => panic!("dangling pointer: {:?}", loc),
    Some(node) => node.be_node() // This is a weird workaround; TODO-Later: Investigate.
  }
}

fn get_top_stack_loc(st:&DCG) -> Option<Rc<Loc>> {
    if st.stack.len() > 0 {
        Some(st.stack.get(st.stack.len() - 1).unwrap().loc.clone())
    } else {
        None
    }
}

fn assert_graphnode_res_type<Res:'static> (loc:&Loc, node:&Box<GraphNode>, top_stack:Option<Rc<Loc>>) {
    let res_typeid = TypeId::of::<Res>();
    let node_res_typeid = node.res_typeid();
    if node_res_typeid != res_typeid {
        let alloc_preds = node.preds_alloc().reflect();
        panic!("\
Adapton engine: Detected a dynamic type error, possibly due to an ambiguous name:
\t              at location: {:?}
\t    existing allocator(s): {:?}
\tcontext/current allocator: {:?}

\t location has result type: {:?}
\tbut context expected type: {:?}",
               loc, alloc_preds, top_stack.reflect(), node_res_typeid, res_typeid
        );
    }
}

// This function uses 'unsafe' to transmute pointer types. Unintended
// double-uses of names and hashes will cause dynamic type errors, via
// assert_graphnode_res_type.
fn res_node_of_loc<'r,Res:'static> (st:&'r mut DCG, loc:&Rc<Loc>) -> &'r mut Box<Node<Res>> {
  let top_loc = get_top_stack_loc(st) ;
  let abs_node = lookup_abs(st, loc) ;
  assert_graphnode_res_type::<Res>(&*loc, abs_node, top_loc);
  unsafe { transmute::<_,_>(abs_node) }
}

// ---------- Node implementation:

impl <Res:'static+Debug+Hash> GraphNode for Node<Res> {

  fn res_typeid(self:&Self) -> TypeId {
      return TypeId::of::<Res>()
  }
    
  fn preds_alloc(self:&Self) -> Vec<Rc<Loc>> {
    match *self { Node::Mut(ref nd) => nd.preds.iter().filter_map(|pred| if pred.effect == Effect::Allocate { Some(pred.loc.clone()) } else { None } ).collect::<Vec<_>>(),
                  Node::Comp(ref nd) => nd.preds.iter().filter_map(|pred| if pred.effect == Effect::Allocate { Some(pred.loc.clone()) } else { None } ).collect::<Vec<_>>(),
                  Node::Pure(_) => unreachable!(),
                  _ => unreachable!(),
    }}

  fn preds_obs(self:&Self) -> Vec<(Rc<Loc>, Option<Rc<Box<DCGDep>>>)> {
      match *self { 
          Node::Mut(ref nd) => 
              nd.preds.iter().filter_map(
                  |pred| 
                  if pred.effect == Effect::Observe { Some((pred.loc.clone(), pred.dep.clone())) } 
                  else { None } 
              ).collect::<Vec<_>>(),
          
          Node::Comp(ref nd) => 
              nd.preds.iter().filter_map(
                  |pred| 
                  if pred.effect == Effect::Observe { Some((pred.loc.clone(), None)) } 
                  else { None } 
              ).collect::<Vec<_>>(),
          
          Node::Pure(_) => unreachable!(),
          _ => unreachable!(),
    }}
  fn preds_insert (self:&mut Self, eff:Effect, loc:&Rc<Loc>, dep:Option<Rc<Box<DCGDep>>>) -> () {
    match *self { Node::Mut(ref mut nd) => nd.preds.push (Pred{effect:eff,loc:loc.clone(),dep:dep.clone()}),
                  Node::Comp(ref mut nd) => nd.preds.push (Pred{effect:eff,loc:loc.clone(),dep:dep.clone()}),
                  Node::Pure(_) => unreachable!(),
                  _ => unreachable!(),
    }}
  fn preds_remove (self:&mut Self, loc:&Rc<Loc>) -> () {
    match *self { Node::Mut(ref mut nd) => nd.preds.retain (|pred|{ &pred.loc != loc}),
                  Node::Comp(ref mut nd) => nd.preds.retain (|pred|{ &pred.loc != loc}),
                  Node::Pure(_) => unreachable!(),
                  _ => unreachable!(),
    }}
  fn succs_def(self:&Self) -> bool {
    match *self { Node::Comp(_) => true, _ => false
    }}
  fn succs_mut<'r>(self:&'r mut Self) -> &'r mut Vec<Succ> {
    match *self { Node::Comp(ref mut n) => &mut n.succs,
                  _ => panic!("undefined"),
    }
  }
  fn succs<'r>(self:&'r Self) -> &'r Vec<Succ> {
    match *self { Node::Comp(ref n) => &n.succs,
                  _ => panic!("undefined"),
    }
  }
  fn hash_seeded(self:&Self, seed:u64) -> u64 {
    let mut hasher = DefaultHasher::new();
    seed.hash(&mut hasher);
    self.hash(&mut hasher);
    hasher.finish()
  }
}

trait ShapeShifter {
  fn be_node<'r> (self:&'r mut Self) -> &'r mut Box<GraphNode> ;
}

impl <Res> ShapeShifter for Box<Node<Res>> {
  fn be_node<'r>(self:&'r mut Self) -> &'r mut Box<GraphNode> {
    // TODO-Later: Why is this transmute needed here ??
    unsafe { transmute::<_,_>(self) }
  }
}

impl ShapeShifter for Box<GraphNode> {
  fn be_node<'r>(self:&'r mut Self) -> &'r mut Box<GraphNode> {
    // TODO-Later: Why is this transmute needed here ??
    unsafe { transmute::<_,_>(self) }
  }
}

impl<Res> fmt::Debug for CompNode<Res> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    //write!(f, "(CompNode)")
    write!(f, "{:?}", self.producer)
  }
}

impl<Res:Hash> Hash for CompNode<Res> {
  fn hash<H:Hasher>(&self, h: &mut H) {
    self.preds.hash(h);
    self.succs.hash(h);
    self.res.hash(h);
    (format!("{:?}",self.producer)).hash(h); // Todo-Later: This defines hash value based on debug string for producer.
  }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
/// CLEANING and DIRTYING (aka "CHANGE PROPAGATION"), including
/// re-evaluation.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Re-evaluation: `loc_produce` performs the computation at `loc`,
/// and produces a result of type `Res`.  Error if `loc` is not a
/// `Node::Comp`.
fn loc_produce<Res:'static+Debug+PartialEq+Eq+Clone+Hash>(g:&RefCell<DCG>, loc:&Rc<Loc>) -> Res
{
  let (producer, prev_path) = {
    let st : &mut DCG = &mut *g.borrow_mut() ;
    let succs : Vec<Succ> = {
      let succs : Vec<Succ> = Vec::new();
      let node : &mut Node<Res> = res_node_of_loc( st, loc ) ;
      replace(node.succs_mut(), succs)
    } ;
    revoke_succs( st, loc, &succs );
    st.stack.push ( Frame{loc:loc.clone(), succs:Vec::new(), } );
    //st.cnt.stack = if st.cnt.stack > st.stack.len() { st.cnt.stack } else { st.stack.len() } ;
    let prev_path = st.path.clone () ;
    st.path = loc.path.clone() ;
    let producer : Box<Producer<Res>> = {
      let node : &mut Node<Res> = res_node_of_loc( st, loc ) ;
      match *node {
        Node::Comp(ref nd) => nd.producer.copy(),
        _ => panic!("internal error"),
      }
    } ;
    //st.cnt.eval += 1 ; 
    drop(st);  // End mutable borrow of global RefCell
    (producer, prev_path)
  };   
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  /// Invoke producer: Run the user's code, and get a result.
  /// Critical note: This call will generally call back into this
  /// engine library.  That's why we end the mutable borrow of `g`
  /// above, before making this call.  We re-borrow `g` below, when
  /// the call is complete.
  let res = producer.produce() ;
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  let st = &mut * g.borrow_mut() ;
  st.path = prev_path ;
  let frame = match st.stack.pop() {
    None => panic!("expected Some _: stack invariants are broken"),
    Some(frame) => frame
  } ;
  assert!( &frame.loc == loc );
  for succ in &frame.succs {
    if succ.0.dirty {
      // This case witnesses an illegal use of nominal side effects
      panic!("invariants broken: newly-built DCG edge should be clean, but is dirty.")
    } ;
    let succ_node = lookup_abs( st, &succ.0.loc );
    succ_node.preds_insert( succ.0.effect.clone(), loc, succ.1.clone() );
  } ;
  {
    let node : &mut Node<Res> = res_node_of_loc( st, loc ) ;
    match *node {
      Node::Comp(ref mut node) => {
        replace(&mut node.succs, frame.succs.into_iter().map(|(succ,_)|succ).collect() ) ;
        replace(&mut node.res, Some(res.clone()))
      },
      _ => panic!("internal error"),
    }
  } ;
  res
}

fn clean_comp<Res:'static+Sized+Debug+PartialEq+Clone+Eq+Hash>
  (g:&RefCell<DCG>,
   this_dep:&ForceDep<Res>,
   loc:&Rc<Loc>, cache:Res, succs:Vec<Succ>) -> DCGRes
{
  for succ in succs.iter() {
    let dirty = {
      let mut st = &mut *g.borrow_mut();
      get_succ_mut(st, loc, succ.effect.clone(), &succ.loc).dirty
    } ;
    if dirty {
      dcg_effect_begin!(reflect::trace::Effect::CleanRec, Some(loc), succ);
      let succ_dep = & succ.dep ;
      let res = succ_dep.clean(g, &succ.loc) ;
      if res.changed {        
        dcg_effect_begin!(reflect::trace::Effect::CleanEval, Some(loc), succ);
        let result : Res = loc_produce( g, loc ) ;
        dcg_effect_end!();
        let changed = result != this_dep.res ;
        dcg_effect_end!();
        return DCGRes{changed:changed}
      }
      else {
        let mut st : &mut DCG = &mut *g.borrow_mut();
        //st.cnt.clean += 1 ;
        get_succ_mut(st, loc, succ.effect.clone(), &succ.loc).dirty = false ;
        dcg_effect!(reflect::trace::Effect::CleanEdge, Some(loc), succ);
      }
      dcg_effect_end!();
    }
  } ;
  let changed = this_dep.res != cache ;
  DCGRes{changed:changed}
}

#[derive(Debug)]
struct AllocStructuralThunk;
impl DCGDep for AllocStructuralThunk {
  fn dirty (self:&Self, _g:&mut DCG,      _loc:&Rc<Loc>) -> DCGRes { DCGRes{changed:true} }
  fn clean (self:&Self, _g:&RefCell<DCG>, _loc:&Rc<Loc>) -> DCGRes { DCGRes{changed:false} }
}

#[derive(Debug)]
struct AllocNominalThunk<T> { val:T }
impl<T:Debug> DCGDep for AllocNominalThunk<T> {
  fn dirty (self:&Self, _g:&mut DCG,      _loc:&Rc<Loc>) -> DCGRes { DCGRes{changed:true} }
  fn clean (self:&Self, _g:&RefCell<DCG>, _loc:&Rc<Loc>) -> DCGRes { DCGRes{changed:true} } // TODO-Later: Make this a little better.
}

#[derive(Debug)]
struct AllocCell<T> { val:T }
impl<T:Debug> DCGDep for AllocCell<T> {
  fn dirty (self:&Self, _g:&mut DCG,      _loc:&Rc<Loc>) -> DCGRes { DCGRes{changed:true} }
  fn clean (self:&Self, _g:&RefCell<DCG>, _loc:&Rc<Loc>) -> DCGRes { DCGRes{changed:true} } // TODO-Later: Make this a little better.
}

/// The structure implements DCGDep, caching a value of type `T` to
/// compare against future values.
#[derive(Debug)]
struct ForceDep<T:Debug> { res:T }

/// The structure implements DCGDep, caching a value of type `T` to
/// compare against future values.
struct ForceMapDep<T,S,F:Fn(&Art<T>, T)->S> { raw:PhantomData<T>, mapf:F, res:S }

fn check_force_map_dep 
    <T:'static+Sized+Debug+PartialEq+Eq+Clone+Hash,
     S:'static+Sized+Debug+PartialEq+Eq+Clone+Hash, 
     F:Fn(&Art<T>, T)->S>
    (st:&mut DCG, dep:&ForceMapDep<T,S,F>, loc:&Rc<Loc>) -> DCGRes 
{
    let node : &mut Node<T> = res_node_of_loc(st, loc) ;
    match *node {
        Node::Mut(ref nd) => 
            DCGRes{changed:dep.res != (dep.mapf)
                   (&Art{art:EnumArt::Loc(loc.clone())},
                    nd.val.clone())},
        
        Node::Comp(_) | Node::Pure(_) | Node::Unused => 
            unreachable!()
    }
}

impl <T:'static+Sized+Debug+PartialEq+Eq+Clone+Hash,
      S:'static+Sized+Debug+PartialEq+Eq+Clone+Hash, F:Fn(&Art<T>, T)->S>
    DCGDep for ForceMapDep<T,S,F>
{
    fn dirty(self:&Self, g:&mut DCG, loc:&Rc<Loc>) -> DCGRes {
        check_force_map_dep(g, self, loc)       
    }    
    fn clean(self:&Self, g:&RefCell<DCG>, loc:&Rc<Loc>) -> DCGRes {
        check_force_map_dep(&mut *g.borrow_mut(), self, loc)
    }
}

impl <T:'static+Sized+Debug+PartialEq+Eq+Clone+Hash,
      S:'static+Sized+Debug+PartialEq+Eq+Clone+Hash, F:Fn(&Art<T>, T)->S>
    Debug for ForceMapDep<T,S,F> 
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      // TODO-Later: We dont print the mapping function here
      write!(f, "ForceMapDep({:?})", self.res)
  }    
}



impl <Res:'static+Sized+Debug+PartialEq+Eq+Clone+Hash>
  DCGDep for ForceDep<Res>
{
  fn dirty(self:&Self, _g:&mut DCG, _loc:&Rc<Loc>) -> DCGRes {
      DCGRes{changed:true}
  }

  fn clean(self:&Self, g:&RefCell<DCG>, loc:&Rc<Loc>) -> DCGRes {
    let res_succs = { // Handle cases where there is no internal computation to re-compute:
      let st = &mut *g.borrow_mut();
      let node : &mut Node<Res> = res_node_of_loc(st, loc) ;
      match *node {
        Node::Comp(ref nd) => {
          match nd.res {
            Some(ref res) => Some((res.clone(), nd.succs.clone ())),
            None => None
          }},
        Node::Pure(_) => {
          return DCGRes{changed:false}
        },
        Node::Mut(ref nd) => {
          return DCGRes{changed:nd.val != self.res}
        },
        _ => panic!("undefined")
      }
    } ;
    let none : Option<Loc> = None ;
    match res_succs {
      Some((res,succs)) => clean_comp(g, self, loc, res, succs),
      None => {
        dcg_effect_begin!(
          reflect::trace::Effect::CleanEval,
          none,
          reflect::Succ{
            loc:loc.reflect(),
            dirty:true,
            effect:reflect::Effect::Force,
            value:reflect::Val::ValTODO,
            is_dup:false, // XXX -- Actually: Not checked here.
          }
        );
        let res = loc_produce( g, loc );
        let changed = self.res != res ;
        // TODO: changed to reflect::trace somehow?
        dcg_effect_end!();
        DCGRes{changed:changed}
      }
    }
  }
}

// ---------- Node implementation:

fn revoke_succs<'x> (st:&mut DCG, src:&Rc<Loc>, succs:&Vec<Succ>) {
  //let mut succ_idx = 0;
  for succ in succs.iter() {
    dcg_effect!(reflect::trace::Effect::Remove, Some(src), succ);
    let succ_node : &mut Box<GraphNode> = lookup_abs(st, &succ.loc) ;
    succ_node.preds_remove(src)
  }
}

fn loc_of_id(path:Rc<Path>,id:Rc<ArtId>) -> Rc<Loc> {
  let hash = my_hash(&(&path,&id));
  Rc::new(Loc{path:path,id:id,hash:hash})
}

fn get_succ<'r>(st:&'r DCG, src_loc:&Rc<Loc>, eff:Effect, tgt_loc:&Rc<Loc>) -> &'r Succ {
  let nd = st.table.get(src_loc);
  let nd = match nd {
    None => panic!(""),
    Some(nd) => nd
  } ;    
  for succ in nd.succs() {
    if (succ.effect == eff) && (&succ.loc == tgt_loc) {
      return succ
    } else {}
  } ;
  panic!("tgt_loc is dangling in src_node.dem_succs")
}

// Implement "sharing" of the dirty bit.
// The succ edge is returned as a mutable borrow, to permit checking
// and mutating the dirty bit.
fn get_succ_mut<'r>(st:&'r mut DCG, src_loc:&Rc<Loc>, eff:Effect, tgt_loc:&Rc<Loc>) -> &'r mut Succ {
  let nd = lookup_abs( st, src_loc );
  for succ in nd.succs_mut().iter_mut() {
    if (succ.effect == eff) && (&succ.loc == tgt_loc) {
      return succ
    } else {}
  } ;
  panic!("tgt_loc is dangling in src_node.dem_succs")
}

fn dirty_pred_observers(st:&mut DCG, loc:&Rc<Loc>) {
    let pred_locs : Vec<(Rc<Loc>, Option<Rc<Box<DCGDep>>>)> = lookup_abs( st, loc ).preds_obs() ;
    for (pred_loc, dep) in pred_locs {
        let stop : bool = match dep {
            None => false,
            Some(dep) => dep.dirty(st, loc).changed == false
        };
        let stop : bool = if stop { true } else {
            // The stop bit communicates information from st for use below.
            let succ = get_succ_mut(st, &pred_loc, Effect::Observe, &loc) ;
            if succ.dirty { true } else {
                assert!(&pred_loc != loc);
                dcg_effect_begin!(reflect::trace::Effect::Dirty, Some(&pred_loc), succ);
                replace(&mut succ.dirty, true);
                false
            }} 
        ;
        if !stop {
            dirty_pred_observers(st,&pred_loc);
            dcg_effect_end!();
        } else { }
    }
}

fn dirty_alloc(st:&mut DCG, loc:&Rc<Loc>) {
  dirty_pred_observers(st, loc);
  let pred_locs : Vec<Rc<Loc>> = lookup_abs(st, loc).preds_alloc() ;
  for pred_loc in pred_locs {
    let stop : bool = {
      // The stop bit communicates information from st for use below.
      let succ = get_succ_mut(st, &pred_loc, Effect::Allocate, &loc) ;
      if succ.dirty { true } else {
        replace(&mut succ.dirty, true);
        assert!(&pred_loc != loc);
        dcg_effect_begin!(reflect::trace::Effect::Dirty, Some(&pred_loc), succ);
        false
      }} ;
    if !stop {
      dirty_pred_observers(st,&pred_loc);
      dcg_effect_end!();
    } else {  }
  }
  if false /* XXX Check make this better, as a statically/dynamically-set flag? */ {
    wf::check_stack_is_clean(st)
  }
}

/// Returns true if changed, false if unchanged.
fn check_cell_change<T:'static+Eq+Debug> (st:&mut DCG, cell:AbsArt<T,Loc>, val:&T) -> bool {
    if let AbsArt::Loc(ref loc) = cell { 
        let node = res_node_of_loc::<T>( st, loc ) ;
        match **node {
            Node::Mut(ref mut nd) => { &nd.val != val }
            _ => { /* the location was previously _not_ a cell, so yes */ true }
        }
    }
    else { panic!("{:?} is not a cell", cell) }
}

/// Returns true if changed, false if unchanged.
fn set_<T:'static+Eq+Debug> (st:&mut DCG, cell:AbsArt<T,Loc>, val:T) {
  if let AbsArt::Loc(ref loc) = cell { 
    let changed : bool = {
      let node = res_node_of_loc( st, loc ) ;
      match **node {
        Node::Mut(ref mut nd) => {
          if nd.val == val {
            false
          } else {
            replace(&mut nd.val, val) ;
            true
          }},
        _ => unreachable!(),
      }} ;
    if changed {
      /// TODO: Dirtying isn't quite necessary for *all* allocations.
      /// Only those that allocated a different value than the present
      /// one--- we should check this, but we do not (we are *too*
      /// conservative at present).
      dirty_alloc(st, loc);
    }
  }
  else { panic!("{:?} is not a cell", cell) }
}


fn current_path (st:&DCG) -> Rc<Path> {
  st.path.clone()
}

/// The term "Art" stands for two things here: "Adapton return type",
/// and "Articulation point, for 'articulating' incremental change".
/// The concept of an "Art" also abstracts over whether the producer
/// is eager (like a ref cell) or lazy (like a thunk).
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
enum AbsArt<T,Loc> {
  Rc(Rc<T>),    // No entry in table. No dependency tracking.
  Loc(Rc<Loc>), // Location in table.
}

/// The `Adapton` trait provides a language of
/// dependence-graph-building operations based on the core calculus
/// described in ["Incremental Computation with Names", 2015](http://arxiv.org/abs/1503.07792)
trait Adapton : Debug+PartialEq+Eq+Hash+Clone {
  type Loc  : Debug+PartialEq+Eq+Hash+Clone;
  
  fn new () -> Self ;
    
  /// Creates or re-enters a given namespace; performs the given computation there.
  fn ns<T,F> (g: &RefCell<DCG>, Name, body:F) -> T where F:FnOnce() -> T;
  
  /// Enters a special "namespace" where all name uses are ignored; instead, Adapton uses structural identity.
  fn structural<T,F> (g: &RefCell<DCG>, body:F) -> T where F:FnOnce() -> T;
  
  /// Creates immutable, eager articulation.
  fn put<T:Eq+Debug+Clone> (self:&mut Self, T) -> AbsArt<T,Self::Loc> ;
  
  /// Creates a mutable articulation.
  fn cell<T:Eq+Debug+Clone+Hash+'static> (self:&mut Self, Name, T) -> AbsArt<T,Self::Loc> ;
  
  /// Mutates a mutable articulation.
  fn set<T:'static+Eq+Debug+Clone> (self:&mut Self, AbsArt<T,Self::Loc>, T) ;
  
  /// Creates an articulated computation.
  fn thunk <Arg:Eq+Hash+Debug+Clone+'static,
            Spurious:Clone+'static,
            Res:Eq+Debug+Clone+Hash+'static
            >
    (self:&mut Self,
     id:ArtIdChoice,
     prog_pt:ProgPt,
     fn_box:Rc<Box< Fn(Arg, Spurious) -> Res >>,
     arg:Arg, spurious:Spurious)
     -> AbsArt<Res,Self::Loc> ;
  
  /// Demand & observe arts (all kinds): force
  fn force<T:Eq+Debug+Clone+Hash+'static> (g:&RefCell<DCG>, &AbsArt<T,Self::Loc>) -> T ;

  /// Demand & observe arts (all kinds): force
  fn force_map<T:Eq+Debug+Clone+Hash+'static,
               S:Eq+Debug+Clone+Hash+'static, 
               F:'static>
        (g:&RefCell<DCG>, &AbsArt<T,Self::Loc>, F) -> S        
        where F:Fn(&Art<T>, T) -> S
        ;
}

impl Adapton for DCG {
  type Loc  = Loc;

  fn new () -> DCG {
    let path = Rc::new(Path::Empty);
    let stack = Vec::new() ;
    let table = HashMap::new ();
    DCG {
      flags : Flags {
        use_purity_optimization       : { match env::var("ADAPTON_NO_PURITY")  { Ok(_) => false, _ => true } },
        ignore_nominal_use_structural : { match env::var("ADAPTON_STRUCTURAL") { Ok(_) => true,  _ => false } },
        check_dcg_is_wf               : { match env::var("ADAPTON_CHECK_DCG")  { Ok(_) => true,  _ => false } },
        write_dcg                     : { match env::var("ADAPTON_WRITE_DCG")  { Ok(_) => true,  _ => false } },
        gmlog_dcg                     : { match env::var("ADAPTON_GMLOG_DCG")  { Ok(_) => true,  _ => false } },
      },
      table : table,
      stack : stack,
      path  : path,
      dcg_count : 0,
      dcg_hash : 0, // XXX This makes assumptions about hashing implementation
    }
  }
                     
  fn structural<T,F> (g: &RefCell<DCG>, body:F) -> T
    where F:FnOnce() -> T
  {
    let saved = {
      let st = &mut *g.borrow_mut();
      let saved = st.flags.ignore_nominal_use_structural ;
      st.flags.ignore_nominal_use_structural = true ;
      saved
    } ;
    let x = body() ;
    g.borrow_mut().flags.ignore_nominal_use_structural = saved;
    x
  }
  
  fn ns<T,F> (g: &RefCell<DCG>, nm:Name, body:F) -> T
    where F:FnOnce() -> T
  {
    let saved = {
      let st = &mut *g.borrow_mut();
      let saved = st.path.clone();
      st.path = Rc::new(Path::Child(st.path.clone(), nm)) ; // Todo-Minor: Avoid this clone.
      saved
    };
    let x = body() ;
    g.borrow_mut().path = saved ;
    x
  }

  fn put<T:Eq> (self:&mut DCG, x:T) -> AbsArt<T,Self::Loc> { AbsArt::Rc(Rc::new(x)) }

  fn cell<T:Eq+Debug+Clone+Hash
    +'static // TODO-Later: Needed on T because of lifetime issues.
    >
    (self:&mut DCG, nm:Name, val:T) -> AbsArt<T,Self::Loc> {
      wf::check_dcg(self);
      let path = current_path(self) ;
      let (id, is_pure) = {
        if ! self.flags.ignore_nominal_use_structural {
          (Rc::new(ArtId::Nominal(nm)), false) // Ordinary case: Use provided name.
        } else {
          let hash = my_hash (&val) ;           
          (Rc::new(ArtId::Structural(hash)), self.flags.use_purity_optimization) // Ignore the name; do hash-consing instead.
        }
      };            
      let hash = my_hash(&(&path,&id));
      let loc  = Rc::new(Loc{path:path,id:id,hash:hash})
      ;
      let (do_dirty, do_set, succs, do_insert, is_fresh) =
        if self.table.contains_key(&loc) {
          let node : &Box<Node<T>> = res_node_of_loc(self, &loc) ;
          match **node {
            Node::Mut(_)       => { (false, true,  None, false, false) }
            Node::Comp(ref nd) => { (true,  false, Some(nd.succs.clone()),  false, false ) }
            Node::Pure(_)      => { (false, false, None, false, false) }
            Node::Unused       => unreachable!()
          }} else                 { (false, false, None, true, true ) } 
      ;
      // - - - - - - - - - -
      /// Begin an allocation.  Because this allocation may require
      /// dirtying some allocation edges. (See value of bit
      /// `do_dirty`, which is true when we are overwriting what was
      /// once a computation with a value). This allocation may also
      /// require dirtying some observers, when the new value is
      /// different from the one that they last observed. (Similarly,
      /// allocations that allocated a different value need to be
      /// dirtied too).  Hence, this effect may contain other effects
      /// to the DCG, namely, those dirtying steps.
      // - - - - - - - 
      dcg_effect_begin!(
        reflect::trace::Effect::Alloc(
            if is_fresh { reflect::trace::AllocCase::LocFresh } 
            else { 
                let changed = 
                    if check_cell_change(self, AbsArt::Loc(loc.clone()), &val) { 
                        reflect::trace::ChangeFlag::ContentDiff 
                    } else { 
                        reflect::trace::ChangeFlag::ContentSame 
                    };
                reflect::trace::AllocCase::LocExists(changed)
            },
            reflect::trace::AllocKind::RefCell,
        ),
        current_loc!(self),
        reflect::Succ{
          loc:loc.reflect(), 
          effect:reflect::Effect::Alloc, 
          value:reflect::Val::ValTODO, 
          dirty:false,
          is_dup:false, // XXX -- Actually: Not checked here.
        }
      );      
      if do_set   { set_(self, AbsArt::Loc(loc.clone()), val.clone()) };
      if do_dirty { dirty_alloc(self, &loc) } ;
      match succs { Some(succs) => revoke_succs(self, &loc, &succs), None => () } ;
      dcg_effect_end!();
      
      if do_insert {
        let node = if is_pure { Node::Pure(PureNode{val:val.clone()}) } else {
          Node::Mut(MutNode{
            preds:Vec::new(),
            val:val.clone(),
          })} ;
        self.table.insert(loc.clone(), Box::new(node));
      } ;
      if ! is_pure { match self.stack.last_mut() { 
        None => (),        
        Some(frame) => {
          let succ =
            Succ{loc:loc.clone(),
                 dep:Rc::new(Box::new(AllocCell{val:val})),
                 effect:Effect::Allocate,
                 dirty:false};
          frame.succs.push((succ, None))
        }}} ;
      wf::check_dcg(self);
      AbsArt::Loc(loc)
    }

  fn set<T:'static+Eq+Debug> (self:&mut Self, cell:AbsArt<T,Self::Loc>, val:T) {
    wf::check_dcg(self);
    assert!( self.stack.is_empty() ); // => outer layer has control.
    set_(self, cell, val);
    wf::check_dcg(self);
  }

  fn thunk<Arg:Eq+Hash+Debug+Clone+'static,Spurious:'static+Clone,Res:Eq+Debug+Clone+Hash+'static>
    (self:&mut DCG,
     id:ArtIdChoice,
     prog_pt:ProgPt,
     fn_box:Rc<Box<Fn(Arg, Spurious) -> Res>>,
     arg:Arg, spurious:Spurious)
     -> AbsArt<Res,Self::Loc>
  {
    wf::check_dcg(self);
    let id =
      // Apply the logic of engine's flags:
      match id { ArtIdChoice::Nominal(_)
                 if self.flags.ignore_nominal_use_structural
                 => ArtIdChoice::Structural,
                 id => id } ;
    match id {
      ArtIdChoice::Eager => {
        AbsArt::Rc(Rc::new(fn_box(arg,spurious)))
      },

      ArtIdChoice::Structural => {
        wf::check_dcg(self);
        let hash = my_hash (&(&prog_pt, &arg)) ;
        let loc = loc_of_id(current_path(self),
                            Rc::new(ArtId::Structural(hash)));
        {   // If the node exists, return early.
          let node = self.table.get_mut(&loc);
          match node { None    => { },
                       Some(_) => { return AbsArt::Loc(loc) }, // Nothing to do; it already exists.
          }
        } ;
        // assert: node does not exist.
        match self.stack.last_mut() {
          None => (),
          Some(frame) => {
            let succ =
              Succ{loc:loc.clone(),
                   dep:Rc::new(Box::new(AllocStructuralThunk)),
                   effect:Effect::Allocate,
                   dirty:false};
            frame.succs.push((succ, None))
          }};
        let producer : Box<Producer<Res>> =
          Box::new(App{prog_pt:prog_pt,
                       fn_box:fn_box,
                       arg:arg.clone(),
                       spurious:spurious.clone()})
          ;
        let node : CompNode<Res> = CompNode{
          preds:Vec::new(),
          succs:Vec::new(),
          producer:producer,
          res:None,
        } ;
        //self.cnt.create += 1;
        self.table.insert(loc.clone(),
                          Box::new(Node::Comp(node)));
        wf::check_dcg(self);
        AbsArt::Loc(loc)
      },

      ArtIdChoice::Nominal(nm) => {
        wf::check_dcg(self);
        let loc = loc_of_id(current_path(self),
                            Rc::new(ArtId::Nominal(nm)));
        let producer : App<Arg,Spurious,Res> =
          App{prog_pt:prog_pt.clone(),
              fn_box:fn_box,
              arg:arg.clone(),
              spurious:spurious.clone(),
          }
        ;
        let top_loc = get_top_stack_loc( self );
        let (do_dirty, do_insert, is_fresh) = { match self.table.get_mut( &loc ) {
          None => {
            // do_dirty=false; do_insert=true
            (false, true, true)
          },
          Some(node) => {
            let node: &mut Box<GraphNode> = node ;
            assert_graphnode_res_type::<Res>(&loc, node, top_loc);
            let res_nd: &mut Box<Node<Res>> = unsafe { transmute::<_,_>( node ) } ;
            match ** res_nd {
              Node::Pure(_)=> unreachable!(),
              Node::Mut(_) => {
                (true, true, false) // Todo: Do we need to preserve preds?
              },
              Node::Comp(ref mut comp_nd) => {
                let equal_producer_prog_pts : bool =
                  comp_nd.producer.prog_pt().eq( producer.prog_pt() ) ;
                if equal_producer_prog_pts { // => safe cast to Box<Consumer<Arg>>
                  let app: &mut Box<App<Arg,Spurious,Res>> =
                    // TODO-Soon: Follow pattern above for assert_graphnode_res_type to dynamically check the safety of this cast
                    unsafe { transmute::<_,_>( &mut comp_nd.producer ) }
                  ;
                  if app.get_arg() == arg {
                    // Case: Same argument; Nothing else to do:
                    // do_dirty=false; do_insert=false
                    (false, false, false)
                  }
                  else { // Case: Not the same argument:
                    app.consume(arg.clone()); // overwrite the old argument
                    comp_nd.res = None ; // clear the cache
                    // do_dirty=true; do_insert=false
                    (true, false, false)
                  }}
                else {
                  panic!("Memozied functions not equal!
                            Function was: {:?}
                           with Producer: {:?}

                            Function now: {:?}
                           with Producer: {:?}

                        Common location: {:?}

                        ** Hint: Consider using distinct namespaces, via `Adapton::ns`
                           (See: https://docs.rs/adapton/0.3/adapton/engine/fn.ns.html)
                        ",
                         comp_nd.producer.prog_pt(), &comp_nd.producer,
                         producer.prog_pt(), &producer,
                         &loc,
                  )
                }
              },
              _ => unreachable!(),
            }
          }
        } } ;

        dcg_effect_begin!(
          reflect::trace::Effect::Alloc(
            if is_fresh { reflect::trace::AllocCase::LocFresh }
            else { 
                let cf = 
                    if do_dirty { reflect::trace::ChangeFlag::ContentDiff } 
                    else { reflect::trace::ChangeFlag::ContentSame };
                reflect::trace::AllocCase::LocExists(cf) },
              reflect::trace::AllocKind::Thunk
          ),
          current_loc!(self),
          reflect::Succ{
            loc:loc.reflect(),
            effect:reflect::Effect::Alloc,
            value:reflect::Val::ValTODO,
            dirty:false,
            is_dup:false, // XXX -- Actually: Not checked here.
          });
        if do_dirty {dirty_alloc(self, &loc) };
        dcg_effect_end!();

        match self.stack.last_mut() { None => (), Some(frame) => {
          let succ =
            Succ{loc:loc.clone(),
                 dep:Rc::new(Box::new(AllocNominalThunk{val:arg.clone()})),
                 effect:Effect::Allocate,
                 dirty:false};
          frame.succs.push((succ, None))
        }};
        if do_insert {
          let node : CompNode<Res> = CompNode{
            preds:Vec::new(),
            succs:Vec::new(),
            producer:Box::new(producer),
            res:None,
          } ;
          self.table.insert(loc.clone(), Box::new(Node::Comp(node)));
          wf::check_dcg(self);
          AbsArt::Loc(loc)
        }
        else {
          wf::check_dcg(self);
          AbsArt::Loc(loc)
        }
      }
    }
  }

  fn force_map<T:'static+Eq+Debug+Clone+Hash, 
               S:'static+Eq+Debug+Clone+Hash, 
               F:'static> 
        (g:&RefCell<DCG>,
         art:&AbsArt<T,Self::Loc>, mapf:F) -> S
        where F:Fn(&Art<T>, T) -> S
    {      
      match *art {
          AbsArt::Rc(ref v) => mapf(&Art{art:EnumArt::Rc(v.clone())}, 
                                    (**v).clone()),
          AbsArt::Loc(ref loc) => {
              let cell_val : Option<T> = {
                  let st : &mut DCG = &mut *g.borrow_mut();
                  let node : &mut Node<T> = res_node_of_loc(st, &loc) ;
                  match *node {
                      Node::Unused => unreachable!(),
                      Node::Comp(_) => { None }
                      Node::Pure(_) => { None }
                      Node::Mut(ref nd)  => { Some(nd.val.clone()) }
                  }
              } ;              
              match cell_val {
                  None => {
                      mapf(&Art{art:EnumArt::Loc(loc.clone())},
                           <DCG as Adapton>::force(g, art))
                  },
                  Some(val) => { 
                      // Case: We _are_ forcing a cell; so, we record
                      // the mapped value, and the mapping function,
                      // in the DCG.
                      dcg_effect!(
                          // TODO-Now: Reflect the fact that we are doing a mapping here
                          reflect::trace::Effect::Force(reflect::trace::ForceCase::RefGet),
                          current_loc!(*g.borrow()),
                          reflect::Succ{
                              loc:loc.reflect(),
                              value:reflect::Val::ValTODO,
                              effect:reflect::Effect::Force,
                              dirty:false,
                              is_dup:false,
                          });
                      let st : &mut DCG = &mut *g.borrow_mut() ;
                      let res = mapf(&Art{art:EnumArt::Loc(loc.clone())}, val.clone());
                      match st.stack.last_mut() { None => (), Some(frame) => {
                          let dep : Rc<Box<DCGDep>> = Rc::new(Box::new(ForceMapDep{
                              raw:PhantomData,
                              mapf:mapf,
                              res:res.clone()}));
                          let succ =                              
                              // TODO: Record the mapping function
                              // here, for use when we decide whether
                              // or not to dirty.
                              Succ{loc:loc.clone(),
                                   dep:dep.clone(),
                                   effect:Effect::Observe,
                                   dirty:false};
                          frame.succs.push((succ, Some(dep.clone())));
                      }};
                      res
                  }
              }              
          }
      }
  }

  fn force<T:'static+Eq+Debug+Clone+Hash> (g:&RefCell<DCG>,
                                           art:&AbsArt<T,Self::Loc>) -> T
  {
    {
      let st : &mut DCG = &mut *g.borrow_mut();
      wf::check_dcg(st);
      drop(st)
    }
    match *art {
      AbsArt::Rc(ref v) => (**v).clone(),
      AbsArt::Loc(ref loc) => {
        let (is_comp, is_dup, is_pure, cached_result) : (bool, bool, bool, Option<T>) = {
          let st : &mut DCG = &mut *g.borrow_mut();
          let is_pure_opt : bool = st.flags.use_purity_optimization ;
          let is_dup : bool = match st.stack.last_mut() { None => false, Some(frame) => {
              let mut is_dup = false; // XXX -- Actually: unknown and does not matter.
              for &(ref succ, ref _pred_dep) in frame.succs.iter() { 
                  if &succ.loc == loc && succ.effect == Effect::Observe 
                  { is_dup = true }
              };
              is_dup
          }};
          let node : &mut Node<T> = res_node_of_loc(st, &loc) ;
          match *node {
            Node::Pure(ref mut nd) => (false, is_dup, true, Some(nd.val.clone())),
            Node::Mut(ref mut nd)  => (false, is_dup, false, Some(nd.val.clone())),
            Node::Comp(ref mut nd) => {
              let is_pure = match *loc.id {
                ArtId::Structural(_) => nd.succs.len() == 0 && is_pure_opt,
                ArtId::Nominal(_)    => false } ;
              (true, is_dup, is_pure, nd.res.clone()) },
            _ => panic!("undefined")
          }
        } ;
        let result = match cached_result {
          None => {
            assert!(is_comp);
            dcg_effect_begin!(
              reflect::trace::Effect::Force(reflect::trace::ForceCase::CompCacheMiss),
              current_loc!(*g.borrow()),
              reflect::Succ{
                loc:loc.reflect(),
                value:reflect::Val::ValTODO,
                effect:reflect::Effect::Force,
                dirty:false,
                is_dup:is_dup,
              }
            );
            assert_eq!(is_dup, false);
            let res = loc_produce(g, &loc);
            dcg_effect_end!();
            res
          },
          Some(ref res) => {
            if is_comp {
              dcg_effect_begin!(
                reflect::trace::Effect::Force(reflect::trace::ForceCase::CompCacheHit),
                current_loc!(*g.borrow()),
                reflect::Succ{
                  loc:loc.reflect(),
                  value:reflect::Val::ValTODO,
                  effect:reflect::Effect::Force,
                  dirty:false,
                  is_dup:is_dup,
                }
              );
              let _ = ForceDep{res:res.clone()}.clean(g, &loc) ;
              dcg_effect_end!();
              let st : &mut DCG = &mut *g.borrow_mut();
              let node : &mut Node<T> = res_node_of_loc(st, &loc) ;
              match *node {
                Node::Comp(ref nd) => match nd.res {
                  None => unreachable!(),
                  Some(ref res) =>
                    // Testing: Reached by `pure_caching` tests
                    res.clone()
                },
                _ => unreachable!(),
              }}
            else {
              dcg_effect!(
                reflect::trace::Effect::Force(reflect::trace::ForceCase::RefGet),
                current_loc!(*g.borrow()),
                reflect::Succ{
                  loc:loc.reflect(),
                  value:reflect::Val::ValTODO,
                  effect:reflect::Effect::Force,
                  dirty:false,
                  is_dup:is_dup,
                });
              res.clone()
            }
          }
        } ;
        let st : &mut DCG = &mut *g.borrow_mut() ;
        if !is_dup && !is_pure { match st.stack.last_mut() { None => (), Some(frame) => {
          let succ =
            Succ{loc:loc.clone(),
                 dep:Rc::new(Box::new(ForceDep{res:result.clone()})),
                 effect:Effect::Observe,
                 dirty:false};
          frame.succs.push((succ, None));
        }}} ;
        wf::check_dcg(st);
        result
      }
    }}
}

/// *Articulations:* for incrementally-changing data/computation.
///
///  - Introduced by (produced by) `thunk`, `cell` and `put` 
///
///  - Eliminated by (consumed by) `force` (and `set`).
///
/// The term *Art* stands for two things here: _Adapton Return Type_
/// (of `thunk` and `cell`), and _Articulation point for
/// incrementally-changing data/computation_.  
///
/// Each art has a unique identity. (See also: `Name`s, and functions
/// to produce them).  Because this identity, an art can be hashed and
/// compared for equality efficiently, in O(1) time.
///
/// The concept of an art abstracts over whether the producer is eager
/// (like a ref `cell`) or lazy (like a `thunk`).  One uses `force` to
/// inspect both. Consequently, code that consumes structures with
/// arts need only ever use `force` (not two different functions,
/// depending on whether the art is lazy or eager).
#[derive(Clone,PartialEq,Eq,Hash,Debug)]
pub struct Art<T> {
  art:EnumArt<T>,
}

#[derive(Clone)]
enum EnumArt<T> {
  /// No entry in table. No dependency tracking.
  Rc(Rc<T>),    
  /// Location in table.
  Loc(Rc<Loc>), 
  /// A closure that is 'force-able'
  Force(Rc<Force<T>>),
}

impl<T:Hash> Hash for EnumArt<T> {
  fn hash<H>(&self, hasher:&mut H) where H:Hasher {
    match *self {
      EnumArt::Rc(ref rc)   => rc.hash( hasher ),
      EnumArt::Loc(ref loc) => loc.hash( hasher ),
      EnumArt::Force(ref f) => f.hash_u64().hash( hasher ),
    }
  }
}

impl<T:PartialEq> PartialEq for EnumArt<T> {
  fn eq(&self, other:&Self) -> bool {
    match *self {
      EnumArt::Rc(ref rc) =>
        if let EnumArt::Rc(ref rc2) = *other { rc == rc2 } else { false },
      EnumArt::Loc(ref loc) =>
        if let EnumArt::Loc(ref loc2) = *other { loc == loc2 } else { false },
      EnumArt::Force(ref f) =>
        if let EnumArt::Force(ref f2) = *other { f.eq(&**f2) } else { false },
    }
  }
}

impl<T:Debug> Debug for EnumArt<T> {
  fn fmt(&self, f:&mut Formatter) -> Result {
    match *self {
      EnumArt::Rc(ref rc)     => rc.fmt(f),
      EnumArt::Loc(ref loc)   => loc.fmt(f),
      EnumArt::Force(ref frc) => frc.fmt(f),
    }
  }
}

impl<T:Eq> Eq for EnumArt<T> { }

trait Force<T> {
  fn force(&self) -> T;
  fn copy(self:&Self) -> Box<Force<T>>;
  fn eq(self:&Self, other:&Force<T>) -> bool;
  fn id<'r>(self:&'r Self) -> &'r ArtIdChoice;
  fn prog_pt<'r>(self:&'r Self) -> &'r ProgPt;
  fn hash_u64(self:&Self) -> u64;
  fn fmt(&self, f:&mut Formatter) -> fmt::Result;
}

#[derive(Clone)]
struct NaiveThunk<Arg, Spurious, Res> {
  id:ArtIdChoice,
  prog_pt:ProgPt,
  fn_box:Rc<Box< Fn(Arg, Spurious) -> Res >>,
  arg:Arg,
  spurious:Spurious
}

impl<A:Hash+Clone+Eq+Debug+'static,S:Clone+'static,T:'static>
  Force<T>
  for NaiveThunk<A,S,T>
{
  fn force(&self) -> T {
    (*self.fn_box)(self.arg.clone(), self.spurious.clone())
  }
  fn copy(self:&Self) -> Box<Force<T>> {
    Box::new(NaiveThunk{id:self.id.clone(),
                        prog_pt:self.prog_pt.clone(),
                        fn_box:self.fn_box.clone(),
                        arg:self.arg.clone(),
                        spurious:self.spurious.clone()})
  }
  fn prog_pt<'r>(self:&'r Self) -> &'r ProgPt {
    & self.prog_pt
  }
  fn id<'r>(self:&'r Self) -> &'r ArtIdChoice {
    & self.id
  }
  fn hash_u64(&self) -> u64 {
    let mut hasher = DefaultHasher::new();
    self.id.hash( &mut hasher );
    self.prog_pt.hash( &mut hasher );
    self.arg.hash( &mut hasher );
    hasher.finish()
  }
  fn eq (&self, other:&Force<T>) -> bool {    
    if   &self.id      == other.id()
      && &self.prog_pt == other.prog_pt()
    {
      let other = Box::new(other) ;
      // This is safe if the prog_pt implies unique Arg and Res types.
      // TODO-Soon: Program points should store argument + result types; we should check these dynamically here
      let other : &Box<NaiveThunk<A,S,T>> = unsafe { transmute::<_,_>( other ) } ;
      self.arg == other.arg
    } else {
      false
    }
  }
  fn fmt(&self, f:&mut Formatter) -> Result {
    write!(f,"NaiveThunk{{id:{:?},prog_pt:{:?},arg:{:?}}}",
           self.id, self.prog_pt, self.arg)
  }
}

impl<A:Hash,S,T> Hash for NaiveThunk<A,S,T> {
  fn hash<H:Hasher>(&self, hasher: &mut H) {
    self.prog_pt.hash( hasher );
    self.arg.hash( hasher );
  }
}
impl<A:Debug,S,T> Debug for NaiveThunk<A,S,T> {
  fn fmt(&self, f:&mut Formatter) -> fmt::Result {
    // TODO: Make this formatting better
    write!(f, "NaiveThunk({:?},{:?})", self.prog_pt, self.arg)
  }
}
impl<A:PartialEq,S,T> PartialEq for NaiveThunk<A,S,T> {
  fn eq(&self, other:&Self) -> bool {
    self.prog_pt == other.prog_pt &&
      self.arg == other.arg      
  }
}





/// Create a name from unit, that is, create a "leaf" name.
pub fn name_unit () -> Name {
  UNIT_NAME.with(|r|r.clone())
}

/// Create one name from two (binary name composition)
pub fn name_pair (n1:Name, n2:Name) -> Name {
  let h = my_hash( &(n1.hash,n2.hash) ) ;
  let p = NameSym::Pair(n1.symbol, n2.symbol) ;
  Name{ hash:h, symbol:Rc::new(p) }
}

/// Create a name from a hash value.
///
/// Do not call this function directly; we introduced it in order to
/// get reflection to type-check.  We should think of ways to avoid
/// using this in the future.
pub fn name_of_hash64(h:u64) -> Name {
  // TODO: Get rid of need for Rc here; 
  // Rc should be optional in names?
  Name{ hash:h, symbol:Rc::new(NameSym::Hash64) }
}

/// Create a name from a `usize`
pub fn name_of_usize (u:usize) -> Name {
  let h = my_hash(&u) ;
  let s = NameSym::Usize(u) ;
  Name{ hash:h, symbol:Rc::new(s) }
}

/// Create a name from a `isize`
pub fn name_of_isize (i:isize) -> Name {
  let h = my_hash(&i) ;
  let s = NameSym::Isize(i) ;
  Name{ hash:h, symbol:Rc::new(s) }
}

/// Create a name from a `string`
pub fn name_of_string (s:String) -> Name {
  let h = my_hash(&s);
  let s = NameSym::String(s) ;
  Name{ hash:h, symbol:Rc::new(s) }
}

/// Create a name from a `str`
pub fn name_of_str (s:&'static str) -> Name {
  let h = my_hash(&s);
  let s = NameSym::String(s.to_string()) ;
  Name{ hash:h, symbol:Rc::new(s) }
}

/// Create two names from one
pub fn name_fork (n:Name) -> (Name, Name) {
  let h1 = my_hash( &(&n, 11111111) ) ; // TODO-Later: make this hashing better.
  let h2 = my_hash( &(&n, 22222222) ) ;
  ( Name{ hash:h1,
          symbol:Rc::new(NameSym::ForkL(n.symbol.clone())) } ,
    Name{ hash:h2,
          symbol:Rc::new(NameSym::ForkR(n.symbol)) } )    
}

/// Create three names from one
pub fn name_fork3 (n:Name)
                   -> (Name,Name,Name)
{
  let (n1,n)  = name_fork(n);
  let (n2,n3) = name_fork(n);
  (n1,n2,n3)
}

/// Create four names from one
pub fn name_fork4 (n:Name)
                   -> (Name,Name,Name,Name)
{
  let (n1,n)  = name_fork(n);
  let (n2,n)  = name_fork(n);
  let (n3,n4) = name_fork(n);
  (n1,n2,n3,n4)
}  

/// Creates or re-enters a given namespace; performs the given computation there.
pub fn ns<T,F> (n:Name, body:F) -> T
  where F:FnOnce() -> T {
    GLOBALS.with(|g| {
      match g.borrow().engine {
        Engine::DCG(ref dcg) => <DCG as Adapton>::ns(dcg, n, body),
        Engine::Naive => (body)()
      }
    })   
  }

/// Enters a special "namespace" where all name uses are ignored; instead, Adapton uses structural identity.
pub fn structural<T,F> (body:F) -> T
  where F:FnOnce() -> T {
    GLOBALS.with(|g| {
      match g.borrow().engine {
        Engine::DCG(ref dcg) => <DCG as Adapton>::structural(dcg,body), // XXX borrow is too long
        Engine::Naive => (body)()
      }
    })    
  }

/// Creates an unnamed, immutable reference cell (an eager `Art<_>`)
/// whose content may not change over time.
pub fn put<T:Eq+Debug+Clone> (val:T) -> Art<T> {
  Art{art:EnumArt::Rc(Rc::new(val))}
}

/// Creates a named reference cell (an eager `Art<_>`) whose content
/// can change over time.
///
/// From the editor's perspective, this cell is mutable.  From the
/// archivist's perspective, this cell is a "one-shot" reference cell:
/// Once allocated, it is immutable.
pub fn cell<T:Hash+Eq+Debug+Clone+'static> (n:Name, val:T) -> Art<T> {
  GLOBALS.with(|g| {
    match g.borrow().engine {
      Engine::DCG(ref dcg) => {
        if
          let AbsArt::Loc(loc) = (dcg.borrow_mut()).cell(n,val) {
            Art{art:EnumArt::Loc(loc)} }
        else { unreachable!() } }
      Engine::Naive => Art{art:EnumArt::Rc(Rc::new(val))}
    }
  })
}

/// Mutates a mutable articulation.
pub fn set<T:'static+Eq+Debug+Clone> (a:&Art<T>, val:T) {
  match (*a).art {
    EnumArt::Rc(_)    => { panic!("set: Cannot mutate immutable Rc articulation; use an DCG cell instead") },
    EnumArt::Force(_) => { panic!("set: Cannot mutate immutable Force articulation; use an DCG cell instead") },
    EnumArt::Loc(ref l) => {
      GLOBALS.with(|g| {
        match g.borrow().engine {
          Engine::Naive => unimplemented!(), // TODO: Think more about this case.
          Engine::DCG(ref dcg) => {
            (dcg.borrow_mut()).set(AbsArt::Loc(l.clone()), val)
          }
        }
      })
    }
  }
}

/// Allocates a thunk, an `Art<T>` that consists of a suspended
/// computation that produces a value of type `T`.
///
/// Use the macros `thunk!`, `memo!` and `eager!` to create and force
/// thunks with less typing.
///
/// A full invocation of `thunk` consists of the following:
///
///  - It has an `id` of type `ArtIdChoice`, either giving a `Name`,
///    requesting structural identity, or requesting no caching
///    whatsoever.
///
///  - Its code resides at a _program point_, of type `ProgPt`.  This
///    uniquely identifies the static elements of the suspended
///    computation (the code, but not the closing environment).
///
///  - It has a function for this code, of type
///    `Rc<Box<Fn(Art,Spurious) -> Res >>`.  The `Rc<_>` is required
///    for cloning this function, which we generally want to do.  We
///    divide the arguments into the ordinary arguments of type `Arg`,
///    and additional "spurious" arguments.  When we judge whether a
///    thunk's closing environment is equal to another, we use the
///    type `Arg`.  When we judge whether the result of a thunk has
///    changed or not, we use the type `Res`.
///
///  - Spurious arguments are not compared for equality, and are
///    ignored during the memo-matching process.  They are, however,
///    saved in the thunk and supplied to the suspended computation
///    when it runs.  Typically, these arguments consist of
///    higher-order functions that parameterize the thunk; we make
///    them spurious because (1) we cannot, and do not wish to compare
///    them for equality and (2) the triple of type (`ProgPt`,
///    `ArtIdChoice`, `Arg`) should determine these spurious
///    arguments, if any.  Because some arguments have no equality
///    relation, the presence of these arguments is sometimes a
///    necessary hack.
pub fn thunk<Arg:Hash+Eq+Debug+Clone+'static,Spurious:Clone+'static,Res:Hash+Eq+Debug+Clone+'static>
  (id:ArtIdChoice,
   prog_pt:ProgPt,
   fn_box:Rc<Box< Fn(Arg, Spurious) -> Res >>,
   arg:Arg, spurious:Spurious)
   -> Art<Res>
{
  GLOBALS.with(|g| {
    match g.borrow().engine {
      Engine::DCG(ref dcg) => {
        Art{art:EnumArt::Loc({
          if let AbsArt::Loc(loc) = 
            (dcg.borrow_mut()).thunk(id, prog_pt, fn_box, arg, spurious)
          { loc } else { unreachable!() }})}              
      },
      Engine::Naive => {
        Art{art:EnumArt::Force(
          Rc::new(NaiveThunk{
            id:id,prog_pt:prog_pt,
            fn_box:fn_box,arg:arg,
            spurious:spurious} ))}}}
  })
}

/// Demands and observes the value of an `&Art<T>`, returning a (cloned) value of type `T`.
pub fn force<T:Hash+Eq+Debug+Clone+'static> (a:&Art<T>) -> T {
  match a.art {
    EnumArt::Force(ref f) => f.force(),
    EnumArt::Rc(ref rc) => (&**rc).clone(),
    EnumArt::Loc(ref loc) => {
      GLOBALS.with(|g| {
        match g.borrow().engine {
          Engine::DCG(ref dcg_refcell) => 
            <DCG as Adapton>::force(dcg_refcell, &AbsArt::Loc(loc.clone())),
          Engine::Naive => panic!("cannot force a non-naive location with the naive engine")
      }})
    }
  }
}

/// Demands and observes the value of an `&Art<T>`, returning a
/// (cloned) value of type `S`, mapped by function `mapf`.
///
/// The _pure_ map function `mapf` transforms the value before
/// `force_map` returns.  For correctness, it is critical that `mapf`
/// does not itself allocate or observe any DCG nodes.  When the given
/// `Art` is a cell, this mapping function enables the engine to prune
/// otherwise-dirtied dependencies; consequently, the map function
/// permits finer-grained dependency tracking without additional,
/// fine-grained `Art`s.
pub fn force_map<T:Hash+Eq+Debug+Clone+'static,
                 S:Hash+Eq+Debug+Clone+'static, 
                 MapF:'static> 
    (a:&Art<T>, mapf:MapF) -> S 
    where MapF:Fn(&Art<T>, T) -> S
{
    match a.art {
        EnumArt::Force(ref f) => mapf(a, f.force()),
        EnumArt::Rc(ref rc) => mapf(a, (&**rc).clone()),
        EnumArt::Loc(ref loc) => {
            GLOBALS.with(|g| {
                match g.borrow().engine {
                    Engine::DCG(ref dcg_refcell) => 
                        <DCG as Adapton>::force_map(dcg_refcell, &AbsArt::Loc(loc.clone()), mapf),
                    Engine::Naive => panic!("cannot force a non-naive location with the naive engine")
                }
            })
        }
    }
}

/// Operations that monitor and alter the active engine.  Incremental
/// applications should not use these operations directly.
pub mod manage {
  use super::*;


  /// Initializes global state with a fresh DCG-based engine; returns the old engine.
  /// The DCG is the central implementation structure behind Adapton.
  /// At a high level, it consists of a data dependence graph (the "demanded computation graph"), and an associated memoization table.
  pub fn init_dcg () -> Engine { init_engine(Engine::DCG(RefCell::new(DCG::new()))) }
  
  /// Initializes global state with a ("fresh") Naive engine; returns the old engine.
  /// The naive engine is stateless, and performs no memoization and builds no dependence graphs.
  /// (Since the naive engine is stateless, every instance of the naive engine is equivalent to a "fresh" one).
  pub fn init_naive () -> Engine { init_engine(Engine::Naive) }
  
  /// Switch to using the given `Engine`; returns the `Engine` that was in use.
  pub fn use_engine (engine: Engine) -> Engine {
    use std::mem;
    let mut engine = engine;
    GLOBALS.with(|g| {
      mem::swap(&mut g.borrow_mut().engine, &mut engine);
    });
    return engine
  }
  
  /// alias for `use_engine`
  pub fn init_engine (engine: Engine) -> Engine {
    use_engine(engine)
  }  

  /// True iff the current engine is `Naive`
  pub fn engine_is_naive () -> bool {
    GLOBALS.with(|g| {
      match g.borrow().engine {
        Engine::DCG(_) => false,
        Engine::Naive  => true
      }})    
  }
  
  /// True iff the current engine is a `DCG`
  pub fn engine_is_dcg () -> bool {
    GLOBALS.with(|g| {
      match g.borrow().engine {
        Engine::DCG(_) => true,
        Engine::Naive  => false
      }})
  }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
///
/// Well-formedness tests; for documentation and for debugging.
///
mod wf {
  use std::collections::HashMap;
  use std::rc::Rc;
  use std::io::BufWriter;
  use std::io::Write;
  use std::fs::File;
  //use macros::{ProgPt};

  use super::*;

  #[derive(Eq,PartialEq,Clone)]
  enum NodeStatus {
    Dirty, Clean, Unknown
  }

  type Cs = HashMap<Rc<Loc>, NodeStatus> ;

  fn add_constraint (cs:&mut Cs, loc:&Rc<Loc>, new_status: NodeStatus)
  {
    let old_status = match
      cs.get(loc) { None => NodeStatus::Unknown,
                    Some(x) => (*x).clone() } ;
    match (old_status, new_status) {
      (NodeStatus::Clean, NodeStatus::Dirty) |
      (NodeStatus::Dirty, NodeStatus::Clean) => {
        panic!("{:?}: Constrained to be both clean and dirty: Inconsistent status => DCG is not well-formed.")
      },
      (NodeStatus::Unknown, new_status) => { cs.insert(loc.clone(), new_status); () },
      (old_status, NodeStatus::Unknown) => { cs.insert(loc.clone(), old_status); () },
      (ref old_status, ref new_status) if old_status == new_status => { },
      _ => unreachable!(),
    }
  }

  // Constrains loc and all predecessors (transitive) to be dirty
  fn dirty (st:&DCG, cs:&mut Cs, loc:&Rc<Loc>) {
    add_constraint(cs, loc, NodeStatus::Dirty) ;
    let node = match st.table.get(loc) { Some(x) => x, None => panic!("") } ;
    for (pred,_) in node.preds_obs () {
      // Todo: Assert that pred has a dirty succ edge that targets loc
      let succ = super::get_succ(st, &pred, super::Effect::Observe, loc) ;
      if succ.dirty {} else {
        debug_dcg(st);
        write_next_dcg(st, None);
        panic!("Expected dirty edge, but found clean edge: {:?} --Observe--dirty:!--> {:?}", &pred, loc);
      } ; // The edge is dirty.
      dirty(st, cs, &pred)                
    }
  }

  // Constrains loc and all successors (transitive) to be clean
  fn clean (st:&DCG, cs:&mut Cs, loc:&Rc<Loc>) {
    add_constraint(cs, loc, NodeStatus::Clean) ;
    let node = match st.table.get(loc) {
      Some(x) => x,
      None => { panic!("dangling: {:?}", loc) }
    } ;
    if ! node.succs_def () { return } ;
    for succ in node.succs () {
      let succ = super::get_succ(st, loc, super::Effect::Observe, &succ.loc) ;
      assert!( ! succ.dirty ); // The edge is clean.
      clean(st, cs, &succ.loc)
    }
  }

  pub fn check_dcg (st:&mut DCG) {
    if st.flags.write_dcg {
      let dcg_hash = my_hash(format!("{:?}",st.table)); // XXX: This assumes that the table's debugging string identifies it uniquely
      if dcg_hash != st.dcg_hash {
        println!("adapton: dcg #{} hash: {:?}", st.dcg_count, dcg_hash);
        st.dcg_hash = dcg_hash;
        let dcg_count = st.dcg_count;
        st.dcg_count += 1;
        write_next_dcg(st, Some(dcg_count));
      }
    } ;
    if st.flags.check_dcg_is_wf {
      let mut cs = HashMap::new() ;
      for frame in st.stack.iter() {
        clean(st, &mut cs, &frame.loc)
      }
      for (loc, node) in &st.table {
        if ! node.succs_def () { continue } ;
        for succ in node.succs () {
          if succ.dirty {
            dirty(st, &mut cs, loc)
          }
        }
      }        
    }}

  pub fn write_next_dcg (st:&DCG, num:Option<usize>) {
    let name = match num {
      None => format!("adapton-dcg.dot"),
      Some(n) => format!("adapton-dcg-{:08}.dot", n),
    } ;
    let mut file = File::create(name).unwrap() ;
    write_dcg_file(st, &mut file);
  }
  
  pub fn write_dcg_file (st:&DCG, file:&mut File) {
    let mut writer = BufWriter::new(file);
    writeln!(&mut writer, "digraph {{\n").unwrap();
    writeln!(&mut writer, "ordering=out;").unwrap();
    for frame in st.stack.iter() {
      writeln!(&mut writer, "\"{:?}\" [color=blue,penwidth=10];", frame.loc).unwrap();
      for succ in frame.succs.iter() {
        writeln!(&mut writer, "\"{:?}\" -> \"{:?}\" [color=blue,weight=10,penwidth=10];", &frame.loc, &succ.0.loc).unwrap();
      }
    };
    for (loc, node) in &st.table {
      if ! node.succs_def () {
        writeln!(&mut writer, "\"{:?}\" [shape=box];", loc).unwrap();
        continue;
      } ;
      for succ in node.succs () {
        if succ.dirty {
          writeln!(&mut writer, "\"{:?}\" -> \"{:?}\" [color=red,weight=5,penwidth=5];", &loc, &succ.loc).unwrap();
        } else {
          let (weight, penwidth, color) =
            match succ.effect {
              super::Effect::Observe => (0.1, 1, "grey"),
              super::Effect::Allocate => (2.0, 3, "darkgreen") } ;
          writeln!(&mut writer, "\"{:?}\" -> \"{:?}\" [weight={},penwidth={},color={}];",
                   &loc, &succ.loc, weight, penwidth, color).unwrap();
        }
      }
    }
    writeln!(&mut writer, "}}\n").unwrap();
  }
  
  pub fn debug_dcg (st:&DCG) {
    let prefix = "debug_dcg::stack: " ;
    let mut frame_num = 0;
    for frame in st.stack.iter() {
      println!("{} frame {}: {:?}", prefix, frame_num, frame.loc);
      for succ in frame.succs.iter() {
        println!("{} frame {}: \t\t {:?}", prefix, frame_num, &succ);
      }
      frame_num += 1;
    }
    let prefix = "debug_dcg::table: " ;
    for (loc, node) in &st.table {
      println!("{} {:?} ==> {:?}", prefix, loc, node);
      if ! node.succs_def () { continue } ;
      for succ in node.succs () {
        println!("{}\t\t{:?}", prefix, succ);
      }
    }      
  }

  // XXX Does not catch errors in IC_Edit that I expected it would
  // XXX Not sure if it works as I expected
  pub fn check_stack_is_clean (st:&DCG) {
    let stack = st.stack.clone() ;
    for frame in stack.iter() {
      let node = match st.table.get(&frame.loc) {
        Some(x) => x,
        None => { panic!("dangling: {:?}", &frame.loc) }
      } ;
      if ! node.succs_def () { return } ;
      for succ in node.succs () {
        let succ = super::get_succ(st, &frame.loc, succ.effect.clone(), &succ.loc) ;
        assert!( succ.dirty ); // The edge is clean.
      }
    }
  }
}
