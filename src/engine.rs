use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fmt::Debug;
use std::fmt::{Formatter,Result};
use std::fmt;
use std::fs::{OpenOptions};
use std::hash::{Hash,Hasher,SipHasher};
use std::mem::replace;
use std::mem::transmute;
use std::num::Zero;
use std::ops::Add;
use std::rc::Rc;
 
use macros::*;

thread_local!(static GLOBALS: RefCell<Globals> = RefCell::new(Globals{engine:Engine::Naive}));
thread_local!(static ROOT_NAME: Name = Name{ hash:0, symbol: Rc::new(NameSym::Root) });

// Names provide a symbolic way to identify nodes.
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
pub struct Loc {
  hash : u64, // hash of (path,id)
  path : Rc<Path>,
  id   : Rc<ArtId>,
}
impl Debug for Loc {
  fn fmt(&self, f:&mut Formatter) -> Result {
    write!(f,"{:?}*{:?}",self.path,self.id)
  }
}
impl Hash for Loc {
  fn hash<H>(&self, state: &mut H) where H: Hasher {
    self.hash.hash(state)
  }
}

#[derive(Hash,PartialEq,Eq,Clone)]
enum ArtId {
  Structural(u64), // Identifies an Art::Loc based on hashing content.
  Nominal(Name),   // Identifies an Art::Loc based on a programmer-chosen name.
}

impl Debug for ArtId {
  fn fmt(&self, f:&mut Formatter) -> Result {
    match *self {
      ArtId::Structural(ref hash) => write!(f, "{}", hash),
      ArtId::Nominal(ref name) => write!(f, "{:?}", name),
    }
  }
}

#[derive(Debug)]
pub struct Flags {
  pub use_purity_optimization : bool,
  pub ignore_nominal_use_structural : bool, // Ignore the Nominal ArtIdChoice, and use Structural behavior instead
  pub check_dcg_is_wf : bool, // After each Adapton operation, check that the DCG is well-formed
  pub write_dcg : bool, // Within each well-formedness check, write the DCG to the local filesystem
  pub gmlog_dcg : bool, // At certain points in the Engine's code, write state changes as graph-movie output
}

pub struct Globals {
  engine: Engine,
}

#[derive(Debug,Clone)]
pub enum Engine {
  DCG(RefCell<DCG>),
  Naive
}

#[derive(Debug)]
pub struct DCG {
  pub flags : Flags, // public because I dont want to write / design abstract accessors
  root  : Rc<Loc>,
  table : HashMap<Rc<Loc>, Box<GraphNode>>,
  stack : Vec<Frame>,
  path  : Rc<Path>,
  cnt   : Cnt,
  dcg_count : usize,
  dcg_hash  : u64,  
  //gmfile : Option<File>,
}

impl Hash  for     DCG { fn hash<H>(&self, _state: &mut H) where H: Hasher { unimplemented!() }}
impl Eq    for     DCG { }
impl PartialEq for DCG { fn eq(&self, _other:&Self) -> bool { unimplemented!() } }
impl Clone for     DCG { fn clone(&self) -> Self { unimplemented!() } }

// NameSyms: For a general semantics of symbols, see Chapter 31 of PFPL 2nd Edition. Harper 2015:
// http://www.cs.cmu.edu/~rwh/pfpl/2nded.pdf
//
#[derive(Hash,PartialEq,Eq,Clone)]
enum NameSym {
  Root,           // Unit value for name symbols
  String(String), // Strings encode globally-unique symbols.
  Usize(usize),   // USizes encode globally-unique symbols.
  Isize(isize),   // USizes encode globally-unique symbols.
  Pair(Rc<NameSym>,Rc<NameSym>), // A pair of unique symbols, interpeted as a symbol, is unique
  ForkL(Rc<NameSym>), // Left projection of a unique symbol is unique
  ForkR(Rc<NameSym>), // Right projection of a unique symbol is unique
}

impl Debug for NameSym {
  fn fmt(&self, f:&mut Formatter) -> Result {
    match *self {
      NameSym::Root => write!(f, "/"),
      NameSym::String(ref s) => write!(f, "{}", s),
      NameSym::Usize(ref n) => write!(f, "{}", n),
      NameSym::Isize(ref n) => write!(f, "{}", n),
      NameSym::Pair(ref l, ref r) => write!(f, "({:?},{:?})",l,r),
      NameSym::ForkL(ref s) => write!(f, "{:?}.L", s),
      NameSym::ForkR(ref s) => write!(f, "{:?}.R", s),
    }
  }
}

// Paths are built implicitly via the Adapton::ns command.
#[derive(Hash,PartialEq,Eq,Clone)]
enum Path {
  Empty,
  Child(Rc<Path>,Name),
}

impl Debug for Path {
  fn fmt(&self, f:&mut Formatter) -> Result {
    match *self {
      Path::Empty => write!(f, ""),
      Path::Child(ref p, ref n) => write!(f, "{:?}::{:?}", p, n),
    }
  }
}

// The DCG structure consists of `GraphNode`s:
trait GraphNode : Debug {
  fn preds_alloc<'r> (self:&Self) -> Vec<Rc<Loc>> ;
  fn preds_obs<'r>   (self:&Self) -> Vec<Rc<Loc>> ;
  fn preds_insert<'r>(self:&'r mut Self, Effect, &Rc<Loc>) -> () ;
  fn preds_remove<'r>(self:&'r mut Self, &Rc<Loc>) -> () ;
  fn succs_def<'r>   (self:&Self) -> bool ;
  fn succs_mut<'r>   (self:&'r mut Self) -> &'r mut Vec<Succ> ;
  fn succs<'r>       (self:&'r Self) -> &'r Vec<Succ> ;
  fn hash_seeded     (self:&Self, u64) -> u64 ;
}

#[derive(Debug,Clone)]
struct Frame {
  loc   : Rc<Loc>,    // The currently-executing node
  //path  : Rc<Path>,   // The current path for creating new nodes; invariant: (prefix-of frame.loc.path frame.path)
  succs : Vec<Succ>,  // The currently-executing node's effects (viz., the nodes it demands)
}

#[derive(Debug,Clone)]
struct Succ {
  dirty  : bool,    // mutated to dirty when loc changes, or any of its successors change
  loc    : Rc<Loc>, // Target of the effect, aka, the successor, by this edge
  effect : Effect,
  dep    : Rc<Box<DCGDep>>, // Abstracted dependency information (e.g., for Observe Effect, the prior observed value)
}

#[derive(PartialEq,Eq,Debug,Clone,Hash)]
enum Effect {
  Observe,
  Allocate,
}
struct DCGRes {
  changed : bool,
}
// DCGDep abstracts over the value produced by a dependency, as
// well as mechanisms to update and/or re-produce it.
trait DCGDep : Debug {
  fn clean (self:&Self, g:&RefCell<DCG>, loc:&Rc<Loc>) -> DCGRes ;
}

impl Hash for Succ {
  fn hash<H>(&self, hasher: &mut H) where H: Hasher {
    self.dirty.hash( hasher );
    self.loc.hash( hasher );
    self.effect.hash( hasher );
  }
}

// ----------------------------------------------------------------------------------------------------

#[derive(Debug)]
struct NoDependency;
impl DCGDep for NoDependency {
  fn clean (self:&Self, _g:&RefCell<DCG>, _loc:&Rc<Loc>) -> DCGRes { DCGRes{changed:false} }
}

#[derive(Debug)]
struct AllocDependency<T> { val:T }
impl<T:Debug> DCGDep for AllocDependency<T> {
  fn clean (self:&Self, _g:&RefCell<DCG>, _loc:&Rc<Loc>) -> DCGRes { DCGRes{changed:true} } // TODO-Later: Make this a little better.
}

trait ShapeShifter {
  fn be_node<'r> (self:&'r mut Self) -> &'r mut Box<GraphNode> ;
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
  preds : Vec<(Effect,Rc<Loc>)>,
  val   : T,
}

// CompNode<Res> for a suspended computation whose resulting value of
// type T.  The result of the CompNode is affected in two ways: the
// (1) producer may change, which may affect the result and (2) the
// values produced by the successors may change, indirectly
// influencing how the producer produces its resulting value.
struct CompNode<Res> {
  preds    : Vec<(Effect, Rc<Loc>)>,
  succs    : Vec<Succ>,
  producer : Box<Producer<Res>>, // Producer can be App<Arg,Res>, where type Arg is hidden.
  res      : Option<Res>,
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

// ---------- App implementation of Debug and Hash

impl<Arg:Debug,Spurious,Res> Debug for App<Arg,Spurious,Res> {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f,"App({:?} {:?})", self.prog_pt, self.arg)
  }
}

impl<Arg:Hash+Debug,Spurious,Res> Hash for App<Arg,Spurious,Res> {
  fn hash<H>(&self, state: &mut H) where H: Hasher { (&self.prog_pt,&self.arg).hash(state) }
}

// ---------- App implementation of Producer and Consumer traits:

impl<Arg:'static+PartialEq+Eq+Clone+Debug,Spurious:'static+Clone,Res:'static+Debug+Hash> Producer<Res>
  for App<Arg,Spurious,Res>
{
  fn produce(self:&Self) -> Res {
    let f = self.fn_box.clone() ;
    ////debug!("{} producer begin: ({:?} {:?})", engineMsg!(st), &self.prog_pt, &self.arg);
    let res = f (self.arg.clone(),self.spurious.clone()) ;
    ////debug!("{} producer end: ({:?} {:?}) produces {:?}", engineMsg!(st), &self.prog_pt, &self.arg, &res);
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
      let other : &Box<App<Arg,Spurious,Res>> = unsafe { transmute::<_,_>( other ) } ;
      self.arg == other.arg
    } else {
      false
    }
  }
}
impl<Arg:Clone+PartialEq+Eq+Debug,Spurious,Res> Consumer<Arg> for App<Arg,Spurious,Res> {
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

// This only is safe in contexts where the type of loc is known.
// Unintended double-uses of names and hashes will generally cause uncaught type errors.
fn res_node_of_loc<'r,Res> (st:&'r mut DCG, loc:&Rc<Loc>) -> &'r mut Box<Node<Res>> {
  let abs_node = lookup_abs(st, loc) ;
  unsafe { transmute::<_,_>(abs_node) }
}


/// Well-formedness tests; for documentation and for debugging.
mod wf {
  use std::collections::HashMap;
  use std::rc::Rc;
  //use std::io;
  use std::io::prelude::*;
  use std::io::BufWriter;
  use std::fs::File;
  use macros::*;

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
    for pred in node.preds_obs () {
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
      None => {
        if &st.root == loc { return } // Todo-Question: Dead code?
        else { panic!("dangling: {:?}", loc) } }
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
    //let mut frame_num = 0;
    for frame in st.stack.iter() {
      writeln!(&mut writer, "\"{:?}\" [color=blue,penwidth=10];", frame.loc);
      for succ in frame.succs.iter() {
        writeln!(&mut writer, "\"{:?}\" -> \"{:?}\" [color=blue,weight=10,penwidth=10];", &frame.loc, &succ.loc).unwrap();
      }
      //frame_num += 1;
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
        None => {
          if &st.root == &frame.loc { return } // Todo-Question: Dead code?
          else { panic!("dangling: {:?}", &frame.loc) } }
      } ;
      if ! node.succs_def () { return } ;
      for succ in node.succs () {
        let succ = super::get_succ(st, &frame.loc, succ.effect.clone(), &succ.loc) ;
        assert!( succ.dirty ); // The edge is clean.
      }
    }
  }
}

/// An `ArtIdChoice` is a symbolic identity for an articulation point made by
/// Adapton::thunk.  An `ArtIdChoice` is chosen by the programmer to identify
/// the point during evaluation (and simultaneously, to identify the
/// point during re-evaluation).
/// An `Eager` identity is special, and it means "do not introduce any
/// laziness/memoization overhead here"; when Eager is used, no thunk
/// is created; rather, the computation eagerly produces an articulated
/// value of the form Art::Rc(v), for some value v.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum ArtIdChoice {
  /// Eagerly produces an `Art` that merely consists of an `Rc`; no additional indirection is needed/used.
  Eager,
  /// Identifies an `Art` based on hashing content (e.g., `prog_pt` for code and argument(s)).
  Structural,
  /// Identifies an `Art` based on a programmer-chosen name.
  Nominal(Name),
}

#[derive(Debug,Hash,PartialEq,Eq,Clone,Encodable)]
pub struct Cnt {
  pub create : usize, // Add trait performs sum
  pub eval   : usize, // Add trait performs sum
  pub dirty  : usize, // Add trait performs sum
  pub clean  : usize, // Add trait performs sum
  pub stack  : usize, // Add trait performs max
}

impl Add for Cnt {
  type Output=Cnt;
  fn add(self, rhs: Self) -> Self::Output {
    Cnt {
      create : self.create + rhs.create,
      eval   : self.eval + rhs.eval,
      dirty  : self.dirty + rhs.dirty,
      clean  : self.clean + rhs.clean,
      stack  : if self.stack > rhs.stack { self.stack } else { rhs.stack }
    }
  }
}

impl<'a> Add for &'a Cnt {
  type Output=Cnt;
  fn add(self, rhs: Self) -> Self::Output {
    Cnt {
      create : self.create + rhs.create,
      eval   : self.eval + rhs.eval,
      dirty  : self.dirty + rhs.dirty,
      clean  : self.clean + rhs.clean,
      stack  : if self.stack > rhs.stack { self.stack } else { rhs.stack }
    }
  }
}

impl Zero for Cnt {
  fn zero() -> Self {
    Cnt {
      create : 0 as usize,
      eval   : 0 as usize,
      dirty  : 0 as usize,
      clean  : 0 as usize,
      stack  : 0 as usize,
    }
  }
}

// ---------- Node implementation:

impl <Res:Debug+Hash> GraphNode for Node<Res> {
  fn preds_alloc(self:&Self) -> Vec<Rc<Loc>> {
    match *self { Node::Mut(ref nd) => nd.preds.iter().filter_map(|&(ref effect,ref loc)| if effect == &Effect::Allocate { Some(loc.clone()) } else { None } ).collect::<Vec<_>>(),
                  Node::Comp(ref nd) => nd.preds.iter().filter_map(|&(ref effect,ref loc)| if effect == &Effect::Allocate { Some(loc.clone()) } else { None } ).collect::<Vec<_>>(),
                  Node::Pure(_) => unreachable!(),
                  _ => unreachable!(),
    }}

  fn preds_obs(self:&Self) -> Vec<Rc<Loc>> {
    match *self { Node::Mut(ref nd) => nd.preds.iter().filter_map(|&(ref effect,ref loc)| if effect == &Effect::Observe { Some(loc.clone()) } else { None } ).collect::<Vec<_>>(),
                  Node::Comp(ref nd) => nd.preds.iter().filter_map(|&(ref effect,ref loc)| if effect == &Effect::Observe { Some(loc.clone()) } else { None } ).collect::<Vec<_>>(),
                  Node::Pure(_) => unreachable!(),
                  _ => unreachable!(),
    }}
  fn preds_insert (self:&mut Self, eff:Effect, loc:&Rc<Loc>) -> () {
    match *self { Node::Mut(ref mut nd) => nd.preds.push ((eff,loc.clone())),
                  Node::Comp(ref mut nd) => nd.preds.push ((eff,loc.clone())),
                  Node::Pure(_) => unreachable!(),
                  _ => unreachable!(),
    }}
  fn preds_remove (self:&mut Self, loc:&Rc<Loc>) -> () {
    match *self { Node::Mut(ref mut nd) => nd.preds.retain (|eff_pred|{ let (_,ref pred) = *eff_pred; *pred != *loc }),
                  Node::Comp(ref mut nd) => nd.preds.retain (|eff_pred|{ let (_, ref pred) = *eff_pred; *pred != *loc}),
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
    let mut hasher = SipHasher::new();
    seed.hash(&mut hasher);
    self.hash(&mut hasher);
    hasher.finish()
  }
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

// Performs the computation at loc, produces a result of type Res.
// Error if loc is not a Node::Comp.
fn loc_produce<Res:'static+Debug+PartialEq+Eq+Clone+Hash>(g:&RefCell<DCG>, loc:&Rc<Loc>) -> Res
{
  let (producer, prev_path) = {
    let st : &mut DCG = &mut *g.borrow_mut() ;
    //debug!("{} produce begin: {:?}", engineMsg!(st), &loc);
    let succs : Vec<Succ> = {
      let succs : Vec<Succ> = Vec::new();
      let node : &mut Node<Res> = res_node_of_loc( st, loc ) ;
      replace(node.succs_mut(), succs)
    } ;
    revoke_succs( st, loc, &succs );
    st.stack.push ( Frame{loc:loc.clone(),
                          //path:loc.path.clone(),
                          succs:Vec::new(), } );
    st.cnt.stack = if st.cnt.stack > st.stack.len() { st.cnt.stack } else { st.stack.len() } ;
    let prev_path = st.path.clone () ;
    st.path = loc.path.clone() ;
    let producer : Box<Producer<Res>> = {
      let node : &mut Node<Res> = res_node_of_loc( st, loc ) ;
      match *node {
        Node::Comp(ref nd) => nd.producer.copy(),
        _ => panic!("internal error"),
      }
    } ;
    st.cnt.eval += 1 ; 
    drop(st);  // End mutable borrow of global RefCell
    (producer, prev_path)
  }; 

  let res = producer.produce() ;

  let st = &mut * g.borrow_mut() ;
  st.path = prev_path ;
  let frame = match st.stack.pop() {
    None => panic!("expected Some _: stack invariants are broken"),
    Some(frame) => frame
  } ;
  assert!( &frame.loc == loc );
  //let mut succ_idx = 0;
  for succ in &frame.succs {
    //debug!("{} produce: edge: {:?} --{:?}--dirty?:{:?}--> {:?}", engineMsg!(st), &loc, &succ.effect, &succ.dirty, &succ.loc);
    // let (effect, is_weak) =
    //   match succ.effect {
    //     //Effect::Observe => ("observe", true),
    //     Effect::Observe => ("observe", false), // XXX
    //     Effect::Allocate => ("allocate", false)
    //   } ;
    // if st.flags.gmlog_dcg {
    //   // gm::startdframe(st, &format!("{:?}--{}->{:?}", loc, effect, succ.loc), None);
    //   // gm::addedge(st, &format!("{:?}",loc), &format!("{:?}",succ.loc),
    //   //             &format!("{}",succ_idx),
    //   //             effect, "", None, is_weak);
    // }
    // succ_idx += 1;
    if succ.dirty {
      // This case witnesses an illegal use of nominal side effects
      panic!("invariants broken: newly-built DCG edge should be clean, but is dirty.")
    } ;
    let succ_node = lookup_abs( st, &succ.loc );
    succ_node.preds_insert( succ.effect.clone(), loc );
  } ;
  {
    let node : &mut Node<Res> = res_node_of_loc( st, loc ) ;
    match *node {
      Node::Comp(ref mut node) => {
        replace(&mut node.succs, frame.succs) ;
        replace(&mut node.res, Some(res.clone()))
      },
      _ => panic!("internal error"),
    }
  } ;
  //debug!("{} produce end: {:?} produces {:?}", engineMsg!(st), &loc, &res);
  res
}



// ---------- DCGDep implementation:

#[derive(Debug)]
struct ProducerDep<T> { res:T }

fn clean_comp<Res:'static+Sized+Debug+PartialEq+Clone+Eq+Hash>
  (g:&RefCell<DCG>,
   this_dep:&ProducerDep<Res>,
   loc:&Rc<Loc>, cache:Res, succs:Vec<Succ>) -> DCGRes
{
  for succ in succs.iter() {
    let dirty = {
      let mut st = &mut *g.borrow_mut();
      get_succ_mut(st, loc, succ.effect.clone(), &succ.loc).dirty
    } ;
    if dirty {
      let succ_dep = & succ.dep ;
      let res = succ_dep.clean(g, &succ.loc) ;
      if res.changed {        
        let result : Res = loc_produce( g, loc ) ;
        let changed = result != this_dep.res ;
        return DCGRes{changed:changed}
      }
      else {
        let mut st : &mut DCG = &mut *g.borrow_mut();
        st.cnt.clean += 1 ;
        get_succ_mut(st, loc, succ.effect.clone(), &succ.loc).dirty = false ;
      }
    }
  } ;
  let changed = this_dep.res != cache ;
  DCGRes{changed:changed}
}

impl <Res:'static+Sized+Debug+PartialEq+Eq+Clone+Hash>
  DCGDep for ProducerDep<Res>
{
  fn clean(self:&Self, g:&RefCell<DCG>, loc:&Rc<Loc>) -> DCGRes {
    //let stackLen = mut_dcg_of_globals.stack.len() ;
    ////debug!("{} change_prop begin: {:?}", engineMsg!(st), loc);
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
          ////debug!("{} change_prop early end: {:?} is Pure(_)", engineMsg(Some(stackLen)), loc);
          return DCGRes{changed:false}
        },
        Node::Mut(ref nd) => {
          ////debug!("{} change_prop early end: {:?} is Mut(_)", engineMsg(Some(stackLen)), loc);
          return DCGRes{changed:nd.val != self.res}
        },
        _ => panic!("undefined")
      }
    } ;
    match res_succs {
      Some((res,succs)) => clean_comp(g, self, loc, res, succs),
      None => {
        let res = loc_produce( g, loc );
        let changed = self.res != res ;
        DCGRes{changed:changed}
      }
    }
  }
}

// ---------- Node implementation:

fn revoke_succs<'x> (st:&mut DCG, src:&Rc<Loc>, succs:&Vec<Succ>) {
  //let mut succ_idx = 0;
  for succ in succs.iter() {
    if st.flags.gmlog_dcg {
      // gm::startdframe(st, &format!("revoke_succ {:?} {} --> {:?}", src, succ_idx, succ.loc), None);
      // gm::remedge(st, &format!("{:?}",src), &format!("{:?}",succ.loc),
      //             &format!("{}",succ_idx), "", None);
    } ;
    let succ_node : &mut Box<GraphNode> = lookup_abs(st, &succ.loc) ;
    //succ_idx += 1;
    succ_node.preds_remove(src)
  }
}

fn loc_of_id(path:Rc<Path>,id:Rc<ArtId>) -> Rc<Loc> {
  let hash = my_hash(&(&path,&id));
  Rc::new(Loc{path:path,id:id,hash:hash})
}

fn get_succ<'r>(st:&'r DCG, src_loc:&Rc<Loc>, eff:Effect, tgt_loc:&Rc<Loc>) -> &'r Succ {
  //let stackLen = st.stack.len() ;
  let nd = st.table.get(src_loc);
  let nd = match nd {
    None => panic!(""),
    Some(nd) => nd
  } ;    
  //debug!("{} get_succ_mut: resolving {:?} --{:?}--dirty:?--> {:?}", engineMsg(Some(stackLen)), &src_loc, &eff, &tgt_loc);
  for succ in nd.succs() {
    if (succ.effect == eff) && (&succ.loc == tgt_loc) {
      //debug!("{} get_succ_mut:  resolved {:?} --{:?}--dirty:{:?}--> {:?}", engineMsg(Some(stackLen)), &src_loc, &succ.effect, &succ.dirty, &tgt_loc);
      return succ
    } else {}
  } ;
  panic!("tgt_loc is dangling in src_node.dem_succs")
}

// Implement "sharing" of the dirty bit.
// The succ edge is returned as a mutable borrow, to permit checking
// and mutating the dirty bit.
fn get_succ_mut<'r>(st:&'r mut DCG, src_loc:&Rc<Loc>, eff:Effect, tgt_loc:&Rc<Loc>) -> &'r mut Succ {
  //let stackLen = st.stack.len() ;
  let nd = lookup_abs( st, src_loc );
  //debug!("{} get_succ_mut: resolving {:?} --{:?}--dirty:?--> {:?}", engineMsg(Some(stackLen)), &src_loc, &eff, &tgt_loc);
  for succ in nd.succs_mut().iter_mut() {
    if (succ.effect == eff) && (&succ.loc == tgt_loc) {
      //debug!("{} get_succ_mut:  resolved {:?} --{:?}--dirty:{:?}--> {:?}", engineMsg(Some(stackLen)), &src_loc, &succ.effect, &succ.dirty, &tgt_loc);
      return succ
    } else {}
  } ;
  panic!("tgt_loc is dangling in src_node.dem_succs")
}

fn dirty_pred_observers(st:&mut DCG, loc:&Rc<Loc>) {
  //debug!("{} dirty_pred_observers: {:?}", engineMsg!(st), loc);
  //let stackLen = st.stack.len() ;
  let pred_locs : Vec<Rc<Loc>> = lookup_abs( st, loc ).preds_obs() ;
  let mut dirty_edge_count = 0;
  for pred_loc in pred_locs {
    if st.root.eq (&pred_loc) { panic!("root in preds") } // Todo-Question: Dead code?
    else {
      let stop : bool = {
        // The stop bit communicates information from st for use below.
        //debug!("{} dirty_pred_observers: edge {:?} --> {:?} ...", engineMsg(Some(stackLen)), &pred_loc, &loc);
        let succ = get_succ_mut(st, &pred_loc, Effect::Observe, &loc) ;
        if succ.dirty { true } else {
          dirty_edge_count += 1 ;
          replace(&mut succ.dirty, true);
          //debug!("{} dirty_pred_observers: edge marked dirty: {:?} --{:?}--dirty:{:?}--> {:?}", engineMsg(Some(stackLen)), &pred_loc, &succ.effect, &succ.dirty, &loc);
          false
        }} ;
      if !stop {
        dirty_pred_observers(st,&pred_loc);
      } else {
        //debug!("{} dirty_pred_observers: already dirty", engineMsg(Some(stackLen)))
      }
    }
  }
  st.cnt.dirty += dirty_edge_count ;
}

fn dirty_alloc(st:&mut DCG, loc:&Rc<Loc>) {
  //debug!("{} dirty_alloc: {:?}", engineMsg!(st), loc);
  dirty_pred_observers(st, loc);
  //let stackLen = st.stack.len() ;
  let pred_locs : Vec<Rc<Loc>> = lookup_abs(st, loc).preds_alloc() ;
  for pred_loc in pred_locs {
    if st.root.eq (&pred_loc) { panic!("root in preds") } // Todo-Question: Dead code?
    else {
      let stop : bool = {
        // The stop bit communicates information from st for use below.
        //debug!("{} dirty_alloc: edge {:?} --> {:?} ...", engineMsg(Some(stackLen)), &pred_loc, &loc);
        let succ = get_succ_mut(st, &pred_loc, Effect::Allocate, &loc) ;
        if succ.dirty { true } else {
          //debug!("{} dirty_alloc: edge {:?} --> {:?} marked dirty", engineMsg(Some(stackLen)), &pred_loc, &loc);
          replace(&mut succ.dirty, true);
          false
        }} ;
      if !stop {
        dirty_pred_observers(st,&pred_loc);
      } else {
        //debug!("{} dirty_alloc: early stop", engineMsg(Some(stackLen)))
      }
    }
  }
  if false /* XXX Check make this better, as a statically/dynamically-set flag? */ {
    wf::check_stack_is_clean(st)
  }
}

fn set_<T:Eq+Debug> (st:&mut DCG, cell:AbsArt<T,Loc>, val:T) {
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
      dirty_alloc(st, loc)
    }
    else { }
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
  //type Name : Debug+PartialEq+Eq+Hash+Clone;
  type Loc  : Debug+PartialEq+Eq+Hash+Clone;
  
  fn new () -> Self ;
    
  /// Creates or re-enters a given namespace; performs the given computation there.
  fn ns<T,F> (g: &RefCell<DCG>, Name, body:F) -> T where F:FnOnce() -> T;
  
  /// Enters a special "namespace" where all name uses are ignored; instead, Adapton uses structural identity.
  fn structural<T,F> (g: &RefCell<DCG>, body:F) -> T where F:FnOnce() -> T;
  
  fn cnt<Res,F> (g: &RefCell<DCG>, body:F) -> (Res, Cnt) where F:FnOnce() -> Res;
  
  /// Creates immutable, eager articulation.
  fn put<T:Eq+Debug+Clone> (self:&mut Self, T) -> AbsArt<T,Self::Loc> ;
  
  /// Creates a mutable articulation.
  fn cell<T:Eq+Debug+Clone+Hash> (self:&mut Self, Name, T) -> AbsArt<T,Self::Loc> ;
  
  /// Mutates a mutable articulation.
  fn set<T:Eq+Debug+Clone> (self:&mut Self, AbsArt<T,Self::Loc>, T) ;
  
  /// Creates an articulated computation.
  fn thunk<Arg:Eq+Hash+Debug+Clone,Spurious:Clone,Res:Eq+Debug+Clone+Hash>
    (self:&mut Self,
     id:ArtIdChoice,
     prog_pt:ProgPt,
     fn_box:Rc<Box< Fn(Arg, Spurious) -> Res >>,
     arg:Arg, spurious:Spurious)
     -> AbsArt<Res,Self::Loc> ;
  
  /// Demand & observe arts (all kinds): force
  // fn force<T:Eq+Debug+Clone+Hash+GMLog<Self>> (self:&mut Self, &Art<T,Self::Loc>) -> T ;
  fn force<T:Eq+Debug+Clone+Hash> (g:&RefCell<DCG>, &AbsArt<T,Self::Loc>) -> T ;
}

impl Adapton for DCG {
  //type Name = Name;
  type Loc  = Loc;

  fn new () -> DCG {
    let path = Rc::new(Path::Empty);
    let root = { // Todo-Next: Kill this; new design tests for empty stacks to detect when current node is root node
      let path   = path.clone();
      let symbol = Rc::new(NameSym::Root);
      let hash   = my_hash(&symbol);
      let name   = Name{symbol:symbol,hash:hash};
      let id     = Rc::new(ArtId::Nominal(name));
      let hash   = my_hash(&(&path,&id));
      let loc    = Rc::new(Loc{path:path.clone(),id:id,hash:hash});
      loc
    } ;
    let mut stack = Vec::new() ;
    if false { // Todo-Minor: Kill this code once we are happy with new design.
      stack.push( Frame{loc:root.clone(),
                        //path:root.path.clone(),
                        succs:Vec::new()} ) ;
    }
    let table = HashMap::new ();
    let outfile =
      match env::var("ADAPTON_WRITE_GMLOG")  {
        Ok(ref filename) if filename.len() > 0 => 
          Some(OpenOptions::new()
               .create(true)
               .write(true)
               .append(true)
               .open(filename)
               .unwrap()),
        _ => None
      } ;
    drop(outfile) ; // Do something with this in the future
    DCG {
      flags : Flags {
        use_purity_optimization       : { match env::var("ADAPTON_NO_PURITY")  { Ok(_) => false, _ => true } },
        ignore_nominal_use_structural : { match env::var("ADAPTON_STRUCTURAL") { Ok(_) => true,  _ => false } },
        check_dcg_is_wf               : { match env::var("ADAPTON_CHECK_DCG")  { Ok(_) => true,  _ => false } },
        write_dcg                     : { match env::var("ADAPTON_WRITE_DCG")  { Ok(_) => true,  _ => false } },
        gmlog_dcg                     : { match env::var("ADAPTON_GMLOG_DCG")  { Ok(_) => true,  _ => false } },
      },
      root  : root, // Todo-Next: Remove this
      table : table,
      stack : stack,
      path  : path,
      cnt   : Cnt::zero (),
      dcg_count : 0,
      dcg_hash : 0, // XXX This makes assumptions about hashing implementation
      //gmfile : outfile,
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
      //println!("{:?}", st.path);
      saved
    };
    let x = body() ;
    g.borrow_mut().path = saved ;
    x
  }

  fn cnt<Res,F> (g: &RefCell<DCG>, body:F) -> (Res,Cnt)
    where F:FnOnce() -> Res
  {    
    let c : Cnt = {
      let st = &mut *g.borrow_mut();
      let c = st.cnt.clone() ;
      st.cnt = Zero::zero();
      c
    };
    let x = body() ;
    let d : Cnt = {
      let st = &mut *g.borrow_mut();      
      let d : Cnt = st.cnt.clone() ;
      st.cnt = c + d.clone();
      d
    };
    (x, d)
  }

  fn put<T:Eq> (self:&mut DCG, x:T) -> AbsArt<T,Self::Loc> { AbsArt::Rc(Rc::new(x)) }

  fn cell<T:Eq+Debug+Clone+Hash //+gm::GMLog<Self>
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
      let loc  = Rc::new(Loc{path:path,id:id,hash:hash});
      //debug!("{} alloc cell: {:?} <--- {:?}", engineMsg!(self), &loc, &val);
      let (do_dirty, do_set, succs, do_insert) =
        if self.table.contains_key(&loc) {
          let node : &Box<Node<T>> = res_node_of_loc(self, &loc) ;
          match **node {
            Node::Mut(_)       => { (false, true,  None, false) }
            Node::Comp(ref nd) => { (true,  false, Some(nd.succs.clone()),  true ) }
            Node::Pure(_)      => { (false, false, None, false) }
            Node::Unused       => unreachable!()
          }} else                 { (false, false, None, true ) } ;
      if do_set || do_insert {
        if self.flags.gmlog_dcg {
          // gm::startdframe(self, &format!("cell {:?}", loc), None);
          // gm::addnode(self, &format!("{:?}",loc), "cell", "", Some(&format!("cell {:?}", loc)));
          //val.log_snapshot(self, &format!("{:?}",loc), None);
        }
      }
      if do_dirty { dirty_alloc(self, &loc) } ;
      if do_set   { set_(self, AbsArt::Loc(loc.clone()), val.clone()) } ;
      match succs { Some(succs) => revoke_succs(self, &loc, &succs), None => () } ;
      if do_insert {
        let node = if is_pure { Node::Pure(PureNode{val:val.clone()}) } else {
          Node::Mut(MutNode{
            preds:Vec::new(),
            val:val.clone(),
          })} ;
        self.cnt.create += 1;
        //println!("create: {:?}", &loc);
        self.table.insert(loc.clone(), Box::new(node));
      } ;
      //let stackLen = self.stack.len() ;
      if ! is_pure { match self.stack.last_mut() { None => (), Some(frame) => {
        let succ =
          Succ{loc:loc.clone(),
               dep:Rc::new(Box::new(AllocDependency{val:val})),
               effect:Effect::Allocate,
               dirty:false};
        //debug!("{} alloc cell: edge: {:?} --> {:?}", engineMsg(Some(stackLen)), &frame.loc, &loc);
        frame.succs.push(succ)
      }}} ;
      wf::check_dcg(self);
      AbsArt::Loc(loc)
    }

  fn set<T:Eq+Debug> (self:&mut Self, cell:AbsArt<T,Self::Loc>, val:T) {
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
        if false {
          //debug!("{} alloc thunk: Structural {:?}\n{} ;; {:?}\n{} ;; {:?}",
          // engineMsg!(self), &loc,
          // engineMsg!(self), &prog_pt.symbol,
          //engineMsg!(self), &arg);
        } ;
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
            //let pred = frame.loc.clone();
            let succ =
              Succ{loc:loc.clone(),
                   dep:Rc::new(Box::new(NoDependency)),
                   effect:Effect::Allocate,
                   dirty:false};
            frame.succs.push(succ)
          }};
        //println!("create: {:?} {:?} {:?}", &loc, &prog_pt, &arg);
        let producer : Box<Producer<Res>> =
          Box::new(App{prog_pt:prog_pt,
                       fn_box:fn_box,
                       arg:arg.clone(),
                       spurious:spurious.clone()})
          ;
        if self.flags.gmlog_dcg {
          // gm::startdframe(self, &format!("structural thunk {:?}", loc), None);
          // gm::addnode(self, &format!("{:?}",loc), "structural-thunk", "", Some(&format!("thunk {:?}", &producer)));
        } ;
        let node : CompNode<Res> = CompNode{
          preds:Vec::new(),
          succs:Vec::new(),
          producer:producer,
          res:None,
        } ;
        self.cnt.create += 1;
        self.table.insert(loc.clone(),
                          Box::new(Node::Comp(node)));
        wf::check_dcg(self);
        AbsArt::Loc(loc)
      },

      ArtIdChoice::Nominal(nm) => {
        wf::check_dcg(self);
        let loc = loc_of_id(current_path(self),
                            Rc::new(ArtId::Nominal(nm)));
        //debug!("{} alloc thunk: Nominal {:?}\n{} ;; {:?}\n{} ;; {:?}",
        //engineMsg!(self), &loc,
        //engineMsg!(self), &prog_pt.symbol,
        //engineMsg!(self), &arg);
        let producer : App<Arg,Spurious,Res> =
          App{prog_pt:prog_pt.clone(),
              fn_box:fn_box,
              arg:arg.clone(),
              spurious:spurious.clone(),
          }
        ;
        //let stackLen = self.stack.len() ;
        let (do_dirty, do_insert) = { match self.table.get_mut( &loc ) {
          None => {
            // do_dirty=false; do_insert=true
            (false, true)
          },
          Some(node) => {
            let node: &mut Box<GraphNode> = node ;
            let res_nd: &mut Box<Node<Res>> = unsafe { transmute::<_,_>( node ) } ;
            match ** res_nd {
              Node::Pure(_)=> unreachable!(),
              Node::Mut(_) => {
                //panic!("TODO-Sometime: {:?}: Was mut, now a thunk: {:?} {:?}", &loc, prog_pt, &arg)
                (true, true) // Todo: Do we need to preserve preds?
              },
              Node::Comp(ref mut comp_nd) => {
                let equal_producer_prog_pts : bool =
                  comp_nd.producer.prog_pt().eq( producer.prog_pt() ) ;
                //debug!("{} alloc thunk: Nominal match: equal_producer_prog_pts: {:?}",
                // engineMsg(Some(stackLen)), equal_producer_prog_pts);
                if equal_producer_prog_pts { // => safe cast to Box<Consumer<Arg>>
                  let app: &mut Box<App<Arg,Spurious,Res>> =
                    unsafe { transmute::<_,_>( &mut comp_nd.producer ) }
                  ;
                  //debug!("{} alloc thunk: Nominal match: app: {:?}", engineMsg(Some(stackLen)), app);
                  if app.get_arg() == arg {
                    // Case: Same argument; Nothing else to do:
                    // do_dirty=false; do_insert=false
                    (false, false)
                  }
                  else { // Case: Not the same argument:
                    //debug!("{} alloc thunk: Nominal match: replacing {:?} ~~> {:?}",
                    //       engineMsg(Some(stackLen)), app.get_arg(), arg);
                    app.consume(arg.clone()); // overwrite the old argument
                    comp_nd.res = None ; // clear the cache
                    // do_dirty=true; do_insert=false
                    (true, false)
                  }}
                else {
                  panic!("TODO-Sometime: Memozied functions not equal!\nFunction was: {:?} \tProducer: {:?}\nFunction now: {:?} \tProducer: {:?}\nCommon location: {:?}\nHint:Consider using distinct namespaces, via `Adapton::ns`\n",
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
        if do_dirty {
          //debug!("{} alloc thunk: dirty_alloc {:?}.", engineMsg!(self), &loc);
          dirty_alloc(self, &loc);
        } else {
          //debug!("{} alloc thunk: No dirtying.", engineMsg!(self))
        } ;
        match self.stack.last_mut() { None => (), Some(frame) => {
          //let pred = frame.loc.clone();
          //debug!("{} alloc thunk: edge {:?} --> {:?}", engineMsg(Some(stackLen)), &pred, &loc);
          let succ =
            Succ{loc:loc.clone(),
                 dep:Rc::new(Box::new(AllocDependency{val:arg.clone()})),
                 effect:Effect::Allocate,
                 dirty:false};
          frame.succs.push(succ)
        }};
        if do_insert {
          if self.flags.gmlog_dcg {
            // gm::startdframe(self, &format!("nominal thunk {:?}", loc), None);
            // gm::addnode(self, &format!("{:?}",loc), "nominal-thunk", "", Some(&format!("thunk {:?}", &producer)));
          } ;
          let node : CompNode<Res> = CompNode{
            preds:Vec::new(),
            succs:Vec::new(),
            producer:Box::new(producer),
            res:None,
          } ;
          self.cnt.create += 1;
          //println!("create: {:?}", &loc);
          self.table.insert(loc.clone(),
                            Box::new(Node::Comp(node)));
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
        let (is_comp, is_pure, cached_result) : (bool, bool, Option<T>) = {
          let st : &mut DCG = &mut *g.borrow_mut();
          let is_pure_opt : bool = st.flags.use_purity_optimization ;
          let node : &mut Node<T> = res_node_of_loc(st, &loc) ;
          match *node {
            Node::Pure(ref mut nd) => (false, true, Some(nd.val.clone())),
            Node::Mut(ref mut nd)  => (false, false, Some(nd.val.clone())),
            Node::Comp(ref mut nd) => {
              let is_pure = match *loc.id {
                ArtId::Structural(_) => nd.succs.len() == 0 && is_pure_opt,
                ArtId::Nominal(_)    => false } ;
              (true, is_pure, nd.res.clone()) },
            _ => panic!("undefined")
          }
        } ;
        let result = match cached_result {
          None => {
            //println!("force {:?}: cache empty", &loc);
            assert!(is_comp);
            //drop(st);            
            loc_produce(g, &loc)
          },
          Some(ref res) => {
            if is_comp {
              ////debug!("{} force {:?}: cache holds {:?}.  Using change propagation.", engineMsg!(st), &loc, &res);
              // ProducerDep change-propagation precondition:
              // loc is a computational node:
              //drop(st);
              let _ = ProducerDep{res:res.clone()}.clean(g, &loc) ;
              ////debug!("{} force {:?}: result changed?: {}", engineMsg!(self), &loc, res.changed) ;
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
              ////debug!("{} force {:?}: not a computation. (no change prop necessary).", engineMsg!(self), &loc);
              res.clone()
            }
          }
        } ;
        let st : &mut DCG = &mut *g.borrow_mut() ;
        if !is_pure { match st.stack.last_mut() { None => (), Some(frame) => {
          let succ =
            Succ{loc:loc.clone(),
                 dep:Rc::new(Box::new(ProducerDep{res:result.clone()})),
                 effect:Effect::Observe,
                 dirty:false};
          frame.succs.push(succ);                  
        }}} ;
        wf::check_dcg(st);
        // if self.flags.gmlog_dcg {
        //gm::startdframe(self, &format!("force {:?}", loc), None);
        //result.log_snapshot(self, &format!("{:?}",loc), None);
        // }
        result
      }
    }}
}

/// The term "Art" stands for two things here: "Adapton return type",
/// and "Articulation point, for 'articulating' incremental change".
/// The concept of an "Art" also abstracts over whether the producer
/// is eager (like a ref cell) or lazy (like a thunk).
#[derive(Clone,PartialEq,Eq,Hash,Debug)]
pub struct Art<T> {
  art:EnumArt<T>,
}

#[derive(Clone)]
enum EnumArt<T> {
  Rc(Rc<T>),    // No entry in table. No dependency tracking.
  Loc(Rc<Loc>), // Location in table.
  Force(Rc<Force<T>>), // A closure that is 'force-able'
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
    let mut hasher = SipHasher::new();
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

/// Initializes global state with a fresh DCG-based engine; returns the old engine.
/// The DCG is the central implementation structure behind Adapton.
/// At a high level, it consists of a data dependence graph (the "demanded computation graph"), and an associated memoization table.
pub fn init_dcg () -> Engine { init_engine(Engine::DCG(RefCell::new(DCG::new()))) }

/// Initializes global state with a ("fresh") Naive engine; returns the old engine.
/// The naive engine is stateless, and performs no memoization and builds no dependence graphs.
/// (Since the naive engine is stateless, every instance of the naive engine is equivalent to a "fresh" one).
pub fn init_naive () -> Engine { init_engine(Engine::Naive) }

/// Initializes global state with a fresh DCG-based engine; returns the old engine
pub fn use_engine (engine: Engine) -> Engine {
  use std::mem;
  let mut engine = engine;
  GLOBALS.with(|g| {
    mem::swap(&mut g.borrow_mut().engine, &mut engine);
  });
  return engine
}

pub fn init_engine (engine: Engine) -> Engine {
  use_engine(engine)
}

pub fn name_unit () -> Name {
  ROOT_NAME.with(|r|r.clone())
}

pub fn name_of_usize (u:usize) -> Name {
  let h = my_hash(&u) ;
  let s = NameSym::Usize(u) ;
  Name{ hash:h, symbol:Rc::new(s) }
}

pub fn name_of_isize (i:isize) -> Name {
  let h = my_hash(&i) ;
  let s = NameSym::Isize(i) ;
  Name{ hash:h, symbol:Rc::new(s) }
}

pub fn name_of_string (s:String) -> Name {
  let h = my_hash(&s);
  let s = NameSym::String(s) ;
  Name{ hash:h, symbol:Rc::new(s) }
}

pub fn name_of_str (s:&'static str) -> Name {
  let h = my_hash(&s);
  let s = NameSym::String(s.to_string()) ;
  Name{ hash:h, symbol:Rc::new(s) }
}

pub fn name_pair (n1:Name, n2:Name) -> Name {
  let h = my_hash( &(n1.hash,n2.hash) ) ;
  let p = NameSym::Pair(n1.symbol, n2.symbol) ;
  Name{ hash:h, symbol:Rc::new(p) }
}

pub fn name_fork (n:Name) -> (Name, Name) {
  let h1 = my_hash( &(&n, 11111111) ) ; // TODO-Later: make this hashing better.
  let h2 = my_hash( &(&n, 22222222) ) ;
  ( Name{ hash:h1,
          symbol:Rc::new(NameSym::ForkL(n.symbol.clone())) } ,
    Name{ hash:h2,
          symbol:Rc::new(NameSym::ForkR(n.symbol)) } )    
}

pub fn name_fork3 (n:Name)
                   -> (Name,Name,Name)
{
  let (n1,n)  = name_fork(n);
  let (n2,n3) = name_fork(n);
  (n1,n2,n3)
}

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

pub fn cnt<Res,F> (body:F) -> (Res, Cnt)
  where F:FnOnce() -> Res {
    GLOBALS.with(|g| {
      match g.borrow().engine {
        Engine::DCG(ref dcg) => <DCG as Adapton>::cnt(dcg,body),
        Engine::Naive => ((body)(), Cnt::zero())
      }
    })
  }

/// Creates immutable, eager articulation.
pub fn put<T:Eq+Debug+Clone> (val:T) -> Art<T> {
  Art{art:EnumArt::Rc(Rc::new(val))}
}

/// Creates a mutable articulation.
pub fn cell<T:Hash+Eq+Debug+Clone> (n:Name, val:T) -> Art<T> {
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
pub fn set<T:Eq+Debug+Clone> (a:&Art<T>, val:T) {
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

/// Creates an articulated computation.
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

/// Demand & observe arts (all kinds): force
pub fn force<T:Hash+Eq+Debug+Clone> (a:&Art<T>) -> T {
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

pub fn engine_is_naive () -> bool {
  GLOBALS.with(|g| {
    match g.borrow().engine {
      Engine::DCG(_) => false,
      Engine::Naive  => true
    }})    
}

pub fn engine_is_dcg () -> bool {
  GLOBALS.with(|g| {
    match g.borrow().engine {
      Engine::DCG(_) => true,
      Engine::Naive  => false
    }})
}
