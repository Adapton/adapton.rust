use std::fmt::Debug;
use std::collections::HashMap;
use std::mem::replace;
use std::mem::transmute;
use std::rc::Rc;
use std::fmt;
use std::marker::PhantomData;
use std::fmt::{Formatter,Result};
use std::hash::{Hash,Hasher,SipHasher};
use std::num::Zero;
use std::env;
use std::fs::{OpenOptions,File};
use std::cell::RefCell;
use std::thread;

use macros::*;
use adapton_sigs::*;
use gm;

thread_local!(static GLOBALS: RefCell<Globals> = RefCell::new(Globals{engine:Engine::Naive}));

const engineMsgStr : &'static str = "adapton::engine:";

fn engineMsg (indent:Option<usize>) -> String {
    match indent {
        None => "adapton::engine:".to_string(),
        Some(indent) => {
            let mut indent_str = "".to_string() ;
            for i in 1..indent {
                indent_str.push_str("···〉")
            };
            ("adapton::engine:".to_string() + &indent_str)
    }}}

macro_rules! engineMsg {
    ( $st:expr ) => {{
        engineMsg(Some($st.stack.len()))
    }}
}

// Names provide a symbolic way to identify nodes.
#[derive(Hash,PartialEq,Eq,Clone)]
pub struct Name {
    hash : u64, // hash of symbol
    symbol : Rc<NameSym>,
}
impl Debug for Name {
    fn fmt(&self, f:&mut Formatter) -> Result { self.symbol.fmt(f) }
}

// Each location identifies a node in the DCG.
#[derive(Hash,PartialEq,Eq,Clone)]
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

// enum MemoRegime {
//   UserDefined,
//   IgnoreNominal,
//   IgnoreThunks,  
// }

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

#[derive(Debug)]
pub enum Engine {
  DCG(DCG),
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
  gmfile : Option<File>,
}

impl Hash  for     DCG { fn hash<H>(&self, _state: &mut H) where H: Hasher { unimplemented!() }}
impl Eq    for     DCG { }
impl PartialEq for DCG { fn eq(&self, _other:&Self) -> bool { unimplemented!() } }
impl Clone for     DCG { fn clone(&self) -> Self { unimplemented!() } }

// NameSyms: For a general semantics of symbols, see Chapter 31 of PFPL 2nd Edition. Harper 2015:
// http://www.cs.cmu.edu/~rwh/plbook/2nded.pdf
//
#[derive(Hash,PartialEq,Eq,Clone)]
enum NameSym {
    Root, // Root identifies the outside environment of Rust code.
    String(String), // Strings encode globally-unique symbols.
    Usize(usize),   // USizes encode globally-unique symbols.
    Pair(Rc<NameSym>,Rc<NameSym>), // A pair of unique symbols, interpeted as a symbol, is unique
    ForkL(Rc<NameSym>), // Left projection of a unique symbol is unique
    ForkR(Rc<NameSym>), // Right projection of a unique symbol is unique
    //Rc(Rc<NameSym>),
    //Nil,  // Nil for non-symbolic, hash-based names.
}

impl Debug for NameSym {
    fn fmt(&self, f:&mut Formatter) -> Result {
        match *self {
            NameSym::Root => write!(f, "/"),
            NameSym::String(ref s) => write!(f, "{}", s),
            NameSym::Usize(ref n) => write!(f, "{}", n),
            NameSym::Pair(ref l, ref r) => write!(f, "({:?},{:?})",l,r),
            NameSym::ForkL(ref s) => write!(f, "{:?}.l", s),
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
            Path::Child(ref p, ref n) => write!(f, "{:?}.{:?}", p, n),
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
  // Todo-later(?): Rename `change_prop` to `clean`?
  fn change_prop (self:&Self, st:&mut DCG, loc:&Rc<Loc>) -> DCGRes ;
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
    fn change_prop (self:&Self, _st:&mut DCG, _loc:&Rc<Loc>) -> DCGRes { DCGRes{changed:false} }
}

#[derive(Debug)]
struct AllocDependency<T> { val:T }
impl<T:Debug> DCGDep for AllocDependency<T> {
    fn change_prop (self:&Self, _st:&mut DCG, _loc:&Rc<Loc>) -> DCGRes { DCGRes{changed:true} } // TODO-Later: Make this a little better.
}


trait ShapeShifter {
    fn be_node<'r> (self:&'r mut Self) -> &'r mut Box<GraphNode> ;
}

// impl fmt::Debug for GraphNode {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "(GraphNode)")
//     }
// }

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
    fn produce(self:&Self, st:&mut DCG) -> Res;
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
    fn_box:   Rc<Box<Fn(&mut DCG, Arg, Spurious) -> Res>>,
    arg:      Arg,
    spurious: Spurious,
}

// ---------- App implementation of Debug and Hash

impl<Arg:Debug,Spurious,Res> Debug for App<Arg,Spurious,Res> {
    fn fmt(&self, f: &mut Formatter) -> Result { self.prog_pt.fmt(f) ; self.arg.fmt(f) }
}

impl<Arg:Hash+Debug,Spurious,Res> Hash for App<Arg,Spurious,Res> {
    fn hash<H>(&self, state: &mut H) where H: Hasher { (&self.prog_pt,&self.arg).hash(state) }
}

// ---------- App implementation of Producer and Consumer traits:

impl<Arg:'static+PartialEq+Eq+Clone+Debug,Spurious:'static+Clone,Res:'static+Debug+Hash> Producer<Res>
    for App<Arg,Spurious,Res>
{
    fn produce(self:&Self, st:&mut DCG) -> Res {
        let f = self.fn_box.clone() ;
        st.cnt.eval += 1 ;
        debug!("{} producer begin: ({:?} {:?})", engineMsg!(st), &self.prog_pt, &self.arg);
        let res = f (st,self.arg.clone(),self.spurious.clone()) ;
        debug!("{} producer end: ({:?} {:?}) produces {:?}", engineMsg!(st), &self.prog_pt, &self.arg, &res);
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
    use std::io;
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
    let mut frame_num = 0;
    for frame in st.stack.iter() {
      writeln!(&mut writer, "\"{:?}\" [color=blue,penwidth=10];", frame.loc);
      for succ in frame.succs.iter() {
        writeln!(&mut writer, "\"{:?}\" -> \"{:?}\" [color=blue,weight=10,penwidth=10];", &frame.loc, &succ.loc).unwrap();
      }
      frame_num += 1;
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
      self.producer.fmt(f)
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
fn produce<Res:'static+Debug+PartialEq+Eq+Clone+Hash>(st:&mut DCG, loc:&Rc<Loc>) -> Res
{
    debug!("{} produce begin: {:?}", engineMsg!(st), &loc);
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
    let res = producer.produce( st ) ;
    st.path = prev_path ;
    let frame = match st.stack.pop() {
        None => panic!("expected Some _: stack invariants are broken"),
        Some(frame) => frame
    } ;
  assert!( &frame.loc == loc );
  let mut succ_idx = 0;
    for succ in &frame.succs {
      debug!("{} produce: edge: {:?} --{:?}--dirty?:{:?}--> {:?}", engineMsg!(st), &loc, &succ.effect, &succ.dirty, &succ.loc);
      let (effect, is_weak) =
        match succ.effect {
          //Effect::Observe => ("observe", true),
          Effect::Observe => ("observe", false), // XXX
          Effect::Allocate => ("allocate", false)
        } ;
      if st.flags.gmlog_dcg {
        gm::startdframe(st, &format!("{:?}--{}->{:?}", loc, effect, succ.loc), None);
        gm::addedge(st, &format!("{:?}",loc), &format!("{:?}",succ.loc),
                    &format!("{}",succ_idx),
                    effect, "", None, is_weak);
      }
      succ_idx += 1;
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
    debug!("{} produce end: {:?} produces {:?}", engineMsg!(st), &loc, &res);
    res
}



// ---------- DCGDep implementation:

#[derive(Debug)]
struct ProducerDep<T> { res:T }

fn change_prop_comp<Res:'static+Sized+Debug+PartialEq+Clone+Eq+Hash>
    (st:&mut DCG, this_dep:&ProducerDep<Res>, loc:&Rc<Loc>, cache:Res, succs:Vec<Succ>) -> DCGRes
{
    for succ in succs.iter() {
        let dirty = { get_succ_mut(st, loc, succ.effect.clone(), &succ.loc).dirty } ;
        if dirty {
            let succ_dep = & succ.dep ;
            let res = succ_dep.change_prop(st, &succ.loc) ;
            if res.changed {
                debug!("{} change_prop end (1/2): {:?} has a changed succ dependency: {:?}. Begin re-production:", engineMsg!(st), loc, &succ.loc);
                let result : Res = produce( st, loc ) ;
                let changed = result != this_dep.res ;
                debug!("{} change_prop end (2/2): {:?} has a changed succ dependency: {:?}. End re-production.", engineMsg!(st), loc, &succ.loc);
                return DCGRes{changed:changed}
            }
            else {
              // BUGFIX: Set this flag back to false after change
              // propagation is finished.  Otherwise, the code that
              // omits this would violate the post condition of
              // change propagation (viz., all succs are clean,
              // transitively).
              st.cnt.clean += 1 ;                
              get_succ_mut(st, loc, succ.effect.clone(), &succ.loc).dirty = false ;
            }
        }
    } ;
    // BUGFIX: Do this comparison here; do not return 'false' unconditionally, as before!
    let changed = this_dep.res != cache ;
    debug!("{} change_prop end: {:?} is clean.. Dependency changed?:{}", engineMsg!(st), &loc, changed);
    DCGRes{changed:changed}
}

impl <Res:'static+Sized+Debug+PartialEq+Eq+Clone+Hash>
    DCGDep for ProducerDep<Res>
{
    fn change_prop(self:&Self, st:&mut DCG, loc:&Rc<Loc>) -> DCGRes {
        let stackLen = st.stack.len() ;
        debug!("{} change_prop begin: {:?}", engineMsg!(st), loc);
        let res_succs = { // Handle cases where there is no internal computation to re-compute:
            let node : &mut Node<Res> = res_node_of_loc(st, loc) ;
            match *node {
                Node::Comp(ref nd) => {
                    match nd.res {
                        Some(ref res) => Some((res.clone(), nd.succs.clone ())),
                        None => None
                    }},
                Node::Pure(_) => {
                    debug!("{} change_prop early end: {:?} is Pure(_)", engineMsg(Some(stackLen)), loc);
                    return DCGRes{changed:false}
                },
                Node::Mut(ref nd) => {
                    debug!("{} change_prop early end: {:?} is Mut(_)", engineMsg(Some(stackLen)), loc);
                    return DCGRes{changed:nd.val != self.res}
                },
                _ => panic!("undefined")
            }
        } ;
        match res_succs {
            Some((res,succs)) => change_prop_comp(st, self, loc, res, succs),
            None => {
                let res = produce( st, loc );
                let changed = self.res != res ;
                DCGRes{changed:changed}
            }
        }
    }
}

// ---------- Node implementation:

fn revoke_succs<'x> (st:&mut DCG, src:&Rc<Loc>, succs:&Vec<Succ>) {
  let mut succ_idx = 0;
  for succ in succs.iter() {
    if st.flags.gmlog_dcg {
      gm::startdframe(st, &format!("revoke_succ {:?} {} --> {:?}", src, succ_idx, succ.loc), None);
      gm::remedge(st, &format!("{:?}",src), &format!("{:?}",succ.loc),
                  &format!("{}",succ_idx), "", None);
    } ;
    let succ_node : &mut Box<GraphNode> = lookup_abs(st, &succ.loc) ;
    succ_idx += 1;
    succ_node.preds_remove(src)
  }
}

fn loc_of_id(path:Rc<Path>,id:Rc<ArtId>) -> Rc<Loc> {
    let hash = my_hash(&(&path,&id));
    Rc::new(Loc{path:path,id:id,hash:hash})
}

fn get_succ<'r>(st:&'r DCG, src_loc:&Rc<Loc>, eff:Effect, tgt_loc:&Rc<Loc>) -> &'r Succ {
    let stackLen = st.stack.len() ;
    let nd = st.table.get(src_loc);
    let nd = match nd {
        None => panic!(""),
        Some(nd) => nd
    } ;    
    debug!("{} get_succ_mut: resolving {:?} --{:?}--dirty:?--> {:?}", engineMsg(Some(stackLen)), &src_loc, &eff, &tgt_loc);
    for succ in nd.succs() {
        if (succ.effect == eff) && (&succ.loc == tgt_loc) {
            debug!("{} get_succ_mut:  resolved {:?} --{:?}--dirty:{:?}--> {:?}", engineMsg(Some(stackLen)), &src_loc, &succ.effect, &succ.dirty, &tgt_loc);
            return succ
        } else {}
    } ;
    panic!("tgt_loc is dangling in src_node.dem_succs")
}

// Implement "sharing" of the dirty bit.
// The succ edge is returned as a mutable borrow, to permit checking
// and mutating the dirty bit.
fn get_succ_mut<'r>(st:&'r mut DCG, src_loc:&Rc<Loc>, eff:Effect, tgt_loc:&Rc<Loc>) -> &'r mut Succ {
    let stackLen = st.stack.len() ;
    let nd = lookup_abs( st, src_loc );
    debug!("{} get_succ_mut: resolving {:?} --{:?}--dirty:?--> {:?}", engineMsg(Some(stackLen)), &src_loc, &eff, &tgt_loc);
    for succ in nd.succs_mut().iter_mut() {
        if (succ.effect == eff) && (&succ.loc == tgt_loc) {
            debug!("{} get_succ_mut:  resolved {:?} --{:?}--dirty:{:?}--> {:?}", engineMsg(Some(stackLen)), &src_loc, &succ.effect, &succ.dirty, &tgt_loc);
            return succ
        } else {}
    } ;
    panic!("tgt_loc is dangling in src_node.dem_succs")
}

fn dirty_pred_observers(st:&mut DCG, loc:&Rc<Loc>) {
    debug!("{} dirty_pred_observers: {:?}", engineMsg!(st), loc);
    let stackLen = st.stack.len() ;
    let pred_locs : Vec<Rc<Loc>> = lookup_abs( st, loc ).preds_obs() ;
    let mut dirty_edge_count = 0;
    for pred_loc in pred_locs {
        if st.root.eq (&pred_loc) { panic!("root in preds") } // Todo-Question: Dead code?
        else {
            let stop : bool = {
                // The stop bit communicates information from st for use below.
                debug!("{} dirty_pred_observers: edge {:?} --> {:?} ...", engineMsg(Some(stackLen)), &pred_loc, &loc);
                let succ = get_succ_mut(st, &pred_loc, Effect::Observe, &loc) ;
                if succ.dirty { true } else {
                    dirty_edge_count += 1 ;
                    replace(&mut succ.dirty, true);
                    debug!("{} dirty_pred_observers: edge marked dirty: {:?} --{:?}--dirty:{:?}--> {:?}", engineMsg(Some(stackLen)), &pred_loc, &succ.effect, &succ.dirty, &loc);
                    false
                }} ;
            if !stop {
                dirty_pred_observers(st,&pred_loc);
            } else { debug!("{} dirty_pred_observers: already dirty", engineMsg(Some(stackLen))) }
        }
    }
    st.cnt.dirty += dirty_edge_count ;
}

fn dirty_alloc(st:&mut DCG, loc:&Rc<Loc>) {
    debug!("{} dirty_alloc: {:?}", engineMsg!(st), loc);
    dirty_pred_observers(st, loc);
    let stackLen = st.stack.len() ;
    let pred_locs : Vec<Rc<Loc>> = lookup_abs(st, loc).preds_alloc() ;
    for pred_loc in pred_locs {
        if st.root.eq (&pred_loc) { panic!("root in preds") } // Todo-Question: Dead code?
        else {
            let stop : bool = {
                // The stop bit communicates information from st for use below.
                debug!("{} dirty_alloc: edge {:?} --> {:?} ...", engineMsg(Some(stackLen)), &pred_loc, &loc);
                let succ = get_succ_mut(st, &pred_loc, Effect::Allocate, &loc) ;
                if succ.dirty { true } else {
                    debug!("{} dirty_alloc: edge {:?} --> {:?} marked dirty", engineMsg(Some(stackLen)), &pred_loc, &loc);
                    replace(&mut succ.dirty, true);
                    false
                }} ;
            if !stop {
                dirty_pred_observers(st,&pred_loc);
            } else { debug!("{} dirty_alloc: early stop", engineMsg(Some(stackLen))) }
        }
    }
  if false /* XXX Check make this better, as a statically/dynamically-set flag? */ {
    wf::check_stack_is_clean(st)
  }
}

fn set_<T:Eq+Debug> (st:&mut DCG, cell:MutArt<T,Loc>, val:T) {
    let changed : bool = {
        let node = res_node_of_loc( st, &cell.loc ) ;
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
        dirty_alloc(st, &cell.loc)
    }
    else { }
}


fn current_path (st:&DCG) -> Rc<Path> {
  // if false { // Todo-Minor: Kill this dead code, once we are happy.
  //   match st.stack.last() {
  //       None => panic!(""),
  //       Some(frame) => frame.path.clone()
  //   }
  // } else {
    st.path.clone()
  //}  
}

impl Adapton for DCG {
    type Name = Name;
    type Loc  = Loc;

    fn new () -> DCG {
        let path = Rc::new(Path::Empty);
        let root = {
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
      let mut outfile =
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
      DCG {
        flags : Flags {
          use_purity_optimization       : { match env::var("ADAPTON_NO_PURITY")  { Ok(val) => false, _ => true } },
          ignore_nominal_use_structural : { match env::var("ADAPTON_STRUCTURAL") { Ok(val) => true,  _ => false } },
          check_dcg_is_wf               : { match env::var("ADAPTON_CHECK_DCG")  { Ok(val) => true,  _ => false } },
          write_dcg                     : { match env::var("ADAPTON_WRITE_DCG")  { Ok(val) => true,  _ => false } },
          gmlog_dcg                     : { match env::var("ADAPTON_GMLOG_DCG")  { Ok(val) => true,  _ => false } },
        },
        root  : root, // Todo-Question: Don't need this?
        table : table,
        stack : stack,
        path  : path,
        cnt   : Cnt::zero (),
        dcg_count : 0,
        dcg_hash : 0, // XXX This makes assumptions about hashing implementation
        gmfile : outfile,
      }
    }

  fn gmlog (self:& mut DCG) -> Option<& mut File> {
    self.gmfile.as_mut()
  }
  
    fn name_of_string (self:&mut DCG, sym:String) -> Name {
        let h = my_hash(&sym);
        let s = NameSym::String(sym) ;
        Name{ hash:h, symbol:Rc::new(s) }
    }

    fn name_of_usize (self:&mut DCG, sym:usize) -> Name {
        let h = my_hash(&sym) ;
        let s = NameSym::Usize(sym) ;
        Name{ hash:h, symbol:Rc::new(s) }
    }

    fn name_pair (self: &mut DCG, fst: Name, snd: Name) -> Name {
        let h = my_hash( &(fst.hash,snd.hash) ) ;
        let p = NameSym::Pair(fst.symbol, snd.symbol) ;
        Name{ hash:h, symbol:Rc::new(p) }
    }

    fn name_fork (self:&mut DCG, nm:Name) -> (Name, Name) {
        let h1 = my_hash( &(&nm, 11111111) ) ; // TODO-Later: make this hashing better.
        let h2 = my_hash( &(&nm, 22222222) ) ;
        ( Name{ hash:h1,
                symbol:Rc::new(NameSym::ForkL(nm.symbol.clone())) } ,
          Name{ hash:h2,
                symbol:Rc::new(NameSym::ForkR(nm.symbol)) } )
    }

    fn structural<T,F> (self: &mut Self, body:F) -> T where F:FnOnce(&mut Self) -> T {
      let saved = self.flags.ignore_nominal_use_structural ;
      self.flags.ignore_nominal_use_structural = true ;
      let x = body(self) ;
      self.flags.ignore_nominal_use_structural = saved;
      x
    }
  
    fn ns<T,F> (self: &mut Self, nm:Name, body:F) -> T where F:FnOnce(&mut Self) -> T {
      // if false { // Todo-Minor: Kill this dead code, once we are happy.
      //   let path = match self.stack.last() { None => unreachable!(), Some(frame) => frame.path.clone() } ;
      //   let path_body = Rc::new(Path::Child(path, nm)) ;
      //   let path_pre = match self.stack.last_mut() { None => unreachable!(), Some(frame) => replace(&mut frame.path, path_body) } ;
      //   let x = body(self) ;
      //   let path_body = match self.stack.last_mut() { None => unreachable!(), Some(frame) => replace(&mut frame.path, path_pre) } ;
      //   drop(path_body);
      //   x
      // } else {
        let base_path = self.path.clone();
        self.path = Rc::new(Path::Child(self.path.clone(), nm)) ; // Todo-Minor: Avoid this clone.
        let x = body(self) ;
        self.path = base_path ;
        x
      //}
    }

  fn cnt<Res,F> (self: &mut Self, body:F) -> (Res,Cnt)
    where F:FnOnce(&mut Self) -> Res
  {    
    let c : Cnt = self.cnt.clone() ;
    self.cnt = Zero::zero();
    let x = body(self) ;
    let d : Cnt = self.cnt.clone() ;
    self.cnt = c + d.clone();
    (x, d)
  }

    fn put<T:Eq> (self:&mut DCG, x:T) -> Art<T,Self::Loc> { Art::Rc(Rc::new(x)) }

    fn cell<T:Eq+Debug+Clone+Hash+gm::GMLog<Self>
        +'static // TODO-Later: Needed on T because of lifetime issues.
        >
        (self:&mut DCG, nm:Self::Name, val:T) -> MutArt<T,Self::Loc> {
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
            debug!("{} alloc cell: {:?} <--- {:?}", engineMsg!(self), &loc, &val);
            let (do_dirty, do_set, succs, do_insert) =
                if self.table.contains_key(&loc) {
                    let node : &Box<Node<T>> = res_node_of_loc(self, &loc) ;
                    match **node {
                      Node::Mut(ref nd)  => { (false, true,  None, false) }
                      Node::Comp(ref nd) => { (true,  false, Some(nd.succs.clone()),  true ) }
                      Node::Pure(ref nd) => { (false, false, None, false) }
                      Node::Unused       => unreachable!()
                    }} else                 { (false, false, None, true ) } ;
          if do_set || do_insert {
            if self.flags.gmlog_dcg {
              gm::startdframe(self, &format!("cell {:?}", loc), None);
              gm::addnode(self, &format!("{:?}",loc), "cell", "", Some(&format!("cell {:?}", loc)));
              val.log_snapshot(self, &format!("{:?}",loc), None);
            }
            }
            if do_dirty { dirty_alloc(self, &loc) } ;
            if do_set   { set_(self, MutArt{loc:loc.clone(), phantom:PhantomData}, val.clone()) } ;
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
            let stackLen = self.stack.len() ;
            if ! is_pure { match self.stack.last_mut() { None => (), Some(frame) => {
                let succ =
                    Succ{loc:loc.clone(),
                         dep:Rc::new(Box::new(AllocDependency{val:val})),
                         effect:Effect::Allocate,
                         dirty:false};
                debug!("{} alloc cell: edge: {:?} --> {:?}", engineMsg(Some(stackLen)), &frame.loc, &loc);
                frame.succs.push(succ)
            }}} ;
            wf::check_dcg(self);
            MutArt{loc:loc,phantom:PhantomData}
        }

    fn set<T:Eq+Debug> (self:&mut Self, cell:MutArt<T,Self::Loc>, val:T) {
        wf::check_dcg(self);
        assert!( self.stack.is_empty() ); // => outer layer has control.
        set_(self, cell, val);
        wf::check_dcg(self);
    }

    fn thunk<Arg:Eq+Hash+Debug+Clone+'static,Spurious:'static+Clone,Res:Eq+Debug+Clone+Hash+'static>
        (self:&mut DCG,
         id:ArtIdChoice<Self::Name>,
         prog_pt:ProgPt,
         fn_box:Rc<Box<Fn(&mut DCG, Arg, Spurious) -> Res>>,
         arg:Arg, spurious:Spurious)
         -> Art<Res,Self::Loc>
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
                Art::Rc(Rc::new(fn_box(self,arg,spurious)))
            },

            ArtIdChoice::Structural => {
                wf::check_dcg(self);
                let hash = my_hash (&(&prog_pt, &arg)) ;
                let loc = loc_of_id(current_path(self),
                                    Rc::new(ArtId::Structural(hash)));
                if false {
                    debug!("{} alloc thunk: Structural {:?}\n{} ;; {:?}\n{} ;; {:?}",
                             engineMsg!(self), &loc,
                             engineMsg!(self), &prog_pt.symbol,
                             engineMsg!(self), &arg);
                } ;
                {   // If the node exists, return early.
                    let node = self.table.get_mut(&loc);
                    match node { None    => { },
                                 Some(_) => { return Art::Loc(loc) }, // Nothing to do; it already exists.
                    }
                } ;
                // assert: node does not exist.
                match self.stack.last_mut() {
                    None => (),
                    Some(frame) => {
                        let pred = frame.loc.clone();
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
                  gm::startdframe(self, &format!("structural thunk {:?}", loc), None);
                  gm::addnode(self, &format!("{:?}",loc), "structural-thunk", "", Some(&format!("thunk {:?}", &producer)));
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
                Art::Loc(loc)
            },

            ArtIdChoice::Nominal(nm) => {
                wf::check_dcg(self);
                let loc = loc_of_id(current_path(self),
                                    Rc::new(ArtId::Nominal(nm)));
                debug!("{} alloc thunk: Nominal {:?}\n{} ;; {:?}\n{} ;; {:?}",
                         engineMsg!(self), &loc,
                         engineMsg!(self), &prog_pt.symbol,
                         engineMsg!(self), &arg);
                let producer : App<Arg,Spurious,Res> =
                    App{prog_pt:prog_pt.clone(),
                        fn_box:fn_box,
                        arg:arg.clone(),
                        spurious:spurious.clone(),
                    }
                ;
                let stackLen = self.stack.len() ;
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
                                debug!("{} alloc thunk: Nominal match: equal_producer_prog_pts: {:?}",
                                       engineMsg(Some(stackLen)), equal_producer_prog_pts);
                                if equal_producer_prog_pts { // => safe cast to Box<Consumer<Arg>>
                                    let app: &mut Box<App<Arg,Spurious,Res>> =
                                        unsafe { transmute::<_,_>( &mut comp_nd.producer ) }
                                    ;
                                    debug!("{} alloc thunk: Nominal match: app: {:?}", engineMsg(Some(stackLen)), app);
                                    if app.get_arg() == arg {
                                        // Case: Same argument; Nothing else to do:
                                        // do_dirty=false; do_insert=false
                                        (false, false)
                                    }
                                    else { // Case: Not the same argument:
                                        debug!("{} alloc thunk: Nominal match: replacing {:?} ~~> {:?}",
                                               engineMsg(Some(stackLen)), app.get_arg(), arg);
                                        app.consume(arg.clone()); // overwrite the old argument
                                        comp_nd.res = None ; // clear the cache
                                        // do_dirty=true; do_insert=false
                                        (true, false)
                                    }}
                                else {
                                  panic!("TODO-Sometime: Memozied functions not equal!\nFunction was:{:?} \tProducer:{:?}\nFunction now:{:?} \tProducer:{:?}\nCommon location:{:?}\nHint:Consider using distinct namespaces, via `Adapton::ns`\n",
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
                    debug!("{} alloc thunk: dirty_alloc {:?}.", engineMsg!(self), &loc);
                    dirty_alloc(self, &loc);
                } else {
                    debug!("{} alloc thunk: No dirtying.", engineMsg!(self))
                } ;
                match self.stack.last_mut() { None => (), Some(frame) => {
                    let pred = frame.loc.clone();
                    debug!("{} alloc thunk: edge {:?} --> {:?}", engineMsg(Some(stackLen)), &pred, &loc);
                    let succ =
                        Succ{loc:loc.clone(),
                             dep:Rc::new(Box::new(AllocDependency{val:arg.clone()})),
                             effect:Effect::Allocate,
                             dirty:false};
                    frame.succs.push(succ)
                }};
              if do_insert {
                    if self.flags.gmlog_dcg {
                      gm::startdframe(self, &format!("nominal thunk {:?}", loc), None);
                      gm::addnode(self, &format!("{:?}",loc), "nominal-thunk", "", Some(&format!("thunk {:?}", &producer)));
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
                    Art::Loc(loc)
                }
                else {
                    wf::check_dcg(self);
                    Art::Loc(loc)
                }
            }
        }
    }

    fn force<T:'static+Eq+Debug+Clone+Hash> (self:&mut DCG,
                                        art:&Art<T,Self::Loc>) -> T
    {
        wf::check_dcg(self);
        match *art {
            Art::Rc(ref v) => (**v).clone(),
            Art::Loc(ref loc) => {
                let (is_comp, is_pure, cached_result) : (bool, bool, Option<T>) = {
                    let is_pure_opt : bool = self.flags.use_purity_optimization ;
                    let node : &mut Node<T> = res_node_of_loc(self, &loc) ;
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
                        debug!("{} force {:?}: cache empty", engineMsg!(self), &loc);
                        assert!(is_comp);
                        produce(self, &loc)
                    },
                    Some(ref res) => {
                        if is_comp {
                            debug!("{} force {:?}: cache holds {:?}.  Using change propagation.", engineMsg!(self), &loc, &res);
                            // ProducerDep change-propagation precondition:
                            // loc is a computational node:
                            let res = ProducerDep{res:res.clone()}.change_prop(self, &loc) ;
                            debug!("{} force {:?}: result changed?: {}", engineMsg!(self), &loc, res.changed) ;
                            let node : &mut Node<T> = res_node_of_loc(self, &loc) ;
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
                            debug!("{} force {:?}: not a computation. (no change prop necessary).", engineMsg!(self), &loc);
                            res.clone()
                        }
                    }
                } ;
                if !is_pure { match self.stack.last_mut() { None => (), Some(frame) => {
                    let succ =
                        Succ{loc:loc.clone(),
                             dep:Rc::new(Box::new(ProducerDep{res:result.clone()})),
                             effect:Effect::Observe,
                             dirty:false};
                     frame.succs.push(succ);                  
                }}} ;
              wf::check_dcg(self);
              // if self.flags.gmlog_dcg {
              //gm::startdframe(self, &format!("force {:?}", loc), None);
              //result.log_snapshot(self, &format!("{:?}",loc), None);
              // }
              result
            }
        }}
}

#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum Artic<T> {
    Rc(Rc<T>),    // No entry in table. No dependency tracking.
    Loc(Rc<Loc>), // Location in table.
}

pub fn init_dcg   () -> Engine { init_engine(Engine::DCG(DCG::new())) }
pub fn init_naive () -> Engine { init_engine(Engine::Naive) }

/// Initializes global state with a fresh DCG-based engine; returns the old engine
pub fn init_engine (engine: Engine) -> Engine {
  use std::mem;
  let mut engine = engine;
  GLOBALS.with(|g| {
    mem::swap(&mut g.borrow_mut().engine, &mut engine);
  });
  return engine
}


pub fn name_of_usize  (u:usize)  -> Name { panic!("") }
pub fn name_of_string (s:String) -> Name { panic!("") }
pub fn name_of_str    (s:&'static str) -> Name { panic!("") }
pub fn name_pair      (n1:Name, n2:Name) -> Name { panic!("") }
pub fn name_fork      (n:Name) -> (Name, Name) { panic!("") }

/// Creates or re-enters a given namespace; performs the given computation there.
pub fn ns<T,F> (n:Name, body:F) -> T
  where F:FnOnce() -> T {
    panic!("")
  }

/// Enters a special "namespace" where all name uses are ignored; instead, Adapton uses structural identity.
pub fn structural<T,F> (body:F) -> T
  where F:FnOnce() -> T {
    panic!("")
  }

pub fn cnt<Res,F> (body:F) -> (Res, Cnt)
  where F:FnOnce() -> Res {
    panic!("")
  }

/// Creates immutable, eager articulation.
pub fn put<T:Eq+Debug+Clone> (val:T) -> Artic<T> {
  panic!("")
}

/// Creates a mutable articulation.
pub fn cell<T:Eq+Debug+Clone+Hash> (n:Name, val:T) -> Artic<T> {
  panic!("")
}

/// Mutates a mutable articulation.
pub fn set<T:Eq+Debug+Clone> (a:Artic<T>, val:T) {
  panic!("")
}

/// Creates an articulated computation.
pub fn thunk<Arg:Eq+Hash+Debug+Clone,Spurious:Clone,Res:Eq+Debug+Clone+Hash>
  (id:ArtIdChoice<Name>,
   prog_pt:ProgPt,
   fn_box:Rc<Box< Fn(Arg, Spurious) -> Res >>,
   arg:Arg, spurious:Spurious)
   -> Artic<Res> {
     panic!("")
   }

/// Demand & observe arts (all kinds): force
pub fn force<T:Eq+Debug+Clone+Hash> (a:&Artic<T>) -> T {
  panic!("")
}
