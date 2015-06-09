use std::fmt::Debug;
use std::hash::{hash,Hash,Hasher,SipHasher};
use std::collections::HashMap;
use std::mem::replace;
use std::mem::transmute;
use std::rc::Rc;
use std::fmt;
use std::marker::PhantomData;
use std::fmt::{Formatter,Result};

use adapton_syntax::{ProgPt};

// use adapton_syntax::*;
use adapton_sigs::*;

#[derive(Debug)]
pub struct Frame {
    loc   : Rc<Loc>,    // The currently-executing node
    path  : Rc<Path>,   // The current path for creating new nodes; invariant: (prefix-of frame.loc.path frame.path)
    succs : Vec<Succ>,  // The currently-executing node's effects (viz., the nodes it demands)
}

// Each location identifies a node in the DCG.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub struct Loc {
    path : Rc<Path>,
    id   : Rc<ArtId<Name>>,
    hash : u64, // hash of (path,id)
}

// Paths are built using the ns command.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum Path {
    Empty,
    Child(Rc<Path>,Name),
}

// Names provide a symbolic way to identify nodes.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub struct Name {
    hash : u64, // hash of symbol
    symbol : Rc<Symbol>,
}

// Symbols
//
// For a general semantics of symbols, see Chapter 31 of PFPL 2nd Edition. Harper 2015:
// http://www.cs.cmu.edu/~rwh/plbook/2nded.pdf
//
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum Symbol {
    Root, // Root identifies the outside environment of Rust code.
    Nil,  // Nil for non-symbolic, hash-based names.
    String(String), U64(u64),   // Strings and U64s are unique symbols.
    Pair(Rc<Symbol>,Rc<Symbol>),
    ForkL(Rc<Symbol>),
    ForkR(Rc<Symbol>),
    //Rc(Rc<Symbol>),
}

#[derive(PartialEq,Eq,Debug,Clone)]
pub enum Effect {
    Observe,
    Allocate,
}

#[derive(Debug,Clone)]
pub struct Succ {
    effect : Effect,
    dep    : Rc<Box<AdaptonDep>>, // Abstracted dependency information (e.g., for Observe Effect, the prior observed value)
    loc    : Rc<Loc>, // Target of the effect, aka, the successor, by this edge
    dirty  : bool,    // mutated to dirty when loc changes, or any of its successors change
}


// AdaptonDep abstracts over the value produced by a dependency, as
// well as mechanisms to update and/or re-produce it.
pub trait AdaptonDep : Debug {
    fn change_prop (self:&Self, st:&mut AdaptonState, loc:&Rc<Loc>) -> AdaptonRes ;
}

#[derive(Debug)]
pub struct NoDependency;
impl AdaptonDep for NoDependency {
    fn change_prop (self:&Self, _st:&mut AdaptonState, _loc:&Rc<Loc>) -> AdaptonRes { AdaptonRes{changed:false} }
}

#[derive(Debug)]
pub struct AllocDependency<T> { val:T }
impl<T:Debug> AdaptonDep for AllocDependency<T> {
    fn change_prop (self:&Self, _st:&mut AdaptonState, _loc:&Rc<Loc>) -> AdaptonRes { AdaptonRes{changed:true} } // TODO-Later: Make this a little better.
}

pub struct AdaptonRes {
    changed : bool,
}

pub trait AdaptonNode {
    // DCG structure:
    fn preds_alloc<'r> (self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> ;
    fn preds_obs<'r>   (self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> ;
    fn succs_def<'r>   (self:&'r mut Self) -> bool ;
    fn succs<'r>       (self:&'r mut Self) -> &'r mut Vec<Succ> ;
}

pub trait ShapeShifter {
    fn be_node<'r> (self:&'r mut Self) -> &'r mut Box<AdaptonNode> ;
}

impl fmt::Debug for AdaptonNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(AdaptonNode)")
    }
}

#[derive(Debug)]
pub struct AdaptonState {
    table : HashMap<Rc<Loc>, Box<AdaptonNode>>,
    stack : Vec<Frame>
}

// Structureful (Non-opaque) nodes:
#[derive(Debug)]
pub enum Node<Res> {
    Pure(PureNode<Res>),
    Mut(MutNode<Res>),
    Comp(CompNode<Res>),
}

// PureNode<T> for pure hash-consing of T's.
// Location in table never changes value.
#[derive(Debug)]
pub struct PureNode<T> {
    val : T,
}

// MutNode<T> for mutable content of type T.
// The set operation mutates a MutNode; set may only be called by *outer* Rust environment.
// Its notable that the CompNodes' producers do not directly change the value of MutNodes with set.
// They may indirectly mutate these nodes by performing nominal allocation; mutation is limited to "one-shot" changes.
#[derive(Debug)]
pub struct MutNode<T> {
    preds_alloc : Vec<Rc<Loc>>,
    preds_obs   : Vec<Rc<Loc>>,
    val         : T,
}

// CompNode<Res> for a suspended computation whose resulting value of
// type T.  The result of the CompNode is affected in two ways: the
// (1) producer may change, which may affect the result and (2) the
// values produced by the successors may change, indirectly
// influencing how the producer produces its resulting value.
pub struct CompNode<Res> {
    preds_alloc : Vec<Rc<Loc>>,
    preds_obs   : Vec<Rc<Loc>>,
    succs       : Vec<Succ>,
    producer    : Box<Producer<Res>>, // Producer can be App<Arg,Res>, where type Arg is hidden.
    res         : Option<Res>,
}
// Produce a value of type Res.
pub trait Producer<Res> {
    fn produce(self:&Self, st:&mut AdaptonState) -> Rc<Res>;
    fn copy(self:&Self) -> Box<Producer<Res>>;
}
// Consume a value of type Arg.
pub trait Consumer<Arg> {
    fn consume(self:&mut Self, Rc<Arg>);
    fn get_arg(self:&mut Self) -> Rc<Arg>;
}
// struct App is hidden by traits Comp<Res> and CompWithArg<Res>, below.
#[derive(Clone)]
pub struct App<Arg,Res> {
    prog_pt: ProgPt,
    fn_box:  Rc<Box<Fn(&mut AdaptonState, Rc<Arg>) -> Rc<Res>>>,
    arg:     Rc<Arg>,
}

// ---------- App implementation of Debug and Hash

impl<Arg,Res> Debug for App<Arg,Res> {
    fn fmt(&self, f: &mut Formatter) -> Result { self.prog_pt.fmt(f) }
}

impl<Arg:Hash,Res> Hash for App<Arg,Res> {
    fn hash<H>(&self, state: &mut H) where H: Hasher { (&self.prog_pt,&self.arg).hash(state) }
}

// ---------- App implementation of Producer and Consumer traits:

impl<Arg:'static+Clone+PartialEq+Eq,Res:'static+Clone+PartialEq+Eq> Producer<Res> for App<Arg,Res> {
    fn produce(self:&Self, st:&mut AdaptonState) -> Rc<Res> {
        let f = self.fn_box.clone() ;
        f (st,self.arg.clone())
    }
    fn copy(self:&Self) -> Box<Producer<Res>> {
        Box::new(self.clone())
    }
}
impl<Arg:Clone+PartialEq+Eq,Res> Consumer<Arg> for App<Arg,Res> {
    fn consume(self:&mut Self, arg:Rc<Arg>) { replace(&mut self.arg, arg); }
    fn get_arg(self:&mut Self) -> Rc<Arg>   { self.arg.clone() }
}

// // ---------- Ret implementation of Producer:

// #[derive(Debug,PartialEq,Eq)]
// pub enum Ret<Res> { Ret(Res) }

// impl<Res:Clone> Producer<Res> for Ret<Res> {
//     fn produce(self:&Self, _st:&mut AdaptonState) -> Res {
//         let &Ret::Ret(ref val) = self;
//         val.clone()
//     }
// }

// ----------- Location resolution:

pub fn abs_node_of_loc<'r> (st:&'r mut AdaptonState, loc:&'r Rc<Loc>) -> Option<&'r mut Box<AdaptonNode>> {
    panic!("st.table.get_mut(loc)")
}

fn lookup_abs<'r>(st:&'r mut AdaptonState, loc:&Rc<Loc>) -> &'r mut Box<AdaptonNode> {
    match st.table.get_mut( loc ) {
        None => panic!("dangling pointer"),
        Some(node) => node.be_node() // This is a wierd workaround; TODO-Later: Investigate.
    }
}

// This only is safe in contexts where the type of loc is known.
// Unintended double-uses of names and hashes will generally cause uncaught type errors.
pub fn res_node_of_loc<'r,Res> (st:&'r mut AdaptonState, loc:&Rc<Loc>) -> &'r mut Box<Node<Res>> {
    let abs_node = lookup_abs(st, loc) ;
    unsafe { transmute::<_,_>(abs_node) }
}

// ---------- Node implementation:

impl <Res> AdaptonNode for Node<Res> {
    fn preds_alloc<'r>(self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> {
        match *self { Node::Mut(ref mut nd) => &mut nd.preds_alloc,
                      Node::Comp(ref mut nd) => &mut nd.preds_alloc,
                      Node::Pure(_) => unreachable!(),
        }}
                      
    fn preds_obs<'r>(self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> {
        match *self { Node::Mut(ref mut nd) => &mut nd.preds_obs,
                      Node::Comp(ref mut nd) => &mut nd.preds_obs,
                      Node::Pure(_) => unreachable!(),
        }}
    fn succs_def<'r>(self:&'r mut Self) -> bool {
        match *self { Node::Comp(_) => true, _ => false
        }}
    fn succs<'r>(self:&'r mut Self) -> &'r mut Vec<Succ> {
        match *self { Node::Comp(ref mut n) => &mut n.succs,
                     _ => panic!("undefined"),
        }
    }
}

impl <Res> ShapeShifter for Box<Node<Res>> {
    fn be_node<'r>(self:&'r mut Self) -> &'r mut Box<AdaptonNode> {
        // TODO-Later: Why is this transmute needed here ??
        unsafe { transmute::<_,_>(self) }
    }
}

impl ShapeShifter for Box<AdaptonNode> {
    fn be_node<'r>(self:&'r mut Self) -> &'r mut Box<AdaptonNode> {
        // TODO-Later: Why is this transmute needed here ??
        unsafe { transmute::<_,_>(self) }
    }
}



impl<Res> fmt::Debug for CompNode<Res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(CompNode)")
    }
}

pub fn main () { }
