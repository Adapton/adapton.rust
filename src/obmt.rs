#![feature(box_syntax)]

use std::fmt::Debug;
use std::hash::{hash,Hash,SipHasher};
use std::collections::HashMap;
//use std::thunk::Invoke;
use std::mem::replace;
use std::mem::transmute;
//use std::sync::Arc;
use std::rc::Rc;
use std::fmt;

// "One big memo table" design

pub trait Adapton {
    fn new () -> Self ;

    // Names
    fn name_of_string (self:&mut Self, String) -> Name ;
    fn name_of_u64 (self:&mut Self, u64) -> Name ;
    fn name_pair (self: &Self, Name, Name) -> Name ;
    fn name_fork (self:&mut Self, Name) -> (Name, Name) ;

    fn ns<T,F> (self: &mut Self, Name, body:F) -> T
        where F:FnOnce(&mut Self) -> T ;

    fn put<T:Eq+Debug> (self:&mut Self, T) -> Art<T> ;

    // Mutable cells
    fn cell<T:Eq+Debug> (self:&mut Self, ArtId, T) -> MutArt<T> ;
    fn set<T:Eq+Debug> (self:&mut Self, MutArt<T>, T) ;

    fn thunk<Arg:Eq+Hash+Debug,T:Eq+Debug>
        (self:&mut Self, id:ArtId,
         fn_body:Box<Fn(Arg) -> T>, arg:Arg) -> Art<T> ;

    fn force<T:Eq+Debug> (self:&mut Self, Art<T>) -> T ;
}

#[derive(Hash,Debug,PartialEq,Eq)]
pub enum Lineage {
    Unknown, Structural,
    Root,
    Pair(Rc<Lineage>,Rc<Lineage>),
    ForkL(Rc<Lineage>),
    ForkR(Rc<Lineage>),
    String(String),
    U64(u64),
    Rc(Rc<Lineage>),
}

#[derive(Hash,Debug,PartialEq,Eq)]
pub enum Path {
    Empty,
    Child(Rc<Path>,Name),
}

#[derive(Hash,Debug,PartialEq,Eq)]
pub struct Name {
    hash : u64,
    lineage : Rc<Lineage>,
}

#[derive(Hash,Debug,PartialEq,Eq)]
pub struct Loc {
    hash : u64,
    name : Rc<Name>,
    path : Rc<Path>
}

pub enum Node<Res> {
    Pure(PureNode<Res>),
    Mut(MutNode<Res>),
    Compute(Box<ResNode<Res>>),
}

pub trait OpaqueNode {
    fn loc (self:&Self) -> Rc<Loc> ;
    fn creators (self:&Self) -> Vec<DemPrec> ;
    fn dem_precs (self:&Self) -> Vec<DemPrec> ;
    fn dem_succs (self:&Self) -> Vec<DemSucc> ;
}

impl <Res> OpaqueNode for Node<Res> {
    fn loc (self:&Self) -> Rc<Loc> { panic!("") }
    fn creators (self:&Self) -> Vec<DemPrec> { panic!("") }
    fn dem_precs (self:&Self) -> Vec<DemPrec> { panic!("") }
    fn dem_succs (self:&Self) -> Vec<DemSucc> { panic!("") }
}

pub trait ResNode<Res> : OpaqueNode {
    fn get_res (self:&Self) -> Option<Res> ;
}


#[derive(Hash,Debug,PartialEq,Eq)]
pub enum ArtId {
    None,            // Identifies an Art::Box. No dependency tracking.
    Structural(u64), // Identifies an Art::Loc.
    Nominal(Name),   // Identifies an Art::Loc.
}

#[derive(Hash,Debug,PartialEq,Eq)]
pub enum Art<T> {
    Box(Box<T>),  // No entry in table. No dependency tracking.
    Loc(Rc<Loc>), // Location in table.
}

#[derive(Hash,Debug,PartialEq,Eq)]
pub enum MutArt<T> {
    MutArt(Art<T>)
}

#[derive(Debug)]
pub struct PureNode<T> {
    loc : Rc<Loc>,
    val : T,
}

#[derive(Debug)]
pub struct MutNode<T> {
    loc : Rc<Loc>,
    creators  : Vec<DemPrec>,
    dem_precs : Vec<DemPrec>,
    val : T,
}

pub struct ComputeNode<Arg,Res> {
    loc : Rc<Loc>,
    creators : Vec<DemPrec>,
    dem_precs : Vec<DemPrec>,
    dem_succs : Vec<DemSucc>,
    res : Option<Res>,
    computer : Box<Computer<Res,Arg=Arg>>
}

pub trait Computer<Res> {    
    type Arg;
    fn get_arg(self:&Self) -> Self::Arg;
    fn compute(self:&Self, st:&mut AdaptonState, arg:Self::Arg) -> Res;
}

impl<Arg,Res> fmt::Debug for ComputeNode<Arg,Res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(ComputeNode)")
    }
}

 

#[derive(Debug)]
pub struct DemPrec {
    loc : Rc<Loc>,
}

#[derive(Debug)]
pub struct DemSucc {
    loc : Rc<Loc>,
    dirty : bool
}

pub struct Frame {
    path : Rc<Path>,
    name : Rc<Name>,
    succs : Vec<DemSucc>,
}

pub struct AdaptonState {
    table : HashMap<Rc<Loc>, Box<OpaqueNode>>,
    stack : Vec<Frame>
}

// fn dem_precs<'x,T:'x> (st: &'x mut AdaptonState, loc: &Rc<Loc>) -> &'x mut Vec<DemPrec> {    
//     let node = st.table.get(loc) ;
//     match node {
//         None => panic!("dangling pointer"),
//         Some(ptr) => {
//             match *(ptr.get_node()) {
//                 Node::Pure(_) => {
//                     panic!("Node::Pure: no precs")
//                 },
//                 Node::Compute(ref mut nd) => {
//                     &mut nd.dem_precs
//                 },
//                 Node::Mut(ref mut nd) => {
//                     &mut nd.dem_precs
//                 }
//             }
//         }
//     }    
// }

pub fn revoke_demand<'x> (st:&mut AdaptonState, src:&Rc<Loc>, succs:&Vec<DemSucc>) {
    // for succ in succs.iter() {
    //     let precs = dem_precs(st, &succ.loc);
    //     precs.retain(|ref prec| &prec.loc != src);
    // }
    panic!("")
}

pub fn invoke_demand<'x> (st:&mut AdaptonState, src:Rc<Loc>, succs:& Vec<DemSucc>) {
    // for succ in succs.iter() {
    //     let precs = dem_precs(st, &succ.loc);
    //     precs.push(DemPrec{loc:src.clone()})
    // }
    panic!("")
}

pub fn main () {


}
