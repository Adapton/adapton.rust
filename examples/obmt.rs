use std::hash::{hash,Hash};
use std::collections::HashMap;
use std::thunk::Invoke;
use std::mem::replace;
//use std::sync::Arc;
use std::cell::RefCell;
use std::rc::Rc;

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

    fn put<T:Eq> (self:&mut Self, T) -> Art<T> ;

    // Mutable cells
    fn cell<T:Eq> (self:&mut Self, ArtId, T) -> MutArt<T> ;
    fn set<T:Eq> (self:&mut Self, MutArt<T>) ;

    fn thunk<Arg:Eq+Hash,T:Eq>
        (self:&mut Self, id:ArtId,
         fn_body:Box<Invoke<Arg, T> + 'static>, arg:Arg) -> Art<T> ;
    
    fn force<T:Eq> (self:&mut Self, Art<T>) -> T ;
}

#[derive(Hash,Show,PartialEq,Eq)]
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

#[derive(Hash,Show,PartialEq,Eq)]
pub enum Path {
    Empty,
    Child(Rc<Path>,Name),
}

#[derive(Hash,Show,PartialEq,Eq)]
pub struct Name {
    hash : u64,
    lineage : Rc<Lineage>,
}

#[derive(Hash,Show,PartialEq,Eq)]
pub struct Loc {
    hash : u64,
    name : Rc<Name>,
    path : Rc<Path>
}

#[derive(Hash,Show,PartialEq,Eq)]
pub enum ArtId {
    None,
    Structural(u64),
    Nominal(Name),
}

#[derive(Hash,Show,PartialEq,Eq)]
pub enum Art<T> {
    Box(Box<T>),
    Loc(Rc<Loc>),
}

#[derive(Hash,Show,PartialEq,Eq)]
pub enum MutArt<T> {
    MutArt(Art<T>)
}

enum Node<T> {
    Pure(PureNode<T>),
    Mut(MutNode<T>),
    Compute(ComputeNode<T>),
}

struct PureNode<T> {
    loc : Rc<Loc>,
    val : T,
}

struct MutNode<T> {
    loc : Rc<Loc>,
    dem_precs : Vec<DemPrec>,
    val : RefCell<T>,
}

trait Body<T> {
    fn run (self:&Self, st:&mut AdaptonState) -> T ;
}

struct ComputeNode<T> {
    loc : Rc<Loc>,
    dem_precs : Vec<DemPrec>,
    dem_succs : Vec<DemSucc>,
    body : Box<Body<T> + 'static>, // Trait in Type: Need lifetime.
    val  : RefCell<Option<T>>,
}

struct DemPrec {
    loc : Rc<Loc>,
}

struct DemSucc {
    loc : Rc<Loc>,
    dirty : RefCell<bool>
}

pub struct AdaptonState {
    memo_table : HashMap<Rc<Loc>, *mut ()>, // memo table
    curr_path : Rc<Path>, // current_namespace
    curr_name : Rc<Name>, // current articulation point
}

impl Adapton for AdaptonState {
    fn new () -> AdaptonState {
        let empty = Rc::new(Path::Empty);
        let root = Rc::new(Name{hash:0, lineage:Rc::new(Lineage::Root) });
        AdaptonState {
            memo_table : HashMap::new (),
            curr_path : empty,
            curr_name : root,
        }
    }
    fn name_of_string (self:&mut AdaptonState, sym:String) -> Name {
        let h = hash::<_>(&sym) ;
        let s = Lineage::String(sym) ;
        Name{ hash:h, lineage:Rc::new(s) }
    }
    fn name_of_u64 (self:&mut AdaptonState, sym:u64) -> Name {
        let h = hash::<_>(&sym) ;
        let s = Lineage::U64(sym) ;
        Name{ hash:h, lineage:Rc::new(s) }
    }
    fn name_pair (self: &AdaptonState, fst: Name, snd: Name) -> Name {
        let h = hash::<_>( &(fst.hash,snd.hash) ) ;
        let p = Lineage::Pair(fst.lineage, snd.lineage) ;
        Name{ hash:h, lineage:Rc::new(p) }
    }
    fn name_fork (self:&mut AdaptonState, nm:Name) -> (Name, Name) {
        let h1 = hash::<_>( &(&nm, 11111111) ) ; // TODO: make this hashing better.
        let h2 = hash::<_>( &(&nm, 22222222) ) ;
        ( Name{ hash:h1,
                lineage:Rc::new(Lineage::ForkL(nm.lineage.clone())) } ,
          Name{ hash:h2,
                lineage:Rc::new(Lineage::ForkR(nm.lineage)) } )
    }
    fn ns<T,F> (self: &mut Self, nm:Name, body:F) -> T where F:FnOnce(&mut Self) -> T {
        let path_body = Rc::new(Path::Child(self.curr_path.clone(), nm)) ;
        let path_pre = replace(&mut self.curr_path, path_body ) ;
        let x = body(self) ;
        let path_body = replace(&mut self.curr_path, path_pre) ;
        drop(path_body);
        x
    }
    fn put<T:Eq> (self:&mut AdaptonState, x:T) -> Art<T> {
        Art::Box(box x)
    }
    fn cell<T:Eq> (self:&mut AdaptonState, id:ArtId, x:T) -> MutArt<T> {        
        panic!("")
    }
    fn set<T:Eq> (self:&mut Self, cell:MutArt<T>) {
        panic!("")
    }
    fn thunk<Arg:Eq+Hash,T:Eq>
        (self:&mut AdaptonState,
         id:ArtId, fn_body:Box<Invoke<Arg,T>+'static>, arg:Arg) -> Art<T>
    {
        panic!("")
    }        
    fn force<T:Eq> (self:&mut AdaptonState, art:Art<T>) -> T {
        match art {
            Art::Box(b) => *b,
            Art::Loc(l) => panic!(""),
        }
    }
}

pub fn main () {
   
    
}
