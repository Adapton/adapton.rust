use std::hash::{hash,Hash};
use std::collections::HashMap;
use std::thunk::Invoke;
use std::mem::replace;
use std::sync::Arc;

// "One big memo table" design

pub trait Adapton {
    fn new () -> Self ;
    fn name_of_string (self:&mut Self, String) -> Name ;
    fn name_of_u64 (self:&mut Self, u64) -> Name ;
    fn name_pair (self: &Self, Name, Name) -> Name ;
    fn name_fork (self:&mut Self, Name) -> (Name, Name) ;

    fn child<T> (self: &mut Self, Name, body:Box<Invoke<(),T>+'static>) -> T ;
    
    fn cell<T:Eq+Hash> (self:&mut Self, ArtId, T) -> Art<T> ;

    fn thunk<Arg:Eq+Hash,T:Eq+Hash>
        (self:&mut Self, id:ArtId, fn_body:Box<Invoke<Arg,T>+'static>, arg:Arg) -> Art<T> ;
    
    fn force<T:Eq+Hash> (self:&mut Self, Art<T>) -> T ;
}

#[derive(Hash,Show,PartialEq,Eq)]
pub enum Lineage {
    Unknown,
    Root,
    Pair(Arc<Lineage>,Arc<Lineage>),
    ForkL(Arc<Lineage>),
    ForkR(Arc<Lineage>),
    String(String),
    U64(u64),
    Arc(Arc<Lineage>),
}

#[derive(Hash,Show,PartialEq,Eq)]
pub enum Path {
    Empty,
    Child(Arc<Path>,Name),
}

#[derive(Hash,Show,PartialEq,Eq)]
pub struct Name {
    hash : u64,
    lineage : Arc<Lineage>,
    path : Arc<Path>,
}

#[derive(Hash,Show,PartialEq,Eq)]
pub enum ArtId {
    None,
    Structural,
    Nominal(Name),
}

#[derive(Hash,Show,PartialEq,Eq)]
pub enum Art<T> {
    Hash(u64),
    Box(Box<T>),
}

pub struct AdaptonState {
    memo_table : HashMap<ArtId, *mut ()>, // memo table
    curr_path : Arc<Path>, // current_namespace
    curr_name : Arc<Name>, // current articulation point
}

impl Adapton for AdaptonState {
    fn new () -> AdaptonState {
        let empty = Arc::new(Path::Empty);
        let root = Arc::new(Name{hash:0, lineage:Arc::new(Lineage::Root), path:empty.clone()});
        AdaptonState {
            memo_table : HashMap::new (),
            curr_path : empty,
            curr_name : root,
        }
    }
    fn name_of_string (self:&mut AdaptonState, sym:String) -> Name {
        let h = hash::<_>(&sym) ;
        let s = Lineage::String(sym) ;
        Name{ hash:h, lineage:Arc::new(s), path:self.curr_path.clone() }
    }
    fn name_of_u64 (self:&mut AdaptonState, sym:u64) -> Name {
        let h = hash::<_>(&sym) ;
        let s = Lineage::U64(sym) ;
        Name{ hash:h, lineage:Arc::new(s), path:self.curr_path.clone() }
    }
    fn name_pair (self: &AdaptonState, fst: Name, snd: Name) -> Name {
        let h = hash::<_>( &(&fst,&snd) ) ;
        let p = Lineage::Pair(fst.lineage, snd.lineage) ;
        Name{ hash:h, lineage:Arc::new(p), path:self.curr_path.clone() }
    }
    fn name_fork (self:&mut AdaptonState, nm:Name) -> (Name, Name) {
        let h1 = hash::<_>( &(&nm, 1) ) ;
        let h2 = hash::<_>( &(&nm, 2) );
        ( Name{ hash:h1,
                lineage:Arc::new(Lineage::ForkL(nm.lineage.clone())),
                path:self.curr_path.clone() } ,
          Name{ hash:h2,
                lineage:Arc::new(Lineage::ForkR(nm.lineage)),
                path:self.curr_path.clone() } )
    }
    fn child<T> (self: &mut Self, nm:Name, body:Box<Invoke<(),T>+'static>) -> T {
        let path_body = Arc::new(Path::Child(self.curr_path.clone(), nm)) ;
        let path_pre = replace(&mut self.curr_path, path_body ) ;
        let x = body.invoke(()) ;
        let path_body = replace(&mut self.curr_path, path_pre) ;
        drop(path_body); x
    }
    fn cell<T:Eq+Hash> (self:&mut AdaptonState, id:ArtId, x:T) -> Art<T> {
        panic!("")
    }
    fn thunk<Arg:Eq+Hash,T:Eq+Hash>
        (self:&mut AdaptonState,
         id:ArtId, fn_body:Box<Invoke<Arg,T>+'static>, arg:Arg) -> Art<T>
    {
        panic!("")
    }        
    fn force<T:Eq+Hash> (self:&mut AdaptonState, art:Art<T>) -> T {
        panic!("")
    }
}

pub fn main () {
   
    
}
