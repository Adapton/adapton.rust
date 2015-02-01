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

    fn cell<T:Eq+Hash> (self:&mut Self, ArtId, T) -> Art<T> ;

    fn thunk<Arg:Eq+Hash,T:Eq+Hash>
        (self:&mut Self, id:ArtId, fn_body:Box<Invoke<Arg,T>+'static>, arg:Arg) -> Art<T> ;
    
    fn force<T:Eq+Hash> (self:&mut Self, Art<T>) -> T ;
}

#[derive(Hash,Show,PartialEq,Eq)]
pub enum Lineage {
    Unknown,
    Root,
    Taken,
    Pair(Arc<Lineage>,Arc<Lineage>),
    ForkL(Arc<Lineage>),
    ForkR(Arc<Lineage>),
    String(String),
    U64(u64),
    Arc(Arc<Lineage>),
}

#[derive(Hash,Show,PartialEq,Eq)]
pub struct Name {
    hash : u64,
    lineage : Arc<Lineage>,
}

#[derive(Hash,Show,PartialEq,Eq)]
pub enum Art<T> {
    Hash(u64),
    Box(Box<T>),
}

#[derive(Hash,Show,PartialEq,Eq)]
pub enum ArtId {
    None,
    Structural,
    Nominal(Name),
}

pub struct AdaptonState {
    memo_table : HashMap<ArtId, *mut ()>, // memo table
    curr_nmspc : Name, // current_namespace
    curr_artnm : Name, // current articulation point
}

impl Adapton for AdaptonState {
    fn new () -> AdaptonState {
        let root = Arc::new(Lineage::Root);
        let root1 = Name{hash:0, lineage:root.clone()};
        let root2 = Name{hash:0, lineage:root};
        AdaptonState {
            memo_table : HashMap::new (),
            curr_nmspc : root1,
            curr_artnm : root2,
        }
    }
    fn name_of_string (self:&mut AdaptonState, sym:String) -> Name {
        let h = hash::<_>(&sym) ;
        let s = Lineage::String(sym) ;
        Name{ hash:h, lineage:Arc::new(s) }
    }
    fn name_of_u64 (self:&mut AdaptonState, sym:u64) -> Name {
        let h = hash::<_>(&sym) ;
        let s = Lineage::U64(sym) ;
        Name{ hash:h, lineage:Arc::new(s) }
    }
    fn name_pair (self: &AdaptonState, fst: Name, snd: Name) -> Name {
        let h = hash::<_>( &(&fst,&snd) ) ;
        let p = Lineage::Pair(fst.lineage, snd.lineage) ;
        Name{ hash:h, lineage:Arc::new(p) }
    }
    fn name_fork (self:&mut AdaptonState, nm:Name) -> (Name, Name) {
        panic!("")
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
