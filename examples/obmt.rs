use std::hash::Hash;
use std::collections::HashMap;
use std::thunk::Invoke;

// "One big memo table" design

pub trait Adapton {
    fn name_of_string (self:&mut Self, String) -> Name ;
    fn name_of_u64 (self:&mut Self, u64) -> Name ;
    fn pair (self: &Self, Name, Name) -> Name ;        
    fn fork (self:&mut Self, Name) -> (Name, Name) ;

    fn cell<T:Eq+Hash> (self:&mut Self, ArtId, T) -> Art<T> ;

    fn thunk<Arg:Eq+Hash,T:Eq+Hash>
        (self:&mut Self, id:ArtId, fn_body:Box<Invoke<Arg,T>+'static>, arg:Arg) -> Art<T> ;
    
    fn force<T:Eq+Hash> (self:&mut Self, Art<T>) -> T ;
}

pub enum Lineage {
    Unknown,
    Pair(Box<Lineage>,Box<Lineage>),
    ForkL(Box<Lineage>),
    ForkR(Box<Lineage>),
    String(String),
    U64(u64),
}

pub struct Name {
    hash : u64,
    lineage : Lineage,
}

pub enum Art<T> {
    Hash(u64),
    Box(Box<T>),
}

pub enum ArtId {
    None,
    Structural,
    Nominal(Name),
}

pub struct AdaptonState {
    mtbl : HashMap<Name, *mut ()>,
}

impl Adapton for AdaptonState {
    fn name_of_string (self:&mut AdaptonState, sym:String) -> Name {
        panic!("")            
    }
    fn name_of_u64 (self:&mut AdaptonState, sym:u64) -> Name {
        panic!("")            
    }
    fn pair (self: &AdaptonState, fst: Name, snd: Name) -> Name {
        panic!("")
    }
    fn fork (self:&mut AdaptonState, nm:Name) -> (Name, Name) {
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
