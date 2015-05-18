use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Hash,Debug,PartialEq,Eq)]
pub enum Art<T,Loc> {
    Box(Box<T>),  // No entry in table. No dependency tracking.
    Loc(Rc<Loc>), // Location in table.
}

#[derive(Hash,Debug,PartialEq,Eq)]
pub enum MutArt<T,Loc> {
    MutArt(Art<T,Loc>)
}

#[derive(Hash,Debug,PartialEq,Eq)]
pub enum ArtId<Name> {
    None,            // Identifies an Art::Box. No dependency tracking.
    Structural(u64), // Identifies an Art::Loc.
    Nominal(Name),   // Identifies an Art::Loc.
}

pub trait Adapton {
    type Name;
    type Loc;
    
    fn new () -> Self ;

    // Names
    fn name_of_string (self:&mut Self, String) -> Self::Name ;
    fn name_of_u64 (self:&mut Self, u64) -> Self::Name ;
    fn name_pair (self: &Self, Self::Name, Self::Name) -> Self::Name ;
    fn name_fork (self:&mut Self, Self::Name) -> (Self::Name, Self::Name) ;

    // Namespaces
    fn ns<T,F> (self: &mut Self, Self::Name, body:F) -> T
        where F:FnOnce(&mut Self) -> T ;

    // Create immutable, eager arts: put
    fn put<T:Eq+Debug> (self:&mut Self, T) -> Art<T,Self::Loc> ;

    // Mutable arts: cell and set
    fn cell<T:Eq+Debug> (self:&mut Self, ArtId<Self::Name>, T) -> MutArt<T,Self::Loc> ;
    fn set<T:Eq+Debug> (self:&mut Self, MutArt<T,Self::Loc>, T) ;

    // Computation arts: thunk
    fn thunk<Arg:Eq+Hash+Debug,T:Eq+Debug>
        (self:&mut Self, id:ArtId<Self::Name>,
         fn_body:Box<Fn(Arg) -> T>, arg:Arg) -> Art<T,Self::Loc> ;

    // Demand & observe arts (all kinds): force
    fn force<T:Eq+Debug> (self:&mut Self, Art<T,Self::Loc>) -> & T ;
}
