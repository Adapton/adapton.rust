use std::fmt::{Debug};
use std::hash::{Hash,Hasher};
use std::rc::Rc;
use std::marker::PhantomData;
use adapton_syntax::{ProgPt};

// The `Adapton` trait provides a language of
// dependence-graph-building operations based on the core calculus
// described here:
//
//    http://arxiv.org/abs/1503.07792
//    ("Incremental Computation with Names", 2015).
//
//  Types:
//
//   - Art<Loc,T> : "articulations" that produce a value of type T.
//
//     Examples:
//      * Pure values (see `Adapton::put`)
//      * Mutable reference cells (see `Adapton::cell`),
//      * Thunks (see `Adapton::thunk`),
//                  
//   - Adapton::Name : identify DCG nodes before they are made
//   - Adapton::Loc  : identify DCG nodes after they are made
//
// 
pub trait Adapton {
    type Name;
    type Loc;
    
    fn new () -> Self ;

    // Names
    fn name_of_u64    (self:&mut Self, u64)    -> Self::Name ;
    fn name_of_string (self:&mut Self, String) -> Self::Name ;
    fn name_pair      (self:&mut Self, Self::Name, Self::Name) -> Self::Name               ;
    fn name_fork      (self:&mut Self, Self::Name)             -> (Self::Name, Self::Name) ;

    // Namespaces
    fn ns<T,F> (self: &mut Self, Self::Name, body:F) -> T
        where F:FnOnce(&mut Self) -> T ;

    // Create immutable, eager arts: put
    fn put<T:Eq+Debug> (self:&mut Self, Rc<T>) -> Art<T,Self::Loc> ;

    // Mutable arts: cell and set
    fn cell<T:Eq+Debug> (self:&mut Self, ArtId<Self::Name>, Rc<T>) -> MutArt<T,Self::Loc> ;
    fn set<T:Eq+Debug> (self:&mut Self, MutArt<T,Self::Loc>, Rc<T>) ;

    // Computation arts: thunk
    fn thunk<Arg:Eq+Hash+Debug,T:Eq+Debug>
        (self:&mut Self,
         id:ArtId<Self::Name>,
         prog_pt:ProgPt,
         fn_box:Rc<Box<Fn(&mut Self, Rc<Arg>) -> Rc<T>>>,
         arg:Rc<Arg>)
         -> Art<T,Self::Loc> ;

    // Demand & observe arts (all kinds): force
    fn force<T:Eq+Debug> (self:&mut Self, Art<T,Self::Loc>) -> Rc<T> ;
}

// TODO: I'd like the Art<T> definition to live within the Adapton trait below.
// Then, it need not be parameterized by Loc. It can simply use Adapton::Loc.
// However, I run into problems with rustc accepting associated types for traits that are parameterized (by type T).
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum Art<T,Loc> {
    Rc(Rc<T>),    // No entry in table. No dependency tracking.
    Loc(Rc<Loc>), // Location in table.
}

// TODO: Same scoping issue as Art above.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub struct MutArt<T,Loc> {
    pub loc:Rc<Loc>,
    pub phantom: PhantomData<T>
}

// ArtId -- A symbolic identity for an articulation point made by
// Adapton::thunk.  An ArtId is chosen by the programmer to identify
// the point during evaluation (and simultaneously, to identify the
// point during re-evaluation).

// An `Eager` identity is special, and it means "do not introduce any
// laziness/memoization overhead here"; when Eager is used, no thunk
// is created; rather, the computation eagerly produces an articulated
// value of the form Art::Rc(v), for some value v.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum ArtId<Name> {
    Structural(u64), // Identifies an Art::Loc based on hashing content.
    Nominal(Name),   // Identifies an Art::Loc based on a name.
    Eager,           // Eagerly produce an Art::Rc, no additional indirection is needed/used.
}

