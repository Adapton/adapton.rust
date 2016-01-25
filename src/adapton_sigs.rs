use std::fmt::{Debug};
use std::hash::{Hash,Hasher};
use std::rc::Rc;
use std::marker::PhantomData;
use macros::{ProgPt};
use std::ops::Sub;
use std::ops::Add;
use std::num::Zero;

// The `Adapton` trait provides a language of
// dependence-graph-building operations based on the core calculus
// described here:
//
//    http://arxiv.org/abs/1503.07792
//    ("Incremental Computation with Names", 2015).
//
//  Types:
//
//   - Art<Loc,T> : An incremental "articulation", in a data structure
//                  or computation, that when `force`d, produces a value of type T.
//                  Each articulation is implemented as a node in the DCG.
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

/// The `Adapton` trait provides a language of
/// dependence-graph-building operations based on the core calculus
/// described in ["Incremental Computation with Names", 2015](http://arxiv.org/abs/1503.07792)

pub trait Adapton : Debug+PartialEq+Eq+Hash+Clone {
    // TODO-later: Report ICE: If I replace the trait combinations below with `AdaptonData`:
    type Name : Debug+PartialEq+Eq+Hash+Clone; // Always be mindful of clones.
    type Loc  : Debug+PartialEq+Eq+Hash+Clone; // Always be mindful of clones.
        
    fn new () -> Self ;

    // Names
    fn name_of_usize  (self:&mut Self, usize)  -> Self::Name ;
    fn name_of_string (self:&mut Self, String) -> Self::Name ;
    fn name_pair      (self:&mut Self, Self::Name, Self::Name) -> Self::Name               ;
    fn name_fork      (self:&mut Self, Self::Name)             -> (Self::Name, Self::Name) ;
    
    /// Creates or re-enters a given namespace; performs the given computation there.
    fn ns<T,F> (self: &mut Self, Self::Name, body:F) -> T
        where F:FnOnce(&mut Self) -> T ;

    fn cnt<Res,F> (self: &mut Self, body:F) -> (Res, Cnt)
        where F:FnOnce(&mut Self) -> Res ;
    
    /// Creates immutable, eager articulation.
    fn put<T:Eq+Debug+Clone> (self:&mut Self, T) -> Art<T,Self::Loc> ;

    /// Creates a mutable articulation.
    fn cell<T:Eq+Debug+Clone+Hash> (self:&mut Self, Self::Name, T) -> MutArt<T,Self::Loc> ;

    /// Mutates a mutable articulation.
    fn set<T:Eq+Debug+Clone> (self:&mut Self, MutArt<T,Self::Loc>, T) ;

    /// Creates an articulated computation.
    fn thunk<Arg:Eq+Hash+Debug+Clone,Spurious:Clone,Res:Eq+Debug+Clone+Hash>
        (self:&mut Self,
         id:ArtIdChoice<Self::Name>,
         prog_pt:ProgPt,
         fn_box:Rc<Box< Fn(&mut Self, Arg, Spurious) -> Res >>,
         arg:Arg, spurious:Spurious)
         -> Art<Res,Self::Loc> ;

    /// Demand & observe arts (all kinds): force
    fn force<T:Eq+Debug+Clone+Hash> (self:&mut Self, &Art<T,Self::Loc>) -> T ;
  

    ///  # Derived fork functions:
    
    fn name_fork3 (self:&mut Self, n:Self::Name)
                   -> (Self::Name,Self::Name,Self::Name) {
        let (n1,n)  = self.name_fork(n);
        let (n2,n3) = self.name_fork(n);
        (n1,n2,n3)
    }
    
    fn name_fork4 (self:&mut Self, n:Self::Name)
                   -> (Self::Name,Self::Name,Self::Name,Self::Name) {
        let (n1,n2,n) = self.name_fork3(n);
        let (n3,n4)   = self.name_fork(n);
        (n1,n2,n3,n4)
    }

    fn read_only<T> (self:&mut Self, mutart:MutArt<T,Self::Loc>) -> Art<T,Self::Loc> {
        Art::Loc(mutart.loc)
    }
}

// I wanted to have a shorthand, but I get an ICE if I use this.
pub trait AdaptonData : Debug+Hash+PartialEq+Eq+Clone {}
impl<X:Debug+Hash+PartialEq+Eq+Clone> AdaptonData for X { }


/// The term "Art" stands for two things here: "Adapton return type",
/// and "Articulation point, for 'articulating' incremental change".
/// The concept of an "Art" also abstracts over whether the producer
/// is eager (like a ref cell) or lazy (like a thunk).
///
/// TODO: I'd like the `Art<T,Loc>` definition to live within the `Adapton` trait.
/// Then, it need not be parameterized by `Loc`. It can simply use `Adapton::Loc`, internally ("privately").
/// However, I run into problems with rustc accepting associated types
/// for traits that are parameterized by other types (viz., by type
/// `T`).
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum Art<T,Loc> {
    Rc(Rc<T>),    // No entry in table. No dependency tracking.
    Loc(Rc<Loc>), // Location in table.
}

/// TODO: Same scoping issue as `Art`; should be in `Adapton` trait.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub struct MutArt<T,Loc> {
    pub loc:Rc<Loc>,
    pub phantom: PhantomData<T>
}

/// An `ArtId` is a symbolic identity for an articulation point made by
/// Adapton::thunk.  An ArtId is chosen by the programmer to identify
/// the point during evaluation (and simultaneously, to identify the
/// point during re-evaluation).
/// An `Eager` identity is special, and it means "do not introduce any
/// laziness/memoization overhead here"; when Eager is used, no thunk
/// is created; rather, the computation eagerly produces an articulated
/// value of the form Art::Rc(v), for some value v.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum ArtIdChoice<Name> {
    Eager,         // Eagerly produce an Art::Rc, no additional indirection is needed/used.
    Structural,    // Identifies an Art::Loc based on hashing content (prog_pt and arg).
    Nominal(Name), // Identifies an Art::Loc based on a programmer-chosen name.
}

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub struct Cnt {
    pub dirty : usize,
    pub eval  : usize,
    pub change_prop : usize,
}

// pub trait Sub<RHS = Self> {
//     type Output;
//     fn sub(self, rhs: RHS) -> Self::Output;
// }

impl Sub for Cnt {
    type Output=Cnt;
    fn sub(self, rhs: Self) -> Self::Output {
        Cnt {
            dirty : self.dirty - rhs.dirty,
            eval  : self.eval - rhs.eval,
            change_prop : self.change_prop - rhs.change_prop,
        }
    }
}

impl Add for Cnt {
    type Output=Cnt;
    fn add(self, rhs: Self) -> Self::Output {
        Cnt {
            dirty : self.dirty + rhs.dirty,
            eval  : self.eval + rhs.eval,
            change_prop : self.change_prop + rhs.change_prop,
        }
    }
}

impl<'a> Add for &'a Cnt {
    type Output=Cnt;
    fn add(self, rhs: Self) -> Self::Output {
        Cnt {
            dirty : self.dirty + rhs.dirty,
            eval  : self.eval + rhs.eval,
            change_prop : self.change_prop + rhs.change_prop,
        }
    }
}

impl Zero for Cnt {
    fn zero() -> Self {
        Cnt {
            dirty : 0 as usize,
            change_prop : 0 as usize,
            eval : 0 as usize,
        }
    }
}
