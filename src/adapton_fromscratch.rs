use std::fmt::Debug;
use adapton_syntax::*;
use adapton_sigs::*;
use std::rc::Rc;
use std::hash::{Hash,Hasher};

pub type Loc = usize;

#[derive(Debug,Clone,Hash,Eq,PartialEq)]
pub struct Name;

#[derive(Debug)]
pub struct Whatever;
pub struct AdaptonFromScratch {
    /// just need a store; the Adapton trait provides a store semantics
    store : Vec<Box<Whatever>>
}

impl Adapton for AdaptonFromScratch {
    type Name = Name;
    type Loc  = Loc;

    fn new () -> AdaptonFromScratch {
        AdaptonFromScratch {
            store : Vec::new (),
        }
    }

    fn name_of_string (self:&mut AdaptonFromScratch, sym:String) -> Name { Name }
    fn name_of_u64 (self:&mut AdaptonFromScratch, sym:u64) -> Name { Name }
    fn name_pair (self: &mut AdaptonFromScratch, fst:Name, snd:Name) -> Name { Name }
    fn name_fork (self:&mut AdaptonFromScratch, nm:Name) -> (Name, Name) { (Name,Name) }
    fn ns<T,F> (self: &mut AdaptonFromScratch, nm:Name, body:F) -> T where F:FnOnce(&mut AdaptonFromScratch) -> T { body(self) }
    fn put<T:Eq> (self:&mut AdaptonFromScratch, x:T) -> Art<T,Loc> { Art::Rc(Rc::new(x)) }

    fn cell<T:Eq+Debug+Clone
        +'static // TODO-Later: Needed on T because of lifetime issues.
        >
        (self:&mut AdaptonFromScratch, nm:Name, val:T) -> MutArt<T,Loc> {
            panic!("")
        }

    fn set<T:Eq+Debug> (self:&mut AdaptonFromScratch, cell:MutArt<T,Loc>, val:T) {
        panic!("")
    }

    fn thunk<Arg:Eq+Hash+Debug+Clone+'static,Spurious:'static+Clone,Res:Eq+Debug+Clone+'static>
        (self:&mut AdaptonFromScratch,
         id:ArtIdChoice<Name>,
         prog_pt:ProgPt,
         fn_box:Rc<Box<Fn(&mut AdaptonFromScratch, Arg, Spurious) -> Res>>,
         arg:Arg, spurious:Spurious)
         -> Art<Res,Loc>
    {
        panic!("")
    }

    fn force<T:'static+Eq+Debug+Clone> (self:&mut AdaptonFromScratch,
                                        art:&Art<T,Loc>) -> T
    {
        panic!("")
    }
}
