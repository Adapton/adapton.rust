use std::fmt::Debug;
use adapton_syntax::*;
use adapton_sigs::*;
use std::rc::Rc;
use std::hash::{Hash,Hasher};
use std::fmt::{Formatter,Result};
use std::mem::transmute;
use std::marker::PhantomData;

pub type Loc = usize;

#[derive(Debug,Clone,Hash,Eq,PartialEq)]
pub struct Name;

trait Void { }

pub struct AdaptonFromScratch {
    /// need a store; the Adapton trait provides a store semantics (viz., see `set` and `force`).
    store : Vec<Box<Void>>
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
        (self:&mut AdaptonFromScratch, nm:Name, val:T) -> MutArt<T,Loc>
    {
        let val : Box<Producer<T>> = Box::new( Val{val:Rc::new(val)} ) ;
        let val : Box<Void> = unsafe { transmute::<_,_>( val ) } ;
        MutArt{loc:{ self.store.push( val );
                     Rc::new(self.store.len()-1)},
               phantom:PhantomData
        }
    }

    fn set<T:'static+Eq+Debug+Clone>
        (self:&mut AdaptonFromScratch, cell:MutArt<T,Loc>, val:T)
    {
        let val : Box<Producer<T>> = Box::new( Val{val:Rc::new( val )} ) ;
        let val : Box<Void>  = unsafe { transmute::<_,_>( val ) } ;
        self.store.insert( *cell.loc, val );
    }

    fn thunk<Arg:Eq+Hash+Debug+Clone+'static,Spurious:'static+Clone,Res:Eq+Debug+Clone+'static>
        (self:&mut AdaptonFromScratch,
         id:ArtIdChoice<Name>,
         prog_pt:ProgPt,
         fn_box:Rc<Box<Fn(&mut AdaptonFromScratch, Arg, Spurious) -> Res>>,
         arg:Arg, spurious:Spurious)
         -> Art<Res,Loc>
    {
        let producer : Box<Producer<Res>> =
            Box::new(App{prog_pt:prog_pt,
                         fn_box:fn_box,
                         arg:arg.clone(),
                         spurious:spurious.clone()});
        let producer : Box<Void> =
            unsafe {
                // This transmute is always safe: `Void`s have no information.
                // However, transmute in `force` from `Void` to a `Producer<Res>` does require justification.
                // The justification is the phantom type `Res` in the `Art<Res,Loc>` type.
                transmute::<_,_>(producer)
            };        
        self.store.push( producer );
        let index = self.store.len()-1;
        println!("allocated {} as {:?}", index, arg);
        Art::Loc( Rc::new( index ) )
    }

    fn force<Res:'static+Eq+Debug+Clone> (self:&mut AdaptonFromScratch,
                                          art:&Art<Res,Loc>) -> Res
    {
        match *art {
            Art::Loc(ref index) => {
                let producer = {
                    let producer : &Box<Void> = & self.store[ **index ] ;
                    let producer : &Box<Producer<Res>> =
                        unsafe {
                            // This transmute is always safe:
                            // The justification is the phantom type `Res` in the `Art<Res,Loc>` type.
                            transmute::<_,_>(producer)
                        };
                    producer.copy()
                };
                println!("forcing {}", index);
                producer.produce(self)
            },
            Art::Rc(ref rc) => (**rc).clone(),
        }
    }
}

// Produce a value of type Res.
trait Producer<Res> {
    fn produce(self:&Self, st:&mut AdaptonFromScratch) -> Res;
    fn copy(self:&Self) -> Box<Producer<Res>>;
}

#[derive(Clone,Hash,Eq,PartialEq,Debug)]
struct Val<Res> {
    val:Rc<Res>
}

impl<Res:Clone+'static>
    Producer<Res> for Val<Res>
{
    fn produce(self:&Self, st:&mut AdaptonFromScratch) -> Res {
        (*self.val).clone()
    }
    fn copy(self:&Self) -> Box<Producer<Res>> {
        Box::new(Val{val:self.val.clone()})
    }
}

#[derive(Clone)]
struct App<Arg,Spurious,Res> {
    prog_pt:  ProgPt,
    fn_box:   Rc<Box<Fn(&mut AdaptonFromScratch, Arg, Spurious) -> Res>>,
    arg:      Arg,
    spurious: Spurious,
}

impl<Arg,Spurious,Res>
    Debug for App<Arg,Spurious,Res>
{
    fn fmt(&self, f: &mut Formatter) -> Result { self.prog_pt.fmt(f) }
}

impl<Arg:Hash,Spurious,Res>
    Hash for App<Arg,Spurious,Res>
{
    fn hash<H>(&self, state: &mut H) where H: Hasher { (&self.prog_pt,&self.arg).hash(state) }
}

impl<Arg:'static+PartialEq+Eq+Clone,Spurious:'static+Clone,Res:'static>
    Producer<Res>
    for App<Arg,Spurious,Res>
{
    fn produce(self:&Self, st:&mut AdaptonFromScratch) -> Res {
        let f = self.fn_box.clone() ;
        f (st,self.arg.clone(),self.spurious.clone())
    }
    fn copy(self:&Self) -> Box<Producer<Res>> {
        Box::new(App{
            prog_pt:self.prog_pt.clone(),
            fn_box:self.fn_box.clone(),
            arg:self.arg.clone(),
            spurious:self.spurious.clone(),
        })
    }
}
