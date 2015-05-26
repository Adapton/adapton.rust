use std::fmt::Debug;
use std::hash::{hash,Hash,Hasher,SipHasher};
use std::collections::HashMap;
use std::mem::replace;
use std::mem::transmute;
use std::rc::Rc;
use std::fmt;
use std::marker::PhantomData;

use adapton_sigs::*;

#[derive(Debug)]
pub struct Frame {
    loc   : Rc<Loc>,    // The currently-executing node
    path  : Rc<Path>,   // The current path for creating new nodes; invariant: (prefix-of frame.loc.path frame.path)
    succs : Vec<Succ>,  // The currently-executing node's effects (viz., the nodes it demands)
}

// Each location identifies a node in the DCG.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub struct Loc {
    path : Rc<Path>,
    id   : Rc<ArtId<Name>>,
    hash : u64, // hash of (path,id)
}

// Paths are built using the ns command.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum Path {
    Empty,
    Child(Rc<Path>,Name),
}

// Names provide a symbolic way to identify nodes.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub struct Name {
    hash : u64, // hash of symbol
    symbol : Rc<Symbol>,
}

// Symbols
//
// For a general semantics of symbols, see Chapter 31 of PFPL 2nd Edition. Harper 2015:
// http://www.cs.cmu.edu/~rwh/plbook/2nded.pdf
//
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub enum Symbol {
    Root, // Root identifies the outside environment of Rust code.
    Nil,  // Nil for non-symbolic, hash-based names.
    String(String), U64(u64),   // Strings and U64s are unique symbols.
    Pair(Rc<Symbol>,Rc<Symbol>),
    ForkL(Rc<Symbol>),
    ForkR(Rc<Symbol>),
    //Rc(Rc<Symbol>),
}

#[derive(PartialEq,Eq,Debug,Clone)]
pub enum Effect {
    Observe,
    Allocate,
}

#[derive(Debug,Clone)]
pub struct Succ {
    effect : Effect,
    dep    : Rc<Box<AdaptonDep>>, // Abstracted dependency information (e.g., for Observe Effect, the prior observed value)
    loc    : Rc<Loc>, // Target of the effect, aka, the successor, by this edge
    dirty  : bool,    // mutated to dirty when loc changes, or any of its successors change
}


pub trait AdaptonDep : Debug {
    fn change_prop     (self:&Self, st:&mut AdaptonState, loc:&Rc<Loc>) -> AdaptonRes ;
    fn re_produce      (self:&Self, st:&mut AdaptonState, loc:&Rc<Loc>) -> AdaptonRes ;
}

pub struct AdaptonRes {
    changed : bool,
}

pub trait AdaptonNode {
    // DCG structure:
    fn preds_alloc<'r> (self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> ;
    fn preds_obs<'r>   (self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> ;
    fn succs<'r>       (self:&'r mut Self) -> &'r mut Vec<Succ> ;
}

#[derive(Debug)]
pub struct AdaptonState {
    table : HashMap<Rc<Loc>, Box<AdaptonNode>>,
    stack : Vec<Frame>
}

// Structureful (Non-opaque) nodes:
#[derive(Debug)]
pub enum Node<Res> {
    Pure(PureNode<Res>),
    Mut(MutNode<Res>),
    Comp(CompNode<Res>),
}

// PureNode<T> for pure hash-consing of T's.
// Location in table never changes value.
#[derive(Debug)]
pub struct PureNode<T> {
    val : T,
}

// MutNode<T> for mutable content of type T.
// Location in table *does* change value, but only by *outer* environment.
// CompNode's do not directly change the value of MutNodes.
#[derive(Debug)]
pub struct MutNode<T> {
    preds_alloc : Vec<Rc<Loc>>,
    preds_obs   : Vec<Rc<Loc>>,
    val         : T,
}

// CompNode<Res> for a suspended computation whose resulting value
// of type T.  Location in table *does not* change its "compute",
// except that (1) the arguments for these "computes" may change and
// (2) by virue of depending on other "computes" and on MutNodes, the
// resulting value stored in field res may change.
pub struct CompNode<Res> {
    preds_alloc : Vec<Rc<Loc>>,
    preds_obs   : Vec<Rc<Loc>>,
    succs       : Vec<Succ>,
    producer    : Rc<Box<Producer<Res>>>, // E.g., a Box<App<Arg,Res>> where type Arg is hidden.
    res         : Option<Res>,
}
// Produce a value of type Res.
pub trait Producer<Res> {
    fn produce(self:&Self, st:&mut AdaptonState) -> Res;
}
pub trait MutableArg<Res> {
    type Arg;
    fn set_arg(self:&mut Self, Self::Arg);
    fn get_arg(self:&Self) -> Self::Arg;
}
// struct App is hidden by traits Comp<Res> and CompWithArg<Res>, below.
pub struct App<Arg,Res> {
    fn_body : Box<Fn(&mut AdaptonState,Arg)->Res>,
    arg     : Arg,
}


// ---------- Ret implementation of Producer:

pub enum Ret<Res> { Ret(Res) }

impl<Res:Clone> Producer<Res> for Ret<Res> {
    fn produce(self:&Self, _st:&mut AdaptonState) -> Res {
        let &Ret::Ret(ref val) = self;
        val.clone()
    }
}

// ---------- App implementation of Producer:

impl<Arg:Clone,Res:Clone> Producer<Res> for App<Arg,Res> {
    fn produce(self:&Self, st:&mut AdaptonState) -> Res {
        let fn_body = &self.fn_body;
        fn_body(st,self.arg.clone())
    }
}

impl<Arg:Clone,Res> MutableArg<Res> for App<Arg,Res> {
    type Arg = Arg;
    fn set_arg(self:&mut Self, arg:Arg) { replace(&mut self.arg, arg); }
    fn get_arg(self:&Self) -> Arg { self.arg.clone() }
}


// ----------- Location resolution:

pub fn abs_node_of_loc<'r> (_st:&'r mut AdaptonState, _loc:&Rc<Loc>) -> &'r mut Box<AdaptonNode> {
    // TODO-Later: Figure out how to get rustc to abstract a table lookup.
    panic!("")
    // match st.table.get_mut(loc) {
    //     None => panic!("dangling pointer"),
    //     Some(node) => { node }
    // }
}

pub fn res_node_of_loc<'r,Res> (st:&'r mut AdaptonState, loc:&Rc<Loc>) -> &'r mut Node<Res> {
    match st.table.get(loc) {
        None => panic!("dangling pointer"),
        Some(ref mut node) => {
            unsafe { transmute::<_,_>(node) }
        }
    }
}

// ---------- Node implementation:

impl <Res> AdaptonNode for Node<Res> {
    // TODO-Later: Implement these methods:
    fn preds_alloc<'r> (self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> { panic!("") }
    fn preds_obs<'r>   (self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> { panic!("") }
    fn succs<'r>       (self:&'r mut Self) -> &'r mut Vec<Succ>    { panic!("") }
}

// Performs the computation at loc, produces a result of type Res.
// Error if loc is not a Node::Comp.
fn produce<Res:'static+Clone+Debug+PartialEq+Eq>(st:&mut AdaptonState, loc:&Rc<Loc>) -> Res
{
    let succs : Vec<Succ> = {
        let succs : Vec<Succ> = Vec::new();
        let node : &mut Node<Res> = res_node_of_loc( st, loc ) ;
        replace(node.succs(), succs)
    } ;
    revoke_succs( st, loc, &succs );
    st.stack.push ( Frame{loc:loc.clone(),
                          path:loc.path.clone(),
                          succs:Vec::new(), } );
    let producer : Rc<Box<Producer<Res>>> = {
        let node : &mut Node<Res> = res_node_of_loc( st, loc ) ;
        match *node {
            Node::Comp(ref nd) => nd.producer.clone(),
            _ => panic!("internal error"),
        }
    } ;
    let res = producer.produce( st ) ;
    let frame = match st.stack.pop() {
        None => panic!("expected Some _: stack invariants are broken"),
        Some(frame) => frame
    } ;
    {
        let node : &mut Node<Res> = res_node_of_loc( st, loc ) ;
        match *node {
            Node::Comp(ref mut node) => {
                replace(&mut node.succs, frame.succs) ;
                replace(&mut node.res, Some(res.clone()))
            },
            Node::Mut(_) => panic!(""),
            Node::Pure(_) => panic!("")
        }
    } ;
    if st.stack.is_empty() { } else {
        st.stack[0].succs.push(Succ{loc:loc.clone(),
                                    effect:Effect::Observe,
                                    dep:Rc::new(Box::new(Some(res.clone()))),
                                    dirty:false});
    };
    res
}

// ---------- AdaptonDep implementation:

impl <Res:'static+Sized+Clone+Debug+PartialEq+Eq>
    AdaptonDep for Res
{
    fn re_produce(self:&Res, st:&mut AdaptonState, loc:&Rc<Loc>) -> AdaptonRes {
        let result : Res = produce( st, loc ) ;
        let changed = result == *self ;
        AdaptonRes{changed:changed}
    }

    fn change_prop(self:&Res, st:&mut AdaptonState, loc:&Rc<Loc>) -> AdaptonRes {
        let succs = {
            let node : &mut Node<Res> = res_node_of_loc(st, loc) ;
            assert!( match *node { Node::Comp(_) => true, _ => false } ) ;
            node.succs().clone()
        } ;
        for succ in succs.iter() {
            if succ.dirty {
                let dep = & succ.dep ;
                let res = dep.change_prop(st, &succ.loc) ;
                if res.changed {
                    return self.re_produce (st, &succ.loc)
                }
            }
        } ;
        // No early return =>
        //   all immediate dependencies are change-free:
        AdaptonRes{changed:false}
    }
}

// impl AdaptonDep for ()
// {
//     fn re_produce(self:&Self, st:&mut AdaptonState, loc:&Rc<Loc>) -> AdaptonRes {
//         // lack of dependency => lack of change
//         AdaptonRes{changed:false}
//     }
//     fn change_prop(self:&Self, st:&mut AdaptonState, loc:&Rc<Loc>) -> AdaptonRes {
//         // lack of dependency => lack of change
//         AdaptonRes{changed:false}
//     }
// }


// ---------- Node implementation:

impl<Res> fmt::Debug for CompNode<Res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(CompNode)")
    }
}

impl fmt::Debug for AdaptonNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(AdaptonNode)")
    }
}

pub fn revoke_succs<'x> (st:&mut AdaptonState, src:&Rc<Loc>, succs:&Vec<Succ>) {
    for succ in succs.iter() {
        let node : &mut Box<AdaptonNode> = abs_node_of_loc(st, &succ.loc) ;
        node.preds_obs().retain(|ref pred| **pred != *src);
    }
}

fn my_hash<T>(obj: T) -> u64
    where T: Hash
{
    let mut hasher = SipHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}

fn loc_of_id(path:Rc<Path>,id:Rc<ArtId<Name>>) -> Rc<Loc> {
    let hash = my_hash(&(&path,&id));
    Rc::new(Loc{path:path,id:id,hash:hash})
}

fn loc_of_name(path:Rc<Path>,name:Name) -> Rc<Loc> {
    loc_of_id(path,Rc::new(ArtId::Nominal(name)))
}

// Implement "sharing" of the dirty bit.
// The succ edge is returned as a mutable borrow, to permit checking
// and mutating the dirty bit.
fn get_succ_mut<'r>(st:&'r mut AdaptonState, src_loc:&Rc<Loc>, eff:Effect, tgt_loc:&Rc<Loc>) -> &'r mut Succ {
    let src_node = st.table.get_mut( src_loc ) ;
    match src_node {
        None => panic!("src_loc is dangling"),
        Some(nd) => {
            for succ in nd.succs().iter_mut() {
                if (succ.effect == eff) && (&succ.loc == tgt_loc) {
                    return succ
                } else {}
            } ;
            panic!("tgt_loc is dangling in src_node.dem_succs")
        }
    }
}

fn dirty_pred_observers(st:&mut AdaptonState, loc:&Rc<Loc>) {
    let pred_locs : Vec<Rc<Loc>> = {
        let node = st.table.get_mut(loc) ;
        match node {
            None => panic!("dangling pointer"),
            Some(nd) => { nd.preds_obs().clone() }}}
    ;
    for pred_loc in pred_locs {
        let stop : bool = {
            // The stop bit communicates information from st for use below.
            let succ = get_succ_mut(st, &pred_loc, Effect::Observe, &loc) ;
            if succ.dirty { true } else {
                replace(&mut succ.dirty, true);
                false
            }} ;
        if !stop {
            dirty_pred_observers(st,&pred_loc);
        } else {}
    }
}

impl Adapton for AdaptonState {
    type Name = Name;
    type Loc  = Loc;

    fn new () -> AdaptonState {
        let path   = Rc::new(Path::Empty);
        let symbol = Rc::new(Symbol::Root);
        let hash   = my_hash(&symbol);
        let name   = Name{symbol:symbol,hash:hash};

        let id     = Rc::new(ArtId::Nominal(name));
        let hash   = my_hash(&(&path,&id));
        let loc    = Rc::new(Loc{path:path.clone(),id:id,hash:hash});
        let mut stack = Vec::new();
        stack.push( Frame{loc:loc, path:path, succs:Vec::new()} ) ;
        AdaptonState {
            table : HashMap::new (),
            stack : stack,
        }
    }

    fn name_of_string (self:&mut AdaptonState, sym:String) -> Name {
        let h = my_hash(&sym);
        let s = Symbol::String(sym) ;
        Name{ hash:h, symbol:Rc::new(s) }
    }

    fn name_of_u64 (self:&mut AdaptonState, sym:u64) -> Name {
        let h = my_hash(&sym) ;
        let s = Symbol::U64(sym) ;
        Name{ hash:h, symbol:Rc::new(s) }
    }

    fn name_pair (self: &mut AdaptonState, fst: Name, snd: Name) -> Name {
        let h = my_hash( &(fst.hash,snd.hash) ) ;
        let p = Symbol::Pair(fst.symbol, snd.symbol) ;
        Name{ hash:h, symbol:Rc::new(p) }
    }

    fn name_fork (self:&mut AdaptonState, nm:Name) -> (Name, Name) {
        let h1 = my_hash( &(&nm, 11111111) ) ; // TODO-Later: make this hashing better.
        let h2 = my_hash( &(&nm, 22222222) ) ;
        ( Name{ hash:h1,
                symbol:Rc::new(Symbol::ForkL(nm.symbol.clone())) } ,
          Name{ hash:h2,
                symbol:Rc::new(Symbol::ForkR(nm.symbol)) } )
    }

    fn ns<T,F> (self: &mut Self, nm:Name, body:F) -> T where F:FnOnce(&mut Self) -> T {
        let path_body = Rc::new(Path::Child(self.stack[0].path.clone(), nm)) ;
        let path_pre = replace(&mut self.stack[0].path, path_body ) ;
        let x = body(self) ;
        let path_body = replace(&mut self.stack[0].path, path_pre) ;
        drop(path_body);
        x
    }

    fn put<T:Eq> (self:&mut AdaptonState, x:T) -> Art<T,Self::Loc> {
        Art::Box(Box::new(x))
    }

    fn cell<T:Eq+Debug+Clone
        +'static // TODO-Later: Needed on T because of lifetime issues.
        >
        (self:&mut AdaptonState, id:ArtId<Self::Name>, val:T) -> MutArt<T,Self::Loc> {
            let path = self.stack[0].path.clone();
            let id   = Rc::new(id);
            let hash = my_hash(&(&path,&id));
            let loc  = Rc::new(Loc{path:path,id:id,hash:hash});
            let cell = match self.table.get_mut(&loc) {
                None => None,
                Some(ref mut nd) => {
                    let creators = nd.preds_alloc() ;
                    if ! self.stack.is_empty () {
                        creators.push(self.stack[0].loc.clone())
                    } ;
                    Some(MutArt{loc:loc.clone(),
                                phantom:PhantomData})
                },
            } ;
            match cell {
                Some(cell) => {
                    self.set(cell, val.clone()) ;
                },
                None => {
                    let mut creators = Vec::new();
                    if ! self.stack.is_empty () {
                        creators.push(self.stack[0].loc.clone())
                    } ;
                    let node = Node::Mut(MutNode{
                        preds_alloc:creators,
                        preds_obs:Vec::new(),
                        val:val.clone(),
                    }) ;
                    self.table.insert(loc.clone(), Box::new(node));
                },
            } ;
            if ! self.stack.is_empty () {
                self.stack[0].succs.push(Succ{loc:loc.clone(),
                                              dep:Rc::new(Box::new(Some(val))),
                                              effect:Effect::Allocate,
                                              dirty:false});
            } ;
            MutArt{loc:loc,phantom:PhantomData}
        }

    fn set<T:Eq+Debug> (self:&mut Self, cell:MutArt<T,Self::Loc>, val:T) {
        assert!( self.stack.is_empty() );
        let changed : bool = {
            let node = self.table.get_mut(&cell.loc) ;
            match node {
            None => panic!("dangling location"),
            Some(nd) => {
                let node : &mut Node<T> = unsafe { transmute::<_,_>(nd) } ;
                match *node {
                    Node::Mut(ref mut nd) => {
                        if nd.val == val {
                            false
                        } else {
                            replace(&mut nd.val, val) ;
                            true
                        }},
                    _ => panic!("dangling location"),
                }},
            }} ;
        if changed {
            dirty_pred_observers(self, &cell.loc)
        }
        else { }
    }

    fn thunk<Arg:Eq+Hash+Debug+Clone
        +'static // Needed on Arg because of lifetime issues.
        ,T:Eq+Debug+Clone
        +'static // Needed on T because of lifetime issues.
        >
        (self:&mut AdaptonState,
         id:ArtId<Self::Name>,
         fn_body:Box<Fn(&mut AdaptonState,Arg)->T>,
         arg:Arg)
         -> Art<T,Self::Loc>
    {
        match id {
            ArtId::None => {
                Art::Box(Box::new(fn_body(self,arg)))
            },
            ArtId::Structural(hash) => {
                let loc = loc_of_id(self.stack[0].path.clone(),
                                    Rc::new(ArtId::Structural(hash)));
                let _ = {
                    // If the node exists; there's nothing else to do.
                    let node = self.table.get_mut(&loc);
                    match node {
                        Some(_) => { // Nothing to do; it's there.
                            return Art::Loc(loc)
                        },
                        None => { } }
                } ;
                let creators =
                    if self.stack.is_empty() {
                        Vec::new()
                    } else {
                        let pred = self.stack[0].loc.clone();
                        self.stack[0].succs.push(Succ{loc:loc.clone(),
                                                      dep:Rc::new(Box::new(())), // No dependencies
                                                      effect:Effect::Allocate,
                                                      dirty:false});
                        let mut v = Vec::new();
                        v.push(pred);
                        v
                    };
                let producer : Box<Producer<T>> = Box::new(App{fn_body:fn_body,arg:arg.clone()}) ;
                let node : CompNode<T> = CompNode{
                    preds_alloc:creators,
                    preds_obs:Vec::new(),
                    succs:Vec::new(),
                    producer:Rc::new(producer),
                    res:None,
                } ;
                self.table.insert(loc.clone(),
                                  Box::new(Node::Comp(node)));
                Art::Loc(loc)
            },
            ArtId::Nominal(nm) => {
                let loc = loc_of_id(self.stack[0].path.clone(),
                                    Rc::new(ArtId::Nominal(nm)));
                let _ = {
                    match self.table.get_mut(&loc) {
                        Some(nd) => {
                            // TODO-Now: Check if the computer's arg is the same, or if its different.
                            // If different, re-set argument; dirty its creators and observers.
                            return Art::Loc(loc)
                        },
                        None => { } }
                } ;
                // Next steps: Finish this
                panic!("TODO")
            }
        }
    }

    fn force<T:'static+Eq+Debug+Clone> (self:&mut AdaptonState,
                                        art:Art<T,Self::Loc>) -> T
    {
        match art {
            Art::Box(b) => *b.clone(),
            Art::Loc(loc) => {
                let (is_comp, cached_result) : (bool, Option<T>) = {
                    let node : &mut Node<T> = res_node_of_loc(self, &loc) ;
                    match *node {
                        Node::Pure(ref mut nd) => (false, Some(nd.val.clone())),
                        Node::Mut(ref mut nd)  => (false, Some(nd.val.clone())),
                        Node::Comp(ref mut nd) => (true,  nd.res.clone()),
                    }
                } ;
                let result = match cached_result {
                    None          => { assert!(is_comp); produce(self, &loc) },
                    Some(ref res) => {
                        if is_comp {
                            // Change-propagation precondition: loc is a computational node:
                            res.change_prop(self, &loc) ;
                            let node : &mut Node<T> = res_node_of_loc(self, &loc) ;
                            match *node {
                                Node::Comp(ref nd) => match nd.res {
                                    None => panic!("impossible"),
                                    Some(ref res) => res.clone()
                                },
                                _ => panic!("impossible"),
                            }}
                        else {
                            res.clone()
                        }
                    }
                } ;
                if self.stack.is_empty() { } else {
                    self.stack[0].succs.push(Succ{loc:loc.clone(),
                                                  dep:Rc::new(Box::new(Some(result.clone()))),
                                                  effect:Effect::Observe,
                                                  dirty:false});
                } ;
                result
            }
        }}
}

pub fn main () {


}
