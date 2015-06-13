use std::fmt::Debug;
use std::hash::{hash,Hash,Hasher,SipHasher};
use std::collections::HashMap;
use std::mem::replace;
use std::mem::transmute;
use std::rc::Rc;
use std::fmt;
use std::marker::PhantomData;
use std::fmt::{Formatter,Result};

use adapton_syntax::{ProgPt};

// use adapton_syntax::*;
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


// AdaptonDep abstracts over the value produced by a dependency, as
// well as mechanisms to update and/or re-produce it.
pub trait AdaptonDep : Debug {
    fn change_prop (self:&Self, st:&mut AdaptonState, loc:&Rc<Loc>) -> AdaptonRes ;
}

#[derive(Debug)]
pub struct NoDependency;
impl AdaptonDep for NoDependency {
    fn change_prop (self:&Self, _st:&mut AdaptonState, _loc:&Rc<Loc>) -> AdaptonRes { AdaptonRes{changed:false} }
}

#[derive(Debug)]
pub struct AllocDependency<T> { val:Rc<T> }
impl<T:Debug> AdaptonDep for AllocDependency<T> {
    fn change_prop (self:&Self, _st:&mut AdaptonState, _loc:&Rc<Loc>) -> AdaptonRes { AdaptonRes{changed:true} } // TODO-Later: Make this a little better.
}

pub struct AdaptonRes {
    changed : bool,
}

pub trait AdaptonNode {
    // DCG structure:
    fn preds_alloc<'r> (self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> ;
    fn preds_obs<'r>   (self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> ;
    fn succs_def<'r>   (self:&'r mut Self) -> bool ;
    fn succs<'r>       (self:&'r mut Self) -> &'r mut Vec<Succ> ;
}

pub trait ShapeShifter {
    fn be_node<'r> (self:&'r mut Self) -> &'r mut Box<AdaptonNode> ;
}

impl fmt::Debug for AdaptonNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(AdaptonNode)")
    }
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
    val : Rc<T>,
}

// MutNode<T> for mutable content of type T.
// The set operation mutates a MutNode; set may only be called by *outer* Rust environment.
// Its notable that the CompNodes' producers do not directly change the value of MutNodes with set.
// They may indirectly mutate these nodes by performing nominal allocation; mutation is limited to "one-shot" changes.
#[derive(Debug)]
pub struct MutNode<T> {
    preds_alloc : Vec<Rc<Loc>>,
    preds_obs   : Vec<Rc<Loc>>,
    val         : Rc<T>,
}

// CompNode<Res> for a suspended computation whose resulting value of
// type T.  The result of the CompNode is affected in two ways: the
// (1) producer may change, which may affect the result and (2) the
// values produced by the successors may change, indirectly
// influencing how the producer produces its resulting value.
pub struct CompNode<Res> {
    preds_alloc : Vec<Rc<Loc>>,
    preds_obs   : Vec<Rc<Loc>>,
    succs       : Vec<Succ>,
    producer    : Box<Producer<Res>>, // Producer can be App<Arg,Res>, where type Arg is hidden.
    res         : Option<Rc<Res>>,
}
// Produce a value of type Res.
pub trait Producer<Res> {
    fn produce(self:&Self, st:&mut AdaptonState) -> Rc<Res>;
    fn copy(self:&Self) -> Box<Producer<Res>>;
    fn eq(self:&Self, other:&Producer<Res>) -> bool;
    fn prog_pt<'r>(self:&'r Self) -> &'r ProgPt;
}
// Consume a value of type Arg.
pub trait Consumer<Arg> {
    fn consume(self:&mut Self, Rc<Arg>);
    fn get_arg(self:&mut Self) -> Rc<Arg>;
}
// struct App is hidden by traits Comp<Res> and CompWithArg<Res>, below.
#[derive(Clone)]
pub struct App<Arg,Res> {
    prog_pt: ProgPt,
    fn_box:  Rc<Box<Fn(&mut AdaptonState, Rc<Arg>) -> Rc<Res>>>,
    arg:     Rc<Arg>,
}

// ---------- App implementation of Debug and Hash

impl<Arg,Res> Debug for App<Arg,Res> {
    fn fmt(&self, f: &mut Formatter) -> Result { self.prog_pt.fmt(f) }
}

impl<Arg:Hash,Res> Hash for App<Arg,Res> {
    fn hash<H>(&self, state: &mut H) where H: Hasher { (&self.prog_pt,&self.arg).hash(state) }
}

// ---------- App implementation of Producer and Consumer traits:

impl<Arg:'static+PartialEq+Eq,Res:'static> Producer<Res> for App<Arg,Res> {
    fn produce(self:&Self, st:&mut AdaptonState) -> Rc<Res> {
        let f = self.fn_box.clone() ;
        f (st,self.arg.clone())
    }
    fn copy(self:&Self) -> Box<Producer<Res>> {
        Box::new(App{
            prog_pt:self.prog_pt.clone(),
            fn_box:self.fn_box.clone(),
            arg:self.arg.clone(),
        })
    }
    fn prog_pt<'r>(self:&'r Self) -> &'r ProgPt {
        & self.prog_pt
    }
    fn eq (&self, other:&Producer<Res>) -> bool {
        if &self.prog_pt == other.prog_pt() {
            let other = Box::new(other) ;
            let other : &Box<App<Arg,Res>> = unsafe { transmute::<_,_>( other ) } ;
            self.arg == other.arg
        } else {
            false
        }
    }
}
impl<Arg:Clone+PartialEq+Eq,Res> Consumer<Arg> for App<Arg,Res> {
    fn consume(self:&mut Self, arg:Rc<Arg>) { replace(&mut self.arg, arg); }
    fn get_arg(self:&mut Self) -> Rc<Arg>   { self.arg.clone() }
}

// ----------- Location resolution:

pub fn lookup_abs<'r>(st:&'r mut AdaptonState, loc:&Rc<Loc>) -> &'r mut Box<AdaptonNode> {
    match st.table.get_mut( loc ) {
        None => panic!("dangling pointer"),
        Some(node) => node.be_node() // This is a weird workaround; TODO-Later: Investigate.
    }
}

// This only is safe in contexts where the type of loc is known.
// Unintended double-uses of names and hashes will generally cause uncaught type errors.
pub fn res_node_of_loc<'r,Res> (st:&'r mut AdaptonState, loc:&Rc<Loc>) -> &'r mut Box<Node<Res>> {
    let abs_node = lookup_abs(st, loc) ;
    unsafe { transmute::<_,_>(abs_node) }
}

// ---------- Node implementation:

impl <Res> AdaptonNode for Node<Res> {
    fn preds_alloc<'r>(self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> {
        match *self { Node::Mut(ref mut nd) => &mut nd.preds_alloc,
                      Node::Comp(ref mut nd) => &mut nd.preds_alloc,
                      Node::Pure(_) => unreachable!(),
        }}
                      
    fn preds_obs<'r>(self:&'r mut Self) -> &'r mut Vec<Rc<Loc>> {
        match *self { Node::Mut(ref mut nd) => &mut nd.preds_obs,
                      Node::Comp(ref mut nd) => &mut nd.preds_obs,
                      Node::Pure(_) => unreachable!(),
        }}
    fn succs_def<'r>(self:&'r mut Self) -> bool {
        match *self { Node::Comp(_) => true, _ => false
        }}
    fn succs<'r>(self:&'r mut Self) -> &'r mut Vec<Succ> {
        match *self { Node::Comp(ref mut n) => &mut n.succs,
                     _ => panic!("undefined"),
        }
    }
}

impl <Res> ShapeShifter for Box<Node<Res>> {
    fn be_node<'r>(self:&'r mut Self) -> &'r mut Box<AdaptonNode> {
        // TODO-Later: Why is this transmute needed here ??
        unsafe { transmute::<_,_>(self) }
    }
}

impl ShapeShifter for Box<AdaptonNode> {
    fn be_node<'r>(self:&'r mut Self) -> &'r mut Box<AdaptonNode> {
        // TODO-Later: Why is this transmute needed here ??
        unsafe { transmute::<_,_>(self) }
    }
}



impl<Res> fmt::Debug for CompNode<Res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(CompNode)")
    }
}

// Performs the computation at loc, produces a result of type Res.
// Error if loc is not a Node::Comp.
pub fn produce<Res:'static+Debug+PartialEq+Eq>(st:&mut AdaptonState, loc:&Rc<Loc>) -> Rc<Res>
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
    let producer : Box<Producer<Res>> = {
        let node : &mut Node<Res> = res_node_of_loc( st, loc ) ;
        match *node {
            Node::Comp(ref nd) => nd.producer.copy(),
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
            _ => panic!("internal error"),
        }
    } ;
    if st.stack.is_empty() { } else {
        st.stack[0].succs.push(Succ{loc:loc.clone(),
                                    effect:Effect::Observe,
                                    dep:Rc::new(Box::new(ProducerDep{res:res.clone()})),
                                    dirty:false});
    };
    res
}

fn re_produce<Res:'static+Debug+PartialEq+Eq>(dep:&ProducerDep<Res>, st:&mut AdaptonState, loc:&Rc<Loc>) -> AdaptonRes {
    let result : Rc<Res> = produce( st, loc ) ;
    let changed = result == dep.res ;
    AdaptonRes{changed:changed}
}


// ---------- AdaptonDep implementation:

#[derive(Debug)]
pub struct ProducerDep<T> { res:Rc<T> }
impl <Res:'static+Sized+Debug+PartialEq+Eq>
    AdaptonDep for ProducerDep<Res>
{
    fn change_prop(self:&Self, st:&mut AdaptonState, loc:&Rc<Loc>) -> AdaptonRes {
        { // Handle cases where there is no internal computation to re-compute:
            let node : &mut Node<Res> = res_node_of_loc(st, loc) ;
            match *node {
                Node::Comp(_) => (),
                Node::Pure(_) =>
                    return AdaptonRes{changed:false},
                Node::Mut(ref nd) =>
                    return AdaptonRes{changed:nd.val == self.res},
            }
        };
        let succs = {
            let node : &mut Node<Res> = res_node_of_loc(st, loc) ;
            assert!( node.succs_def() );
            node.succs().clone()
        } ;
        for succ in succs.iter() {
            if succ.dirty {
                let dep = & succ.dep ;
                let res = dep.change_prop(st, &succ.loc) ;
                if res.changed {
                    return re_produce (self, st, &succ.loc)
                }
            }
        } ;
        // No early return =>
        //   all immediate dependencies are change-free:
        AdaptonRes{changed:false}
    }
}

// ---------- Node implementation:

pub fn my_hash<T>(obj: T) -> u64
    where T: Hash
{
    let mut hasher = SipHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}

pub fn revoke_succs<'x> (st:&mut AdaptonState, src:&Rc<Loc>, succs:&Vec<Succ>) {
    for succ in succs.iter() {
        let node : &mut Box<AdaptonNode> = lookup_abs(st, &succ.loc) ;
        node.preds_obs().retain  (|ref pred| **pred != *src);
        node.preds_alloc().retain(|ref pred| **pred != *src);
    }
}

pub fn loc_of_id(path:Rc<Path>,id:Rc<ArtId<Name>>) -> Rc<Loc> {
    let hash = my_hash(&(&path,&id));
    Rc::new(Loc{path:path,id:id,hash:hash})
}

// Implement "sharing" of the dirty bit.
// The succ edge is returned as a mutable borrow, to permit checking
// and mutating the dirty bit.
pub fn get_succ_mut<'r>(st:&'r mut AdaptonState, src_loc:&Rc<Loc>, eff:Effect, tgt_loc:&Rc<Loc>) -> &'r mut Succ {
    let nd = lookup_abs( st, src_loc );
    for succ in nd.succs().iter_mut() {
        if (succ.effect == eff) && (&succ.loc == tgt_loc) {
            return succ
        } else {}
    } ;
    panic!("tgt_loc is dangling in src_node.dem_succs")
}

pub fn dirty_pred_observers(st:&mut AdaptonState, loc:&Rc<Loc>) {
    let pred_locs : Vec<Rc<Loc>> = lookup_abs( st, loc ).preds_obs().clone() ;
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

pub fn dirty_alloc(st:&mut AdaptonState, loc:&Rc<Loc>) {
    dirty_pred_observers(st, loc);
    let pred_locs : Vec<Rc<Loc>> = lookup_abs(st, loc).preds_alloc().clone() ;
    for pred_loc in pred_locs {
        let stop : bool = {
            // The stop bit communicates information from st for use below.
            let succ = get_succ_mut(st, &pred_loc, Effect::Allocate, &loc) ;
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

    fn put<T:Eq> (self:&mut AdaptonState, x:Rc<T>) -> Art<T,Self::Loc> { Art::Rc(x) }

    fn cell<T:Eq+Debug
        +'static // TODO-Later: Needed on T because of lifetime issues.
        >
        (self:&mut AdaptonState, id:ArtId<Self::Name>, val:Rc<T>) -> MutArt<T,Self::Loc> {
            let path = self.stack[0].path.clone();
            let id   = Rc::new(id);
            let hash = my_hash(&(&path,&id));
            let loc  = Rc::new(Loc{path:path,id:id,hash:hash});
            let cell = match self.table.get_mut(&loc) {
                None => None,
                Some(ref mut _nd) => {
                    Some(MutArt{loc:loc.clone(),
                                phantom:PhantomData})
                },
            } ;
            match cell {
                Some(cell) => {
                    let cell_loc = cell.loc.clone();
                    self.set(cell, val.clone()) ;
                    if ! self.stack.is_empty () {
                        // Current loc is an alloc predecessor of the cell:
                        let top_loc = self.stack[0].loc.clone();
                        let cell_nd = lookup_abs(self, &cell_loc);
                        cell_nd.preds_alloc().push(top_loc)
                    }
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
                                              dep:Rc::new(Box::new(AllocDependency{val:val})),
                                              effect:Effect::Allocate,
                                              dirty:false});
            } ;
            MutArt{loc:loc,phantom:PhantomData}
        }

    fn set<T:Eq+Debug> (self:&mut Self, cell:MutArt<T,Self::Loc>, val:Rc<T>) {
        assert!( self.stack.is_empty() ); // outer layer has control.
        let changed : bool = {
            let node = lookup_abs( self, &cell.loc ) ;
            let node : &mut Node<T> = unsafe { transmute::<_,_>(node) } ;
            match *node {
                Node::Mut(ref mut nd) => {
                    if nd.val == val {
                        false
                    } else {
                        replace(&mut nd.val, val) ;
                        true
                    }},
                _ => unreachable!(),
            }} ;
        if changed {
            dirty_alloc(self, &cell.loc)
        }
        else { }
    }

    fn thunk<Arg:Eq+Hash+Debug
        +'static // Needed on Arg because of lifetime issues.
        ,T:Eq+Debug
        +'static // Needed on T because of lifetime issues.
        >
        (self:&mut AdaptonState,
         id:ArtId<Self::Name>,
         prog_pt:ProgPt,
         fn_box:Rc<Box<Fn(&mut AdaptonState, Rc<Arg>) -> Rc<T>>>,
         arg:Rc<Arg>)
         -> Art<T,Self::Loc>
    {
        match id {
            ArtId::Eager => {
                Art::Rc(fn_box(self,arg))
            },
            ArtId::Structural(hash) => {
                let loc = loc_of_id(self.stack[0].path.clone(),
                                    Rc::new(ArtId::Structural(hash)));
                {   // If the node exists, return early.
                    let node = self.table.get_mut(&loc);
                    match node { None    => { },
                                 Some(_) => { return Art::Loc(loc) }, // Nothing to do; it already exists.
                    }
                } ;
                // assert: node does not exist.
                let creators =
                    if self.stack.is_empty() { Vec::new() }
                    else {
                        let pred = self.stack[0].loc.clone();
                        self.stack[0].succs.push(Succ{loc:loc.clone(),
                                                      dep:Rc::new(Box::new(NoDependency)),
                                                      effect:Effect::Allocate,
                                                      dirty:false});
                        let mut v = Vec::new();
                        v.push(pred);
                        v
                    };
                let producer : Box<Producer<T>> =
                    Box::new(App{prog_pt:prog_pt,
                                 fn_box:fn_box,
                                 arg:arg.clone()})
                    ;
                let node : CompNode<T> = CompNode{
                    preds_alloc:creators,
                    preds_obs:Vec::new(),
                    succs:Vec::new(),
                    producer:producer,
                    res:None,
                } ;
                self.table.insert(loc.clone(),
                                  Box::new(Node::Comp(node)));
                Art::Loc(loc)
            },
            
            ArtId::Nominal(nm) => {
                let loc = loc_of_id(self.stack[0].path.clone(),
                                    Rc::new(ArtId::Nominal(nm)));
                let producer : App<Arg,T> =
                    App{prog_pt:prog_pt,
                        fn_box:fn_box,
                        arg:arg.clone()}
                ;
                let do_dirty : bool =
                { match self.table.get_mut(&loc) {
                    None => panic!("dangling pointer"),
                    Some(ref mut nd) => {
                        let res_nd: &mut Node<T> = unsafe { transmute::<_,_>( nd ) };
                        let comp_nd: &mut CompNode<T> = match *res_nd {
                            Node::Pure(_)=> unreachable!(),
                            Node::Mut(_) => panic!("TODO-Sometime"),
                            Node::Comp(ref mut comp) => comp
                        } ;
                        let equal_producers : bool = comp_nd.producer.eq( &producer ) ;
                        let consumer:&mut Box<Consumer<Arg>> =
                            unsafe { transmute::<_,_>( &mut comp_nd.producer ) }
                        ;
                        if equal_producers {
                            if consumer.get_arg() == arg {
                                // Same argument; Nothing else to do:
                                false
                            }
                            else {
                                consumer.consume(arg.clone());
                                true
                            }}
                        else {
                            panic!("TODO-Sometime: producers not equal!")
                        }
                    }}} ;
                if do_dirty {
                    dirty_alloc(self, &loc)
                } ;
                let creators = {
                    if self.stack.is_empty() { Vec::new() }
                    else
                    {
                        let pred = self.stack[0].loc.clone();
                        self.stack[0].succs.push(Succ{loc:loc.clone(),
                                                      dep:Rc::new(Box::new(AllocDependency{val:arg})),
                                                      effect:Effect::Allocate,
                                                      dirty:false});
                        let mut v = Vec::new();
                        v.push(pred);
                        v
                    }};
                let node : CompNode<T> = CompNode{
                    preds_alloc:creators,
                    preds_obs:Vec::new(),
                    succs:Vec::new(),
                    producer:Box::new(producer),
                    res:None,
                } ;
                self.table.insert(loc.clone(),
                                  Box::new(Node::Comp(node)));
                Art::Loc(loc)
            }
        }
    }

    fn force<T:'static+Eq+Debug> (self:&mut AdaptonState,
                                  art:Art<T,Self::Loc>) -> Rc<T>
    {
        match art {
            Art::Rc(v) => v.clone(),
            Art::Loc(loc) => {
                let (is_comp, cached_result) : (bool, Option<Rc<T>>) = {
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
                            // ProducerDep change-propagation precondition:
                            // loc is a computational node:
                            ProducerDep{res:res.clone()}.change_prop(self, &loc) ;
                            let node : &mut Node<T> = res_node_of_loc(self, &loc) ;
                            match *node {
                                Node::Comp(ref nd) => match nd.res {
                                    None => unreachable!(),
                                    Some(ref res) => res.clone()
                                },
                                _ => unreachable!(),
                            }}
                        else {
                            res.clone()
                        }
                    }
                } ;
                if self.stack.is_empty() { } else {
                    self.stack[0].succs.push(Succ{loc:loc.clone(),
                                                  dep:Rc::new(Box::new(ProducerDep{res:result.clone()})),
                                                  effect:Effect::Observe,
                                                  dirty:false});
                } ;
                result
            }
        }}
}

pub fn main () { }
