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

use adapton_impl::*;

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

    fn put<T:Eq> (self:&mut AdaptonState, x:Rc<T>) -> Art<T,Self::Loc> {
        Art::Box(x)
    }

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
                        let cell_nd = abs_node_of_loc(self, &cell_loc);
                        cell_nd.preds_alloc().push(top_loc);
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
                    _ => unreachable!(),
                }},
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
            ArtId::None => {
                Art::Box(Box::new(fn_box(self,arg)))
            },
            ArtId::Structural(hash) => {
                let loc = loc_of_id(self.stack[0].path.clone(),
                                    Rc::new(ArtId::Structural(hash)));
                {   // If the node exists; there's nothing else to do.
                    let node = self.table.get_mut(&loc);
                    match node { None    => { },
                                 Some(_) => { return Art::Loc(loc) }, // Nothing to do; it already exists.
                    }
                } ;
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
                let producer : Box<Producer<T>> = Box::new(App{prog_pt:prog_pt,fn_box:fn_box,arg:arg.clone()}) ;
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
                let producer : Box<Producer<T>> = Box::new(App{prog_pt:prog_pt,fn_box:fn_box,arg:arg.clone()}) ;
                { match self.table.get_mut(&loc) {
                    None => { },
                    Some(ref mut nd) => {
                        let res_nd: &mut Node<T> = unsafe { transmute::<_,_>( nd ) };
                        let comp_nd: &mut CompNode<T> = match *res_nd {
                            Node::Pure(_)=> panic!("impossible"),
                            Node::Mut(_) => panic!("TODO-Sometime"),
                            Node::Comp(ref mut comp) => comp
                        } ;
                        let consumer:&mut Box<Consumer<Arg>> = unsafe { transmute::<_,_>( &mut comp_nd.producer ) };
                        if panic!("&producer == nd_producer") {
                            if consumer.get_arg() == arg {
                                // Same argument; Nothing else to do:
                                return Art::Loc(loc)
                            }
                            else {
                                consumer.consume(arg);
                                dirty_alloc(self, &loc);
                                return Art::Loc(loc)
                            }}
                        else {
                            // TODO-Sometime: Not yet implemented
                            panic!("producers not equal!")
                        }
                    }}}
                ;
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
                    producer:producer,
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
                            // ProducerDep change-propagation precondition:
                            // loc is a computational node:
                            ProducerDep{res:res.clone()}.change_prop(self, &loc) ;
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
                                                  dep:Rc::new(Box::new(ProducerDep{res:result.clone()})),
                                                  effect:Effect::Observe,
                                                  dirty:false});
                } ;
                result
            }
        }}
}
