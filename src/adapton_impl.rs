#![feature(box_syntax)]

use std::fmt::Debug;
use std::hash::{hash,Hash,SipHasher};
use std::collections::HashMap;
//use std::thunk::Invoke;
use std::mem::replace;
use std::mem::transmute;
//use std::sync::Arc;
use std::rc::Rc;
use std::fmt;

use adapton_sigs::*;

impl Adapton for AdaptonState {
    fn new () -> AdaptonState {
        let empty = Rc::new(Path::Empty);
        let root = Rc::new(Name{hash:0, lineage:Rc::new(Lineage::Root) });
        let mut stack = Vec::new();
        stack.push( Frame{path:empty, name:root, succs:Vec::new()} ) ;
        AdaptonState {
            table : HashMap::new (),
            stack : stack,
        }
    }
    
    fn name_of_string (self:&mut AdaptonState, sym:String) -> Name {
        let h = hash::<_,SipHasher>(&sym) ;
        let s = Lineage::String(sym) ;
        Name{ hash:h, lineage:Rc::new(s) }
    }
    
    fn name_of_u64 (self:&mut AdaptonState, sym:u64) -> Name {
        let h = hash::<_,SipHasher>(&sym) ;
        let s = Lineage::U64(sym) ;
        Name{ hash:h, lineage:Rc::new(s) }
    }
    
    fn name_pair (self: &AdaptonState, fst: Name, snd: Name) -> Name {
        let h = hash::<_,SipHasher>( &(fst.hash,snd.hash) ) ;
        let p = Lineage::Pair(fst.lineage, snd.lineage) ;
        Name{ hash:h, lineage:Rc::new(p) }
    }
    
    fn name_fork (self:&mut AdaptonState, nm:Name) -> (Name, Name) {
        let h1 = hash::<_,SipHasher>( &(&nm, 11111111) ) ; // TODO: make this hashing better.
        let h2 = hash::<_,SipHasher>( &(&nm, 22222222) ) ;
        ( Name{ hash:h1,
                lineage:Rc::new(Lineage::ForkL(nm.lineage.clone())) } ,
          Name{ hash:h2,
                lineage:Rc::new(Lineage::ForkR(nm.lineage)) } )
    }
    
    fn ns<T,F> (self: &mut Self, nm:Name, body:F) -> T where F:FnOnce(&mut Self) -> T {
        let path_body = Rc::new(Path::Child(self.stack[0].path.clone(), nm)) ;
        let path_pre = replace(&mut self.stack[0].path, path_body ) ;
        let x = body(self) ;
        let path_body = replace(&mut self.stack[0].path, path_pre) ;
        drop(path_body);
        x
    }
    
    fn put<T:Eq> (self:&mut AdaptonState, x:T) -> Art<T> {
        Art::Box(Box::new(x))
    }
    
    fn cell<T:Eq+Debug> (self:&mut AdaptonState, id:ArtId, val:T) -> MutArt<T> {
        let loc = match id {
            ArtId::None => panic!("a cell requires a unique identity"),
            ArtId::Structural(hash) => {
                Rc::new(Loc{
                    hash:hash,
                    name:Rc::new(Name{hash:hash,
                                      lineage:Rc::new(Lineage::Structural)}),
                    path:self.stack[0].path.clone()
                })
            }
            ArtId::Nominal(nm) => {
                Rc::new(Loc{
                    hash:nm.hash,
                    name:Rc::new(nm),
                    path:self.stack[0].path.clone() })
            }} ;
        let mut node = Node::Mut(MutNode{
            loc:loc.clone(),
            dem_precs:Vec::new(),
            creators:Vec::new(),
            val:val
        }) ;
        // TODO: Check to see if the cell exists;
        // check if its content has changed.
        // dirty its precs if so.

        // self.table.insert(loc.clone(), Box::new(node)) ;
        self.table.insert(loc.clone(), panic!("Box::new(node)")) ;
        
        MutArt::MutArt(Art::Loc(loc))
    }

    fn set<T:Eq+Debug> (self:&mut Self, cell:MutArt<T>, val:T) {
        match cell {
            MutArt::MutArt(Art::Box(b)) => {
                panic!("cannot set pure value");
            }
            MutArt::MutArt(Art::Loc(loc)) => {
                let node = self.table.get(&loc) ;
                let node = match node {
                    None => panic!("dangling pointer"),
                    Some(ptr) => {
                        match cell { MutArt::MutArt( art ) => {
                            match **node_of_opaque(art, ptr) {
                                Node::Mut(ref mut nd) => nd,
                                ref nd => panic!("impossible")
                            }
                        }}
                    }};
                if (node.val == val) {
                    // Nothing.
                }
                else {
                    node.val = val;
                    // TODO: Dirty traversal. Notify/mark demand precs.
                }
            }
        }
    }

    fn thunk<Arg:Eq+Hash+Debug,T:Eq+Debug>
        (self:&mut AdaptonState,
         id:ArtId, fn_body:Box<Fn(Arg)->T>, arg:Arg) -> Art<T>
    {
        match id {
            ArtId::None => {
                Art::Box(Box::new(fn_body((arg))))
            },
            ArtId::Structural(hash) => {
                panic!("")
            },
            ArtId::Nominal(nm) => {
                panic!("")
            }
        }
    }
    
    fn force<T:Eq+Debug> (self:&mut AdaptonState, art:Art<T>) -> & T {
        match art {
            Art::Box(b) => & b,
            Art::Loc(loc) => {
                let node = self.table.get_mut(&loc) ;
                match node {
                    None => panic!("dangling pointer"),
                    Some(ref ptr) => {
                        let node : &mut Node<T> = unsafe {
                            let node : *mut Node<T> = panic!("transmute::<*mut (), *mut Node<T>>(*ptr)") ;
                            &mut ( *node ) } ;
                        match *node {
                            Node::Pure(ref mut nd) => {
                                & nd.val
                            },
                            Node::Mut(ref mut nd) => {
                                if self.stack.is_empty() { } else {
                                    self.stack[0].succs.push(DemSucc{loc:loc.clone(),dirty:false});
                                } ;
                                & nd.val
                            },
                            Node::Compute(ref mut nd) => {
                                self.stack.push ( Frame{name:loc.name.clone(),
                                                        path:loc.path.clone(),
                                                        succs:Vec::new(), } );
                                let val = panic!("TODO: run compute node body") ;
                                let mut frame = match
                                    self.stack.pop() { None => panic!(""), Some(frame) => frame } ;
                                revoke_demand( self, &nd.loc, &nd.dem_succs );
                                invoke_demand( self, nd.loc.clone(), &frame.succs );
                                nd.dem_succs = frame.succs;
                                if self.stack.is_empty() { } else {
                                    self.stack[0].succs.push(DemSucc{loc:loc.clone(),dirty:false});
                                };
                                & val
                            }
                        }
                    }
                }
            }
        }
    }    
}
