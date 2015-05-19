use std::fmt::Debug;
use std::hash::{hash,Hash,Hasher,SipHasher};
use std::collections::HashMap;
use std::mem::replace;
use std::mem::transmute;
use std::rc::Rc;
use std::fmt;

use adapton_sigs::*;

#[derive(Debug)]
pub struct Frame {
    loc   : Rc<Loc>,       // The currently-executing node
    path  : Rc<Path>,      // The current path for creating new nodes; invariant: (prefix-of frame.loc.path frame.path)
    succs : Vec<DemSucc>,  // The currently-executing node's effects (viz., the nodes it demands)
}

// Each location identifies a node in the DCG.
#[derive(Hash,Debug,PartialEq,Eq,Clone)]
pub struct Loc {
    path : Rc<Path>,
    id   : Rc<ArtId<Name>>,
    hash : u64, // hash of (id,path)
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
// For a general account of the semantics of symbolics, see Chapter 31
// of PFPL 2nd Edition. Harper 2015:
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
    Rc(Rc<Symbol>),
}

#[derive(Debug)]
pub struct DemPrec {
    loc : Rc<Loc>,
}

#[derive(Debug)]
pub struct DemSucc {
    loc : Rc<Loc>,
    dirty : bool
}

#[derive(Debug)]
pub struct AdaptonState {
    table : HashMap<Rc<Loc>, Box<OpaqueNode>>,
    stack : Vec<Frame>
}

#[derive(Debug)]
pub enum Node<Res> {
    Pure(PureNode<Res>),
    Mut(MutNode<Res>),
    Compute(ComputeNode<Res>),
}

#[derive(Debug)]
pub struct PureNode<T> {
    loc : Rc<Loc>,
    val : T,
}

#[derive(Debug)]
pub struct MutNode<T> {
    loc : Rc<Loc>,
    creators  : Vec<DemPrec>,
    dem_precs : Vec<DemPrec>,
    val : T,
}

pub struct ComputeNode<Res> {
    loc : Rc<Loc>,
    creators : Vec<DemPrec>,
    dem_precs : Vec<DemPrec>,
    dem_succs : Vec<DemSucc>,
    res : Option<Res>,
    computer : Box<Computer<Res>>
}

pub trait Computer<Res> {    
    fn compute(self:&Self, st:&mut AdaptonState) -> Res;
}

pub trait OpaqueNode {
    fn loc (self:&Self) -> Rc<Loc> ;
    fn creators (self:&Self) -> Vec<DemPrec> ;
    fn dem_precs (self:&Self) -> Vec<DemPrec> ;
    fn dem_succs (self:&Self) -> Vec<DemSucc> ;
}

impl <Res> OpaqueNode for Node<Res> {
    fn loc (self:&Self) -> Rc<Loc> { panic!("") }
    fn creators (self:&Self) -> Vec<DemPrec> { panic!("") }
    fn dem_precs (self:&Self) -> Vec<DemPrec> { panic!("") }
    fn dem_succs (self:&Self) -> Vec<DemSucc> { panic!("") }
}
pub trait ComputerArg<Res> {
    type Arg;
    fn get_arg(self:&Self) -> Self::Arg;
    fn compute(self:&Self, st:&mut AdaptonState, arg:Self::Arg) -> Res;
}

impl<Arg,Res> Computer<Res> for ComputerArg<Res,Arg=Arg> {
    fn compute(self:&Self, st:&mut AdaptonState) -> Res {
        self.compute(st, self.get_arg())
    }
}

impl<Res> fmt::Debug for ComputeNode<Res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(ComputeNode)")
    }
}

impl fmt::Debug for OpaqueNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(OpaqueNode)")
    }
}

// fn dem_precs<'x,T:'x> (st: &'x mut AdaptonState, loc: &Rc<Loc>) -> &'x mut Vec<DemPrec> {    
//     let node = st.table.get(loc) ;
//     match node {
//         None => panic!("dangling pointer"),
//         Some(ptr) => {
//             match *(ptr.get_node()) {
//                 Node::Pure(_) => {
//                     panic!("Node::Pure: no precs")
//                 },
//                 Node::Compute(ref mut nd) => {
//                     &mut nd.dem_precs
//                 },
//                 Node::Mut(ref mut nd) => {
//                     &mut nd.dem_precs
//                 }
//             }
//         }
//     }    
// }

pub fn revoke_demand<'x> (st:&mut AdaptonState, src:&Rc<Loc>, succs:&Vec<DemSucc>) {
    // for succ in succs.iter() {
    //     let precs = dem_precs(st, &succ.loc);
    //     precs.retain(|ref prec| &prec.loc != src);
    // }
    panic!("")
}

pub fn invoke_demand<'x> (st:&mut AdaptonState, src:Rc<Loc>, succs:& Vec<DemSucc>) {
    // for succ in succs.iter() {
    //     let precs = dem_precs(st, &succ.loc);
    //     precs.push(DemPrec{loc:src.clone()})
    // }
    panic!("")
}

fn my_hash<T>(obj: T) -> u64
    where T: Hash
{
    let mut hasher = SipHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
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
        let h1 = my_hash( &(&nm, 11111111) ) ; // TODO: make this hashing better.
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
    
    fn cell<T:Eq+Debug> (self:&mut AdaptonState, id:ArtId<Self::Name>, val:T) -> MutArt<T,Self::Loc> {
        let path = self.stack[0].path.clone();
        let id   = Rc::new(id);
        let hash = my_hash(&(&path,&id));
        let loc = Rc::new(Loc{path:path,id:id,hash:hash});
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

    fn set<T:Eq+Debug> (self:&mut Self, cell:MutArt<T,Self::Loc>, val:T) {
    }

    fn thunk<Arg:Eq+Hash+Debug,T:Eq+Debug>
        (self:&mut AdaptonState,
         id:ArtId<Self::Name>, fn_body:Box<Fn(Arg)->T>, arg:Arg) -> Art<T,Self::Loc>
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
    
    fn force<'a,T:Eq+Debug+Clone> (self:&mut AdaptonState, art:Art<T,Self::Loc>) -> T {
        match art {
            Art::Box(b) => *b.clone(),
            Art::Loc(loc) => {
                let node = self.table.get_mut(&loc) ;
                match node {
                    None => panic!("dangling pointer"),
                    Some(ref ptr) => {
                        let node : &mut Node<T> = unsafe {
                            let node : *mut Node<T> = transmute::<_,_>(ptr) ;
                            &mut ( *node ) } ;
                        match *node {
                            Node::Pure(ref mut nd) => nd.val.clone(),
                            Node::Mut(ref mut nd) => {
                                if self.stack.is_empty() { } else {
                                    self.stack[0].succs.push(DemSucc{loc:loc.clone(),dirty:false});
                                } ;
                                nd.val.clone()
                            },
                            Node::Compute(ref mut nd) => {
                                self.stack.push ( Frame{loc:loc.clone(),
                                                        path:loc.path.clone(),
                                                        succs:Vec::new(), } );
                                let val = nd.computer.compute( panic!("TODO:self") ) ;
                                let frame = match self.stack.pop() {
                                    None => panic!(""),
                                    Some(frame) => frame } ;
                                
                                revoke_demand( self, &nd.loc, &nd.dem_succs );
                                invoke_demand( self, nd.loc.clone(), &frame.succs );
                                nd.dem_succs = frame.succs;
                                if self.stack.is_empty() { } else {
                                    self.stack[0].succs.push(DemSucc{loc:loc.clone(),dirty:false});
                                };
                                val
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn main () {


}
