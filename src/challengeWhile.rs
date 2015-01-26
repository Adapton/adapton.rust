use name::*;
use art::*;
use std::hash;
use std::hash::Hash;
use std::result::Result;
use std::collections::HashMap;

//
// Cmd C ::= Skip
//        | C ; C
//        | While E C
//        | x := E
//
//     C ::= .. | Amb C C
//
// Exp E ::=
//        | x
//        | N
//        | E + E
//

/// Nominal, artful commands for the while language
#[derive(Show,Hash,PartialEq,Eq)]
pub enum Cmd<'x> {
    Skip,
    Seq(CmdB<'x>, CmdB<'x>),
    While(ExpB<'x>, CmdB<'x>),
    Assign(Var<'x>, ExpB<'x>),
    // ----
    Name(Name, CmdB<'x>),
    Art(Art<'x,CmdB<'x>>),
}
pub type CmdB<'x> = Box<Cmd<'x>>;

/// Expressions for the while language
#[derive(Show,Hash,PartialEq,Eq)]
pub enum Exp<'x> {
    Var(Var<'x>),
    Plus(ExpB<'x>,ExpB<'x>),
    Num(int),
}
pub type ExpB<'x> = Box<Exp<'x>>;

/// Variables for the while language
pub type Var<'x> = String;

pub trait Heap<L:Hash+Eq, V> : Sized {
    fn empty () -> Self ;
    fn update (Self, L, V) -> Self ;
    fn select (Self, L) -> (Self, Option<V>) ;
}

pub struct HashMapHeap { hm : HashMap<String, int> }

impl Heap<String, int> for HashMapHeap {
    fn empty () -> Self {
        HashMapHeap { hm : HashMap::new() }
    }
    fn update (hm:Self, l:String, v:int) -> Self {
        panic!("")
    }
    fn select (hm:Self, l:String) -> (Self, Option<int>) {
        panic!("")
    }
}

pub enum Error {
    VarNotDef(String),
}
   
pub fn eval_exp<'x>
    (heap:HashMapHeap, exp:Exp<'x>)
     -> (HashMapHeap, Result<int, Error>)
{
    match exp {
        Exp::Num(n) => (heap, Ok(n)),
        Exp::Var(v) => {
            let (heap,q) = Heap::select(heap, v.clone()) ;
            match q {
                None => (heap, Err(Error::VarNotDef(v))),
                Some(val) => (heap, Ok(val))
            }},
        
        Exp::Plus(l, r) => {
            let (heap, l) = eval_exp(heap, *l) ;
            let (heap, r) = eval_exp(heap, *r) ;
            match (l,r) {
                (Ok(l), Ok(r)) => (heap, Ok(l+r)),
                (Err(l), _) => (heap, Err(l)),
                (_, Err(r)) => (heap, Err(r))
            }},
    }
}
   


#[test]
pub fn doit () {
    println!("while lang!");
}
