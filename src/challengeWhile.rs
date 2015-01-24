use name::*;
use art::*;
use std::hash;
use std::hash::Hash;
use std::result::Result;

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

pub trait Heap<T> {
    fn empty () -> Self ;
    fn update (Self, Hash, T) -> Self ;
    fn select (Self, Hash) -> Result<(Self,T), Self> ;
}

pub enum Err {
    VarNotDef(String)
}

pub fn eval_exp<'x,T:'x> (exp:Exp<'x>, heap:Box<Heap<int>>)
                          -> Result<(Box<Heap<int>>,int), (Box<Heap<int>>,Err)> {
    panic!("not impl")
}
   


#[test]
pub fn doit () {
    println!("while lang!");
}
