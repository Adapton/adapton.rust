use name::*;
use art::*;
use std::hash;
use std::hash::Hash;

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

#[derive(Show,Hash,PartialEq,Eq)]
pub enum Exp<'x> {
    Var(Var<'x>),
    Plus(ExpB<'x>,ExpB<'x>),
    Num(int),
}
pub type ExpB<'x> = Box<Exp<'x>>;

pub type Var<'x> = String;

#[test]
pub fn doit () {
    println!("while lang!");
}
