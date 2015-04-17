use name::*;
use art::*;
use std::hash::{hash,Hash,SipHasher};
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
//        | E < E
//

/// Nominal, artful commands for the while language
#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum Cmd<'x> {
    Skip,
    //Continue,
    Assign(Var<'x>, ExpB<'x>),
    Seq(CmdB<'x>, CmdB<'x>),
    // WHILE-loops:
    While(ExpB<'x>, CmdB<'x>),
    Again(ExpB<'x>, CmdB<'x>),
    If(ExpB<'x>, CmdB<'x>, CmdB<'x>),
    Break,
    // ----
    Name(Name, CmdB<'x>),
    Art(Art<'x,CmdB<'x>>),
}
pub type CmdB<'x> = Box<Cmd<'x>>;

pub enum CmdCxt<'x> {
    Empty,
    Seq(CmdCxtB<'x>, CmdB<'x>),
    // ----
    Name(Name, CmdCxtB<'x>),
    Art(Art<'x,CmdCxtB<'x>>),
}
pub type CmdCxtB<'x> = Box<CmdCxt<'x>>;
    
/// Expressions for the while language
#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum Exp<'x> {
    Var(Var<'x>),
    Plus(ExpB<'x>,ExpB<'x>),
    Less(ExpB<'x>,ExpB<'x>),
    Num(isize),
    Name(Name, ExpB<'x>),
    Art(Art<'x,ExpB<'x>>),
}
pub type ExpB<'x> = Box<Exp<'x>>;

/// Variables for the while language
pub type Var<'x> = String;

pub trait Heap<L:Hash+Eq, V> {
    fn empty () -> Self ;
    fn update (Self, L, V) -> Self ;
    fn select (Self, L) -> (Self, Option<V>) ;
}

pub struct HashMapHeap { hm : HashMap<String, isize> }

impl Heap<String, isize> for HashMapHeap {
    fn empty () -> Self {
        HashMapHeap { hm : HashMap::new() }
    }
    fn update (hm:Self, l:String, v:isize) -> Self {
        let mut hm = hm.hm.clone () ; // TODO: Avoid this clone. Use trie structure instead.
        hm.insert(l,v) ;
        HashMapHeap{ hm : hm }
    }
    fn select (hm:Self, l:String) -> (Self, Option<isize>) {
        let hmq = hm.hm.clone () ; // TODO: Avoid this clone. Use trie structure instead.
        let x = hmq.get(&l) ;
        (hm, match x { None => None, Some(x) => Some(*x) })
    }
}

pub enum Error {
    VarNotDef(String),    
}
   
pub fn eval_exp<'x>
    (heap:HashMapHeap, exp:Exp<'x>)
     -> (HashMapHeap, Result<isize, Error>)
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

        Exp::Less(l, r) => {
            let (heap, l) = eval_exp(heap, *l) ;
            let (heap, r) = eval_exp(heap, *r) ;
            match (l,r) {
                (Ok(l), Ok(r)) => (heap, Ok(if l < r { 1 } else { 0 })),
                (Err(l), _) => (heap, Err(l)),
                (_, Err(r)) => (heap, Err(r))
            }},

    }
}

fn do_break<'x> (cxt:CmdCxt<'x>) -> CmdCxt<'x> {
    match cxt {
        CmdCxt::Empty => CmdCxt::Empty, // Degenerate basecase.
        CmdCxt::Seq(cxt, cmd) =>
            match *cmd {
                Cmd::Again(_,_) => *cxt, // Normal basecase.
                // - - - - - - -
                // Keep searching for basecase...
                Cmd::Art(art) => do_break (CmdCxt::Seq(cxt, force(art))),
                // Cmd::Name(nm, cmd) => { do_break(CmdCxt::Seq(cxt, cmd)) },
                // - - - - - - -
                _ => do_break(*cxt),
            },
        // - - - - - - -
        CmdCxt::Name(_, cxt) => do_break(*cxt),
        CmdCxt::Art(art) => do_break(*force(art)),
    }
}

pub fn step_cmd<'x>
    (heap:HashMapHeap, cxt:CmdCxt<'x>, cmd:Cmd<'x>)
     -> (HashMapHeap, Option<(CmdCxt<'x>, Cmd<'x>)>)
{
    match (cxt, cmd) {
        (CmdCxt::Empty, Cmd::Skip) => (heap, None),
        (CmdCxt::Empty, Cmd::Break) => (heap, None),

        (CmdCxt::Seq(cxt, cmd), Cmd::Skip) => (heap, Some((*cxt, *cmd))),
        (cxt, Cmd::Break) => {
            let cxt = do_break(cxt) ;
            (heap, Some((cxt, Cmd::Skip)))
        }

        (cxt, Cmd::Seq(cmd1, cmd2)) => (heap, Some( (CmdCxt::Seq(Box::new(cxt), cmd2), *cmd1) )),

        // While becomes Again, which repeats again and again..
        // This case only fires when first entering the loop;
        // TODO: use it to extend the "loop path"
        (cxt, Cmd::While(exp, cmd)) => (heap, Some( (cxt, Cmd::Again(exp, cmd)) )),

        (cxt, Cmd::Again(exp, cmd)) =>
            (heap, Some( (CmdCxt::Seq(Box::new(cxt), Box::new(Cmd::Again(exp.clone(), cmd.clone()))),
                          Cmd::If(exp, cmd, Box::new(Cmd::Break))
                          ) )),
        
        (cxt, Cmd::Assign(var, exp)) => {
            let (heap, r) = eval_exp (heap, *exp) ;
            match r {
                Err(error) => (heap, None),
                Ok(val) => {
                    let heap = Heap::update(heap, var, val) ;
                    (heap, Some(( cxt, Cmd::Skip )))
                }
            }
        },

        (cxt, Cmd::If(exp, cmd1, cmd2)) => {
            let (heap, r) = eval_exp (heap, *exp) ;
            match r {
                Err(error) => (heap, None),
                Ok(0) => { (heap, Some((cxt, *cmd2))) },
                Ok(_) => { (heap, Some((cxt, *cmd1))) },
            }
        },

        // TODO: Step to a name here.
        // TODO: to write eta, need "dynamic coordinate" for this static name nm.
        // TODO: This coordinate information consists of counts for each of the loops we are in now.
        (CmdCxt::Name(nm, cxt), cmd) => { step_cmd(heap, *cxt, cmd) }

        // - - - - - - - - - 
        (cxt, Cmd::Art(art))         => { step_cmd(heap, cxt, *force(art)) },
        (cxt, Cmd::Name(nm, cmd))    => { step_cmd(heap, cxt, *cmd) }
        (CmdCxt::Art(art), cmd)      => { step_cmd(heap, *force(art), cmd) }

    }
}    

#[test]
pub fn doit () {
    println!("while lang!");
}
