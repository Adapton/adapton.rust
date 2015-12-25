#![feature(associated_type_defaults)]
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
    
use macros::* ;
use adapton_sigs::* ;
use quickcheck::Arbitrary;
use quickcheck::Gen;
use std::num::Zero;

use rand::{Rng,Rand};
pub trait ListT<A:Adapton,Hd> : Debug+Clone {
    type List : Debug+Hash+PartialEq+Eq+Clone ;
    
    fn nil  (&mut A) -> Self::List ;
    fn cons (&mut A, Hd, Self::List) -> Self::List ;
    
    // requisite "adaptonic" constructors: `name` and `art`:
    fn name (&mut A, A::Name, Self::List) -> Self::List ;
    fn art  (&mut A, Art<Self::List,A::Loc>) -> Self::List ;
    fn rc   (&mut A, Rc<Self::List>) -> Self::List ;

    fn elim<Res,Nil,Cons,Name> (&mut A, Self::List, Nil, Cons, Name) -> Res
        where Nil:FnOnce(&mut A) -> Res
        ,    Cons:FnOnce(&mut A, Hd, Self::List) -> Res
        ,    Name:FnOnce(&mut A, A::Name, Self::List) -> Res ;

    fn elim_move<Arg,Res,Nil,Cons,Name> (&mut A, Self::List, Arg, Nil, Cons, Name) -> Res
        where Nil:FnOnce(&mut A, Arg) -> Res
        ,    Cons:FnOnce(&mut A, Hd, Self::List, Arg) -> Res
        ,    Name:FnOnce(&mut A, A::Name, Self::List, Arg) -> Res ;

    // Derived from `cons` and `nil` above:
    fn singleton (st:&mut A, hd:Hd) -> Self::List {
        let nil = Self::nil(st);
        Self::cons(st, hd, nil)
    }

    // Derived from `elim` above:
    fn is_empty (st:&mut A, list:Self::List) -> bool {
        Self::elim(st, list,
                   |_|       true,
                   |_,_,_|   false,
                   |st,_,tl| Self::is_empty(st,tl))
    }
}

pub trait TreeT<A:Adapton,Leaf> {
    type Lev  : Debug+Hash+PartialEq+Eq+Clone ;
    type Tree : Debug+Hash+PartialEq+Eq+Clone ;

    fn lev<X:Hash>(&X) -> Self::Lev ;
    fn lev_bits () -> Self::Lev ;
    fn lev_zero () -> Self::Lev ;
    fn lev_inc (Self::Lev) -> Self::Lev ;
    fn lev_max () -> Self::Lev ;
    fn lev_add (Self::Lev, Self::Lev) -> Self::Lev ;
    fn lev_lte (Self::Lev, Self::Lev) -> bool ;
    
    fn nil  (&mut A) -> Self::Tree ;
    fn leaf (&mut A, Leaf) -> Self::Tree ;
    fn bin  (&mut A, Self::Lev, Self::Tree, Self::Tree) -> Self::Tree ;

    // requisite "adaptonic" constructors: `name` and `art`:
    fn name (&mut A, A::Name, Self::Lev, Self::Tree, Self::Tree) -> Self::Tree ;
    fn art  (&mut A, Art<Self::Tree,A::Loc>) -> Self::Tree ;
    fn rc   (&mut A, Rc<Self::Tree>) -> Self::Tree ;

    fn elim<Res,NilC,LeafC,BinC,NameC>
        (&mut A, Self::Tree, NilC, LeafC, BinC, NameC) -> Res
        where NilC  : FnOnce(&mut A) -> Res
        ,     LeafC : FnOnce(&mut A, Leaf) -> Res
        ,     BinC  : FnOnce(&mut A, Self::Lev,  Self::Tree, Self::Tree) -> Res
        ,     NameC : FnOnce(&mut A, A::Name, Self::Lev, Self::Tree, Self::Tree) -> Res
        ;

    fn elim_move<Arg,Res,NilC,LeafC,BinC,NameC>
        (&mut A, Self::Tree, Arg, NilC, LeafC, BinC, NameC) -> Res
        where NilC  : FnOnce(&mut A, Arg) -> Res
        ,     LeafC : FnOnce(&mut A, Leaf, Arg) -> Res
        ,     BinC  : FnOnce(&mut A, Self::Lev,  Self::Tree, Self::Tree, Arg) -> Res
        ,     NameC : FnOnce(&mut A, A::Name, Self::Lev, Self::Tree, Self::Tree, Arg) -> Res
        ;
    
    fn fold_lr<Res:Hash+Debug+Eq+Clone,LeafC,BinC,NameC>
        (st:&mut A, tree:Self::Tree, res:Res, leaf:&LeafC, bin:&BinC, name:&NameC) -> Res
        where LeafC:Fn(&mut A, Leaf,    Res ) -> Res
        ,      BinC:Fn(&mut A, Self::Lev,     Res ) -> Res 
        ,     NameC:Fn(&mut A, A::Name, Self::Lev, Res ) -> Res 
    {
        Self::elim_move
            (st, tree, res,
             |_,res| res,
             |st,x,res| leaf(st, x, res),
             |st,x,l,r,res| {
                 let res = Self::fold_lr(st, l, res, leaf, bin, name);
                 let res = bin(st, x, res);
                 let res = Self::fold_lr(st, r, res, leaf, bin, name);
                 res
             },
             |st,n,x,l,r,res| {
                 let (n1,n2,n3) = st.name_fork3(n.clone());
                 let res = memo!(st, n1 =>> Self::fold_lr, tree:l, res:res ;; leaf:leaf, bin:bin, name:name);
                 let res = name(st, n2, x, res);
                 let res = memo!(st, n3 =>> Self::fold_lr, tree:r, res:res ;; leaf:leaf, bin:bin, name:name);
                 res
             }
             )
    }

    fn fold_rl<Res:Hash+Debug+Eq+Clone,LeafC,BinC,NameC>
        (st:&mut A, tree:Self::Tree, res:Res, leaf:&LeafC, bin:&BinC, name:&NameC) -> Res
        where LeafC:Fn(&mut A, Leaf,    Res ) -> Res
        ,      BinC:Fn(&mut A, Self::Lev,     Res ) -> Res
        ,     NameC:Fn(&mut A, A::Name, Self::Lev, Res ) -> Res
    {
        Self::elim_move
            (st, tree, res,
             |_,res| res,
             |st,x,res| leaf(st, x, res),
             |st,x,l,r,res| {
                 let res = Self::fold_rl(st, r, res, leaf, bin, name);
                 let res = bin(st, x, res);
                 let res = Self::fold_rl(st, l, res, leaf, bin, name);
                 res
             },
             |st,n,x,l,r,res| {
                 let (n1,n2,n3) = st.name_fork3(n);
                 let res = memo!(st, n1 =>> Self::fold_rl, tree:r, res:res ;; leaf:leaf, bin:bin, name:name);
                 let res = name(st, n2, x, res);
                 let res = memo!(st, n3 =>> Self::fold_rl, tree:l, res:res ;; leaf:leaf, bin:bin, name:name);
                 res
             }
             )
    }

    fn fold_up<Res:Hash+Debug+Eq+Clone,NilC,LeafC,BinC,NameC>
        (st:&mut A, tree:Self::Tree, nil:&NilC, leaf:&LeafC, bin:&BinC, name:&NameC) -> Res
        where  NilC:Fn(&mut A) -> Res
        ,     LeafC:Fn(&mut A, Leaf              ) -> Res
        ,      BinC:Fn(&mut A, Self::Lev,     Res, Res ) -> Res
        ,     NameC:Fn(&mut A, A::Name, Self::Lev, Res, Res ) -> Res
    {
        Self::elim
            (st, tree,
             |st| nil(st),
             |st,x| leaf(st, x),
             |st,x,l,r| {
                 let resl = Self::fold_up(st, l, nil, leaf, bin, name);
                 let resr = Self::fold_up(st, r, nil, leaf, bin, name);
                 let res = bin(st, x, resl, resr);
                 res
             },
             |st,n,x,l,r| {
                 let (n1,n2,n3) = st.name_fork3(n);
                 let resl = memo!(st, n1 =>> Self::fold_up, tree:l ;; nil:nil, leaf:leaf, bin:bin, name:name);
                 let resr = memo!(st, n2 =>> Self::fold_up, tree:r ;; nil:nil, leaf:leaf, bin:bin, name:name);
                 let res = name(st, n3, x, resl, resr);
                 res
             }
             )
    }
}


pub trait SetT
    <A:Adapton
    ,Elm:Hash+Clone>
{
    type Set;

    // Intro forms:
    fn empty(st:&mut A);

    fn update_elm<Update,Res>
        (st:&mut A, set:Self::Set, elm:Elm,
         update:&Update)
         -> (Self::Set, Res)
        where Update:FnOnce(&mut A, Option<Elm>, Elm) -> (Option<Elm>, Res)
        ;
    
    // Elim forms:
    fn elim_move<Arg,Res,NilC,ElmC,UnionC>
        (st:&mut A, set:Self::Set, arg:Arg,
         nil:&NilC, elm:&ElmC, union:&UnionC)
         -> Res
        where NilC:FnOnce(&mut A, Arg) -> Res
        ,     ElmC:FnOnce(&mut A, Elm, Arg) -> Res
        ,   UnionC:FnOnce(&mut A, Self::Set, Self::Set, Arg) -> Res
        ;

    fn is_mem(st:&mut A, set:Self::Set, elm:Elm) -> bool; // TODO: Write based on update, above.
    fn is_empty(st:&mut A, set:Self::Set) -> bool; // TODO: Write based on elim_with, above.
}

pub trait MapT
    <A:Adapton
    ,Dom:Hash+Clone
    ,Cod:Hash+Clone>
{
    type Map;
    
    // Intro forms:
    fn empty(st:&mut A);

    fn update_pt<Update,Res>
        (st:&mut A, map:Self::Map,
         pt:Dom, cod:Option<Cod>, update:&Update) -> (Self::Map, Res)
        where Update:FnOnce(&mut A, Option<Cod>, Option<Cod>) -> (Option<Cod>, Res)
        ;

    // General elim form:
    fn elim_move<Arg,Res,NilC,ElmC,UnionC>
        (st:&mut A, map:Self::Map, arg:Arg,
         nil:&NilC, elm:&ElmC, union:&UnionC)
        where NilC:FnOnce(&mut A, Arg) -> Res
        ,     ElmC:FnOnce(&mut A, Dom, Option<Cod>) -> Res
        ,   UnionC:FnOnce(&mut A, Self::Map, Self::Map, Arg) -> Res
        ;
    
    // Special, convenient update_pts:
    fn rem(st:&mut A, map:Self::Map, x:Dom) -> (Option<Cod>, Self::Map); // TODO: Implement with update_pt
    fn get(st:&mut A, set:Self::Map, x:Dom) -> Option<Cod>; // TODO: Implement with update_pt
}

pub trait GraphT
    <A:Adapton
    ,Node:Hash+Eq+Clone+Debug
    ,NodeSet:SetT<A,Node>+Clone+Hash
    ,NodeMap:MapT<A,Node,NodeSet>>
{
    type Graph : Hash+Eq+Clone+Debug;

    // Intro forms:
    fn empty(st:&mut A);
    fn add_node(st:&mut A, graph:Self::Graph, node:Node) -> Self::Graph;
    fn add_edge(st:&mut A, graph:Self::Graph, src:Node, tgt:Node) -> Self::Graph;
    fn add_succs(st: &mut A, graph:Self::Graph, node:Node, succs:NodeSet) -> Self::Graph;

    // Query forms:
    fn get_succs(st: &mut A, graph:Self::Graph, node:Node) -> NodeSet;
    
    // Other forms:
    // fn rem_node(st:&mut A, graph:Self::Graph, node:NodeLab) -> Self::Graph;
    // fn rem_edge(st:&mut A, graph:Self::Graph, edge:Self::Edge) -> Self::Graph;    
}

/// Lists are one-dimensional structures; movement admits two possible directions.
#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum Dir2 { Left, Right }

impl Rand for Dir2 {
    fn rand<R: Rng>(rng: &mut R) -> Dir2 {
        if rng.gen_weighted_bool(2) { Dir2::Left } else { Dir2::Right }
    }
}
