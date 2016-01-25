#![feature(associated_type_defaults)]
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use macros::* ;
use adapton_sigs::* ;
use collection_traits::*;
use quickcheck::Arbitrary;
use quickcheck::Gen;
use std::num::Zero;

use rand::{Rng,Rand};

#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub enum List<A:Adapton,Elm> {
    Nil,
    Cons(Elm, Box<List<A,Elm>>),
    Tree(Box<Tree<A,Elm,u32>>, Dir2, Box<List<A,Elm>>),
    Rc(Rc<List<A,Elm>>),
    Name(A::Name, Box<List<A,Elm>>),
    Art(Art<List<A,Elm>, A::Loc>),
}

#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub enum Tree<A:Adapton,X,Lev> {
    Nil,
    Leaf(X),
    Bin(          Lev, Box<Tree<A,X,Lev>>, Box<Tree<A,X,Lev>> ),
    Name(A::Name, Lev, Box<Tree<A,X,Lev>>, Box<Tree<A,X,Lev>> ),
    Rc(                 Rc<Tree<A,X,Lev>>),
    Art(               Art<Tree<A,X,Lev>, A::Loc>),
}
 
// TODO: Why Does Adapton have to implement all of these?
//       It's not actually *contained* within the List structure; it cannot be ecountered there.
//       It's only ever present in a negative position (as a function parameter).
impl< A:Adapton+Debug+Hash+PartialEq+Eq+Clone
    , Elm:Debug+Hash+PartialEq+Eq+Clone
    >
    ListT<A,Elm>
    for List<A,Elm>
{
    type List = List<A,Elm>;

    // XXX
    // type Tree = Tree<A,Elm,u32>;

    fn nil  (_:&mut A)                              -> Self::List { List::Nil }
    fn cons (_:&mut A, hd:Elm, tl:Self::List)       -> Self::List { List::Cons(hd,Box::new(tl)) }
    fn name (_:&mut A, nm:A::Name, tl:Self::List)   -> Self::List { List::Name(nm, Box::new(tl)) }
    fn rc   (_:&mut A, rc:Rc<List<A,Elm>>)          -> Self::List { List::Rc(rc) }
    fn art  (_:&mut A, art:Art<List<A,Elm>,A::Loc>) -> Self::List { List::Art(art) }

    // fn tree (_:&mut A, tr:Self::Tree, dir:Dir2, tl:Self::List) -> Self::List {
    //     List::Tree(tr, dir, Box::new(tl))
    // }

    fn elim<Res,Nil,Cons,Name>
        (st:&mut A, list:Self::List, nilf:Nil, consf:Cons, namef:Name) -> Res
        where Nil:FnOnce(&mut A) -> Res
        ,    Cons:FnOnce(&mut A, Elm, Self::List) -> Res
        ,    Name:FnOnce(&mut A, A::Name, Self::List) -> Res
    {
        match list {
            List::Nil => nilf(st),
            List::Cons(hd, tl) => consf(st, hd, *tl),
            List::Name(nm, tl) => namef(st, nm, *tl),
            List::Rc(rc) => Self::elim(st, (*rc).clone(), nilf, consf, namef),
            List::Art(ref art) => {
                let list = st.force(art);
                Self::elim(st, list, nilf, consf, namef)
            },
            List::Tree(tree, dir, tl) => {
              let res = List::next_leaf_rec(st, *tree, dir, *tl) ;
              match res {
                None => nilf(st),
                Some((elm, rest)) => consf(st, elm, rest),
              }
            }
        }
    }
    
    fn elim_move<Arg,Res,Nil,Cons,Name>
        (st:&mut A, list:Self::List, arg:Arg, nilf:Nil, consf:Cons, namef:Name) -> Res
        where Nil:FnOnce(&mut A, Arg) -> Res
        ,    Cons:FnOnce(&mut A, Elm, Self::List, Arg) -> Res
        ,    Name:FnOnce(&mut A, A::Name, Self::List, Arg) -> Res
    {
        match list {
            List::Nil => nilf(st, arg),
            List::Cons(hd, tl) => consf(st, hd, *tl, arg),
            List::Name(nm, tl) => namef(st, nm, *tl, arg),
            List::Rc(rc) => Self::elim_move(st, (*rc).clone(), arg, nilf, consf, namef),
            List::Art(ref art) => {
                let list = st.force(art);
                Self::elim_move(st, list, arg, nilf, consf, namef)
            },
            List::Tree(tree, dir, tl) => {
              let res = List::next_leaf_rec(st, *tree, dir, *tl) ;
              match res {
                None => nilf(st, arg),
                Some((elm, rest)) => consf(st, elm, rest, arg),
              }
            }
        }
    }
}

impl< A:Adapton+Debug+Hash+PartialEq+Eq+Clone
    , Elm:Debug+Hash+PartialEq+Eq+Clone
    >
    TreeListT<A,Elm,Tree<A,Elm,u32>>
    for List<A,Elm>
{
  fn tree (_:&mut A, tr:Tree<A,Elm,u32>, dir:Dir2, tl:Self::List) -> Self::List {
    List::Tree(Box::new(tr), dir, Box::new(tl))
  }

  fn next_leaf_rec (st:&mut A, tree:Tree<A,Elm,u32>, dir:Dir2, rest:Self::List) -> Option<(Elm,Self::List)> {
    match tree {
      Tree::Nil => {
        List::elim_move(st, rest, dir,
                        |_, _| None,
                        |st, x, xs, _  | Some((x,xs)),
                        |st, _, xs, dir| Self::next_leaf_rec(st, Tree::Nil, dir, xs)
                        )
      },
      Tree::Rc(rc) => Self::next_leaf_rec(st, (*rc).clone(), dir, rest),
      Tree::Art(ref art) => {
        let tree = st.force(art);
        Self::next_leaf_rec(st, tree, dir, rest)
      }
      Tree::Leaf(leaf) => {
        Some((leaf, rest))
      }
      Tree::Bin(_,l,r) | Tree::Name(_,_,l,r) => {
        match dir {
          Dir2::Left  => Self::next_leaf_rec(st, *l, Dir2::Left,  List::Tree(r, Dir2::Left,  Box::new(rest))),
          Dir2::Right => Self::next_leaf_rec(st, *r, Dir2::Right, List::Tree(l, Dir2::Right, Box::new(rest))),
        }
      },
    }
  }

  fn next_leaf (st:&mut A, tree:Tree<A,Elm,u32>, dir:Dir2) -> Option<(Elm,Self::List)> {
    Self::next_leaf_rec(st, tree, dir, List::Nil)
  }

  fn tree_elim_move<Arg,Res,Treef,Nil,Cons,Name>
    (st:&mut A, list:Self::List, arg:Arg, treef:Treef, nilf:Nil, consf:Cons, namef:Name) -> Res
    where Treef:FnOnce(&mut A, Tree<A,Elm,u32>, Dir2, Self::List, Arg) -> Res
    ,       Nil:FnOnce(&mut A, Arg) -> Res
    ,      Cons:FnOnce(&mut A, Elm, Self::List, Arg) -> Res
    ,     Name:FnOnce(&mut A, A::Name, Self::List, Arg) -> Res
  {
    match list {
      List::Nil => nilf(st, arg),
      List::Cons(hd, tl) => consf(st, hd, *tl, arg),
      List::Name(nm, tl) => namef(st, nm, *tl, arg),
      List::Rc(rc) => Self::elim_move(st, (*rc).clone(), arg, nilf, consf, namef),
      List::Art(ref art) => {
        let list = st.force(art);
        Self::elim_move(st, list, arg, nilf, consf, namef)
      },
      List::Tree(tree, dir, tl) => treef(st, *tree, dir, *tl, arg),
    }
  }
}

// TODO: Why Does Adapton have to implement all of these?
//       It's not actually *contained* within the List structure; it cannot be ecountered there.
//       It's only ever present in a negative position (as a function parameter).
impl
    <A:Adapton+Debug+Hash+PartialEq+Eq+Clone
    ,Leaf:     Debug+Hash+PartialEq+Eq+Clone
    >
    TreeT<A,Leaf>
    for Tree<A,Leaf,u32>
{
    type Tree = Tree<A,Leaf,Self::Lev>;
    type Lev  = u32 ;

    //  See Pugh+Teiltelbaum in POPL 1989 for an explanation of this notion of "level":
    fn lev<X:Hash>(x:&X) -> Self::Lev  {
        my_hash(&x).trailing_zeros() as Self::Lev
    }
    fn lev_of_tree (st:&mut A, tree:&Self::Tree) -> Self::Lev {
        Tree::elim_ref(st, tree,
                       |_| 0,
                       |_,leaf| Self::lev(leaf),
                       |_,lev,_,_| lev.clone(),
                       |_,_,lev,_,_| lev.clone(),
                       )
    }
    fn lev_bits () -> Self::Lev { 32 }
    fn lev_zero () -> Self::Lev { 0 }
    fn lev_max_val () -> Self::Lev { u32::max_value() }
    fn lev_add (x:&Self::Lev,y:&Self::Lev) -> Self::Lev { x + y }
    fn lev_inc (x:&Self::Lev) -> Self::Lev { x + 1 }
    fn lev_lte (x:&Self::Lev,y:&Self::Lev) -> bool { x <= y }
    
    fn nil  (_:&mut A)                                                     -> Self::Tree { Tree::Nil }
    fn leaf (_:&mut A, x:Leaf)                                             -> Self::Tree { Tree::Leaf(x) }
    fn bin  (_:&mut A, lev:Self::Lev, l:Self::Tree, r:Self::Tree)            -> Self::Tree { Tree::Bin(lev,Box::new(l),Box::new(r)) }
    fn name (_:&mut A, nm:A::Name, lev:Self::Lev, l:Self::Tree,r:Self::Tree) -> Self::Tree { Tree::Name(nm, lev, Box::new(l),Box::new(r)) }
    fn rc   (_:&mut A, rc:Rc<Self::Tree>)                                  -> Self::Tree { Tree::Rc(rc) }
    fn art  (_:&mut A, art:Art<Self::Tree,A::Loc>)                         -> Self::Tree { Tree::Art(art) }

    fn elim_move<Arg,Res,NilC,LeafC,BinC,NameC>
        (st:&mut A, tree:Self::Tree, arg:Arg, nil:NilC, leaf:LeafC, bin:BinC, name:NameC) -> Res
        where NilC  : FnOnce(&mut A, Arg) -> Res
        ,     LeafC : FnOnce(&mut A, Leaf, Arg) -> Res
        ,     BinC  : FnOnce(&mut A, Self::Lev,  Self::Tree, Self::Tree, Arg) -> Res
        ,     NameC : FnOnce(&mut A, A::Name, Self::Lev,  Self::Tree, Self::Tree, Arg) -> Res
    {
        match tree {
            Tree::Nil => nil(st,arg),
            Tree::Leaf(x) => leaf(st, x, arg),
            Tree::Bin(b, l, r) => bin(st, b, *l, *r, arg),
            Tree::Name(nm, b, l, r) => name(st, nm, b, *l, *r, arg),
            Tree::Rc(rc) => Self::elim_move(st, (*rc).clone(), arg, nil, leaf, bin, name),
            Tree::Art(art) => {
                let list = st.force(&art);
                Self::elim_move(st, list, arg, nil, leaf, bin, name)
            }
        }
    }

    fn elim<Res,NilC,LeafC,BinC,NameC>
        (st:&mut A, tree:Self::Tree, nil:NilC, leaf:LeafC, bin:BinC, name:NameC) -> Res
        where NilC  : FnOnce(&mut A) -> Res
        ,     LeafC : FnOnce(&mut A, Leaf) -> Res
        ,     BinC  : FnOnce(&mut A, Self::Lev,  Self::Tree, Self::Tree) -> Res
        ,     NameC : FnOnce(&mut A, A::Name, Self::Lev, Self::Tree, Self::Tree) -> Res
    {
        match tree {
            Tree::Nil => nil(st),
            Tree::Leaf(x) => leaf(st, x),
            Tree::Bin(b, l, r) => bin(st, b, *l, *r),
            Tree::Name(nm, b, l, r) => name(st, nm, b, *l, *r),
            Tree::Rc(rc) => Self::elim(st, (*rc).clone(), nil, leaf, bin, name),
            Tree::Art(art) => {
                let list = st.force(&art);
                Self::elim(st, list, nil, leaf, bin, name)
            }
        }
    }

    fn elim_ref<Res,NilC,LeafC,BinC,NameC>
        (st:&mut A, tree:&Self::Tree, nil:NilC, leaf:LeafC, bin:BinC, name:NameC) -> Res
        where NilC  : FnOnce(&mut A) -> Res
        ,     LeafC : FnOnce(&mut A, &Leaf) -> Res
        ,     BinC  : FnOnce(&mut A, &Self::Lev, &Self::Tree, &Self::Tree) -> Res
        ,     NameC : FnOnce(&mut A, &A::Name, &Self::Lev, &Self::Tree, &Self::Tree) -> Res
    {
        match *tree {
            Tree::Nil => nil(st),
            Tree::Leaf(ref x) => leaf(st, x),
            Tree::Bin(ref b, ref l, ref r) => bin(st, b, &*l, &*r),
            Tree::Name(ref nm, ref b, ref l, ref r) => name(st, nm, b, &*l, &*r),
            Tree::Rc(ref rc) => Self::elim_ref(st, &*rc, nil, leaf, bin, name),
            Tree::Art(ref art) => {
                let list = st.force(art);
                Self::elim_ref(st, &list, nil, leaf, bin, name)
            }
        }
    }
}
