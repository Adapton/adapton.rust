#![feature(associated_type_defaults)]
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use gm::GMLog;

use macros::* ;
use adapton_sigs::* ;
use collection_traits::*;
use collection_algo::*;
// use quickcheck::Arbitrary;
// use quickcheck::Gen;
use std::num::Zero;
use std::ops::{Add,Rem};
use rand::{Rng,Rand};
use std::marker::PhantomData;

#[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
pub enum CursorEdit<X,Dir> {
    Insert  (Dir,X), // Inserts an element
    Remove  (Dir),   // Removes an element (name-oblivious)
    Replace (Dir,X), // Replace an element (name-oblivious)
    Goto    (Dir),   // Move the cursor
}

// #[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
// pub enum Cmd<X,Dir,Name> {
//     Basic    (CursorEdit<X,Dir>),
//     InsName  (Dir,Name), // Insert name
//     RemName  (Dir),      // Remove immediately-adjacent
//     ShowView (ListReduce),   // Show the given view
//     HideView (ListReduce),   // Remove the given view
// }

#[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
pub enum ListTransf {
    Sort, Reverse
}

#[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
pub enum ListReduce {
    Sum, Max, Min, Median,
    Vec(ListTransf, Option<usize>),
    Tree(ListTransf, Option<usize>),
}

impl Rand for CursorEdit<u32,Dir2>
{
  fn rand<R:Rng> (r: &mut R) -> Self {
    let m : u32 = 1024 * 1024 ;
    let n : u32 = Rand::rand(r) ;
    match r.gen_range(0, 100) {
      0  ... 50  => { CursorEdit::Insert (Rand::rand(r), n % m) },
      50 ... 60  => { CursorEdit::Remove (Rand::rand(r)) },
      60 ... 70  => { CursorEdit::Replace(Rand::rand(r), n % m) },
      70 ... 95 =>  { CursorEdit::Goto   (Dir2::Right) },
      95 ... 100 => { CursorEdit::Goto   (Dir2::Left)  },
      _ => unreachable!()
    }
  }
}

pub trait ExperimentT<A:Adapton,X,Tree:TreeT<A,X>,Out> {
    type ListEdit : ListEdit<A,X,Tree> ;
    fn run (&mut A, Vec<CursorEdit<X,Dir2>>, view:ListReduce) -> Vec<(Out,Cnt)> ;
}

pub fn eval_edit<A:Adapton,X,T:TreeT<A,X>,E:ListEdit<A,X,T>> (st:&mut A, edit:CursorEdit<X,Dir2>, z:E::State, id:usize) -> E::State {
    match edit {
        CursorEdit::Insert(dir,x) => {
            //let z = E::clr_names(st, z, dir.clone()) ;
            let n = A::name_of_usize(st, id);
            let m = A::name_of_string(st, format!("input{}", id)) ;
            let z = E::ins_cell(st, z, dir.clone(), m) ;
            let z = E::ins_name(st, z, dir.clone(), n) ;
            let z = E::insert(st, z, dir, x) ;
            z },
        CursorEdit::Remove(dir) => {
            let (z, _) = E::remove (st, z, dir.clone()) ;
            let (z, _) = E::rem_name(st, z, dir) ;
            z },
        CursorEdit::Goto(dir) => {
            let (z, _) = E::goto (st, z, dir.clone()) ;
            z },
        CursorEdit::Replace(dir,x) => {
            let (z, _, _) = E::replace(st, z, dir, x) ;
            z },
    }
}

pub fn demand_vec
    <A:Adapton
    ,X:Ord+PartialOrd+Hash+Debug+Clone+Eq+PartialEq
    ,L:ListT<A,X>
    ,T:TreeT<A,X>
    >
    (st:&mut A, tree:T::Tree, transform:&ListTransf, count:Option<usize>) -> Vec<X>
{
    match *transform {
        ListTransf::Sort => {
            let output = tree_merge_sort::<A,X,L,T>(st, tree);
            vec_of_list::<A,X,L>(st, output, count)
        },
        ListTransf::Reverse => {
            let output = rev_list_of_tree::<A,X,L,T>(st, tree);
            vec_of_list::<A,X,L>(st, output, count)
        }
    }
}

pub fn demand_tree
    <A:Adapton
    ,X:Ord+PartialOrd+Hash+Debug+Clone+Eq+PartialEq
    ,L:ListT<A,X>
    ,T:TreeT<A,X>
    >
    (st:&mut A, tree:T::Tree, transform:&ListTransf, count:Option<usize>) -> Vec<X>
{
    // Todo: Respect the `count` argument
    match *transform {
        ListTransf::Sort => {
            let output = tree_merge_sort::<A,X,L,T>(st, tree) ;
            let nm = st.name_of_string("tree_of_output".to_string());
            let tree = st.ns(nm, |st| tree_of_list::<A,X,T,L>(st, Dir2::Left, output) );
            vec![]
        },
        ListTransf::Reverse => {
            let output = rev_list_of_tree::<A,X,L,T>(st, tree) ;
            let nm = st.name_of_string("tree_of_output".to_string());
            let tree = st.ns(nm, |st| tree_of_list::<A,X,T,L>(st, Dir2::Left, output) );
            vec![]
        }
    }
}

pub fn eval_reduce
    <A:Adapton
    ,X:Ord+PartialOrd+Hash+Debug+Clone+Eq+PartialEq+Zero+Add<Output=X>+GMLog<A>
    ,L:ListT<A,X>
    ,T:TreeT<A,X>
    >
    (st:&mut A, tree:T::Tree, red:&ListReduce) -> Vec<X>
{
    match *red {
        ListReduce::Sum => { let x = tree_reduce_monoid::<A,X,T,_> (st, tree, X::zero(), &|st,x,y| x + y) ; vec![ x ] },
        ListReduce::Max => { let x = tree_reduce_monoid::<A,X,T,_> (st, tree, X::zero(), &|st,x,y| if x > y {x} else {y}) ; vec![ x ] },
        ListReduce::Min => { let x = tree_reduce_monoid::<A,X,T,_> (st, tree, X::zero(), &|st,x,y| if x < y {x} else {y}) ; vec![ x ] },
        ListReduce::Median => panic!(""),
        ListReduce::Vec (ref transform, ref n) => demand_vec:: <A,X,L,T> (st, tree, transform, n.clone()),
        ListReduce::Tree(ref transform, ref n) => demand_tree::<A,X,L,T> (st, tree, transform, n.clone()),
    }
}

/// `ListEdit<A,X,L>` gives a simple notion of list-editing that is
/// generic with respect to adapton implementation `A`, list element
/// type `X`, and list implementation `L`.
pub trait ListEdit<A:Adapton,X,T:TreeT<A,X>> {
    /// The State of the Editor is abstract.
    type State : Clone+Hash+Eq+PartialEq+Debug;

    // XXX
    // type Tree  : TreeT<A,X>;
        
    fn empty    (&mut A) -> Self::State;
    fn insert   (&mut A, Self::State, Dir2, X) -> Self::State;
    fn remove   (&mut A, Self::State, Dir2)    -> (Self::State, Option<X>);
    fn replace  (&mut A, Self::State, Dir2, X) -> (Self::State, X, bool);
    fn goto     (&mut A, Self::State, Dir2)    -> (Self::State, bool);
    fn shift    (&mut A, Self::State, Dir2)    -> (Self::State, bool);
    fn observe  (&mut A, Self::State, Dir2)    -> (Self::State, Option<X>);

    // Insert/remove names from the list content:
    fn clr_names (&mut A, Self::State, Dir2) -> Self::State;
    fn ins_name  (&mut A, Self::State, Dir2, A::Name) -> Self::State;
    fn ins_cell  (&mut A, Self::State, Dir2, A::Name) -> Self::State;
    fn rem_name  (&mut A, Self::State, Dir2) -> (Self::State, Option<A::Name>);

    fn ins_tree (&mut A, Self::State, Dir2, T::Tree, Dir2) -> Self::State;

    fn clear_side (&mut A, Self::State, Dir2) -> Self::State ;

    fn get_list<L:ListT<A,X>> (&mut A, Self::State, Dir2) -> L::List;
    fn get_tree               (&mut A, Self::State, Dir2) -> T::Tree;


  fn insert_optnm (st:&mut A, z:Self::State, dir:Dir2, nm:Option<A::Name>, elm:X) -> Self::State {
    match nm {
      None => Self::insert(st, z, dir, elm),
      Some(nm) => {
        let z = Self::insert(st, z, dir.clone(), elm) ;
        let z = Self::ins_cell(st, z, dir.clone(), nm.clone()) ;
        let z = Self::ins_name(st, z, dir, nm) ;
        z
      }}}
      
  fn ins_tree_optnm (st:&mut A, z:Self::State, dir:Dir2, nm:Option<A::Name>, tr:T::Tree, tdir:Dir2) -> Self::State {
    match nm {
      None => Self::ins_tree(st, z, dir, tr, tdir),
      Some(nm) => {
        let z = Self::ins_tree(st, z, dir.clone(), tr, tdir) ;
        let z = Self::ins_cell(st, z, dir.clone(), nm.clone()) ;
        let z = Self::ins_name(st, z, dir, nm) ;
        z
      }}}
      
  
  fn move_optnm (st:&mut A, z:Self::State, dir:Dir2, nm:Option<A::Name>) -> (Self::State, bool) {
    match nm {
      None => Self::goto(st, z, dir),
      Some(nm) => {
        let (z, success) = Self::goto(st, z, dir.clone()) ;
        if success {
          let z = Self::ins_cell(st, z, dir.clone().opp(), nm.clone()) ;
          let z = Self::ins_name(st, z, dir.opp(), nm) ;
          (z, true)
        } else {
          (z, false)
        }
      }}}

  // Todo-Later: Consider adding replace_optnm
}


/// Lists with a focus; suitable to implement `ListEdit`.
#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub struct ListZipper<A:Adapton,X,T:TreeT<A,X>,L:TreeListT<A,X,T>> {
    /// Elements to the left of the focus, nearest to furthest.
    pub left: L::List,
    /// Elements to the right of the focus, nearest to furthest.
    pub right: L::List,

    /// Todo-Later: This phantom field should *not* be needed, but is, due to rustc being confused (?).
    /// It complains that the parameter T is not used, though it is (by the type parameter L's trait bound).
    phantom:PhantomData<T::Tree>,
}

macro_rules! zipper {
    { $left:expr , $right:expr } => {
        {
            ListZipper{ left:$left, right:$right, phantom:PhantomData}
        }
    };
}

/// Implement `ListEdit` for `ListZipper` generically with respect to
/// adapton implementation `A`, list element type `X`, and list
/// implementation `L`.
impl<A:Adapton
    ,X:Debug+Hash+PartialEq+Eq+Clone
    ,T:TreeT<A,X>
    ,L:TreeListT<A,X,T>
    >
    ListEdit<A,X,T>
    for
    ListZipper<A,X,T,L>
{
    type State=ListZipper<A,X,T,L>;

    // XXX
    // type Tree=T;
    
  fn clr_names (st:&mut A, zip:Self::State, dir:Dir2) -> Self::State {
    panic!("clr_names generally re-associates names");
        // match dir {
        //     Dir2::Left => L::elim_move
        //         (st, zip.left, zip.right,
        //          |st,right| zipper!{L::nil(st), right},
        //          |st,x,left,right| zipper!{L::cons(st,x,left), right},
        //          |st,nm,left,right| {
        //              let right = L::name(st,nm,right);
        //              Self::clr_names(st, zipper!{left, right}, dir)}
        //          ),
        //     Dir2::Right => L::elim_move
        //         (st, zip.right, zip.left,
        //          |st,left| zipper!{left, L::nil(st)},
        //          |st,x,right,left| zipper!{left, L::cons(st,x,right)},
        //          |st,nm,right,left| {
        //              let left = L::name(st,nm,left);
        //              Self::clr_names(st, zipper!{left, right}, dir)}
        //          ),
        // }
    }

    fn ins_tree (st:&mut A, zip:Self::State, ins_dir:Dir2, tree:T::Tree, tree_dir:Dir2) -> Self::State {
        match ins_dir {
            Dir2::Left =>
                zipper!{L::tree(st, tree, tree_dir, zip.left),
                        zip.right},
            Dir2::Right =>
                zipper!{zip.left,
                        L::tree(st, tree, tree_dir, zip.right)},
        }
    }
    
    fn ins_name (st:&mut A, zip:Self::State, dir:Dir2, name:A::Name) -> Self::State {
        match dir {
            Dir2::Left =>
                zipper!{L::name(st, name, zip.left),
                           zip.right},
            Dir2::Right =>
                zipper!{zip.left,
                           L::name(st, name, zip.right)},
        }
    }

    fn ins_cell (st:&mut A, zip:Self::State, dir:Dir2, name:A::Name) -> Self::State {
        match dir {
            Dir2::Left => {
                let art = st.cell(name, zip.left) ;
                let art = st.read_only(art);
                zipper!{L::art(st, art),
                           zip.right}},                
            Dir2::Right => {
                let art = st.cell(name, zip.right) ;
                let art = st.read_only(art);
                zipper!{zip.left,
                           L::art(st, art)}},
        }
    }

    fn rem_name  (st:&mut A, zip:Self::State, dir:Dir2) -> (Self::State, Option<A::Name>) {
        match dir {
            Dir2::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (zipper!{L::nil(st), right}, None ),
                 |_,x,left,right|   (zipper!{left,       right}, None ),
                 |st,nm,left,right| (zipper!{left, right},       Some(nm))
                 ),
            Dir2::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|          (zipper!{left, L::nil(st)}, None ),
                 |_,x,right,left|   (zipper!{left, right},      None ),
                 |st,nm,right,left| (zipper!{left, right},      Some(nm))
                 ),
        }
    }
    
    fn empty (st: &mut A) -> Self::State {
        let nil1 = L::nil(st);
        let nil2 = nil1.clone();
        zipper!{nil1, nil2}
    }
    
    fn insert (st:&mut A, zip:Self::State, dir:Dir2, x:X) -> Self::State {
        match dir {
            Dir2::Left =>
                zipper!{L::cons(st, x, zip.left),
                           zip.right},
            Dir2::Right =>
                zipper!{zip.left,
                           L::cons(st, x, zip.right)},
        }
    }

    fn remove  (st:&mut A, zip:Self::State, dir:Dir2) -> (Self::State, Option<X>) {
        match dir {
            Dir2::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (zipper!{L::nil(st), right}, None   ),
                 |_,x,left,right|   (zipper!{left,       right}, Some(x)),
                 |st,nm,left,right| {let zip = zipper!{left, right};
                                     Self::remove (st, zip, Dir2::Left)}
                 ),
            Dir2::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|          (zipper!{left, L::nil(st)}, None   ),
                 |_,x,right,left|   (zipper!{left, right},      Some(x)),
                 |st,nm,right,left| {let zip = zipper!{left, right};
                                     Self::remove (st, zip, Dir2::Right)}
                 ),
        }
    }


  // XXX rename 'move'
    fn goto (st:&mut A, zip:Self::State, dir:Dir2) -> (Self::State, bool) {
      match dir {
        Dir2::Left => L::elim_move
          (st, zip.left, zip.right,
           /* Nil */  |st,right|          (zipper!{L::nil(st), right}          , false ),
           /* Cons */ |st,elm,left,right| (zipper!{left, L::cons(st,elm,right)}, true  ),
           /* Name */ |st,_,left,right| {let zip = zipper!{left, right};
                                          Self::goto (st, zip, Dir2::Left)}
           ),
        Dir2::Right => L::elim_move
          (st, zip.right, zip.left,
           /* Nil */ |st,left|           (zipper!{left,              L::nil(st)}, false ),
           /* Cons */ |st,x,right,left|  (zipper!{L::cons(st,x,left),right}     , true  ),
           /* Name */ |st,_,right,left| {let zip = zipper!{left, right};
                                          Self::goto (st, zip, Dir2::Right)}
           ),
      }
    }

    fn shift (st:&mut A, zip:Self::State, dir:Dir2) -> (Self::State, bool) {
      match dir {
        Dir2::Left => L::elim_move
          (st, zip.left, zip.right,
           /* Nil */  |st,right|          (zipper!{L::nil(st), right}          , false ),
           /* Cons */ |st,elm,left,right| (zipper!{left, L::cons(st,elm,right)}, true  ),
           /* Name */ |st,nm,left,right| {let zip = zipper!{left, L::name(st,nm,right)};
                                          Self::shift (st, zip, Dir2::Left)}
           ),
        Dir2::Right => L::elim_move
          (st, zip.right, zip.left,
           /* Nil */ |st,left|           (zipper!{left,              L::nil(st)}, false ),
           /* Cons */ |st,x,right,left|  (zipper!{L::cons(st,x,left),right}     , true  ),
           /* Name */ |st,nm,right,left| {let zip = zipper!{L::name(st,nm,left), right};
                                          Self::shift (st, zip, Dir2::Right)}
           ),
      }
    }

    fn observe (st:&mut A, zip:Self::State, dir:Dir2) -> (Self::State,Option<X>) {
        match dir {
            Dir2::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (zipper!{L::nil(st), right}, None),
                 |st,x,left,right| {let x2 = x.clone();
                                    (zipper!{L::cons(st,x,left), right}, Some(x2))},
                 |st,nm,left,right|{let zip = zipper!{left,right};
                                    Self::observe (st, zip, Dir2::Left)}
                 ),
            Dir2::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|         (zipper!{left, L::nil(st)}, None),
                 |st,x,right,left| {let x2 = x.clone();
                                    (zipper!{left, L::cons(st,x,right)}, Some(x2))},
                 |st,nm,right,left|{let zip = zipper!{left,right};
                                    Self::observe (st, zip, Dir2::Right)}
                 ),
        }
    }

    fn replace (st:&mut A, zip:Self::State, dir:Dir2, y:X) -> (Self::State, X, bool) {
        match dir {
            Dir2::Left => L::elim_move
                (st, zip.left, (zip.right, y),
                 |st,(right,y)|        (zipper!{L::nil(st),         right}, y, false),
                 |st,x,left,(right,y)| (zipper!{L::cons(st,y,left), right}, x, true ),
                 |st,nm,left,(right,y)|{let zip = zipper!{left,right};
                                        Self::replace (st, zip, Dir2::Left, y)}
                 ),
            Dir2::Right => L::elim_move
                (st, zip.right, (zip.left,y),
                 |st,(left,y)|         (zipper!{left, L::nil(st)},          y, false),
                 |st,x,right,(left,y)| (zipper!{left, L::cons(st,y,right)}, x, true ),
                 |st,nm,right,(left,y)|{let zip = zipper!{left,right};
                                        Self::replace (st, zip, Dir2::Right, y)}
                 ),
        }
    }

    fn clear_side
        (st:&mut A, zip:Self::State, dir:Dir2) -> Self::State {
            let emp = L::nil(st) ;
            match dir {
                Dir2::Right => zipper!{zip.left, emp},
                Dir2::Left  => zipper!{emp, zip.right},
            }
        }
        
    fn get_list<N:ListT<A,X>>
        (st:&mut A, zip:Self::State, dir:Dir2) -> N::List
    {
        let tree = Self::get_tree(st, zip, dir);
        list_of_tree::<A,X,N,T>(st, tree)
    }

    /// Creates a tree whose leaves hold the contents of the zipper, in order.
    /// When `dir=Left`,  the tree's leaves are ordered from left-to-right, i.e., as (rev left) @ right.
    /// When `dir=Right`, the tree's leaves are ordered from right-to-left, i.e., as (rev right) @ left.
    fn get_tree(st:&mut A, zip:Self::State, dir:Dir2) -> T::Tree
    {
        match dir {
            Dir2::Left  => {
                let left  = tree_of_treelist::<A,X,T,L>(st, Dir2::Right, zip.left);
                let right = tree_of_treelist::<A,X,T,L>(st, Dir2::Left,  zip.right);
                tree_append::<A,X,T>(st, left, right)}
            
            Dir2::Right => {
                let right = tree_of_treelist::<A,X,T,L>(st, Dir2::Right, zip.right);
                let left  = tree_of_treelist::<A,X,T,L>(st, Dir2::Left,  zip.left);
                tree_append::<A,X,T>(st, right, left)}
        }
    }
}
