#![feature(associated_type_defaults)]
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
    
use macros::* ;
use adapton_sigs::* ;
use collection_traits::*;
use collection_algo::*;
use collection::*;
use quickcheck::Arbitrary;
use quickcheck::Gen;
use std::num::Zero;

use rand::{Rng,Rand};

#[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
pub enum CursorEdit<X,Dir> {
    Insert  (Dir,X), // Inserts an element
    Remove  (Dir),   // Removes an element (name-oblivious)
    Replace (Dir,X), // Replace an element (name-oblivious)
    Goto    (Dir),   // Move the cursor
}

#[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
pub enum Cmd<X,Dir,Name> {
    Basic    (CursorEdit<X,Dir>),
    InsName  (Dir,Name), // Insert name
    RemName  (Dir),      // Remove immediately-adjacent
    ShowView (ListReduce),   // Show the given view
    HideView (ListReduce),   // Remove the given view
}

#[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
pub enum ListTransf {
    Sort, Reverse
}

#[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
pub enum ListReduce {
    Max, Min, Median,
    DemandAll(ListTransf),
    DemandN(ListTransf, usize),
}

impl Arbitrary for Dir2 {
    fn arbitrary<G:Gen> (g: &mut G) -> Self {
        if g.gen() { Dir2::Left  }
        else       { Dir2::Right }
    }
    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        match *self {
            Dir2::Right => Box::new(Some(Dir2::Left).into_iter()),
            Dir2::Left  => Box::new(None.into_iter())
        }
    }
}

impl<X:Arbitrary+Sized+Rand>
    Arbitrary for CursorEdit<X,Dir2>
{
    fn arbitrary<G:Gen> (g: &mut G) -> Self {
        //match Rand::rand(g) as ListEditCmd<X,Name> {
        match g.gen_range(0, 100) {
            0  ... 50  => { CursorEdit::Insert (Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)) },
            50 ... 60  => { CursorEdit::Remove (Arbitrary::arbitrary(g)) },
            60 ... 70  => { CursorEdit::Replace(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)) },
            70 ... 95 =>  { CursorEdit::Goto   (Dir2::Right) },
            95 ... 100 => { CursorEdit::Goto   (Dir2::Left)  },
            _ => unreachable!()
        }
    }
    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        Box::new(None.into_iter())
    }
}

pub trait ExperimentT<A:Adapton,X,Out> {
    type ListEdit : ListEdit<A,X> ;
    fn run (&mut A, Vec<CursorEdit<X,Dir2>>, view:ListReduce) -> Vec<(Out,Cnt)> ;
}

fn has_consecutive_names<A:Adapton,X,L:ListT<A,X>> (st:&mut A, list:L::List) -> bool {
    L::elim(st, list,
            |st| false,
            |st,x,xs| has_consecutive_names::<A,X,L> (st, xs),
            |st,n,xs|
            L::elim(st, xs,
                    |st| false,
                    |st,y,ys| has_consecutive_names::<A,X,L> (st, ys),
                    |st,m,ys| true))
}

pub struct Experiment ;
impl<A:Adapton,X:Zero+Hash+Debug+PartialEq+Eq+Clone+PartialOrd> ExperimentT<A,X,Vec<X>>
    for Experiment
{
    type ListEdit = ListZipper<A,X,List<A,X>> ;
    fn run (st:&mut A, edits:Vec<CursorEdit<X,Dir2>>, view:ListReduce) -> Vec<(Vec<X>,Cnt)> {
        debug!("run");
        let mut outs : Vec<(Vec<X>,Cnt)> = Vec::new();
        let mut z : ListZipper<A,X,List<A,X>> = Self::ListEdit::empty(st) ;
        let mut loop_cnt = 0 as usize;
        for edit in edits.into_iter() {
            debug!("\n----------------------- Loop head; count={}", loop_cnt);
            debug!("zipper: {:?}", z);
            let consecutive_left  = has_consecutive_names::<A,X,List<A,X>>(st, z.left.clone());
            let consecutive_right = has_consecutive_names::<A,X,List<A,X>>(st, z.right.clone());
            debug!("zipper names: consecutive left: {}, consecutive right: {}",
                     consecutive_left, consecutive_right);
            //assert!(!consecutive_left);  // Todo-Later: This assertion generally fails for random interactions
            //assert!(!consecutive_right); // Todo-Later: This assertion generally fails for random interactions
            debug!("edit:   {:?}", edit);
            let (out, cnt) = st.cnt(|st|{
                let z_next = eval_edit::<A,X,Self::ListEdit>(st, edit, z.clone(), loop_cnt);
                let tree = Self::ListEdit::get_tree::<Tree<A,X,u32>>(st, z_next.clone(), Dir2::Left);
                debug!("tree:   {:?}", tree);
                let nm = st.name_of_string("eval_reduce".to_string());
                let out = st.ns(nm, |st|eval_reduce::<A,X,Tree<A,X,u32>>(st, tree, &view) );
                z = z_next;
                loop_cnt = loop_cnt + 1;
                out
            }) ;
            debug!("out:    {:?}", out);
            debug!("cnt:    {:?}", cnt);
            outs.push((out,cnt));
        } outs
    }
}

fn eval_edit<A:Adapton,X,E:ListEdit<A,X>> (st:&mut A, edit:CursorEdit<X,E::Dir>, z:E::State, id:usize) -> E::State {
    match edit {
        CursorEdit::Insert(dir,x) => {
            let z = E::clr_names(st, z, dir.clone()) ;
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

fn eval_reduce<A:Adapton,X:Zero+Hash+Eq+PartialOrd+Debug+Clone,T:TreeT<A,X>> (st:&mut A, tree:T::Tree, red:&ListReduce) -> Vec<X> {
    match *red {
        ListReduce::Max => { let x = tree_reduce_monoid::<A,X,T,_> (st, tree, X::zero(), &|st,x,y| if x > y {x} else {y}) ; vec![ x ] },
        ListReduce::Min => { let x = tree_reduce_monoid::<A,X,T,_> (st, tree, X::zero(), &|st,x,y| if x < y {x} else {y}) ; vec![ x ] },
        ListReduce::Median => panic!(""),
        ListReduce::DemandAll(ListTransf::Sort) => panic!(""),
        ListReduce::DemandAll(ListTransf::Reverse) => panic!(""),
        ListReduce::DemandN(_,_) => panic!(""),
    }
}

/// `ListEdit<A,X,L>` gives a simple notion of list-editing that is
/// generic with respect to adapton implementation `A`, list element
/// type `X`, and list implementation `L`.
pub trait ListEdit<A:Adapton,X> {
    /// The State of the Editor is abstract.
    type State ;
    /// Lists with foci admit two directions for movement.
    type Dir:Clone+Hash+Eq+PartialEq=Dir2;
    fn empty    (&mut A) -> Self::State;
    fn insert   (&mut A, Self::State, Self::Dir, X) -> Self::State;
    fn remove   (&mut A, Self::State, Self::Dir)    -> (Self::State, Option<X>);
    fn replace  (&mut A, Self::State, Self::Dir, X) -> (Self::State, X, bool);
    fn goto     (&mut A, Self::State, Self::Dir)    -> (Self::State, bool);
    fn observe  (&mut A, Self::State, Self::Dir)    -> (Self::State, Option<X>);

    // Insert/remove names from the list content:
    fn clr_names (&mut A, Self::State, Self::Dir) -> Self::State;
    fn ins_name  (&mut A, Self::State, Self::Dir, A::Name) -> Self::State;
    fn ins_cell  (&mut A, Self::State, Self::Dir, A::Name) -> Self::State;
    fn rem_name  (&mut A, Self::State, Self::Dir) -> (Self::State, Option<A::Name>);

    fn get_list<L:ListT<A,X>,T:TreeT<A,X>> (&mut A, Self::State, Self::Dir) -> L::List;
    fn get_tree<T:TreeT<A,X>>              (&mut A, Self::State, Self::Dir) -> T::Tree;


}

/// Lists with a focus; suitable to implement `ListEdit`.
#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub struct ListZipper<A:Adapton,X,L:ListT<A,X>> {
    /// Elements to the left of the focus, nearest to furthest.
    pub left: L::List,
    /// Elements to the right of the focus, nearest to furthest.
    pub right: L::List,
}

/// Implement `ListEdit` for `ListZipper` generically with respect to
/// adapton implementation `A`, list element type `X`, and list
/// implementation `L`.
impl<A:Adapton
    ,X:Debug+Hash+PartialEq+Eq+Clone
    ,L:ListT<A,X>
    >
    ListEdit<A,X>
    for
    ListZipper<A,X,L>
{
    type State=ListZipper<A,X,L>;
    type Dir=Dir2;
    
    fn clr_names (st:&mut A, zip:Self::State, dir:Self::Dir) -> Self::State {
        match dir {
            Dir2::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right| ListZipper{left:L::nil(st), right:right},
                 |st,x,left,right| ListZipper{left:L::cons(st,x,left), right:right},
                 |st,nm,left,right| {
                     let right = L::name(st,nm,right);
                     Self::clr_names(st, ListZipper{left:left, right:right}, dir)}
                 ),
            Dir2::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left| ListZipper{right:L::nil(st), left:left},
                 |st,x,right,left| ListZipper{right:L::cons(st,x,right), left:left},
                 |st,nm,right,left| {
                     let left = L::name(st,nm,left);
                     Self::clr_names(st, ListZipper{left:left, right:right}, dir)}
                 ),
        }
    }

    fn ins_name (st:&mut A, zip:Self::State, dir:Self::Dir, name:A::Name) -> Self::State {
        match dir {
            Dir2::Left =>
                ListZipper{left:L::name(st, name, zip.left),
                           right:zip.right},
            Dir2::Right =>
                ListZipper{left:zip.left,
                           right:L::name(st, name, zip.right)},
        }
    }

    fn ins_cell (st:&mut A, zip:Self::State, dir:Self::Dir, name:A::Name) -> Self::State {
        match dir {
            Dir2::Left => {
                let art = st.cell(name, zip.left) ;
                let art = st.read_only(art);
                ListZipper{left:L::art(st, art),
                           right:zip.right}},                
            Dir2::Right => {
                let art = st.cell(name, zip.right) ;
                let art = st.read_only(art);
                ListZipper{left:zip.left,
                           right:L::art(st, art)}},
        }
    }

    fn rem_name  (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State, Option<A::Name>) {
        match dir {
            Dir2::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right}, None ),
                 |_,x,left,right|   (ListZipper{left:left,       right:right}, None ),
                 |st,nm,left,right| (ListZipper{left:left, right:right},       Some(nm))
                 ),
            Dir2::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|          (ListZipper{left:left, right:L::nil(st)}, None ),
                 |_,x,right,left|   (ListZipper{left:left, right:right},      None ),
                 |st,nm,right,left| (ListZipper{left:left, right:right},      Some(nm))
                 ),
        }
    }
    
    fn empty (st: &mut A) -> Self::State {
        let nil1 = L::nil(st);
        let nil2 = nil1.clone();
        ListZipper{left:nil1, right:nil2}
    }
    
    fn insert (st:&mut A, zip:Self::State, dir:Self::Dir, x:X) -> Self::State {
        match dir {
            Dir2::Left =>
                ListZipper{left:L::cons(st, x, zip.left),
                           right:zip.right},
            Dir2::Right =>
                ListZipper{left:zip.left,
                           right:L::cons(st, x, zip.right)},
        }
    }

    fn remove  (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State, Option<X>) {
        match dir {
            Dir2::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right}, None   ),
                 |_,x,left,right|   (ListZipper{left:left,       right:right}, Some(x)),
                 |st,nm,left,right| {let zip = ListZipper{left:left, right:L::name(st,nm,right)};
                                     Self::remove (st, zip, Dir2::Left)}
                 ),
            Dir2::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|          (ListZipper{left:left, right:L::nil(st)}, None   ),
                 |_,x,right,left|   (ListZipper{left:left, right:right},      Some(x)),
                 |st,nm,right,left| {let zip = ListZipper{left:L::name(st,nm,left), right:right};
                                     Self::remove (st, zip, Dir2::Right)}
                 ),
        }
    }

    
    fn goto (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State, bool) {
        match dir {
            Dir2::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right}              , false ),
                 |st,x,left,right|  (ListZipper{left:left,       right:L::cons(st,x,right)}, true  ),
                 |st,nm,left,right| {let zip = ListZipper{left:left, right:L::name(st,nm,right)};
                                     Self::goto (st, zip, Dir2::Left)}
                 ),
            Dir2::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|          (ListZipper{left:left,              right:L::nil(st)}, false ),
                 |st,x,right,left|  (ListZipper{left:L::cons(st,x,left),right:right}     , true  ),
                 |st,nm,right,left| {let zip = ListZipper{left:L::name(st,nm,left), right:right};
                                     Self::goto (st, zip, Dir2::Right)}
                 ),
        }
    }

    fn observe (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State,Option<X>) {
        match dir {
            Dir2::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right}, None),
                 |st,x,left,right| {let x2 = x.clone();
                                    (ListZipper{left:L::cons(st,x,left), right:right}, Some(x2))},
                 |st,nm,left,right|{let zip = ListZipper{left:left,right:L::name(st,nm,right)};
                                    Self::observe (st, zip, Dir2::Left)}
                 ),
            Dir2::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|         (ListZipper{left:left, right:L::nil(st)}, None),
                 |st,x,right,left| {let x2 = x.clone();
                                    (ListZipper{left:left, right:L::cons(st,x,right)}, Some(x2))},
                 |st,nm,right,left|{let zip = ListZipper{left:L::name(st,nm,left),right:right};
                                    Self::observe (st, zip, Dir2::Right)}
                 ),
        }
    }

    fn replace (st:&mut A, zip:Self::State, dir:Self::Dir, y:X) -> (Self::State, X, bool) {
        match dir {
            Dir2::Left => L::elim_move
                (st, zip.left, (zip.right, y),
                 |st,(right,y)|        (ListZipper{left:L::nil(st),         right:right}, y, false),
                 |st,x,left,(right,y)| (ListZipper{left:L::cons(st,y,left), right:right}, x, true ),
                 |st,nm,left,(right,y)|{let zip = ListZipper{left:left,right:L::name(st,nm,right)};
                                        Self::replace (st, zip, Dir2::Left, y)}
                 ),
            Dir2::Right => L::elim_move
                (st, zip.right, (zip.left,y),
                 |st,(left,y)|         (ListZipper{left:left, right:L::nil(st)},          y, false),
                 |st,x,right,(left,y)| (ListZipper{left:left, right:L::cons(st,y,right)}, x, true ),
                 |st,nm,right,(left,y)|{let zip = ListZipper{left:L::name(st,nm,left),right:right};
                                        Self::replace (st, zip, Dir2::Right, y)}
                 ),
        }
    }

    fn get_list<N:ListT<A,X>,T:TreeT<A,X>>
        (st:&mut A, zip:Self::State, dir:Self::Dir) -> N::List
    {
        let tree = Self::get_tree::<T>(st, zip, dir);
        list_of_tree::<A,X,N,T>(st, tree)
    }

    /// Creates a tree whose leaves hold the contents of the zipper, in order.
    /// When `dir=Left`,  the tree's leaves are ordered from left-to-right, i.e., as (rev left) @ right.
    /// When `dir=Right`, the tree's leaves are ordered from right-to-left, i.e., as (rev right) @ left.
    fn get_tree<T:TreeT<A,X>>
        (st:&mut A, zip:Self::State, dir:Self::Dir) -> T::Tree
    {
        match dir {
            Dir2::Left  => {
                let left  = tree_of_list::<A,X,T,L>(st, Dir2::Right, zip.left);
                let right = tree_of_list::<A,X,T,L>(st, Dir2::Left,  zip.right);
                tree_append::<A,X,T>(st, left, right)}
            
            Dir2::Right => {
                let right = tree_of_list::<A,X,T,L>(st, Dir2::Right, zip.right);
                let left  = tree_of_list::<A,X,T,L>(st, Dir2::Left,  zip.left);
                tree_append::<A,X,T>(st, right, left)}
        }
    }
}
