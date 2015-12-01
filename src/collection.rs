#![feature(associated_type_defaults)]
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use macros::* ;
use adapton_sigs::* ;
use quickcheck::Arbitrary;
use quickcheck::Gen;

use rand::Rand;

#[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
enum ListEditCmd<X,Name> {
    Insert  (ListEditDir,X),    // Inserts an element
    Remove  (ListEditDir),      // Removes an element (name-oblivious)
    Replace (ListEditDir,X),    // Replace an element (name-oblivious)
    Goto    (ListEditDir),      // Move the cursor

    InsName (ListEditDir,Name), // Insert name
    RemName (ListEditDir),      // Remove immediately-adjacent

    Atomic  (Vec<ListEditCmd<X,Name>>), // Edits to be performed as an atomic block

    ShowView(ListReduce),       // Show the given view
    HideView(ListReduce),       // Remove the given view
}
#[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
enum ListTransf {
    Sort, Reverse
}
#[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
enum ListReduce {
    Max, Min, Median,
    DemandAll(ListTransf),
    DemandN(ListTransf, usize),
}

impl Arbitrary for ListEditDir {
    fn arbitrary<G:Gen> (g: &mut G) -> Self {
        if g.gen() { ListEditDir::Left  }
        else       { ListEditDir::Right }
    }
    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        let v = Vec::<ListEditDir>::new();
        match *self {
            ListEditDir::Right => Box::new(Some(ListEditDir::Left).into_iter()),
            ListEditDir::Left  => Box::new(None.into_iter())
        }
    }
}

impl<X:Arbitrary+Sized+Rand+'static,Name:Arbitrary+Sized+Rand+'static>
    Arbitrary for ListEditCmd<X,Name>
{
    fn arbitrary<G:Gen> (g: &mut G) -> Self {
        //match Rand::rand(g) as ListEditCmd<X,Name> {
        match g.gen_range(0, 100) {
            0  ... 50  => { ListEditCmd::Insert(Arbitrary::arbitrary(g), Arbitrary::arbitrary(g)) },
            50 ... 70  => { ListEditCmd::Remove(Arbitrary::arbitrary(g)) },
            70 ... 100 => { ListEditCmd::Goto  (Arbitrary::arbitrary(g)) },
            _ => unreachable!()
        }
    }
    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        // Any command becomes "skip" (the empty command sequence)
        let no_cmds = ListEditCmd::Atomic(Vec::new()) ;
        Box::new(Some(no_cmds).into_iter())
    }
}

/// `ListEdit<A,X,L>` gives a simple notion of list-editing that is
/// generic with respect to adapton implementation `A`, list element
/// type `X`, and list implementation `L`.
pub trait ListEdit<A:Adapton,X> {
    /// The State of the Editor is abstract.
    type State ;
    /// Lists with foci admit two directions for movement.
    type Dir=ListEditDir;
    fn empty    (&mut A) -> Self::State;
    fn insert   (&mut A, Self::State, Self::Dir, X) -> Self::State;
    fn remove   (&mut A, Self::State, Self::Dir)    -> (Self::State, Option<X>);
    fn replace  (&mut A, Self::State, Self::Dir, X) -> (Self::State, X, bool);
    fn goto     (&mut A, Self::State, Self::Dir)    -> (Self::State, bool);
    fn observe  (&mut A, Self::State, Self::Dir)    -> (Self::State, Option<X>);

    // Insert/remove names from the list content:
    fn ins_name (&mut A, Self::State, Self::Dir, A::Name) -> Self::State;
    fn rem_name (&mut A, Self::State, Self::Dir) -> (Self::State, Option<A::Name>);

    fn get_list<L:ListT<A,X>,T:TreeT<A,X>> (&mut A, Self::State, Self::Dir) -> L::List;
    fn get_tree<T:TreeT<A,X>>              (&mut A, Self::State, Self::Dir) -> T::Tree;


}
/// Lists are one-dimensional structures; movement admits two possible directions.
#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum ListEditDir { Left, Right }

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
    type Dir=ListEditDir;
    
    fn ins_name (st:&mut A, zip:Self::State, dir:Self::Dir, x:A::Name) -> Self::State {
        match dir {
            ListEditDir::Left =>
                ListZipper{left:L::name(st, x, zip.left),
                           right:zip.right},
            ListEditDir::Right =>
                ListZipper{left:zip.left,
                           right:L::name(st, x, zip.right)},
        }
    }

    fn rem_name  (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State, Option<A::Name>) {
        match dir {
            ListEditDir::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right}, None ),
                 |_,x,left,right|   (ListZipper{left:left,       right:right}, None ),
                 |st,nm,left,right| (ListZipper{left:left, right:right},       Some(nm))
                 ),
            ListEditDir::Right => L::elim_move
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
            ListEditDir::Left =>
                ListZipper{left:L::cons(st, x, zip.left),
                           right:zip.right},
            ListEditDir::Right =>
                ListZipper{left:zip.left,
                           right:L::cons(st, x, zip.right)},
        }
    }

    fn remove  (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State, Option<X>) {
        match dir {
            ListEditDir::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right}, None   ),
                 |_,x,left,right|   (ListZipper{left:left,       right:right}, Some(x)),
                 |st,nm,left,right| {let zip = ListZipper{left:left, right:L::name(st,nm,right)};
                                     Self::remove (st, zip, ListEditDir::Left)}
                 ),
            ListEditDir::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|          (ListZipper{left:left, right:L::nil(st)}, None   ),
                 |_,x,right,left|   (ListZipper{left:left, right:right},      Some(x)),
                 |st,nm,right,left| {let zip = ListZipper{left:L::name(st,nm,left), right:right};
                                     Self::remove (st, zip, ListEditDir::Right)}
                 ),
        }
    }

    fn goto (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State, bool) {
        match dir {
            ListEditDir::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right}              , false ),
                 |st,x,left,right|  (ListZipper{left:left,       right:L::cons(st,x,right)}, true  ),
                 |st,nm,left,right| {let zip = ListZipper{left:left, right:L::name(st,nm,right)};
                                     Self::goto (st, zip, ListEditDir::Left)}
                 ),
            ListEditDir::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|          (ListZipper{left:left,              right:L::nil(st)}, false ),
                 |st,x,right,left|  (ListZipper{left:L::cons(st,x,left),right:right}     , true  ),
                 |st,nm,right,left| {let zip = ListZipper{left:L::name(st,nm,left), right:right};
                                     Self::goto (st, zip, ListEditDir::Right)}
                 ),
        }
    }

    fn observe (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State,Option<X>) {
        match dir {
            ListEditDir::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right}, None),
                 |st,x,left,right| {let x2 = x.clone();
                                    (ListZipper{left:L::cons(st,x,left), right:right}, Some(x2))},
                 |st,nm,left,right|{let zip = ListZipper{left:left,right:L::name(st,nm,right)};
                                    Self::observe (st, zip, ListEditDir::Left)}
                 ),
            ListEditDir::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|         (ListZipper{left:left, right:L::nil(st)}, None),
                 |st,x,right,left| {let x2 = x.clone();
                                    (ListZipper{left:left, right:L::cons(st,x,right)}, Some(x2))},
                 |st,nm,right,left|{let zip = ListZipper{left:L::name(st,nm,left),right:right};
                                    Self::observe (st, zip, ListEditDir::Right)}
                 ),
        }
    }

    fn replace (st:&mut A, zip:Self::State, dir:Self::Dir, y:X) -> (Self::State, X, bool) {
        match dir {
            ListEditDir::Left => L::elim_move
                (st, zip.left, (zip.right, y),
                 |st,(right,y)|        (ListZipper{left:L::nil(st),         right:right}, y, false),
                 |st,x,left,(right,y)| (ListZipper{left:L::cons(st,y,left), right:right}, x, true ),
                 |st,nm,left,(right,y)|{let zip = ListZipper{left:left,right:L::name(st,nm,right)};
                                        Self::replace (st, zip, ListEditDir::Left, y)}
                 ),
            ListEditDir::Right => L::elim_move
                (st, zip.right, (zip.left,y),
                 |st,(left,y)|         (ListZipper{left:left, right:L::nil(st)},          y, false),
                 |st,x,right,(left,y)| (ListZipper{left:left, right:L::cons(st,y,right)}, x, true ),
                 |st,nm,right,(left,y)|{let zip = ListZipper{left:L::name(st,nm,left),right:right};
                                        Self::replace (st, zip, ListEditDir::Right, y)}
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
            ListEditDir::Left  => {
                let left  = tree_of_list::<A,X,T,L>(st, ListEditDir::Right, zip.left);
                let right = tree_of_list::<A,X,T,L>(st, ListEditDir::Left,  zip.right);
                tree_append::<A,X,T>(st, left, right)}
            
            ListEditDir::Right => {
                let right = tree_of_list::<A,X,T,L>(st, ListEditDir::Right, zip.right);
                let left  = tree_of_list::<A,X,T,L>(st, ListEditDir::Left,  zip.left);
                tree_append::<A,X,T>(st, right, left)}
        }
    }
}


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

pub fn tree_append
    < A:Adapton
    , X:Hash+Clone+Debug
    , T:TreeT<A,X>
    >
    (st:&mut A,tree1:T::Tree,tree2:T::Tree) -> T::Tree
{
    // XXX: This is a hack. Make this balanced, a la Pugh 1989.
    T::bin(st, T::lev_max(), tree1, tree2)
}

// pub fn tree_append
//     <A:Adapton
//     ,X:Clone+Hash+Eq+Debug
//     ,T:TreeT<A,X,()>
//     >
//     (st:&mut A, tree1:T::Tree, tree2:T::Tree) -> T::Tree
// {
//     T::elim_move
//         (st, tree1, tree2,         
//          /* Nil */  |st,       tree2| tree2,
//          /* Leaf */ |st,leaf,  tree2| {
//              T::elim_move
//                  (st, tree2, leaf,
//                   /* Nil */  |st, leaf1| T::leaf(st,leaf1),
//                   /* Leaf */ |st, leaf2, leaf1| {
//                       let l1 = T::leaf(st,leaf1);
//                       let l2 = T::leaf(st,leaf2);
//                       T::bin(st, (), l1, l2)
//                   },
//                   /* Bin */  |st, l2, r2, leaf1| {
//                       let leaf = T::leaf(st, leaf);
//                       /*T::bin(st, (), leaf, tree2)*/
//                       panic!("TODO")
//                   },
//                   /* Name */ |st, nm, l2, r2, leaf1| {
//                       panic!("TODO")
//                   })
//          },
//          /* Bin */ |st,_,l,r, tree2| {
//              /*
//              T::elim_move
//                  (st, tree2, (l,r),
//                   /* Nil */ |st, 
                  
//                   let tree1 = T::bin(st, (), l, r);
//              T::bin(st, (), tree1, tree2)
//                  */
//                  panic!("")
//          },
//          /* Name */ |st,_,l,r, tree2| panic!(""),
//          )
// }

pub fn tree_reduce_monoid<A:Adapton,Elm:Eq+Hash+Clone+Debug,T:TreeT<A,Elm>,BinOp>
    (st:&mut A, tree:T::Tree, zero:Elm, binop:&BinOp) -> Elm
    where BinOp:Fn(&mut A, Elm, Elm) -> Elm
{
    T::fold_up(st, tree,
               &|_|        zero.clone(),
               &|_,leaf|   leaf,
               &|st,_,l,r| binop(st,l,r),
               &|st,_,_,l,r| binop(st,l,r),
               )
}

pub fn list_reduce_monoid<A:Adapton,Elm:Eq+Hash+Clone+Debug,L:ListT<A,Elm>,BinOp,T:TreeT<A,Elm>>
    (st:&mut A, list:L::List, zero:Elm, binop:&BinOp) -> Elm
    where BinOp:Fn(&mut A, Elm, Elm) -> Elm
{
    let tree = tree_of_list::<A,Elm,T,L>(st, ListEditDir::Left, list);
    tree_reduce_monoid::<A,Elm,T,BinOp>(st, tree, zero, binop)
}

// pub fn tree_map<A:Adapton,X:Hash+Clone,T:TreeT<A,X>,FX:Hash+Clone,GY:Hash+Clone,FGT:TreeT<A,FX,GY>,F,G>
//     (st:&mut A, tree:T::Tree, f:&F, g:&G) -> FGT::Tree
//     where F:Fn(&mut A, X) -> FX
//     ,     G:Fn(&mut A, Y) -> GY
// {
//     T::fold_up(st, tree,
//                &|st|       FGT::nil(st),
//                &|st,x|     {let fx = f(st,x);  FGT::leaf(st, fx)},
//                &|st,y,l,r| {let gy = g(st, y); FGT::bin(st, gy, l, r)},
//                &|st,n,l,r| FGT::name(st, n, l, r)
//                )
// }

pub fn tree_filter<A:Adapton,X:Hash+Clone,T:TreeT<A,X>,F>
    (st:&mut A, tree:T::Tree, f:&F) -> T::Tree
    where F:Fn(&mut A, &X) -> bool
{
    T::fold_up(st, tree,
               &|st|       T::nil(st),
               &|st,x|     { let fx = f(st,&x);
                             if fx { T::leaf(st, x) }
                             else  { T::nil(st) } },
               &|st,lev,l,r| T::bin(st, lev, l, r),
               &|st,n,lev,l,r| T::name(st, n, lev, l, r)
               )
}

pub fn list_of_tree<A:Adapton,X:Hash+Clone,L:ListT<A,X>,T:TreeT<A,X>>
    (st:&mut A, tree:T::Tree) -> L::List
{
    let nil = L::nil(st);
    T::fold_rl(st, tree, nil,
               &|st,x,xs| L::cons(st,x,xs),
               &|_,_,xs|  xs,
               &|st,n,_,xs| L::name(st,n,xs)
               )
}

pub fn rev_list_of_tree<A:Adapton,X:Hash+Clone,L:ListT<A,X>,T:TreeT<A,X>>
    (st:&mut A, tree:T::Tree) -> L::List
{
    let nil = L::nil(st);
    T::fold_lr(st, tree, nil,
               &|st,x,xs| L::cons(st,x,xs),
               &|_ ,_,xs| xs,
               &|st,n,_,xs| L::name(st,n,xs)
               )    
}

pub fn list_of_vec<A:Adapton,X:Clone,L:ListT<A,X>> (st:&mut A, v:Vec<X>) -> L::List {
    let mut l = L::nil(st);
    for x in v.iter().rev() { l = L::cons(st,x.clone(),l) }
    return l
}

pub fn rev_list_of_vec<A:Adapton,X:Clone,L:ListT<A,X>> (st:&mut A, v:Vec<X>) -> L::List {
    let mut l = L::nil(st);
    for x in v.iter() { l = L::cons(st,x.clone(), l) }
    return l
}

#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub enum List<A:Adapton,Hd> {
    Nil,
    Cons(Hd,      Box<List<A,Hd>>),
    Rc(           Rc<List<A,Hd>>),
    Name(A::Name, Box<List<A,Hd>>),
    Art(          Art<List<A,Hd>, A::Loc>),
}

// TODO: Why Does Adapton have to implement all of these?
//       It's not actually *contained* within the List structure; it cannot be ecountered there.
//       It's only ever present in a negative position (as a function parameter).
impl< A:Adapton+Debug+Hash+PartialEq+Eq+Clone
    , Hd:Debug+Hash+PartialEq+Eq+Clone
    >
    ListT<A,Hd>
    for List<A,Hd>
{
    type List = List<A,Hd>;

    fn nil  (_:&mut A)                             -> Self::List { List::Nil }
    fn cons (_:&mut A, hd:Hd, tl:Self::List)       -> Self::List { List::Cons(hd,Box::new(tl)) }
    fn name (_:&mut A, nm:A::Name, tl:Self::List)  -> Self::List { List::Name(nm, Box::new(tl)) }
    fn rc   (_:&mut A, rc:Rc<List<A,Hd>>)          -> Self::List { List::Rc(rc) }
    fn art  (_:&mut A, art:Art<List<A,Hd>,A::Loc>) -> Self::List { List::Art(art) }

    fn elim<Res,Nil,Cons,Name>
        (st:&mut A, list:Self::List, nilf:Nil, consf:Cons, namef:Name) -> Res
        where Nil:FnOnce(&mut A) -> Res
        ,    Cons:FnOnce(&mut A, Hd, Self::List) -> Res
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
            }
        }
    }
    
    fn elim_move<Arg,Res,Nil,Cons,Name>
        (st:&mut A, list:Self::List, arg:Arg, nilf:Nil, consf:Cons, namef:Name) -> Res
        where Nil:FnOnce(&mut A, Arg) -> Res
        ,    Cons:FnOnce(&mut A, Hd, Self::List, Arg) -> Res
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
            }
        }
    }
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
    fn lev_bits () -> Self::Lev { 32 }
    fn lev_zero () -> Self::Lev { 0 }
    fn lev_max () -> Self::Lev { u32::max_value() }
    fn lev_add (x:Self::Lev,y:Self::Lev) -> Self::Lev { x + y }
    fn lev_inc (x:Self::Lev) -> Self::Lev { x + 1 }
    fn lev_lte (x:Self::Lev,y:Self::Lev) -> bool { x <= y }
    
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
}

pub fn tree_of_list
    < A:Adapton
    , X:Hash+Clone+Debug
    , T:TreeT<A,X>
    , L:ListT<A,X>
    >
    (st:&mut A, dir_list:ListEditDir, list:L::List) -> T::Tree {
        let tnil = T::nil(st);
        let lnil = L::nil(st);
        let (tree, list) = tree_of_list_rec::<A,X,T,L>(st, dir_list, list, tnil, T::lev_zero(), T::lev_max());
        assert_eq!(list, lnil);
        tree
    }

pub fn tree_of_list_rec
    < A:Adapton
    , X:Hash+Clone+Debug
    , T:TreeT<A,X>
    , L:ListT<A,X>
    >
    (st:&mut A,
     dir_list:ListEditDir, list:L::List,
     tree:T::Tree, tree_lev:T::Lev, parent_lev:T::Lev)
     -> (T::Tree, L::List)
{
    L::elim_move (
        st, list, (dir_list, tree, tree_lev, parent_lev),

        /* Nil */
        |st, (_dir_list, tree, _, _)| (tree, L::nil(st)),

        /* Cons */
        |st, hd, rest, (dir_list, tree, tree_lev, parent_lev)| {
            let lev_hd = T::lev_inc ( T::lev(&hd) ) ;
            if T::lev_lte ( tree_lev , lev_hd.clone() ) && T::lev_lte ( lev_hd.clone() , parent_lev.clone() ) {
                let leaf = T::leaf(st, hd) ;
                let (tree2, rest2) = {
                    tree_of_list_rec::<A,X,T,L> ( st, dir_list.clone(), rest, leaf, T::lev_zero(), lev_hd.clone() )
                };
                let tree3 = match dir_list.clone() {
                    ListEditDir::Left  => T::bin ( st, lev_hd.clone(), tree,  tree2 ),
                    ListEditDir::Right => T::bin ( st, lev_hd.clone(), tree2, tree  ),
                } ;
                tree_of_list_rec::<A,X,T,L> ( st, dir_list, rest2, tree3, lev_hd, parent_lev )
            }
            else {
                (tree, L::cons(st,hd,rest))
            }},

        /* Name */
        |st, nm, rest, (dir_list, tree, tree_lev, parent_lev)|{
            let lev_nm = T::lev_inc( T::lev_add( T::lev_bits() , T::lev(&nm) ) ) ;
            if T::lev_lte ( tree_lev , lev_nm.clone() ) && T::lev_lte ( lev_nm.clone() ,  parent_lev.clone() ) {
                let nil = T::nil(st) ;
                let (nm1, nm2) = st.name_fork(nm.clone());
                let (tree2, rest) =
                    memo!(st, nm1 =>> tree_of_list_rec::<A,X,T,L>,
                          dir_list:dir_list.clone(), list:rest,
                          tree:nil, tree_lev:T::lev_zero(), parent_lev:lev_nm.clone() ) ;
                let tree3 = match dir_list.clone() {
                    ListEditDir::Left  => T::name ( st, nm, lev_nm.clone(), tree,  tree2 ),
                    ListEditDir::Right => T::name ( st, nm, lev_nm.clone(), tree2, tree  ),
                } ;
                memo!(st, nm2 =>> tree_of_list_rec::<A,X,T,L>,
                      dir_list:dir_list.clone(), list:rest,
                      tree:tree3, tree_lev:lev_nm, parent_lev:parent_lev )
            }
            else {
                (tree, L::name(st,nm,rest))
            }},
        )
}

pub fn list_merge<A:Adapton,X:Ord+Clone+Debug,L:ListT<A,X>>
    (st:&mut A,
     n1:Option<A::Name>, l1:L::List,
     n2:Option<A::Name>, l2:L::List  ) -> L::List
{
    L::elim_move
        (st, l1, (n1,n2,l2),
         /* Nil */  |_, (_, _, l2)| l2,
         /* Cons */ |st,h1,t1,(n1,n2,l2)|
         L::elim_move
         (st, l2, (h1,t1,n1,n2),
          /* Nil */  |st, (h1, t1, _, _ )| L::cons(st, h1,t1),
          /* Cons */ |st, h2, t2, (h1, t1, n1, n2)| {
          if &h1 <= &h2 {
              let l2 = L::cons(st,h2,t2);
              match n1 {
                  None => {
                      let rest = list_merge::<A,X,L>(st, None, t1, n2, l2);
                      L::cons(st, h1, rest)
                  }
                  Some(n1) => {
                      let (n1a, n1b) = st.name_fork(n1);
                      let rest = thunk!(st, n1a =>>
                                        list_merge::<A,X,L>,
                                        n1:None, l1:t1, n2:n2, l2:l2);
                      let rest = L::art(st, rest);
                      let rest = L::cons(st, h1, rest);
                      L::name(st, n1b, rest)
                  }
              }
          }
          else {
              let l1 = L::cons(st,h1,t1);
              match n2 {
                  None => {
                      let rest = list_merge::<A,X,L>(st, n1, l1, None, t2);
                      let l = L::cons(st, h2, rest);
                      l
                  }
                  Some(n2) => {
                      let (n2a, n2b) = st.name_fork(n2);
                      let rest = thunk!(st, n2a =>>
                                        list_merge::<A,X,L>,
                                        n1:n1, l1:l1, n2:None, l2:t2);
                      let rest = L::art(st, rest);
                      let rest = L::cons(st, h2, rest);
                      L::name(st, n2b, rest)
                  }
              }
          }},
          |st,m2,t2,(h1,t1,n1,_n2)| {
              let l1 = L::cons(st,h1,t1);
              list_merge::<A,X,L>(st, n1, l1, Some(m2), t2)
          }
          ),
         |st,m1,t1,(_n1,n2,l2)| {
             list_merge::<A,X,L>(st, Some(m1), t1, n2, l2)
         }
         )
}

pub fn list_merge_sort<A:Adapton,X:Ord+Hash+Debug+Clone,L:ListT<A,X>,T:TreeT<A,X>>
    (st:&mut A, list:L::List) -> L::List
{
    let tree = tree_of_list::<A,X,T,L>(st, ListEditDir::Left, list);
    T::fold_up (st, tree,
                &|st|                 L::nil(st), 
                &|st, x|              L::singleton(st, x),
                &|st, _, left, right| { list_merge::<A,X,L>(st, None, left, None, right) },
                &|st, n, _, left, right| { let (n1,n2) = st.name_fork(n);
                                           list_merge::<A,X,L>(st, Some(n1), left, Some(n2), right) },
                )
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
    ,NodeSet:SetT<A,Node>
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
