use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use std::marker::PhantomData;

use adapton_syntax::* ;
use adapton_sigs::* ;

/// Why Zippers?
/// -------------

/// Zippers provide a simple, yet expressive functional model for
/// incremental editing.  We consider a list editor below.  

/// Capturing the notion of "incremental editing" is essential for
/// effective incremental computation.  Specifically, the editor
/// metaphor is the right input-output interface to the "outside user"
/// of the incremental computation, since it directly captures the
/// process by which the user edits, and it gives a language to these
/// edits, viz., the primitive operations supported by the zipper.  

/// Zippers efficiently implement functional editors for many common
/// data structures.  This efficiency comes from avoiding unnecessary
/// traversals.  In functional programming, traversals consist of both
/// allocation and garbage collection, e.g., to reverse pointers along
/// the path to the focused sublist or subtree.  The design of zippers
/// ammortize the work of focusing and unfocusing the structure, over a
/// large number of edits, rather than pay the linear cost for each
/// edit individually.  Just like other functional data structures, and
/// unlike imperative editors, zippers enjoy **referential
/// transparency**, meaning that new states of the editor do not arise
/// by destructively updating the old states.  Instead, new and old
/// states co-exist, and they typically share internal structure
/// wherever the sub-structure before and after the edit are unchanged,
/// and thus common.  This sharing is key to their efficiency.  

/// Unfortunately, zippers appear to have a fundemental limitation:
/// They implement efficient single-cursor editing, but do not
/// efficiently implement multi-cursor editing.  To perform an edit
/// sequence that involves multiple cursors, a naive zipper user would
/// focus and unfocus each time the "current cursor" changes between
/// consecutive edits. The alternative to repeated focusing and
/// unfocusing are designs for specialized multi-cursor zippers (e.g.,
/// a two-cursor zipper, a three-cursor zipper, etc.).  These
/// multi-cursor designs are supported by comparatively little
/// research, and unfortunately, they also appear to be significantly
/// more complex than their single-cursor counterparts, as the number
/// of structural cases to consider for each operation explodes as
/// cursors are added.  Hence, the presence of multiple cursors seems
/// to present a fundamental, insurmountable limitation for the
/// zipper, which otherwise works simply, and beautifully.

/// **Claim 1**: Using nominal IC, the single-focus list zipper can refocus
/// in amortized O(log n) time, for list structures of length n.  

/// **Key idea**: Choose a canonical form for the unfocused structure based
/// on probabilistically-balanced trees, and use nominal IC to memoize
/// the transformations between this canonical form and a single-focus
/// zipper at the desired cursor.  See `tree_of_2lists` below for the
/// specific algorithm I'm currently trying out.  

/// Questions for future designs:
/// -------------------------------
/// To what extent can we exploit laziness and function inverses?  For
/// instance, we want to avoid building zipperized views for parts of
/// the structure that are not edited.  When we canonicalize these
/// parts of the zipper structure, it'd be nice to just apply the
/// identity function, i.e., do nothing.  
///

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

    fn get_list<L:ListT<A,X>,T:TreeT<A,X,()>> (&mut A, Self::State, Self::Dir) -> L::List;
    fn get_tree<T:TreeT<A,X,()>>              (&mut A, Self::State, Self::Dir) -> T::Tree;
}
/// Lists are one-dimensional structures; movement admits two possible directions.
#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum ListEditDir { Left, Right }

/// Lists with a focus; suitable to implement `ListEdit`.
#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub struct ListZipper<A:Adapton,X,L:ListT<A,X>,M:ListT<A,(ListEditDir,L::List)>> {
    /// Elements to the left of the focus, nearest to furthest.
    pub left: L::List,
    /// Elements to the right of the focus, nearest to furthest.
    pub right: L::List,
    pub phantom:PhantomData<M>,
}

/// Implement `ListEdit` for `ListZipper` generically with respect to
/// adapton implementation `A`, list element type `X`, and list
/// implementation `L`.
impl<A:Adapton
    ,X:Debug+Hash+PartialEq+Eq+Clone
    ,L:ListT<A,X>
    ,M:ListT<A,(ListEditDir,L::List)>
    >
    ListEdit<A,X>
    for
    ListZipper<A,X,L,M>
{
    type State=ListZipper<A,X,L,M>;
    type Dir=ListEditDir;
    
    fn empty (st: &mut A) -> Self::State {
        let nil1 = L::nil(st);
        let nil2 = nil1.clone();
        ListZipper{left:nil1, right:nil2, phantom:PhantomData}
    }
    
    fn insert (st:&mut A, zip:Self::State, dir:Self::Dir, x:X) -> Self::State {
        match dir {
            ListEditDir::Left =>
                ListZipper{left:L::cons(st, x, zip.left),
                           right:zip.right,
                           phantom:PhantomData},
            ListEditDir::Right =>
                ListZipper{left:zip.left,
                           right:L::cons(st, x, zip.right),
                           phantom:PhantomData},
        }
    }
    
    fn remove  (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State, Option<X>) {
        match dir {
            ListEditDir::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right, phantom:PhantomData}, None   ),
                 |_,x,left,right|   (ListZipper{left:left,       right:right, phantom:PhantomData}, Some(x)),
                 |st,nm,left,right| {let zip = ListZipper{left:left, right:L::name(st,nm,right), phantom:PhantomData};
                                     Self::remove (st, zip, ListEditDir::Left)}
                 ),
            ListEditDir::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|          (ListZipper{left:left, right:L::nil(st), phantom:PhantomData}, None   ),
                 |_,x,right,left|   (ListZipper{left:left, right:right,      phantom:PhantomData}, Some(x)),
                 |st,nm,right,left| {let zip = ListZipper{left:L::name(st,nm,left), right:right, phantom:PhantomData};
                                     Self::remove (st, zip, ListEditDir::Right)}
                 ),
        }
    }

    fn goto (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State, bool) {
        match dir {
            ListEditDir::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right,               phantom:PhantomData}, false ),
                 |st,x,left,right|  (ListZipper{left:left,       right:L::cons(st,x,right), phantom:PhantomData}, true  ),
                 |st,nm,left,right| {let zip = ListZipper{left:left, right:L::name(st,nm,right), phantom:PhantomData};
                                     Self::goto (st, zip, ListEditDir::Left)}
                 ),
            ListEditDir::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|          (ListZipper{left:left,              right:L::nil(st), phantom:PhantomData}, false ),
                 |st,x,right,left|  (ListZipper{left:L::cons(st,x,left),right:right,      phantom:PhantomData}, true  ),
                 |st,nm,right,left| {let zip = ListZipper{left:L::name(st,nm,left), right:right, phantom:PhantomData};
                                     Self::goto (st, zip, ListEditDir::Right)}
                 ),
        }
    }

    fn observe (st:&mut A, zip:Self::State, dir:Self::Dir) -> (Self::State,Option<X>) {
        match dir {
            ListEditDir::Left => L::elim_move
                (st, zip.left, zip.right,
                 |st,right|         (ListZipper{left:L::nil(st), right:right, phantom:PhantomData}, None),
                 |st,x,left,right| {let x2 = x.clone();
                                    (ListZipper{left:L::cons(st,x,left), right:right, phantom:PhantomData}, Some(x2))},
                 |st,nm,left,right|{let zip = ListZipper{left:left,right:L::name(st,nm,right), phantom:PhantomData};
                                    Self::observe (st, zip, ListEditDir::Left)}
                 ),
            ListEditDir::Right => L::elim_move
                (st, zip.right, zip.left,
                 |st,left|         (ListZipper{left:left, right:L::nil(st), phantom:PhantomData}, None),
                 |st,x,right,left| {let x2 = x.clone();
                                    (ListZipper{left:left, right:L::cons(st,x,right), phantom:PhantomData}, Some(x2))},
                 |st,nm,right,left|{let zip = ListZipper{left:L::name(st,nm,left),right:right, phantom:PhantomData};
                                    Self::observe (st, zip, ListEditDir::Right)}
                 ),
        }
    }

    fn replace (st:&mut A, zip:Self::State, dir:Self::Dir, y:X) -> (Self::State, X, bool) {
        match dir {
            ListEditDir::Left => L::elim_move
                (st, zip.left, (zip.right, y),
                 |st,(right,y)|        (ListZipper{left:L::nil(st),         right:right, phantom:PhantomData}, y, false),
                 |st,x,left,(right,y)| (ListZipper{left:L::cons(st,y,left), right:right, phantom:PhantomData}, x, true ),
                 |st,nm,left,(right,y)|{let zip = ListZipper{left:left,right:L::name(st,nm,right),phantom:PhantomData};
                                        Self::replace (st, zip, ListEditDir::Left, y)}
                 ),
            ListEditDir::Right => L::elim_move
                (st, zip.right, (zip.left,y),
                 |st,(left,y)|         (ListZipper{left:left, right:L::nil(st), phantom:PhantomData},          y, false),
                 |st,x,right,(left,y)| (ListZipper{left:left, right:L::cons(st,y,right), phantom:PhantomData}, x, true ),
                 |st,nm,right,(left,y)|{let zip = ListZipper{left:L::name(st,nm,left),right:right, phantom:PhantomData};
                                        Self::replace (st, zip, ListEditDir::Right, y)}
                 ),
        }
    }

    fn get_list<N:ListT<A,X>,T:TreeT<A,X,()>>
        (st:&mut A, zip:Self::State, dir:Self::Dir) -> N::List
    {
        let tree = Self::get_tree::<T>(st, zip, dir);
        list_of_tree::<A,X,N,T>(st, &tree)
    }

    /// Creates a tree whose leaves hold the contents of the zipper, in order.
    /// When `dir=Left`,  the tree's leaves are ordered from left-to-right, i.e., as (rev left) @ right.
    /// When `dir=Right`, the tree's leaves are ordered from right-to-left, i.e., as (rev right) @ left.
    fn get_tree<T:TreeT<A,X,()>>
        (st:&mut A, zip:Self::State, dir:Self::Dir) -> T::Tree
    {
        let nil_tree = T::nil(st);
        let nil_list = M::nil(st);
        match dir {
            ListEditDir::Left => {
                let next = M::cons(st, (ListEditDir::Right, zip.right), nil_list);
                let (_, tree, _, _, _) =
                    tree_of_lists::<A,X,T,L,M> (st, ListEditDir::Left, zip.left,
                                                ListEditDir::Left, nil_tree, 0 as u32, u32::max_value(), next);
                tree
            },
            ListEditDir::Right => {
                let next = M::cons(st, (ListEditDir::Right, zip.left), nil_list);
                let (_, tree, _, _, _) =
                    tree_of_lists::<A,X,T,L,M> (st, ListEditDir::Right, zip.right,
                                                ListEditDir::Left, nil_tree, 0 as u32, u32::max_value(), next);
                tree
            }
        }
    }
}

pub fn tree_of_2lists <A:Adapton, X:Hash+Clone, T:TreeT<A,X,()>, L:ListT<A,X>>
    (st:&mut A,
     dir1:ListEditDir,list1:L::List,
     dir2:ListEditDir,list2:L::List) -> T::Tree
{
    let nil = T::nil(st) ;
    let (tree, rest, next) = 
        match dir1 {
            ListEditDir::Left =>
                tree_of_list_lr::<A,X,T,L>
                (st, list1, Some((dir2,list2)), nil, 0 as u32, u32::max_value()),
            ListEditDir::Right =>
                tree_of_list_rl::<A,X,T,L>
                (st, list1, Some((dir2,list2)), nil, 0 as u32, u32::max_value()),
        };
    assert!( L::is_empty( st, &rest ) );
    assert!( next == None );
    tree
}


pub trait ListT<A:Adapton,Hd> : Debug+Clone {
    type List : Debug+Hash+PartialEq+Eq+Clone ;
    
    fn nil  (&mut A) -> Self::List ;
    fn cons (&mut A, Hd, Self::List) -> Self::List ;
    
    // requisite "adaptonic" constructors: `name` and `art`:
    fn name (&mut A, A::Name, Self::List) -> Self::List ;
    fn art  (&mut A, Art<Self::List,A::Loc>) -> Self::List ;
    fn rc   (&mut A, Rc<Self::List>) -> Self::List ;

    fn elim<Res,Nil,Cons,Name> (&mut A, &Self::List, Nil, Cons, Name) -> Res
        where Nil:FnOnce(&mut A) -> Res
        ,    Cons:FnOnce(&mut A, &Hd, &Self::List) -> Res
        ,    Name:FnOnce(&mut A, &A::Name, &Self::List) -> Res ;

    fn elim_with<Arg,Res,Nil,Cons,Name> (&mut A, &Self::List, Arg, Nil, Cons, Name) -> Res
        where Nil:FnOnce(&mut A, Arg) -> Res
        ,    Cons:FnOnce(&mut A, &Hd, &Self::List, Arg) -> Res
        ,    Name:FnOnce(&mut A, &A::Name, &Self::List, Arg) -> Res ;
    
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
    fn is_empty (st:&mut A, list:&Self::List) -> bool {
        Self::elim(st, &list,
                   |_|       true,
                   |_,_,_|   false,
                   |st,_,tl| Self::is_empty(st,tl))
    }
}

pub trait TreeT<A:Adapton,Leaf,Bin:Hash> {
    type Tree : Debug+Hash+PartialEq+Eq+Clone ;

    fn nil  (&mut A) -> Self::Tree ;
    fn leaf (&mut A, Leaf) -> Self::Tree ;
    fn bin  (&mut A, Bin, Self::Tree, Self::Tree) -> Self::Tree ;

    // requisite "adaptonic" constructors: `name` and `art`:
    fn name (&mut A, A::Name, Self::Tree, Self::Tree) -> Self::Tree ;
    fn art  (&mut A, Art<Self::Tree,A::Loc>) -> Self::Tree ;
    fn rc   (&mut A, Rc<Self::Tree>) -> Self::Tree ;

    // fn elim_move<Arg,Res,NilC,LeafC,BinC,NameC>
    //     (&mut A, Self::Tree, Arg, NilC, LeafC, BinC, NameC) -> Res
    //     where NilC  : FnOnce(&mut A, Arg) -> Res
    //     ,     LeafC : FnOnce(&mut A, Leaf, Arg) -> Res
    //     ,     BinC  : FnOnce(&mut A, Bin,  Self::Tree, Self::Tree, Arg) -> Res
    //     ,     NameC : FnOnce(&mut A, A::Name, Self::Tree, Self::Tree, Arg) -> Res
    //     ;

    fn elim_with<Arg,Res,NilC,LeafC,BinC,NameC>
        (&mut A, &Self::Tree, Arg, NilC, LeafC, BinC, NameC) -> Res
        where NilC  : FnOnce(&mut A, Arg) -> Res
        ,     LeafC : FnOnce(&mut A, &Leaf, Arg) -> Res
        ,     BinC  : FnOnce(&mut A, &Bin,  &Self::Tree, &Self::Tree, Arg) -> Res
        ,     NameC : FnOnce(&mut A, &A::Name, &Self::Tree, &Self::Tree, Arg) -> Res
        ;

    fn elim<Res,NilC,LeafC,BinC,NameC>
        (&mut A, &Self::Tree, NilC, LeafC, BinC, NameC) -> Res
        where NilC  : FnOnce(&mut A) -> Res
        ,     LeafC : FnOnce(&mut A, &Leaf) -> Res
        ,     BinC  : FnOnce(&mut A, &Bin,  &Self::Tree, &Self::Tree) -> Res
        ,     NameC : FnOnce(&mut A, &A::Name, &Self::Tree, &Self::Tree) -> Res
        ;
    
    fn fold_lr<Res:Hash+Debug+Eq+Clone,LeafC,BinC,NameC>
        (st:&mut A, tree:&Self::Tree, res:Res, leaf:&LeafC, bin:&BinC, name:&NameC) -> Res
        where LeafC:Fn(&mut A, &Leaf,    Res ) -> Res
        ,      BinC:Fn(&mut A, &Bin,     Res ) -> Res 
        ,     NameC:Fn(&mut A, &A::Name, Res ) -> Res 
    {
        Self::elim_with
            (st, tree, res,
             |_,res|    res,
             |st,x,res| leaf(st, x, res),
             |st,x,l,r,res| {
                 let res = Self::fold_lr(st, l, res, leaf, bin, name);
                 let res = bin(st, x, res);
                 let res = Self::fold_lr(st, r, res, leaf, bin, name);
                 res
             },
             |st,n,l,r,res| {
                 let (n1,n2,n3) = st.name_fork3(n.clone());
                 let res = memo!(st, n1 =>> Self::fold_lr, tree:l, res:res ;; leaf:leaf, bin:bin, name:name);
                 let res = name(st, &n2, res);
                 let res = memo!(st, n3 =>> Self::fold_lr, tree:r, res:res ;; leaf:leaf, bin:bin, name:name);
                 res
             }
             )
    }

    fn fold_rl<Res:Hash+Debug+Eq+Clone,LeafC,BinC,NameC>
        (st:&mut A, tree:&Self::Tree, res:Res, leaf:&LeafC, bin:&BinC, name:&NameC) -> Res
        where LeafC:Fn(&mut A, &Leaf,    Res ) -> Res
        ,      BinC:Fn(&mut A, &Bin,     Res ) -> Res
        ,     NameC:Fn(&mut A, &A::Name, Res ) -> Res
    {
        Self::elim_with
            (st, tree, res,
             |_,res| res,
             |st,x,res| leaf(st, x, res),
             |st,x,l,r,res| {
                 let res = Self::fold_rl(st, r, res, leaf, bin, name);
                 let res = bin(st, x, res);
                 let res = Self::fold_rl(st, l, res, leaf, bin, name);
                 res
             },
             |st,n,l,r,res| {
                 let (n1,n2,n3) = st.name_fork3(n.clone());
                 let res = memo!(st, n1 =>> Self::fold_rl, tree:r, res:res ;; leaf:leaf, bin:bin, name:name);
                 let res = name(st, &n2, res);
                 let res = memo!(st, n3 =>> Self::fold_rl, tree:l, res:res ;; leaf:leaf, bin:bin, name:name);
                 res
             }
             )
    }

    fn fold_up<Res:Hash+Debug+Eq+Clone,NilC,LeafC,BinC,NameC>
        (st:&mut A, tree:&Self::Tree, nil:&NilC, leaf:&LeafC, bin:&BinC, name:&NameC) -> Res
        where  NilC:Fn(&mut A) -> Res
        ,     LeafC:Fn(&mut A, &Leaf              ) -> Res
        ,      BinC:Fn(&mut A, &Bin,     Res, Res ) -> Res
        ,     NameC:Fn(&mut A, &A::Name, Res, Res ) -> Res
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
             |st,n,l,r| {
                 let (n1,n2,n3) = st.name_fork3(n.clone());
                 let resl = memo!(st, n1 =>> Self::fold_up, tree:l ;; nil:nil, leaf:leaf, bin:bin, name:name);
                 let resr = memo!(st, n2 =>> Self::fold_up, tree:r ;; nil:nil, leaf:leaf, bin:bin, name:name);
                 let res = name(st, &n3, resl, resr);
                 res
             }
             )
    }
}

pub fn tree_reduce_monoid<A:Adapton,Elm:Eq+Hash+Clone+Debug,T:TreeT<A,Elm,()>,BinOp>
    (st:&mut A, tree:&T::Tree, zero:&Elm, binop:&BinOp) -> Elm
    where BinOp:Fn(&mut A, Elm, Elm) -> Elm
{
    T::fold_up(st, tree,
               &|_|        zero.clone(),
               &|_,leaf|   leaf.clone(),
               &|st,_,l,r| binop(st,l,r),
               &|st,_,l,r| binop(st,l,r),
               )
}

pub fn list_reduce_monoid<A:Adapton,Elm:Eq+Hash+Clone+Debug,L:ListT<A,Elm>,BinOp,T:TreeT<A,Elm,()>>
    (st:&mut A, list:&L::List, zero:&Elm, binop:&BinOp) -> Elm
    where BinOp:Fn(&mut A, Elm, Elm) -> Elm
{
    let tree = tree_of_list::<A,Elm,T,L>(st, list);
    tree_reduce_monoid::<A,Elm,T,BinOp>(st, &tree, zero, binop)
}

pub fn tree_map<A:Adapton,X:Hash+Clone,Y:Hash+Clone,T:TreeT<A,X,Y>,FX:Hash+Clone,GY:Hash+Clone,FGT:TreeT<A,FX,GY>,F,G>
    (st:&mut A, tree:&T::Tree, f:&F, g:&G) -> FGT::Tree
    where F:Fn(&mut A, &X) -> FX
    ,     G:Fn(&mut A, &Y) -> GY
{
    T::fold_up(st, tree,
               &|st|       FGT::nil(st),
               &|st,x|     {let fx = f(st,x);  FGT::leaf(st, fx)},
               &|st,y,l,r| {let gy = g(st, y); FGT::bin(st, gy, l, r)},
               &|st,n,l,r| FGT::name(st, n.clone(), l, r)
               )
}

pub fn tree_filter<A:Adapton,X:Hash+Clone,T:TreeT<A,X,()>,F>
    (st:&mut A, tree:&T::Tree, f:&F) -> T::Tree
    where F:Fn(&mut A, &X) -> bool
{
    T::fold_up(st, tree,
               &|st|       T::nil(st),
               &|st,x|     { let fx = f(st,x);
                             if fx { T::leaf(st, x.clone()) }
                             else  { T::nil(st) } },
               &|st,_,l,r| T::bin(st, (), l, r),
               &|st,n,l,r| T::name(st, n.clone(), l, r)
               )
}

pub fn list_of_tree<A:Adapton,X:Hash+Clone,L:ListT<A,X>,T:TreeT<A,X,()>>
    (st:&mut A, tree:&T::Tree) -> L::List
{
    let nil = L::nil(st);
    T::fold_rl(st, tree, nil,
               &|st,x,xs| L::cons(st,x.clone(),xs),
               &|_,_,xs|  xs,
               &|st,n,xs| L::name(st,n.clone(),xs)
               )
}

pub fn rev_list_of_tree<A:Adapton,X:Hash+Clone,L:ListT<A,X>,T:TreeT<A,X,()>>
    (st:&mut A, tree:&T::Tree) -> L::List
{
    let nil = L::nil(st);
    T::fold_lr(st, tree, nil,
               &|st,x,xs| L::cons(st,x.clone(),xs),
               &|_,_,xs|  xs,
               &|st,n,xs| L::name(st,n.clone(),xs)
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
impl<A:Adapton+Debug+Hash+PartialEq+Eq+Clone,Hd:Debug+Hash+PartialEq+Eq+Clone> ListT<A,Hd> for List<A,Hd> {
    type List = List<A,Hd>;

    fn nil  (_:&mut A)                             -> Self::List { List::Nil }
    fn cons (_:&mut A, hd:Hd, tl:Self::List)       -> Self::List { List::Cons(hd,Box::new(tl)) }
    fn name (_:&mut A, nm:A::Name, tl:Self::List)  -> Self::List { List::Name(nm, Box::new(tl)) }
    fn rc   (_:&mut A, rc:Rc<List<A,Hd>>)          -> Self::List { List::Rc(rc) }
    fn art  (_:&mut A, art:Art<List<A,Hd>,A::Loc>) -> Self::List { List::Art(art) }

    fn elim<Res,Nil,Cons,Name>
        (st:&mut A, list:&Self::List, nilf:Nil, consf:Cons, namef:Name) -> Res
        where Nil:FnOnce(&mut A) -> Res
        ,    Cons:FnOnce(&mut A, &Hd, &Self::List) -> Res
        ,    Name:FnOnce(&mut A, &A::Name, &Self::List) -> Res
    {
        match *list {
            List::Nil => nilf(st),
            List::Cons(ref hd, ref tl) => consf(st, hd, &*tl),
            List::Name(ref nm, ref tl) => namef(st, nm, &*tl),
            List::Rc(ref rc) => Self::elim(st, &*rc, nilf, consf, namef),
            List::Art(ref art) => {
                let list = st.force(art);
                Self::elim(st, &list, nilf, consf, namef)
            }
        }
    }

    fn elim_with<Arg,Res,Nil,Cons,Name>
        (st:&mut A, list:&Self::List, arg:Arg, nilf:Nil, consf:Cons, namef:Name) -> Res
        where Nil:FnOnce(&mut A, Arg) -> Res
        ,    Cons:FnOnce(&mut A, &Hd, &Self::List, Arg) -> Res
        ,    Name:FnOnce(&mut A, &A::Name, &Self::List, Arg) -> Res
    {
        match *list {
            List::Nil => nilf(st, arg),
            List::Cons(ref hd, ref tl) => consf(st, hd, &*tl, arg),
            List::Name(ref nm, ref tl) => namef(st, nm, &*tl, arg),
            List::Rc(ref rc) => Self::elim_with(st, &*rc, arg, nilf, consf, namef),
            List::Art(ref art) => {
                let list = st.force(art);
                Self::elim_with(st, &list, arg, nilf, consf, namef)
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
pub enum Tree<A:Adapton,X,Y> {
    Nil,
    Leaf(X),
    Bin(Y,        Box<Tree<A,X,Y>>, Box<Tree<A,X,Y>> ),
    Name(A::Name, Box<Tree<A,X,Y>>, Box<Tree<A,X,Y>> ),
    Rc(           Rc<Tree<A,X,Y>>),
    Art(          Art<Tree<A,X,Y>, A::Loc>),
}

// TODO: Why Does Adapton have to implement all of these?
//       It's not actually *contained* within the List structure; it cannot be ecountered there.
//       It's only ever present in a negative position (as a function parameter).
impl<A:Adapton+Debug+Hash+PartialEq+Eq+Clone,Leaf:Debug+Hash+PartialEq+Eq+Clone,Bin:Debug+Hash+PartialEq+Eq+Clone> TreeT<A,Leaf,Bin> for Tree<A,Leaf,Bin> {
    type Tree = Tree<A,Leaf,Bin>;

    fn nil  (_:&mut A)                                        -> Self::Tree { Tree::Nil }
    fn leaf (_:&mut A, x:Leaf)                                -> Self::Tree { Tree::Leaf(x) }
    fn bin  (_:&mut A, y:Bin, l:Self::Tree, r:Self::Tree)     -> Self::Tree { Tree::Bin(y,Box::new(l),Box::new(r)) }
    fn name (_:&mut A, nm:A::Name, l:Self::Tree,r:Self::Tree) -> Self::Tree { Tree::Name(nm, Box::new(l),Box::new(r)) }
    fn rc   (_:&mut A, rc:Rc<Self::Tree>)                     -> Self::Tree { Tree::Rc(rc) }
    fn art  (_:&mut A, art:Art<Self::Tree,A::Loc>)            -> Self::Tree { Tree::Art(art) }

    fn elim_with<Arg,Res,NilC,LeafC,BinC,NameC>
        (st:&mut A, tree:&Self::Tree, arg:Arg, nil:NilC, leaf:LeafC, bin:BinC, name:NameC) -> Res
        where NilC  : FnOnce(&mut A, Arg) -> Res
        ,     LeafC : FnOnce(&mut A, &Leaf, Arg) -> Res
        ,     BinC  : FnOnce(&mut A, &Bin,  &Self::Tree, &Self::Tree, Arg) -> Res
        ,     NameC : FnOnce(&mut A, &A::Name, &Self::Tree, &Self::Tree, Arg) -> Res
    {
        match *tree {
            Tree::Nil => nil(st,arg),
            Tree::Leaf(ref x) => leaf(st, x, arg),
            Tree::Bin(ref b, ref l, ref r) => bin(st, b, &*l, &*r, arg),
            Tree::Name(ref nm, ref l, ref r) => name(st, nm, &*l, &*r, arg),
            Tree::Rc(ref rc) => Self::elim_with(st, &*rc, arg, nil, leaf, bin, name),
            Tree::Art(ref art) => {
                let list = st.force(art);
                Self::elim_with(st, &list, arg, nil, leaf, bin, name)
            }
        }
    }

    fn elim<Res,NilC,LeafC,BinC,NameC>
        (st:&mut A, tree:&Self::Tree, nil:NilC, leaf:LeafC, bin:BinC, name:NameC) -> Res
        where NilC  : FnOnce(&mut A) -> Res
        ,     LeafC : FnOnce(&mut A, &Leaf) -> Res
        ,     BinC  : FnOnce(&mut A, &Bin,  &Self::Tree, &Self::Tree) -> Res
        ,     NameC : FnOnce(&mut A, &A::Name, &Self::Tree, &Self::Tree) -> Res
    {
        match *tree {
            Tree::Nil => nil(st),
            Tree::Leaf(ref x) => leaf(st, x),
            Tree::Bin(ref b, ref l, ref r) => bin(st, b, &*l, &*r),
            Tree::Name(ref nm, ref l, ref r) => name(st, nm, &*l, &*r),
            Tree::Rc(ref rc) => Self::elim(st, &*rc, nil, leaf, bin, name),
            Tree::Art(ref art) => {
                let list = st.force(art);
                Self::elim(st, &list, nil, leaf, bin, name)
            }
        }
    }
}


pub fn tree_of_lists
    < A:Adapton
    , X:Hash+Clone
    , T:TreeT<A,X,()>
    , L:ListT<A,X>
    , M:ListT<A,(ListEditDir,L::List)>
    >
    (st:&mut A,
     dir_list:ListEditDir, list:L::List,
     dir_tree:ListEditDir, tree:T::Tree, tree_lev:u32, parent_lev:u32,
     next:M::List)
     -> (ListEditDir, T::Tree,
         ListEditDir, L::List,
         /*  Left, */ M::List)
{
    L::elim_move (
        st, list, (dir_list, dir_tree, tree, next),

        /* Nil */
        |st, (dir_list, dir_tree, tree, next)| M::elim_move (
            st, next, (dir_list, dir_tree, tree),

            /* Nil */ |st, (dir_list, dir_tree, tree)| {
                let nil1 = L::nil(st);
                let nil2 = M::nil(st);
                (dir_tree, tree, dir_list, nil1, nil2)
            },            
            /* Cons */ |st, (dir_list,list), next, (_dir_list, _dir_tree, tree)|{
                /* ignore _dir_list: That directionality is now "shadowed" by dir_list. */
                /* ignore _dir_tree: We always append inner lists in left-to-right order. */
                tree_of_lists::<A,X,T,L,M>(st, dir_list, list,
                                           ListEditDir::Left, tree,
                                           tree_lev, parent_lev, next)
            },
            /* Name */ |st, nm, next, (dir_list, dir_tree, tree)| {
                let nil1 = L::nil(st);
                memo!(st, nm =>> tree_of_lists::<A,X,T,L,M>,
                      dir_list:dir_list, list:nil1,
                      dir_tree:dir_tree, tree:tree, tree_lev:tree_lev, parent_lev:parent_lev,
                      next:next)
            }),
            
        /* Cons */
        |st, hd, rest, (dir_list, dir_tree, tree, next)| {
            let lev_hd = (1 + (my_hash(&hd).leading_zeros())) as u32 ;
            if tree_lev <= lev_hd && lev_hd <= parent_lev {
                let leaf = T::leaf(st, hd) ;
                let (_dir_tree2, tree2, dir_list, rest, next) =
                    tree_of_lists::<A,X,T,L,M> ( st, dir_list.clone(), rest, dir_list.clone(), leaf, 0 as u32, lev_hd, next ) ;
                let tree3 = match dir_tree.clone() {
                    ListEditDir::Left  => T::bin ( st, (), tree,  tree2 ),
                    ListEditDir::Right => T::bin ( st, (), tree2, tree  ),
                } ;
                tree_of_lists::<A,X,T,L,M> ( st, dir_list, rest, dir_tree, tree3, lev_hd, parent_lev, next )
            }
            else {
                (dir_tree, tree, dir_list, L::cons(st,hd,rest), next)
            }},

        /* Name */
        |st, nm, rest, (dir_list, dir_tree, tree, next)|{
            let lev_nm = (1 + (my_hash(&nm).leading_zeros())) as u32 ;
            if tree_lev <= lev_nm && lev_nm <= parent_lev {
                let nil = T::nil(st) ;
                let (nm1, nm2) = st.name_fork(nm.clone());
                let (_dir_tree2, tree2, dir_list, rest, next) =
                    memo!(st, nm1 =>> tree_of_lists::<A,X,T,L,M>,
                          dir_list:dir_list.clone(), list:rest,
                          dir_tree:dir_list.clone(), tree:nil,
                          tree_lev:0 as u32, parent_lev:lev_nm,
                          next:next ) ;
                let tree3 = match dir_tree.clone() {
                    ListEditDir::Left  => T::name ( st, nm, tree,  tree2 ),
                    ListEditDir::Right => T::name ( st, nm, tree2, tree  ),
                } ;
                memo!(st, nm2 =>> tree_of_lists::<A,X,T,L,M>,
                      dir_list:dir_list, list:rest,
                      dir_tree:dir_tree, tree:tree3,
                      tree_lev:lev_nm, parent_lev:parent_lev,
                      next:next )
            }
            else {
                (dir_tree, tree, dir_list, L::name(st,nm,rest), next)
            }},            
        )
}

pub fn tree_of_list_dir
    <A:Adapton, X:Hash+Clone, T:TreeT<A,X,()>, L:ListT<A,X>>
    (st:&mut A,
     next:Option<(ListEditDir,L::List)>,
     tree:T::Tree, tree_lev:u32, parent_lev:u32)
     -> (T::Tree, L::List, Option<(ListEditDir,L::List)>)
{
    match next {
        None => (tree, L::nil(st), None),
        Some((ListEditDir::Left,rest)) =>
            tree_of_list_lr::<A,X,T,L>
            (st, rest, None, tree, tree_lev, parent_lev)
            ,
        Some((ListEditDir::Right,rest)) =>
            tree_of_list_rl::<A,X,T,L>
            (st, rest, None, tree, tree_lev, parent_lev)
    }
}

pub fn tree_of_list_lr <A:Adapton, X:Hash+Clone, T:TreeT<A,X,()>, L:ListT<A,X>>
    (st:&mut A, list:L::List,
     next:Option<(ListEditDir,L::List)>,
     left_tree:T::Tree, left_tree_lev:u32, parent_lev:u32)
     -> (T::Tree, L::List, Option<(ListEditDir,L::List)>)
{
    L::elim_move (
        st, list, (left_tree, next),

        /* Nil */  |st, (left_tree, next)|
        tree_of_list_dir::<A,X,T,L>(st, next, left_tree, left_tree_lev, parent_lev),
        
        /* Cons */ |st, hd, rest, (left_tree, next)| {
            let lev_hd = (1 + (my_hash(&hd).leading_zeros())) as u32 ;
            if left_tree_lev <= lev_hd && lev_hd <= parent_lev {
                let leaf = T::leaf(st, hd) ;
                let (right_tree, rest, next) =
                    tree_of_list_lr::<A,X,T,L> ( st, rest, next, leaf, 0 as u32, lev_hd ) ;
                let tree = T::bin ( st, (), left_tree, right_tree ) ;
                tree_of_list_lr::<A,X,T,L> ( st, rest, next, tree, lev_hd, parent_lev )
            }
            else {
                (left_tree, L::cons(st,hd,rest), next)
            }},

        /* Name */ |st, nm, rest, (left_tree, next)| {
            let lev_nm = (1 + 64 + (my_hash(&nm).leading_zeros())) as u32 ;
            if left_tree_lev <= lev_nm && lev_nm <= parent_lev {
                let nil = T::nil(st) ;
                let (right_tree, rest, next) =
                    memo!(st, tree_of_list_lr::<A,X,T,L>,
                          list:rest, next:next,
                          left_tree:nil,
                          left_tree_lev:0 as u32,
                          parent_lev:lev_nm ) ;
                let tree = T::name( st, nm, left_tree, right_tree ) ;
                memo!(st, tree_of_list_lr::<A,X,T,L>,
                      list:rest, next:next,
                      left_tree:tree,
                      left_tree_lev:lev_nm,
                      parent_lev:parent_lev )
            }
            else {
                (left_tree, L::name(st,nm,rest), next)
            }}
        )
}

pub fn tree_of_list_rl <A:Adapton, X:Hash+Clone, T:TreeT<A,X,()>, L:ListT<A,X>>
    (st:&mut A, list:L::List,
     next:Option<(ListEditDir,L::List)>,
     right_tree:T::Tree, right_tree_lev:u32, parent_lev:u32)
     -> (T::Tree, L::List, Option<(ListEditDir,L::List)>)
{
    L::elim_move (
        st, list, (right_tree, next),
        
        /* Nil */  |st, (right_tree, next)|
        tree_of_list_dir::<A,X,T,L>(st, next, right_tree, right_tree_lev, parent_lev),
        
        /* Cons */ |st, hd, rest, (right_tree, next)| {
            let lev_hd = (1 + (my_hash(&hd).leading_zeros())) as u32 ;
            if right_tree_lev <= lev_hd && lev_hd <= parent_lev {
                let leaf = T::leaf(st, hd) ;
                let (left_tree, rest, next) =
                    tree_of_list_rl::<A,X,T,L> ( st, rest, next, leaf, 0 as u32, lev_hd ) ;
                let tree = T::bin ( st, (), left_tree, right_tree ) ;
                tree_of_list_rl::<A,X,T,L> ( st, rest, next, tree, lev_hd, parent_lev )
            }
            else {
                (right_tree, L::cons(st,hd,rest), next)
            }},

        /* Name */ |st, nm, rest, (right_tree, next)| {
            let lev_nm = (1 + 64 + (my_hash(&nm).leading_zeros())) as u32 ;
            if right_tree_lev <= lev_nm && lev_nm <= parent_lev {
                let nil = T::nil(st) ;
                let (left_tree, rest, next) =
                    memo!(st, tree_of_list_lr::<A,X,T,L>,
                          list:rest, next:next,
                          right_tree:nil,
                          right_tree_lev:0 as u32,
                          parent_lev:lev_nm ) ;
                let tree = T::name( st, nm, left_tree, right_tree ) ;
                memo!(st, tree_of_list_lr::<A,X,T,L>,
                      list:rest, next:next,
                      right_tree:tree,
                      right_tree_lev:lev_nm,
                      parent_lev:parent_lev )
            }
            else {
                (right_tree, L::name(st,nm,rest), next)
            }}
        )
}


fn tree_of_list_rec <A:Adapton, X:Hash+Clone, T:TreeT<A,X,()>, L:ListT<A,X>>
    (st:&mut A, list:&L::List, left_tree:T::Tree, left_tree_lev:u32, parent_lev:u32)
     -> (T::Tree, L::List)
{
    L::elim_with (
        st, &list, left_tree,
        /* Nil */  |st, left_tree| ( left_tree, L::nil(st) ),
        /* Cons */ |st, hd, rest, left_tree| {
            let lev_hd = (1 + (my_hash(hd).leading_zeros())) as u32 ;
            if left_tree_lev <= lev_hd && lev_hd <= parent_lev {
                let leaf = T::leaf(st, hd.clone()) ;
                let (right_tree, rest) =
                    tree_of_list_rec::<A,X,T,L> ( st, rest, leaf, 0 as u32, lev_hd ) ;
                let tree = T::bin ( st, (), left_tree, right_tree ) ;
                tree_of_list_rec::<A,X,T,L> ( st, &rest, tree, lev_hd, parent_lev )
            }
            else {
                (left_tree, list.clone())
            }},
        /* Name */ |st, nm, rest, left_tree| {
            let lev_nm = (1 + 64 + (my_hash(nm).leading_zeros())) as u32 ;
            if left_tree_lev <= lev_nm && lev_nm <= parent_lev {
                let nil = T::nil(st) ;
                let (right_tree, rest) =
                    memo!(st, tree_of_list_rec::<A,X,T,L>,
                          list:rest, left_tree:nil, left_tree_lev:0 as u32, parent_lev:lev_nm ) ;
                let tree = T::name( st, nm.clone(), left_tree, right_tree ) ;
                memo!(st, tree_of_list_rec::<A,X,T,L>,
                      list:&rest, left_tree:tree,
                      left_tree_lev:lev_nm, parent_lev:parent_lev )
            }
            else {
                (left_tree, list.clone())
            }}
        )
}

pub fn tree_of_list <A:Adapton, X:Hash+Clone, T:TreeT<A,X,()>, L:ListT<A,X>>
    (st:&mut A, list:&L::List) -> T::Tree
{
    let nil = T::nil(st) ;
    let (tree, rest) = tree_of_list_rec::<A,X,T,L> (st, list, nil, 0 as u32, u32::max_value()) ;
    assert!( L::is_empty( st, &rest ) );
    tree
}

pub fn list_merge<A:Adapton,X:Ord+Clone,L:ListT<A,X>>
    (st:&mut A,
     n1:Option<&A::Name>, l1:&L::List,
     n2:Option<&A::Name>, l2:&L::List  ) -> L::List
{
    L::elim
        (st, l1,
         |_| l2.clone(),
         |st,h1,t1| L::elim
         (st, l2,
          |st| L::nil(st),
          |st, h2, t2|
          if h1 <= h2 {
              match n1 {
                  None => {
                      let rest = list_merge::<A,X,L>(st, None, t1, n2, l2);
                      L::cons(st, (*h1).clone(), rest)
                  }
                  Some(n1) => {
                      let (n1a, n1b) = st.name_fork(n1.clone());
                      let rest = thunk!(st, n1a =>>
                                        list_merge::<A,X,L>,
                                        n1:None, l1:t1, n2:n2, l2:l2);
                      let rest = L::art(st, rest);
                      let rest = L::cons(st, (*h1).clone(), rest);
                      L::name(st, n1b, rest)
                  }
              }
          }
          else {
              match n2 {
                  None => {
                      let rest = list_merge::<A,X,L>(st, n1, l1, None, t2);
                      L::cons(st, (*h2).clone(), rest)
                  }
                  Some(n2) => {
                      let (n2a, n2b) = st.name_fork(n2.clone());
                      let rest = thunk!(st, n2a =>>
                                        list_merge::<A,X,L>,
                                        n1:n1, l1:l1, n2:None, l2:t2);
                      let rest = L::art(st, rest);
                      let rest = L::cons(st, (*h2).clone(), rest);
                      L::name(st, n2b, rest)
                  }
              }
          },
          |st,m2,t2| list_merge::<A,X,L>(st, n1, l1, Some(m2), t2)
          ),
         |st,n1,t1| list_merge::<A,X,L>(st, Some(n1), t1, n2, l2)
         )
}

pub fn list_merge_sort<A:Adapton,X:Ord+Hash+Clone,L:ListT<A,X>,T:TreeT<A,X,()>>
    (st:&mut A, list:&L::List) -> L::List
{
    let tree = tree_of_list::<A,X,T,L>(st, list);
    T::fold_up (st, &tree,
                &|st|                 L::nil(st),
                &|st, x|              L::singleton(st, x.clone()),
                &|st, _, left, right| list_merge::<A,X,L>(st, None, &left, None, &right),
                &|st, n, left, right| { let (n1,n2) = st.name_fork(n.clone());
                                        list_merge::<A,X,L>(st, Some(&n1), &left, Some(&n2), &right) },
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
        (st:&mut A, set:&Self::Set, elm:&Elm,
         update:&Update)
         -> (Self::Set, Res)
        where Update:FnOnce(&mut A, Option<Elm>, &Elm) -> (Option<Elm>, Res)
        ;
    
    // Elim forms:
    fn elim_with<Arg,Res,NilC,ElmC,UnionC>
        (st:&mut A, set:&Self::Set, arg:Arg,
         nil:&NilC, elm:&ElmC, union:&UnionC)
         -> Res
        where NilC:FnOnce(&mut A, Arg) -> Res
        ,     ElmC:FnOnce(&mut A, &Elm, Arg) -> Res
        ,   UnionC:FnOnce(&mut A, &Self::Set, &Self::Set, Arg) -> Res
        ;

    fn is_mem(st:&mut A, set:&Self::Set, elm:&Elm) -> bool; // TODO: Write based on update, above.
    fn is_empty(st:&mut A, set:&Self::Set) -> bool; // TODO: Write based on elim_with, above.
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
        (st:&mut A, map:&Self::Map,
         pt:&Dom, cod:&Option<Cod>, update:&Update) -> (Self::Map, Res)
        where Update:FnOnce(&mut A, Option<Cod>, Option<Cod>) -> (Option<Cod>, Res)
        ;

    // General elim form:
    fn elim_with<Arg,Res,NilC,ElmC,UnionC>
        (st:&mut A, map:&Self::Map, arg:Arg,
         nil:&NilC, elm:&ElmC, union:&UnionC)
        where NilC:FnOnce(&mut A, Arg) -> Res
        ,     ElmC:FnOnce(&mut A, &Dom, &Option<Cod>) -> Res
        ,   UnionC:FnOnce(&mut A, &Self::Map, &Self::Map, Arg) -> Res
        ;
    
    // Special, convenient update_pts:
    fn rem(st:&mut A, map:&Self::Map, x:&Dom) -> (Option<Cod>, Self::Map); // TODO: Implement with update_pt
    fn get(st:&mut A, set:&Self::Map, x:&Dom) -> Option<Cod>; // TODO: Implement with update_pt
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
    fn add_node(st:&mut A, graph:&Self::Graph, node:&Node) -> Self::Graph;
    fn add_edge(st:&mut A, graph:&Self::Graph, src:&Node, tgt:&Node) -> Self::Graph;
    fn add_succs(st: &mut A, graph:&Self::Graph, node:&Node, succs:&NodeSet) -> Self::Graph;

    // Query forms:
    fn get_succs(st: &mut A, graph:&Self::Graph, node:&Node) -> NodeSet;
    
    // Other forms:
    // fn rem_node(st:&mut A, graph:&Self::Graph, node:&NodeLab) -> Self::Graph;
    // fn rem_edge(st:&mut A, graph:&Self::Graph, edge:&Self::Edge) -> Self::Graph;    
}
