use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

#[macro_use]
use adapton_syntax::* ;
use adapton_sigs::* ;

pub trait ListT<A:Adapton,Hd> {
    type List ;
    
    fn nil  (&mut A) -> Self::List ;
    fn cons (&mut A, Hd, Rc<Self::List>) -> Self::List ;
    
    // requisite "adaptonic" constructors: `name` and `art`:
    fn name (&mut A, A::Name, Rc<Self::List>) -> Self::List ;
    fn art  (&mut A, Art<Self::List,A::Loc>) -> Self::List ;

    fn elim<Res,Nil,Cons,Name> (&mut A, &Self::List, Nil, Cons, Name) -> Res
        where Nil:FnOnce(&mut A) -> Res
        ,    Cons:FnOnce(&mut A, &Hd, &Rc<Self::List>) -> Res
        ,    Name:FnOnce(&mut A, &A::Name, &Rc<Self::List>) -> Res ;
        
    fn fold<Res,Cons> (self:&Self, &mut A, &Self::List, Res, Cons) -> Res
        where Cons:Fn(&mut A, Res, &Hd) -> Res ;

    // TODO: Add derived operations (max, min, sum, etc.)
}

// Questions:
//  - Should `Name`s always be passed by reference?
//  - Do these Fn argss for fold need to be passed in `Rc<Box<_>>`s ?

pub trait TreeT<A:Adapton,Leaf,Bin:Hash> {
    type Tree ;

    fn nil  (&mut A) -> Self::Tree ;
    fn leaf (&mut A, Leaf) -> Self::Tree ;
    fn bin  (&mut A, Bin, Rc<Self::Tree>, Rc<Self::Tree>) -> Self::Tree ;

    // requisite "adaptonic" constructors: `name` and `art`:
    fn name (&mut A, A::Name, Rc<Self::Tree>, Rc<Self::Tree>) -> Self::Tree ;
    fn art  (&mut A, Art<Self::Tree,A::Loc>) -> Self::Tree ;
        
    fn fold<Res,LeafC,BinC> (&mut A, Self::Tree, Res, LeafC, BinC) -> Res
        where LeafC:Fn(&mut A, Res, Leaf) -> Res
        ,      BinC:Fn(&mut A, Res, Bin ) -> Res ;

    fn fold2<Arg,Res,LeafC,BinC> (&mut A, Self::Tree, Arg, LeafC, BinC) -> Res
        where LeafC:Fn(&mut A, Arg, Leaf) -> Res
        ,      BinC:Fn(&mut A, Arg, Bin, Res, Res ) -> Res ;
}


#[derive(Debug,PartialEq,Eq,Hash)]
enum List<A:Adapton,Hd> {
    Nil,
    Cons(Hd,Rc<List<A,Hd>>),
    Name(A::Name,Rc<List<A,Hd>>),
    Art(Art<List<A,Hd>, A::Loc>),
}

impl<A:Adapton+Debug+PartialEq+Eq+Hash,Hd:Debug+PartialEq+Eq+Hash> ListT<A,Hd> for List<A,Hd> {
    type List = List<A,Hd>;

    fn nil  (_:&mut A)                                 -> Self::List { List::Nil }
    fn cons (_:&mut A, hd:Hd, tl:Rc<Self::List>)       -> Self::List { List::Cons(hd,tl) }
    fn name (_:&mut A, nm:A::Name, tl:Rc<Self::List>)  -> Self::List { List::Name(nm, tl) }
    fn art  (_:&mut A, art:Art<List<A,Hd>,A::Loc>)     -> Self::List { List::Art(art) }

    fn elim<Res,Nil,Cons,Name> (st:&mut A, list:&Self::List, nilf:Nil, consf:Cons, namef:Name) -> Res
        where Nil:FnOnce(&mut A) -> Res
        ,    Cons:FnOnce(&mut A, &Hd, &Rc<Self::List>) -> Res
        ,    Name:FnOnce(&mut A, &A::Name, &Rc<Self::List>) -> Res
    {
        panic!("")
    }
    fn fold<Res,Cons> (self:&Self, st:&mut A, list:&Self::List, res:Res, consf:Cons) -> Res
        where Cons:Fn(&mut A, Res, &Hd) -> Res
    {
        match *list {
            List::Nil => res,
            List::Cons(ref hd, ref tl) => {
                let res = consf(st, res, hd) ;
                self.fold(st, &*tl, res, consf)
            },
            List::Name(_, ref tl) => self.fold(st, &*tl, res, consf),
            List::Art(ref art) => {
                let list = st.force(art) ;
                self.fold(st, &*list, res, consf)
            }
        }
    }
}

pub fn tree_of_list_rec <A:Adapton, X:Hash+Clone, T:TreeT<A,X,X>, L:ListT<A,X>>
    (st:&mut A, list:&Rc<L::List>, left_tree:&Rc<T::Tree>)
     -> (Rc<T::Tree>, Rc<L::List>)
{
    L::elim (
        st, &list,
        /* Nil */  |st| ( Rc::new(T::nil(st)), Rc::new(L::nil(st)) ),
        /* Cons */ |st, hd, rest| {
            if my_hash( hd ) == 0 {
                let nil = Rc::new(T::nil(st)) ;
                let (right_tree, rest) = tree_of_list_rec::<A,X,T,L> ( st, rest, &nil ) ;
                let tree = Rc::new(T::bin ( st, hd.clone(), left_tree.clone(), right_tree )) ;
                tree_of_list_rec::<A,X,T,L> ( st, &rest, &tree )
            }
            else {
                let c = Rc::new(L::cons(st, hd.clone(), rest.clone())) ;
                (left_tree.clone(), c)
            }},
        /* Name */ |st, nm, rest| {
            if my_hash( nm ) == 0 {
                let nil = Rc::new(T::nil(st)) ;
                let (right_tree, rest) = tree_of_list_rec::<A,X,T,L> ( st, rest, &nil ) ;
                // TODO: Place left_ and right_ trees into articulations, named by name.
                let tree = Rc::new(T::name( st, nm.clone(), left_tree.clone(), right_tree )) ;
                tree_of_list_rec::<A,X,T,L> ( st, &rest, &tree )
            }
            else {
                let c = Rc::new(L::name(st, nm.clone(), rest.clone())) ;
                (left_tree.clone(), c)
            }}
        )
}
