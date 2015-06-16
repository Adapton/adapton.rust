use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

#[macro_use]
use adapton_syntax::* ;
use adapton_sigs::* ;

pub trait ListT<A:Adapton> {
    type List ;
    type Hd ;
    
    fn nil  (&mut A) -> Self::List ;
    fn cons (&mut A, Self::Hd, Rc<Self::List>) -> Self::List ;
    
    // requisite "adaptonic" constructors: `name` and `art`:
    fn name (&mut A, A::Name, Rc<Self::List>) -> Self::List ;
    fn art  (&mut A, Art<Self::List,A::Loc>) -> Self::List ;

    fn elim<Res,Cons,Name> (&mut A, &Self::List, Res, Cons, Name) -> Res
        where Cons:Fn(&mut A, &Self::Hd, &Self::List) -> Res
        ,     Name:Fn(&mut A, &A::Name, &Self::List) -> Res ;
        
    fn fold<Res,Cons> (self:&Self, &mut A, &Self::List, Res, Cons) -> Res
        where Cons:Fn(&mut A, Res, &Self::Hd) -> Res ;

    // TODO: Add derived operations (max, min, sum, etc.)
}

pub trait TreeT<A:Adapton> {
    type Tree ;
    type Leaf ;
    type Bin : Hash ;

    fn nil  (&mut A) -> Self::Tree ;
    fn leaf (&mut A, Self::Leaf) -> Self::Tree ;
    fn bin  (&mut A, Self::Bin, Self::Tree, Self::Tree) -> Self::Tree ;

    // requisite "adaptonic" constructors: `name` and `art`:
    fn name (&mut A, A::Name, Self::Tree, Self::Tree) -> Self::Tree ;
    fn art  (&mut A, Art<Self::Tree,A::Loc>) -> Self::Tree ;
        
    fn fold<Res,Leaf,Bin> (&mut A, Self::Tree, Res, Leaf, Bin) -> Res
        where Leaf:Fn(&mut A, Res, Self::Leaf) -> Res
        ,      Bin:Fn(&mut A, Res, Self::Bin ) -> Res ;

    fn fold2<Arg,Res,Leaf,Bin> (&mut A, Self::Tree, Arg, Leaf, Bin) -> Res
        where Leaf:Fn(&mut A, Arg, Self::Leaf) -> Res
        ,      Bin:Fn(&mut A, Arg, Self::Bin, Res, Res ) -> Res ;
}
// Questions:
//  - Do these Fn's for fold need to be passed in Rc<Box<_>>'s ?
//  - 


#[derive(Debug,PartialEq,Eq,Hash)]
enum List<A:Adapton,Hd> {
    Nil,
    Cons(Hd,Rc<List<A,Hd>>),
    Name(A::Name,Rc<List<A,Hd>>),
    Art(Art<List<A,Hd>, A::Loc>),
}

impl<A:Adapton+Debug+PartialEq+Eq+Hash,Hd:Debug+PartialEq+Eq+Hash> ListT<A> for List<A,Hd> {
    type Hd = Hd;
    type List = List<A,Hd>;

    fn nil  (_:&mut A)                                 -> Self::List { List::Nil }
    fn cons (_:&mut A, hd:Hd, tl:Rc<Self::List>)       -> Self::List { List::Cons(hd,tl) }
    fn name (_:&mut A, nm:A::Name, tl:Rc<Self::List>)  -> Self::List { List::Name(nm, tl) }
    fn art  (_:&mut A, art:Art<List<A,Hd>,A::Loc>)     -> Self::List { List::Art(art) }

    fn elim<Res,Cons,Name> (st:&mut A, list:&Self::List, res:Res, consf:Cons, namef:Name) -> Res
        where Cons:Fn(&mut A, &Self::Hd, &Self::List) -> Res
        ,     Name:Fn(&mut A, &A::Name, &Self::List) -> Res
    {
        panic!("")
    }
    fn fold<Res,Cons> (self:&Self, st:&mut A, list:&Self::List, res:Res, consf:Cons) -> Res
        where Cons:Fn(&mut A, Res, &Self::Hd) -> Res
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

pub fn tree_of_list_rec <'x, A:Adapton, X:Hash, T:TreeT<A>, L:ListT<A>>
    (st:&mut A, list:&L::List, left_tree:T::Tree) -> (T::Tree, L::List)
{
    let nil_tree = T::nil(st) ;
    let nil_list = L::nil(st) ;
    L::elim (
        st, &list,
        /* Nil */  ( nil_tree, nil_list ),
        /* Cons */ |st, hd, rest| { panic!("") },
        /* Name */ |st, nm, rest| {
            if my_hash( nm ) == 0 {
                let (right_tree, rest) = tree_of_list_rec ( st, rest, nil_tree ) ;
                // TODO: Place left_ and right_ trees into articulations, named by name.
                let tree = T::name( st, nm, left_tree, right_tree ) ;
                tree_of_list_rec ( st, rest, tree )
            }
            else {
                let c = L::name(st, nm, rest) ;
                (left_tree, c)
            }
        })
}
