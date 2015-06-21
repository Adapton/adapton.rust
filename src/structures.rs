use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use std::mem::replace;

use adapton_syntax::* ;
use adapton_sigs::* ;

pub trait ListT<A:Adapton,Hd> : Debug+Hash+PartialEq+Eq+Clone {
    type List : Debug+Hash+PartialEq+Eq+Clone ;
    
    fn nil  (&mut A) -> Self::List ;
    fn cons (&mut A, Hd, Self::List) -> Self::List ;
    fn rc   (&mut A, Rc<Self::List>) -> Self::List ;
    
    // requisite "adaptonic" constructors: `name` and `art`:
    fn name (&mut A, A::Name, Self::List) -> Self::List ;
    fn art  (&mut A, Art<Self::List,A::Loc>) -> Self::List ;

    // ** Want/need to express a negative product here among Nil, Cons and Name!
    // fn elim<Res,Nil,Cons,Name> (self:&Self, &mut A, &Self::List, Nil, Cons, Name) -> Res
    //     where Nil:FnOnce(&mut A) -> Res
    //     ,    Cons:FnOnce(&mut A, &Hd, &Self::List) -> Res
    //     ,    Name:FnOnce(&mut A, &A::Name, &Self::List) -> Res ;

    // Derived from `cons` and `nil` above:
    fn singleton (st:&mut A, hd:Hd) -> Self::List {
        let nil = Self::nil(st);
        Self::cons(st, hd, nil)
    }    

    fn elim2<Res,Nil,Cons,Name> (&mut A, &Self::List, Nil, Cons, Name) -> Res
        where Nil:FnOnce(&mut A) -> Res
        ,    Cons:FnOnce(&mut A, &Hd, &Self::List) -> Res
        ,    Name:FnOnce(&mut A, &A::Name, &Self::List) -> Res ;

    // Derived from `elim` above:
    fn is_empty (st:&mut A, list:&Self::List) -> bool {
        Self::elim2(st, &list, |_|true, |_,_,_|false, |_,_,_|false)
    }
}

#[allow(dead_code)]
fn list_merge<A:Adapton,X:Ord+Clone,L:ListT<A,X>>
    (st:&mut A, l:&L,
     xo:Option<&X>, // xo is useful for merging binary trees.
     n1:Option<&A::Name>, l1:&L::List,
     n2:Option<&A::Name>, l2:&L::List  ) -> L::List
{
    L::elim2(st, l1,
            |_| l2.clone(),
            |st,h1,t1|
            L::elim2(st, l2,
                    |st| L::nil(st),
                    |st, h2, t2|
                    if match xo {
                        None => false,
                        Some(x) => ( x <= h1 && x <= h2 ) }
                    {
                        let rest = list_merge(st, l, None, n1, l1, n2, l2);
                        L::cons(st, xo.unwrap().clone(), rest)
                    }
                    else if h1 <= h2 {
                        //TODO: Nominal memoization with n1:
                        let rest = list_merge(st, l, xo, None, t1, n2, l2);
                        L::cons(st, (*h1).clone(), rest)
                    }
                    else {
                        //TODO: Nominal memoization with n2:
                        let rest = list_merge(st, l, xo, n1, l1, None, t2);
                        L::cons(st, (*h2).clone(), rest)
                    },
                    |st, m2, t2| list_merge(st, l, xo, n1, l1, Some(m2), t2)
                    ),
            |st,n1,t1| list_merge(st, l, xo, Some(n1), t1, n2, l2)
            )
}


fn list_merge_sort<A:Adapton,X:Ord+Hash+Clone,L:ListT<A,X>,T:TreeT<A,X,X>>
    (st:&mut A, l:&L, list:&L::List) -> L::List
{
    let tree = tree_of_list::<A,X,T,L>(st, list);
    T::fold_up (st, tree,
                |st, x|
                L::singleton(st, x),
                
                |st, x, left, right|
                list_merge(st, l, Some(&x), None, &left, None, &right)
                )
}

// Questions:
//  - Should `Name`s always be passed by reference?
//  - Do these Fn argss for fold need to be passed in `Rc<Box<_>>`s ?

pub trait TreeT<A:Adapton,Leaf,Bin:Hash> : Debug+Hash+PartialEq+Eq+Clone {
    type Tree : Debug+Hash+PartialEq+Eq+Clone ;

    fn nil  (&mut A) -> Self::Tree ;
    fn leaf (&mut A, Leaf) -> Self::Tree ;
    fn bin  (&mut A, Bin, Self::Tree, Self::Tree) -> Self::Tree ;

    // requisite "adaptonic" constructors: `name` and `art`:
    fn name (&mut A, A::Name, Self::Tree, Self::Tree) -> Self::Tree ;
    fn art  (&mut A, Art<Self::Tree,A::Loc>) -> Self::Tree ;
        
    fn fold<Res,LeafC,BinC> (&mut A, Self::Tree, Res, LeafC, BinC) -> Res
        where LeafC:Fn(&mut A, Res, Leaf) -> Res
        ,      BinC:Fn(&mut A, Res, Bin ) -> Res ;

    fn fold_up<Res,LeafC,BinC> (&mut A, Self::Tree, LeafC, BinC) -> Res
        where LeafC:Fn(&mut A, Leaf) -> Res
        ,      BinC:Fn(&mut A, Bin, Res, Res ) -> Res ;
    
    // TODO: Add derived operations (max, min, sum, etc.) for fold_up.
}


#[derive(Debug,PartialEq,Eq,Hash,Clone)]
enum List<A:Adapton,Hd> {
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

    // fn elim<Res,Nil,Cons,Name> (self:&Self, st:&mut A, list:&Self::List, nilf:Nil, consf:Cons, namef:Name) -> Res
    //     where Nil:FnOnce(&mut A) -> Res
    //     ,    Cons:FnOnce(&mut A, &Hd, &Self::List) -> Res
    //     ,    Name:FnOnce(&mut A, &A::Name, &Self::List) -> Res
    // {
    //     match *list {
    //         List::Nil => nilf(st),
    //         List::Cons(ref hd, ref tl) => consf(st, hd, &*tl),
    //         List::Name(ref nm, ref tl) => namef(st, nm, &*tl),
    //         List::Rc(ref rc) => self.elim(st, &*rc, nilf, consf, namef),
    //         List::Art(ref art) => {
    //             let list = st.force(art);
    //             self.elim(st, &list, nilf, consf, namef)
    //         }
    //     }
    // }

    fn elim2<Res,Nil,Cons,Name> (st:&mut A, list:&Self::List, nilf:Nil, consf:Cons, namef:Name) -> Res
        where Nil:FnOnce(&mut A) -> Res
        ,    Cons:FnOnce(&mut A, &Hd, &Self::List) -> Res
        ,    Name:FnOnce(&mut A, &A::Name, &Self::List) -> Res
    {
        match *list {
            List::Nil => nilf(st),
            List::Cons(ref hd, ref tl) => consf(st, hd, &*tl),
            List::Name(ref nm, ref tl) => namef(st, nm, &*tl),
            List::Rc(ref rc) => Self::elim2(st, &*rc, nilf, consf, namef),
            List::Art(ref art) => {
                let list = st.force(art);
                Self::elim2(st, &list, nilf, consf, namef)
            }
        }
    }

}

// https://doc.rust-lang.org/book/macros.html
//
// macro_rules! o_O {
//     (
//         $(
//             $x:expr; [ $( $y:expr ),* ]
//          );*
//     ) => {
//         &[ $($( $x + $y ),*),* ]
//     }
// }
//
// fn main() {
//     let a: &[i32]
//         = o_O!(10; [1, 2, 3];
//                20; [4, 5, 6]);
//
//     assert_eq!(a, [11, 12, 13, 24, 25, 26]);
// }

// TODO: Need to gensym a variable for each argument below:
//
// macro_rules! thunk {
//     ( $f:ident , $st:expr , $( $arg:expr ),* ) => {
//         let fval = Rc::new(Box::new(
//             |st, args|{
//                 let ($( $arg ),*) = args ;
//                 f( st, $( $arg ),* )
//             })) ;
//         ($st).thunk (ArtId::Eager, prog_pt!(f), fval, $( $arg ),* )
//     }}

fn tree_of_list_rec_memo <A:Adapton, X:Hash+Clone, T:TreeT<A,X,X>, L:ListT<A,X>>
    (st:&mut A, list:&L::List, left_tree:&T::Tree, left_tree_lev:u32, parent_lev:u32) ->
    (T::Tree, L::List)
{
    let t = st.thunk (ArtId::Eager,
                      prog_pt!(tree_of_list_rec),
                      Rc::new(Box::new(|st, args|{
                          let (list, left_tree, left_tree_lev, parent_lev) = args ;
                          tree_of_list_rec::<A,X,T,L>
                              (st, list, left_tree, left_tree_lev, parent_lev)
                      })),
                      (list, left_tree, left_tree_lev, parent_lev)
                      ) ;
    st.force( &t )
}

fn tree_of_list_rec <A:Adapton, X:Hash+Clone, T:TreeT<A,X,X>, L:ListT<A,X>>
    (st:&mut A, list:&L::List, left_tree:&T::Tree, left_tree_lev:u32, parent_lev:u32)
     -> (T::Tree, L::List)
{
    L::elim2 (
        st, &list,
        /* Nil */  |st| ( T::nil(st), L::nil(st) ),
        /* Cons */ |st, hd, rest| {
            let lev_hd = (1 + (my_hash(hd).leading_zeros())) as u32 ;
            if left_tree_lev <= lev_hd && lev_hd <= parent_lev {
                let nil = T::nil(st) ;
                let (right_tree, rest) = tree_of_list_rec::<A,X,T,L> ( st, rest, &nil, 0 as u32, lev_hd ) ;
                let tree = T::bin ( st, hd.clone(), left_tree.clone(), right_tree ) ;
                tree_of_list_rec::<A,X,T,L> ( st, &rest, &tree, lev_hd, parent_lev )
            }
            else {
                let rest = L::cons(st, hd.clone(), rest.clone()) ;
                (left_tree.clone(), rest)
            }},
        /* Name */ |st, nm, rest| {
            let lev_nm = (1 + 64 + (my_hash(nm).leading_zeros())) as u32 ;
            if left_tree_lev <= lev_nm && lev_nm <= parent_lev {
                let nil = T::nil(st) ;
                let (right_tree, rest) = tree_of_list_rec_memo::<A,X,T,L> ( st, rest, &nil, 0 as u32, lev_nm ) ;
                // TODO: Place left_ and right_ trees into articulations (not Boxes), named by name.
                let tree = T::name( st, nm.clone(), left_tree.clone(), right_tree ) ;
                tree_of_list_rec_memo::<A,X,T,L> ( st, &rest, &tree, lev_nm, parent_lev )
            }
            else {
                let rest = L::name(st, nm.clone(), rest.clone()) ;
                (left_tree.clone(), rest)
            }}
        )
}

pub fn tree_of_list <A:Adapton, X:Hash+Clone, T:TreeT<A,X,X>, L:ListT<A,X>>
    (st:&mut A, list:&L::List) -> T::Tree
{
    let nil = T::nil(st) ;
    let (tree, rest) = tree_of_list_rec::<A,X,T,L> (st, list, &nil, 0 as u32, u32::max_value()) ;
    assert!( L::is_empty( st, &rest ) );
    tree
}