#![feature(associated_type_defaults)]
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use macros::* ;
//use adapton_sigs::* ;
//use collection_traits::*;
//use collection_traits::*;
// use quickcheck::Arbitrary;
// use quickcheck::Gen;
use adapton::engine::* ;
use std::num::Zero;
use std::marker::PhantomData;

use rand::{Rng,Rand};

#[derive(Clone,Hash,Eq,PartialEq,Debug)]
pub enum Dir2 { Left, Right }

pub trait ListT<X> : Debug+Clone+Hash+PartialEq+Eq {

  /// Introduce an empty list
  fn nil  () -> Self ;

  /// Introduce a Cons cell
  fn cons (X, Self) -> Self ;
  
  /// Introduce a Name "cons" cell
  fn name (Name, Self) -> Self ;

  /// Introduce a list with an articulation that holds a list
  fn art  (Art<Self>) -> Self ;

  /// Eliminate a list with the given functions (for the pattern match
  /// arms) that handle the `nil`, `cons` and `name` cases.
  /// Eliminates the `art` case internally, by forcing the art and
  /// eliminating the resulting list with the given handler functions;
  /// forces multiple `art` cases, if need be.
  fn elim<Res,NilF,ConsF,NameF> (Self, NilF, ConsF, NameF) -> Res
    where NilF:FnOnce(          ) -> Res
    ,    ConsF:FnOnce(X,    Self) -> Res
    ,    NameF:FnOnce(Name, Self) -> Res ;

  /// Like `elim`, except that the functions are given an additional
  /// argument.  This variant is needed due to the move semantics of
  /// Rust. The argument is moved into the body of the activated
  /// handler function when it is applied.
  fn elim_arg<Arg,Res,NilF,ConsF,NameF> (Self, Arg, NilF, ConsF, NameF) -> Res
    where NilF:FnOnce(            Arg) -> Res
    ,    ConsF:FnOnce(X,    Self, Arg) -> Res
    ,    NameF:FnOnce(Name, Self, Arg) -> Res ;

  /// Creates a singleton list. Derived from `cons` and `nil` introduction forms.
  fn singleton (hd:X) -> Self {
    let nil = Self::nil();
    Self::cons(hd, nil)
  }

  /// Tests if the list contains any `cons` cells. Derived from `elim`.
  fn is_empty (list:Self) -> bool {
    Self::elim(list,
               ||       true,
               |_,_|   false,
               |_,tl| Self::is_empty(tl))
  }
}

pub trait TreeT<Leaf> : Debug+Hash+PartialEq+Eq+Clone+'static {
  type Lev  : Debug+Hash+PartialEq+Eq+Clone+'static ;

  fn lev<X:Hash>(&X) -> Self::Lev ;
  fn lev_of_tree (&Self) -> Self::Lev ;
  fn lev_bits () -> Self::Lev ;
  fn lev_zero () -> Self::Lev ;
  fn lev_inc (&Self::Lev) -> Self::Lev ;
  fn lev_add (&Self::Lev, &Self::Lev) -> Self::Lev ;
  fn lev_lte (&Self::Lev, &Self::Lev) -> bool ;
  fn lev_max_val () -> Self::Lev ;

  fn lev_max (a:&Self::Lev, b:&Self::Lev) -> Self::Lev {
    if Self::lev_lte(a, b) { b.clone() } else { a.clone() }
  }
  
  fn nil  () -> Self ;
  fn leaf (Leaf) -> Self ;
  fn bin  (Self::Lev, Self, Self) -> Self ;

  // requisite "adaptonic" constructors: `name` and `art`:
  fn name (Name, Self::Lev, Self, Self) -> Self ;
  fn art  (Art<Self>) -> Self ;
  
  fn elim<Res,NilC,LeafC,BinC,NameC>
    (Self, NilC, LeafC, BinC, NameC) -> Res        
    where NilC  : FnOnce() -> Res
    ,     LeafC : FnOnce(Leaf) -> Res
    ,     BinC  : FnOnce(Self::Lev,  Self, Self) -> Res
    ,     NameC : FnOnce(Name, Self::Lev, Self, Self) -> Res
    ;

  fn elim_ref<Res,NilC,LeafC,BinC,NameC>
    (&Self, NilC, LeafC, BinC, NameC) -> Res
    where NilC  : FnOnce() -> Res
    ,     LeafC : FnOnce(&Leaf) -> Res
    ,     BinC  : FnOnce(&Self::Lev,  &Self, &Self) -> Res
    ,     NameC : FnOnce(&Name, &Self::Lev, &Self, &Self) -> Res
    ;

  fn elim_arg<Arg,Res,NilC,LeafC,BinC,NameC>
    (Self, Arg, NilC, LeafC, BinC, NameC) -> Res
    where NilC  : FnOnce(Arg) -> Res
    ,     LeafC : FnOnce(Leaf, Arg) -> Res
    ,     BinC  : FnOnce(Self::Lev,  Self, Self, Arg) -> Res
    ,     NameC : FnOnce(Name, Self::Lev, Self, Self, Arg) -> Res
    ;

  fn full_move<Arg,Res,NilC,LeafC,BinC,NameC,ArtC>
    (Self, Arg, NilC, LeafC, BinC, NameC, ArtC) -> Res
    where NilC  : FnOnce(Arg) -> Res
    ,     LeafC : FnOnce(Leaf, Arg) -> Res
    ,     BinC  : FnOnce(Self::Lev,  Self, Self, Arg) -> Res
    ,     NameC : FnOnce(Name, Self::Lev, Self, Self, Arg) -> Res
    ,     ArtC  : FnOnce(&Art<Self>, Arg) -> Res
    ;

  //fn get_string(l:Self) -> String ;
  
  // Derived from `elim` above:
  fn is_empty (tree:Self) -> bool {
    Self::elim(tree,
               ||        true,
               |_|       false,
               |_,l,r|   Self::is_empty(l) && Self::is_empty(r),
               |_,_,l,r| Self::is_empty(l) && Self::is_empty(r)
               )
  }
  
  fn fold_lr<Res:Hash+Debug+Eq+Clone+'static,LeafC:'static,BinC:'static,NameC:'static>
    (tree:Self, res:Res,
     leaf:Rc<LeafC>,
     bin: Rc<BinC>,
     name:Rc<NameC>) -> Res
    where LeafC:Fn(Leaf,            Res ) -> Res
    ,      BinC:Fn(Self::Lev,       Res ) -> Res 
    ,     NameC:Fn(Name, Self::Lev, Res ) -> Res 
  {
    Self::elim_arg
      (tree, (res,(leaf,bin,name)),
       |      (res,_)              | res,
       |x,    (res,(leaf,_,_))     | leaf(x, res),
       |x,l,r,(res,(leaf,bin,name))| {
         let res = Self::fold_lr(l, res, leaf.clone(), bin.clone(), name.clone());
         let res = (&bin)(x, res);
         let res = Self::fold_lr(r, res, leaf, bin, name);
         res
       },
       |n,x,l,r,(res,(leaf,bin,name))| {
         let (n1,n2) = name_fork(n.clone());
         let res = memo!(n1 =>> Self::fold_lr, tree:l, res:res ;;
                         leaf:leaf.clone(), bin:bin.clone(), name:name.clone());
         let res = name(n, x, res);
         let res = memo!(n2 =>> Self::fold_lr, tree:r, res:res ;;
                         leaf:leaf, bin:bin, name:name);
         res
       }
       )
  }
  
  // fn fold_rl<Res:Hash+Debug+Eq+Clone,LeafC,BinC,NameC>
  //   (tree:Self, res:Res, leaf:&LeafC, bin:&BinC, name:&NameC) -> Res
  //   where LeafC:Fn(Leaf,    Res ) -> Res
  //   ,      BinC:Fn(Self::Lev,     Res ) -> Res
  //   ,     NameC:Fn(Name, Self::Lev, Res ) -> Res
  // {
  //   Self::elim_arg
  //     (tree, res,
  //      |res| res,
  //      |x,res| leaf(x, res),
  //      |x,l,r,res| {
  //        let res = Self::fold_rl(r, res, leaf, bin, name);
  //        let res = bin(x, res);
  //        let res = Self::fold_rl(l, res, leaf, bin, name);
  //        res
  //      },
  //      |n,x,l,r,res| {
  //        let (n1,n2) = name_fork(n.clone());
  //        let res = memo!(n1 =>> Self::fold_rl, tree:r, res:res ;; leaf:leaf, bin:bin, name:name);
  //        let res = name(n, x, res);
  //        let res = memo!(n2 =>> Self::fold_rl, tree:l, res:res ;; leaf:leaf, bin:bin, name:name);
  //        res
  //      }
  //      )
  // }
  
  // fn fold_up<Res:Hash+Debug+Eq+Clone,NilC,LeafC,BinC,NameC>
  //   (tree:Self, nil:&NilC, leaf:&LeafC, bin:&BinC, name:&NameC) -> Res
  //   where  NilC:Fn() -> Res
  //   ,     LeafC:Fn(Leaf              ) -> Res
  //   ,      BinC:Fn(Self::Lev,     Res, Res ) -> Res
  //   ,     NameC:Fn(Name, Self::Lev, Res, Res ) -> Res
  // {
  //   Self::elim
  //     (tree,
  //      || nil(),
  //      |x| leaf(x),
  //      |x,l,r| {
  //        let resl = Self::fold_up(l, nil, leaf, bin, name);
  //        let resr = Self::fold_up(r, nil, leaf, bin, name);
  //        let res = bin(x, resl, resr);
  //        res
  //      },
  //      |n,x,l,r| {
  //        let (n1,n2) = name_fork(n.clone());
  //        let resl = memo!(n1 =>> Self::fold_up, tree:l ;; nil:nil, leaf:leaf, bin:bin, name:name);
  //        let resr = memo!(n2 =>> Self::fold_up, tree:r ;; nil:nil, leaf:leaf, bin:bin, name:name);
  //        let res = name(n, x, resl, resr);
  //        res
  //      }
  //      )
  // }
}


pub fn tree_of_list
  < X:Hash+Clone+Debug
  , T:TreeT<X>+'static
  , L:ListT<X>+'static
  >
  (dir_list:Dir2, list:L) -> T {
    let tnil = T::nil();
    let lnil = L::nil();
    let (tree, list) = tree_of_list_rec::<X,T,L>(dir_list, list, tnil, T::lev_zero(), T::lev_max_val());
    assert_eq!(list, lnil);
    tree
  }

pub fn tree_of_list_rec
  < X:Hash+Clone+Debug
  , T:TreeT<X>+'static
  , L:ListT<X>+'static
  >
  (dir_list:Dir2, list:L, tree:T, tree_lev:T::Lev, parent_lev:T::Lev) -> (T, L)
{
  L::elim_arg (
    list, (dir_list, tree, tree_lev, parent_lev),
    
    /* Nil */
    |(_dir_list, tree, _, _)| (tree, L::nil()),
    
    /* Cons */
    |hd, rest, (dir_list, tree, tree_lev, parent_lev)| {      
      let lev_hd = T::lev_inc ( &T::lev(&hd) ) ;
      if T::lev_lte ( &tree_lev , &lev_hd ) && T::lev_lte ( &lev_hd , &parent_lev ) {
        let leaf = T::leaf(hd) ;
        let (tree2, rest2) = {
          tree_of_list_rec::<X,T,L> ( dir_list.clone(), rest, leaf, T::lev_zero(), lev_hd.clone() )
        };
        let tree3 = match dir_list.clone() {
          Dir2::Left  => T::bin ( lev_hd.clone(), tree,  tree2 ),
          Dir2::Right => T::bin ( lev_hd.clone(), tree2, tree  ),
        } ;
        tree_of_list_rec::<X,T,L> ( dir_list, rest2, tree3, lev_hd, parent_lev )
      }
      else {
        (tree, L::cons(hd,rest))
      }},
    
    /* Name */
    |nm:Name, rest, (dir_list, tree, tree_lev, parent_lev)|{
      let lev_nm = T::lev_inc( &T::lev_add( &T::lev_bits() , &T::lev(&nm) ) ) ;
      if T::lev_lte ( &tree_lev , &lev_nm ) && T::lev_lte ( &lev_nm ,  &parent_lev ) {
        let nil = T::nil() ;
        let (nm1, nm2, nm3, nm4) = name_fork4(nm.clone());
        let (_, (tree2, rest)) =
          eager!(nm1 =>> tree_of_list_rec::<X,T,L>,
                 dir_list:dir_list.clone(), list:rest,
                 tree:nil, tree_lev:T::lev_zero(), parent_lev:lev_nm.clone() ) ;
        let tree3 = match dir_list.clone() {
          Dir2::Left  => T::name ( nm.clone(), lev_nm.clone(), tree,  tree2 ),
          Dir2::Right => T::name ( nm.clone(), lev_nm.clone(), tree2, tree  ),
        } ;
        let art = cell(nm3, tree3) ;
        let tree3 = T::art( art ) ;
        let (_, (tree, rest)) =
          eager!(nm2 =>> tree_of_list_rec::<X,T,L>,
                 dir_list:dir_list.clone(), list:rest,
                 tree:tree3, tree_lev:lev_nm, parent_lev:parent_lev ) ;
        (tree, rest)
      }
      else {
        (tree, L::name(nm,rest))
      }},
    )
}











// /// Just like ListT, except with an additional constructor: `tree`
// pub trait TreeListT<X,T:TreeT<X>> : ListT<X> {    
//   fn tree (T::Tree, Dir2, Self) -> Self ;

//   fn next_leaf (T::Tree, Dir2) -> (Option<X>,Self) ;

//   fn next_leaf_rec (T::Tree, Dir2, Self) -> (Option<X>, Self) ;

//   fn tree_elim_arg<Arg,Res,Tree,Nil,Cons,Name>
//     (Self, Arg, Tree, Nil, Cons, Name) -> Res
//     where Tree:FnOnce(T::Tree, Dir2, Self, Arg) -> Res
//     ,      Nil:FnOnce(Arg) -> Res
//     ,     Cons:FnOnce(X, Self, Arg) -> Res
//     ,     Name:FnOnce(Name, Self, Arg) -> Res
//     ;

//   fn full_elim_arg<Arg,Res,Treef,Nilf,Consf,Namef,Artf>
//     (Self, Arg, Treef, Nilf, Consf, Namef, Artf) -> Res
//     where Treef:FnOnce(T::Tree, Dir2, Self, Arg) -> Res
//     ,      Nilf:FnOnce(Arg) -> Res
//     ,     Consf:FnOnce(X, Self, Arg) -> Res
//     ,     Namef:FnOnce(Name, Self, Arg) -> Res
//     ,      Artf:FnOnce(&Art<Self,A::Loc>, Arg) -> Res
//     ;
//   fn get_string(l:Self) -> String ;
// }


// pub fn tree_reduce_monoid<Elm:Eq+Hash+Clone+Debug+GMLog<A>,T:TreeT<Elm>,BinOp>
//     (tree:T::Tree, zero:Elm, binop:&BinOp) -> Elm
//     where BinOp:Fn(Elm, Elm) -> Elm
// {
//     T::fold_up(st, tree,
//                         &|_| zero.clone(),
//                    &|_,leaf| leaf,
//                  &|st,_,l,r| binop(st,l,r),
//                &|st,_,_,l,r| binop(st,l,r),
//                )
// }

// pub fn list_reduce_monoid<Elm:Eq+Hash+Clone+Debug+GMLog<A>,L:ListT<Elm>,BinOp,T:TreeT<Elm>>
//     (list:L::List, zero:Elm, binop:&BinOp) -> Elm
//     where BinOp:Fn(Elm, Elm) -> Elm
// {
//     let tree = tree_of_list::<Elm,T,L>(st, Dir2::Left, list);
//     tree_reduce_monoid::<Elm,T,BinOp>(st, tree, zero, binop)
// }

// pub fn tree_filter<X:Hash+Clone,T:TreeT<X>,F>
//     (tree:T::Tree, f:&F) -> T::Tree
//     where F:Fn(&X) -> bool
// {
//     T::fold_up(st, tree,
//                &|st| T::nil(st),
//                &|st,x| { let fx = f(st,&x);
//                          if fx { T::leaf(st, x) }
//                          else  { T::nil(st) } },
//                &|st,lev,l,r| T::bin(st, lev, l, r),
//                &|st,n,lev,l,r| T::name(st, n, lev, l, r)
//                )
// }

// pub fn list_of_tree<X:Hash+Clone,L:ListT<X>,T:TreeT<X>>
//     (tree:T::Tree) -> L::List
// {
//     let nil = L::nil(st);
//     T::fold_rl(st, tree, nil,
//                &|st,x,xs| L::cons(st,x,xs),
//                &|_,_,xs| xs,
//                &|st,n,_,xs| L::name(st,n,xs)
//                )
// }

pub fn rev_list_of_tree<X:Hash+Clone,L:ListT<X>+'static,T:TreeT<X>+'static>
  (tree:T) -> L
{
  let nil = L::nil();
  T::fold_lr(tree, nil,
             Rc::new(|x,xs| L::cons(x,xs)),
             Rc::new(|_,xs| xs),
             Rc::new(|n,_,xs| L::name(n,xs))
             )
}

// pub fn vec_of_list<X:Clone,L:ListT<X>> (list:L::List, limit:Option<usize>) -> Vec<X> {
//     let mut out = vec![];
//     let mut list = list ;
//     loop {
//         let (hd, rest) =
//             L::elim( st, list,
//                      |st| (None, None),
//                      |st, x, rest| { (Some(x), Some(rest)) },
//                      |st, _, rest| { (None,    Some(rest)) }
//                      ) ;
//         match hd { Some(x) => out.push(x), _ => () } ;
//         match limit { Some(limit) if out.len() == limit => return out, _ => () } ;
//         match rest { Some(rest) => { list = rest; continue }, None => return out }
//     }
// }

// pub fn list_of_vec<X:Clone,L:ListT<X>> (v:Vec<X>) -> L::List {
//     let mut l = L::nil(st);
//     for x in v.iter().rev() { l = L::cons(st,x.clone(),l) }
//     return l
// }

// pub fn list_of_vec_w_names<X:Clone,L:ListT<X>> (v:Vec<NameOrElem<Name,X>>) -> L::List {
//   let mut l = L::nil(st);
//   for x in v.iter().rev() {
//     l = match *x {
//       NameOrElem::Name(ref nm) => L::name(st,nm.clone(),l),
//       NameOrElem::Elem(ref el) => L::cons(st,el.clone(),l),
//     }}
//   return l
// }

// pub fn rev_list_of_vec<X:Clone,L:ListT<X>> (v:Vec<X>) -> L::List {
//     let mut l = L::nil(st);
//     for x in v.iter() { l = L::cons(st,x.clone(), l) }
//     return l
// }

// pub fn tree_of_treelist
//     < A:Adapton
//     , X:Hash+Clone+Eq+Debug
//     , T:TreeT<X>
//     , L:TreeListT<X,T>
//     >
//     (dir_list:Dir2, list:L::List) -> T::Tree {
//       let tnil = T::nil(st);
//       let lnil = L::nil(st);
//       let (tree, list) = tree_of_treelist_rec2::<X,T,L>(st, dir_list, list, tnil);
//       if list == lnil { tree }
//       else {
//         panic!("Left over list: {:?}", list);
//       }
//     }

// pub fn tree_of_treelist_rec2 
//     < A:Adapton
//     , X:Hash+Clone+Eq+Debug
//     , T:TreeT<X>
//     , L:TreeListT<X,T>
//     >
//     (dir_list:Dir2, list:L::List, tree0:T::Tree)
//      -> (T::Tree, L::List) {
//        let tnil = T::nil(st);
//        let lnil = L::nil(st);
//        let (tree1, list1) = tree_of_treelist_rec::<X,T,L>(st, dir_list.clone(), list, tnil.clone(), T::lev_zero(), T::lev_max_val());
//        if list1 == lnil {
//          let tree01 = match dir_list {
//            Dir2::Left  => tree_append::<X,T>(st, tree0, tree1),
//            Dir2::Right => tree_append::<X,T>(st, tree1, tree0),
//          } ;
//          (tree01, list1)
//        }
//        else {
//          let (tree2, list2) = tree_of_treelist_rec::<X,T,L>(st, dir_list.clone(), list1, tnil, T::lev_zero(), T::lev_max_val());
//          let tree3 =
//            match dir_list {
//              Dir2::Left  => tree_append::<X,T>(st, tree1, tree2),
//              Dir2::Right => tree_append::<X,T>(st, tree2, tree1),
//            } ;
//          return tree_of_treelist_rec2::<X,T,L>(st, dir_list, list2, tree3)
//        }
//      }

// pub fn tree_of_treelist_rec
//     < A:Adapton
//     , X:Hash+Clone+Eq+Debug
//     , T:TreeT<X>
//     , L:TreeListT<X,T>
//     >
//     (dir_list:Dir2, list:L::List,
//      tree:T::Tree, tree_lev:T::Lev, parent_lev:T::Lev)
//      -> (T::Tree, L::List)
// {
//   L::tree_elim_arg (
//     st, list, (dir_list, tree, tree_lev, parent_lev),

//     /* Tree */
//     |st, tree2, dir_tree2, rest,
//     /* Accums: */ (dir_tree1, tree1, tree1_lev, parent_lev)| {
//       assert!( dir_tree1 == dir_tree2 );
//       let tree2_lev = T::lev_of_tree(st, &tree2);
//       if T::lev_lte ( &tree2_lev , &parent_lev ) {
//         let tree3 =
//           match dir_tree1 {
//             Dir2::Left  => tree_append::<X,T>(st, tree1, tree2),
//             Dir2::Right => tree_append::<X,T>(st, tree2, tree1),
//           } ;
//         // !!! using this tree level may end up failing the invariant that rest == Nil at completion
//         let tree3_lev = T::lev_of_tree(st, &tree3);
//         // !!! XXX Using this tree_lev is not quite right for maintaining balance;
//         // !!! XXX The level may be affected by the append on the prior line.
//         let (tree4, rest2) =
//           tree_of_treelist_rec::<X,T,L>(st, dir_tree1.clone(), rest, tree3, tree3_lev, parent_lev.clone()) ;

//         let tree4_lev = T::lev_of_tree(st, &tree4);
//         return tree_of_treelist_rec::<X,T,L>(st, dir_tree1, rest2, tree4, tree4_lev, parent_lev)
//       }
//       else {
//         // If: tree2_lev > parent_lev
//         (tree1, L::tree(st,tree2,dir_tree2,rest))
//       }
//     },
    
//     /* Nil */
//     |st, (_dir_list, tree, _, _)| (tree, L::nil(st)),

//     /* Cons */
//     |st, hd, rest, (dir_list, tree, tree_lev, parent_lev)| {
//       let lev_hd = T::lev_inc ( &T::lev(&hd) ) ;
//       if T::lev_lte ( &tree_lev , &lev_hd ) && T::lev_lte ( &lev_hd , &parent_lev ) {
//         let leaf = T::leaf(st, hd) ;
//         let (tree2, rest2) = {
//           tree_of_treelist_rec::<X,T,L> ( st, dir_list.clone(), rest, leaf, T::lev_zero(), lev_hd.clone() )
//         };
//         let tree3 = match dir_list.clone() {
//           Dir2::Left  => T::bin ( st, lev_hd.clone(), tree,  tree2 ),
//           Dir2::Right => T::bin ( st, lev_hd.clone(), tree2, tree  ),
//         } ;
//         return tree_of_treelist_rec::<X,T,L> ( st, dir_list, rest2, tree3, lev_hd, parent_lev )
//       }
//       else {
//         // If: lev_hd > parent_lev \/ tree > lev_hd
//         (tree, L::cons(st,hd,rest))
//       }},

//     /* Name */
//     |st, nm, rest, (dir_list, tree, tree_lev, parent_lev)|{
//       let lev_nm = T::lev_inc( &T::lev_add( &T::lev_bits() , &T::lev(&nm) ) ) ;
//       if T::lev_lte ( &tree_lev , &lev_nm ) && T::lev_lte ( &lev_nm ,  &parent_lev ) {
//         let nil = T::nil(st) ;
//         let (nm1, nm2, nm3, nm4) = st.name_fork4(nm.clone());
//         let (_, (tree2, rest)) =
//           eager!(st, nm1 =>> tree_of_treelist_rec::<X,T,L>,
//                  dir_list:dir_list.clone(), list:rest,
//                  tree:nil, tree_lev:T::lev_zero(), parent_lev:lev_nm.clone() ) ;
//         let tree3 = match dir_list.clone() {
//           Dir2::Left  => T::name ( st, nm.clone(), lev_nm.clone(), tree,  tree2 ),
//           Dir2::Right => T::name ( st, nm.clone(), lev_nm.clone(), tree2, tree  ),
//         } ;
//         let art = st.cell(nm3, tree3) ;
//         let art = st.read_only( art ) ;
//         let tree3 = T::art( st, art ) ;
//         let (_, (tree, rest)) =
//           eager!(st, nm2 =>> tree_of_treelist_rec::<X,T,L>,
//                  dir_list:dir_list.clone(), list:rest,
//                  tree:tree3, tree_lev:lev_nm, parent_lev:parent_lev ) ;
//         (tree, rest)
//       }
//       else {
//         // If: lev_nm > parent_lev \/ tree_lev > lev_nm
//         (tree, L::name(st,nm,rest))
//       }},
//     )
// }

// pub fn list_merge<X:Ord+Clone+Debug,L:ListT<X>>
//     (n1:Option<Name>, l1:L::List,
//      n2:Option<Name>, l2:L::List  ) -> L::List
// {
//     L::elim_arg
//         (st, l1, (n1,n2,l2),
//          /* Nil */  |_, (_, _, l2)| l2,
//          /* Cons */ |st,h1,t1,(n1,n2,l2)|
//          L::elim_arg
//          (st, l2, (h1,t1,n1,n2),
//           /* Nil */  |st, (h1, t1, _, _ )| L::cons(st, h1,t1),
//           /* Cons */ |st, h2, t2, (h1, t1, n1, n2)| {
//           if &h1 <= &h2 {
//               let l2 = L::cons(st,h2,t2);
//               match n1 {
//                   None => {
//                       let rest = list_merge::<X,L>(st, None, t1, n2, l2);
//                       L::cons(st, h1, rest)
//                   }
//                   Some(n1) => {
//                       let (n1a, n1b) = st.name_fork(n1);
//                       let rest = thunk!(st, n1a =>>
//                                         list_merge::<X,L>,
//                                         n1:None, l1:t1, n2:n2, l2:l2);
//                       let rest = L::art(st, rest);
//                       let rest = L::cons(st, h1, rest);
//                       L::name(st, n1b, rest)
//                   }
//               }
//           }
//           else {
//               let l1 = L::cons(st,h1,t1);
//               match n2 {
//                   None => {
//                       let rest = list_merge::<X,L>(st, n1, l1, None, t2);
//                       let l = L::cons(st, h2, rest);
//                       l
//                   }
//                   Some(n2) => {
//                       let (n2a, n2b) = st.name_fork(n2);
//                       let rest = thunk!(st, n2a =>>
//                                         list_merge::<X,L>,
//                                         n1:n1, l1:l1, n2:None, l2:t2);
//                       let rest = L::art(st, rest);
//                       let rest = L::cons(st, h2, rest);
//                       L::name(st, n2b, rest)
//                   }
//               }
//           }},
//           |st,m2,t2,(h1,t1,n1,_n2)| {
//               let l1 = L::cons(st,h1,t1);
//               list_merge::<X,L>(st, n1, l1, Some(m2), t2)
//           }
//           ),
//          |st,m1,t1,(_n1,n2,l2)| {
//              list_merge::<X,L>(st, Some(m1), t1, n2, l2)
//          }
//          )
// }

// pub fn list_merge_sort<X:Ord+Hash+Debug+Clone,L:ListT<X>,T:TreeT<X>>
//     (list:L::List) -> L::List
// {
//     let tree = tree_of_list::<X,T,L>(st, Dir2::Left, list);
//     tree_merge_sort::<X,L,T>(st, tree)
// }

// pub fn tree_merge_sort<X:Ord+Hash+Debug+Clone,L:ListT<X>,T:TreeT<X>>
//     (tree:T::Tree) -> L::List
// {
//     T::fold_up (st, tree,
//                 &|st|                 L::nil(st),
//                 &|st, x|              L::singleton(st, x),
//                 &|st, _, left, right| { list_merge::<X,L>(st, None, left, None, right) },
//                 &|st, n, _, left, right| { let (n1,n2) = st.name_fork(n);
//                                            list_merge::<X,L>(st, Some(n1), left, Some(n2), right) },
//                 )
// }

// pub fn tree_append
//   < X:Clone+Hash+Eq+Debug
//   , T:TreeT<X> >
//   (tree1:T::Tree, tree2:T::Tree) -> T::Tree
// {
//   T::elim_arg(
//     st, tree1, tree2,
//     /* Nil */  |_, tree2| tree2,
//     /* Leaf */ |st,leaf,  tree2| {
//       T::elim_arg(
//         st, tree2, leaf,
//         /* Leaf-Nil */  |st, leaf1| T::leaf(st,leaf1),
//         /* Leaf-Leaf */ |st, leaf2, leaf1| {
//           let lev1 = T::lev(&leaf1);
//           let lev2 = T::lev(&leaf2);
//           let lev = T::lev_max(&lev1,&lev2) ;
//           let l1 = T::leaf(st,leaf1);
//           let l2 = T::leaf(st,leaf2);
//           T::bin(st, lev, l1, l2)
//         },
//         /* Leaf-Bin */  |st, lev2, l2, r2, leaf1| {
//           let lev1 = T::lev(&leaf1);
//           if T::lev_lte(&lev2, &lev1) {
//             let lev  = T::lev_max(&lev1, &lev2);
//             let l    = T::leaf(st,leaf1);
//             let r    = T::bin(st, lev2, l2, r2);
//             T::bin(st, lev, l, r) // xxx count me
//           } else {
//             let l1 = T::leaf(st,leaf1);
//             let tree1 = tree_append::<X,T>(st, l1, l2);
//             let tree1lev = T::lev_of_tree(st, &tree1);
//             let levr2 = T::lev_of_tree(st, &r2);
//             let lev = T::lev_max(&tree1lev, &levr2);
//             T::bin(st, lev, tree1, r2)
//           }
//         },
//         /* Leaf-Name */ |st, n2, lev2, l2, r2, leaf1| {
//           let lev1 = T::lev(&leaf1);
//           let l1 = T::leaf(st,leaf1);
//           if T::lev_lte(&lev2, &lev1) {
//             let lev  = T::lev_max(&lev1, &lev2);
//             let r    = T::name(st, n2, lev2, l2, r2);
//             T::bin(st, lev, l1, r) // xxx panic
//           } else {
//             let tree1 = cell_call!(st, n2.clone() =>> tree_append::<X,T>, tree1:l1, tree2:l2);
//             let tree1 = T::art(st, tree1);
//             let tree1lev = T::lev_of_tree(st, &tree1);
//             let levr2 = T::lev_of_tree(st, &r2);
//             let lev = T::lev_max(&tree1lev, &levr2);
//             T::name(st, n2, lev, tree1, r2)
//           }
//         }
//       )
//     }, /* end Leaf */
//     /* Bin */ |st,lev1,l1,r1, tree2| {
//       let bin_data = (lev1,l1,r1) ;
//       T::elim_arg(
//         st, tree2, bin_data,
//         /* Bin-Nil */  |st, (lev1,l1,r1)| T::bin(st,lev1,l1,r1),
//         /* Bin-Leaf */ |st, leaf2, (lev1,l1,r1)| {
//           let tree2 = T::leaf(st, leaf2);
//           let lev2  = T::lev_of_tree(st, &tree2);
//           if T::lev_lte(&lev2, &lev1) {
//             let tree2 = tree_append::<X,T>(st, r1, tree2);
//             T::bin(st, lev1, l1, tree2)
//           } else {
//             let lev   = T::lev_max(&lev1, &lev2);
//             let tree1 = T::bin(st, lev1, l1, r1);
//             T::bin(st, lev, tree1, tree2) // xxx count me
//           }
//         },
//         /* Bin-Bin */  |st, lev2, l2, r2, (lev1, l1, r1)| {
//           if T::lev_lte(&lev2, &lev1) {
//             let tree2 = T::bin(st, lev2, l2, r2);
//             let r = tree_append::<X,T>(st, r1, tree2);
//             T::bin(st, lev1, l1, r)
//           } else {
//             let tree1 = T::bin(st, lev1, l1, r1);
//             let l = tree_append::<X,T>(st, tree1, l2);
//             T::bin(st, lev2, l, r2)
//           }
//         },
//         /* Bin-Name */ |st, n2, lev2, l2, r2, (lev1, l1, r1) | {
//           if T::lev_lte(&lev2, &lev1) {
//             let tree2 = T::name(st, n2, lev2, l2, r2);
//             let r = tree_append::<X,T>(st, r1, tree2);
//             T::bin(st, lev1, l1, r)
//           } else {
//             let tree1 = T::bin(st, lev1, l1, r1);
//             let l = cell_call!(st, n2.clone() =>> tree_append::<X,T>, tree1:tree1, tree2:l2);
//             let l = T::art(st, l);
//             T::name(st, n2, lev2, l, r2)
//           }
//         }
//       )
//     }, /* end Bin */
//     /* Name */ |st, n1, lev1, l1, r1, tree2| {
//       let name_data = (n1,lev1,l1,r1) ;
//       T::elim_arg(
//         st, tree2, name_data,
//         /* Name-Nil */  |st, (n1,lev1,l1,r1)| T::name(st,n1,lev1,l1,r1),
//         /* Name-Leaf */ |st, leaf2, (n1,lev1,l1,r1)| {
//           let tree2 = T::leaf(st, leaf2);
//           let lev2  = T::lev_of_tree(st, &tree2);
//           if T::lev_lte(&lev2, &lev1) {
//             let tree2 = cell_call!(st, n1.clone() =>> tree_append::<X,T>, tree1:r1, tree2:tree2);
//             let tree2 = T::art(st, tree2);
//             T::name(st, n1, lev1, l1, tree2)
//           } else {
//             let lev   = T::lev_max(&lev1, &lev2);
//             let tree1 = T::name(st, n1, lev1, l1, r1);
//             panic!("Name-Leaf");
//             T::bin(st, lev, tree1, tree2) // panic
//           }
//         },
//         /* Name-Bin */  |st, lev2, l2, r2, (n1, lev1, l1, r1)| {
//           if T::lev_lte(&lev2, &lev1) {
//             let tree2 = T::bin(st, lev2, l2, r2);
//             let r = cell_call!(st, n1.clone() =>> tree_append::<X,T>, tree1:r1, tree2:tree2);
//             let r = T::art(st, r);
//             T::name(st, n1, lev1, l1, r)
//           } else {
//             let tree1 = T::name(st, n1, lev1, l1, r1);
//             let l = tree_append::<X,T>(st, tree1, l2);
//             panic!("Name-Bin");
//             T::bin(st, lev2, l, r2) // panic
//           }
//         },
//         /* Name-Name */ |st, n2, lev2, l2, r2, (n1, lev1, l1, r1) | {
//           if T::lev_lte(&lev2, &lev1) {
//             let tree2 = T::name(st, n2, lev2, l2, r2);
//             let r = cell_call!(st, n1.clone() =>> tree_append::<X,T>, tree1:r1, tree2:tree2);
//             let r = T::art(st, r);
//             T::name(st, n1, lev1, l1, r)
//            } else {
//             let tree1 = T::name(st, n1, lev1, l1, r1);
//             let l = cell_call!(st, n2.clone() =>> tree_append::<X,T>, tree1:tree1, tree2:l2);
//             let l = T::art(st, l);
//             T::name(st, n2, lev2, l, r2) 
//           }
//         }
//       )
//     } /* end Name */
//   )
// }


// #[cfg(test)]
// mod test {
//   use super::*;
//   use adapton::adapton_sigs::* ;
//   use adapton::collection_traits::*;
//   use adapton::collection_edit::*;
//   use adapton::collection::*;
//   use adapton::engine;
//   use adapton::naive;
//   use std::fmt::Debug;
//   use std::hash::Hash;
//   use gm::GMLog;

//   fn lev_name<T:TreeT<usize>>(nm:&Name) -> T::Lev {
//     T::lev_inc( &T::lev_add( &T::lev_bits() , &T::lev(nm) ) )
//   }
  
//   fn doit<A:Adapton>(st:&mut A) {
//     let size = 32;
//     let mut v1 : Vec<NameOrElem<Name,usize>> = vec![];
//     let mut v2 : Vec<NameOrElem<Name,usize>> = vec![];
//     for i in 0..(size/2) {
//       let n = st.name_of_usize(i);
//       println!("lev({:?})={:?}", &n, lev_name::<Tree<usize,u32>>(&n));
//       v1.push(NameOrElem::Name(n));
//       v1.push(NameOrElem::Elem(i));      
//     }
//     for i in (size/2)..size {
//       let n = st.name_of_usize(i);
//       println!("lev({:?})={:?}", &n, lev_name::<Tree<usize,u32>>(&n));
//       v2.push(NameOrElem::Name(n));
//       v2.push(NameOrElem::Elem(i));
//     }
//     println!("v1: {:?}", v1);
//     println!("v2: {:?}", v2);
//     let l1 = list_of_vec_w_names::<usize,List<usize>>(st, v1.clone());
//     let l2 = list_of_vec_w_names::<usize,List<usize>>(st, v2.clone());

//     println!("l1: {}", List::get_string(st, l1.clone()));
//     println!("l2: {}", List::get_string(st, l2.clone()));

//     let t1 = st.structural(|st|tree_of_list::<usize,Tree<usize,u32>,List<usize>>(st, Dir2::Left, l1));
//     let t2 = st.structural(|st|tree_of_list::<usize,Tree<usize,u32>,List<usize>>(st, Dir2::Left, l2));

//     println!("t1: {}", Tree::get_string(st, t1.clone()));
//     println!("t2: {}", Tree::get_string(st, t2.clone()));

//     v1.append(&mut v2);
//     let l3 = list_of_vec_w_names::<usize,List<usize>>(st, v1);
//     let t3 = st.structural(|st|tree_of_list::<usize,Tree<usize,u32>,List<usize>>(st, Dir2::Left, l3));
    
//     let t4 = st.structural(|st|tree_append::<usize,Tree<usize,u32>>(st, t1, t2));
    
//     println!("t3: {}", Tree::get_string(st, t3.clone()));
//     println!("t4: {}", Tree::get_string(st, t4.clone()));

//     t3.log_snapshot(st, "t3", None);
//     t4.log_snapshot(st, "t4", None);
//   }

//   // #[test]
//   // fn doit2() {
//   //   let mut st = engine::Engine::new();
//   //   doit(&mut st)
//   // }
// }


#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub enum List<X> {
  Nil,
  Cons(X, Box<List<X>>),
  Tree(Box<Tree<X>>, Dir2, Box<List<X>>),
  Name(Name, Box<List<X>>),
  Art(Art<List<X>>),
}

#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub enum Tree<X> {
  Nil,
  Leaf(X),
  Bin(usize, Box<Tree<X>>, Box<Tree<X>> ),
  Name(Name, usize, Box<Tree<X>>, Box<Tree<X>> ),
  Art(Art<Tree<X>>),
}

#[derive(Debug,Clone)]
pub enum NameElse<X> {
  Name(Name),
  Else(X),
}

impl<X:Debug+Hash+PartialEq+Eq+Clone> ListT<X> for List<X>
{
  fn nil  ()                 -> Self { List::Nil }
  fn cons (hd:X, tl:Self)    -> Self { List::Cons(hd,Box::new(tl)) }
  fn name (nm:Name, tl:Self) -> Self { List::Name(nm, Box::new(tl)) }
  fn art  (art:Art<List<X>>) -> Self { List::Art(art) }
  
  fn elim<Res,NilF,ConsF,NameF>
    (list:Self, nilf:NilF, consf:ConsF, namef:NameF) -> Res
    where NilF:FnOnce() -> Res
    ,    ConsF:FnOnce(X, Self) -> Res
    ,    NameF:FnOnce(Name, Self) -> Res
  {
    match list {
      List::Nil => nilf(),
      List::Cons(hd, tl) => consf(hd, *tl),
      List::Name(nm, tl) => namef(nm, *tl),
      List::Art(ref art) => {
        let list = force(art);
        Self::elim(list, nilf, consf, namef)
      },
      List::Tree(tree, dir, tl) => {
        let tree = *tree;
        let tl = *tl;
        let (res, rest) = structural(|| panic!("List::next_leaf_rec(tree, dir, tl)")) ;
        match res {
          None => Self::elim(rest, nilf, consf, namef),
          Some(elm) => consf(elm, rest),
        }
      }
    }
  }
  
  fn elim_arg<Arg,Res,NilF,ConsF,NameF>
    (list:Self, arg:Arg, nilf:NilF, consf:ConsF, namef:NameF) -> Res
    where NilF:FnOnce(Arg) -> Res
    ,    ConsF:FnOnce(X, Self, Arg) -> Res
    ,    NameF:FnOnce(Name, Self, Arg) -> Res
  {
    match list {
      List::Nil => nilf(arg),
      List::Cons(hd, tl) => consf(hd, *tl, arg),
      List::Name(nm, tl) => namef(nm, *tl, arg),
      List::Art(ref art) => {
        let list = force(art);
        Self::elim_arg(list, arg, nilf, consf, namef)
      },
      List::Tree(tree, dir, tl) => {
        let tree = *tree;
        let tl = *tl;
        let (res, rest) = structural(|| panic!("List::next_leaf_rec(tree, dir, tl)")) ;
        match res {
          None => Self::elim_arg(rest, arg, nilf, consf, namef),
          Some(elm) => consf(elm, rest, arg),
        }
      }
    }
  }
}

// impl< A:Adapton+Debug+Hash+PartialEq+Eq+Clone
//     , X:Debug+Hash+PartialEq+Eq+Clone
//     >
//     TreeListT<X,Tree<X,u32>>
//     for List<X>
// {
//   fn tree (tr:Tree<X,u32>, dir:Dir2, tl:Self) -> Self {
//     List::Tree(Box::new(tr), dir, Box::new(tl))
//   }

//   // Maybe rename this 'deconstruct', and just return a list with the
//   // invarient that the first element is not a tree?
//   fn next_leaf_rec (tree:Tree<X,u32>, dir:Dir2, rest:Self) -> (Option<X>,Self) {
//     match tree {
//       Tree::Nil => { (None, rest) },
//       Tree::Rc(rc) => Self::next_leaf_rec(st, (*rc).clone(), dir, rest),
//       Tree::Art(ref art) => {
//         let tree = st.force(art);
//         Self::next_leaf_rec(st, tree, dir, rest)
//       }
//       Tree::Leaf(leaf) => {
//         (Some(leaf), rest)
//       }
//       Tree::Bin(_,l,r) => {
//         match dir {
//           Dir2::Left  => Self::next_leaf_rec(st, *l, Dir2::Left,  List::Tree(r, Dir2::Left,  Box::new(rest))),
//           Dir2::Right => Self::next_leaf_rec(st, *r, Dir2::Right, List::Tree(l, Dir2::Right, Box::new(rest))),
//         }
//       },
//       Tree::Name(nm,_,l,r) => {
//         if false {
//           match dir {
//             Dir2::Left  => {
//               let art = st.cell(nm.clone(), List::Tree(r, Dir2::Left,  Box::new(rest)));
//               let art = st.read_only(art);
//               Self::next_leaf_rec(st, *l, Dir2::Left, List::Name(nm, Box::new(List::Art(art))))
//             },
//             Dir2::Right => {
//               let art = st.cell(nm.clone(), List::Tree(l, Dir2::Right, Box::new(rest)));
//               let art = st.read_only(art);
//               Self::next_leaf_rec(st, *r, Dir2::Right, List::Name(nm, Box::new(List::Art(art))))
//             }
//           }
//         }
//         else if true {
//           match dir {
//             Dir2::Left  => {
//               let b = Box::new(List::Tree(r, Dir2::Left,  Box::new(rest)));
//               Self::next_leaf_rec(st, *l, Dir2::Left, List::Name(nm, b))
//             },
//             Dir2::Right => {
//               let b = Box::new(List::Tree(l, Dir2::Right, Box::new(rest)));
//               Self::next_leaf_rec(st, *r, Dir2::Right, List::Name(nm, b))
//             }
//           }
//         }
//         else if false {
//           match dir {
//             Dir2::Left  => {
//               let b = Box::new(List::Tree(r, Dir2::Left,  Box::new(List::Rc(Rc::new(rest)))));
//               Self::next_leaf_rec(st, *l, Dir2::Left, List::Name(nm, b))
//             },
//             Dir2::Right => {
//               let b = Box::new(List::Tree(l, Dir2::Right, Box::new(List::Rc(Rc::new(rest)))));
//               Self::next_leaf_rec(st, *r, Dir2::Right, List::Name(nm, b))
//             }
//           }
//         }
//         else { unreachable!() }
//       }
//     }
//   }

//   fn next_leaf (tree:Tree<X,u32>, dir:Dir2) -> (Option<X>,Self) {
//     st.structural(|st| Self::next_leaf_rec(st, tree, dir, List::Nil))
//   }

//   fn tree_elim_arg<Arg,Res,Treef,Nil,Cons,Name>
//     (list:Self, arg:Arg, treef:Treef, nilf:Nil, consf:Cons, namef:Name) -> Res
//     where Treef:FnOnce(Tree<X,u32>, Dir2, Self, Arg) -> Res
//     ,       Nil:FnOnce(Arg) -> Res
//     ,      Cons:FnOnce(X, Self, Arg) -> Res
//     ,     Name:FnOnce(Name, Self, Arg) -> Res
//   {
//     match list {
//       List::Nil => nilf(st, arg),
//       List::Cons(hd, tl) => consf(st, hd, *tl, arg),
//       List::Name(nm, tl) => namef(st, nm, *tl, arg),
//       List::Rc(rc) => Self::elim_arg(st, (*rc).clone(), arg, nilf, consf, namef),
//       List::Art(ref art) => {
//         let list = st.force(art);
//         Self::elim_arg(st, list, arg, nilf, consf, namef)
//       },
//       List::Tree(tree, dir, tl) => treef(st, *tree, dir, *tl, arg),
//     }
//   }

//   fn full_elim_arg<Arg,Res,Treef,Nilf,Consf,Namef,Artf>
//     (list:Self, arg:Arg, treef:Treef, nilf:Nilf, consf:Consf, namef:Namef, artf:Artf) -> Res
//     where Treef:FnOnce(Tree<X,u32>, Dir2, Self, Arg) -> Res
//     ,      Nilf:FnOnce(Arg) -> Res
//     ,     Consf:FnOnce(X, Self, Arg) -> Res
//     ,     Namef:FnOnce(Name, Self, Arg) -> Res
//     ,      Artf:FnOnce(&Art<Self,A::Loc>, Arg) -> Res
//   {
//     match list {
//       List::Nil => nilf(st, arg),
//       List::Cons(hd, tl) => consf(st, hd, *tl, arg),
//       List::Name(nm, tl) => namef(st, nm, *tl, arg),
//       List::Rc(rc) => Self::elim_arg(st, (*rc).clone(), arg, nilf, consf, namef),
//       List::Art(ref art) => { artf(st, art, arg) },
//       List::Tree(tree, dir, tl) => treef(st, *tree, dir, *tl, arg),
//     }
//   }

//   fn get_string(l:Self) -> String {
//     Self::full_elim_arg(st, l, (),
//                          |st,tree,dir,rest,_| { let ts = Tree::get_string(st,tree); format!("Tree({},{:?},{})", ts, dir, Self::get_string(st, rest)) },
//                          |st,_| format!("Nil"),
//                          |st,x,tl,_| format!("Cons({:?},{})",x,Self::get_string(st, tl)),
//                          |st,nm,tl,_| format!("Name({:?},{})",nm,Self::get_string(st, tl)),                         
//                          |st,art,_| format!("Art({})", {let l = st.force(art); Self::get_string(st, l)}),
//                          )}

// }

impl <Leaf:Debug+Hash+PartialEq+Eq+Clone+'static>
  TreeT<Leaf>
  for Tree<Leaf>
{
  type Lev = usize ;
  
  //  See Pugh+Teiltelbaum in POPL 1989 for an explanation of this notion of "level":
  fn lev<X:Hash>(x:&X) -> Self::Lev  {
    my_hash_n(&x,1).trailing_zeros() as Self::Lev
  }
  fn lev_of_tree (tree:&Self) -> Self::Lev {
    Tree::elim_ref
      (tree,
       || 0,
       |leaf|      Self::lev(leaf),
       |lev,_,_|   lev.clone(),
       |_,lev,_,_| lev.clone(),
       )
  }
  fn lev_bits () -> Self::Lev { 32 }
  fn lev_zero () -> Self::Lev { 0 }
  fn lev_max_val () -> Self::Lev { usize::max_value() }
  fn lev_add (x:&Self::Lev,y:&Self::Lev) -> Self::Lev { x + y }
  fn lev_inc (x:&Self::Lev) -> Self::Lev { x + 1 }
  fn lev_lte (x:&Self::Lev,y:&Self::Lev) -> bool { x <= y }
  
  fn nil  ()                                      -> Self { Tree::Nil }
  fn leaf (x:Leaf)                                -> Self { Tree::Leaf(x) }
  fn bin  (lev:Self::Lev, l:Self, r:Self)         -> Self { Tree::Bin(lev,Box::new(l),Box::new(r)) }
  fn name (nm:Name, lev:Self::Lev, l:Self,r:Self) -> Self { Tree::Name(nm, lev, Box::new(l),Box::new(r)) }
  fn art  (art:Art<Self>)                         -> Self { Tree::Art(art) }
  
  fn elim_arg<Arg,Res,NilC,LeafC,BinC,NameC>
    (tree:Self, arg:Arg, nil:NilC, leaf:LeafC, bin:BinC, name:NameC) -> Res
    where NilC  : FnOnce(Arg) -> Res
    ,     LeafC : FnOnce(Leaf, Arg) -> Res
    ,     BinC  : FnOnce(Self::Lev,  Self, Self, Arg) -> Res
    ,     NameC : FnOnce(Name, Self::Lev,  Self, Self, Arg) -> Res
  {
    match tree {
      Tree::Nil => nil(arg),
      Tree::Leaf(x) => leaf(x, arg),
      Tree::Bin(b, l, r) => bin(b, *l, *r, arg),
      Tree::Name(nm, b, l, r) => name(nm, b, *l, *r, arg),
      Tree::Art(art) => {
        let list = force(&art);
        Self::elim_arg(list, arg, nil, leaf, bin, name)
      }
    }
  }
  
  fn full_move<Arg,Res,NilC,LeafC,BinC,NameC,ArtC>
    (tree:Self, arg:Arg, nil:NilC, leaf:LeafC, bin:BinC, name:NameC, artf:ArtC) -> Res
    where NilC  : FnOnce(Arg) -> Res
    ,     LeafC : FnOnce(Leaf, Arg) -> Res
    ,     BinC  : FnOnce(Self::Lev,  Self, Self, Arg) -> Res
    ,     NameC : FnOnce(Name, Self::Lev,  Self, Self, Arg) -> Res
    ,     ArtC  : FnOnce(&Art<Self>, Arg) -> Res
  {
    match tree {
      Tree::Nil => nil(arg),
      Tree::Leaf(x) => leaf(x, arg),
      Tree::Bin(b, l, r) => bin(b, *l, *r, arg),
      Tree::Name(nm, b, l, r) => name(nm, b, *l, *r, arg),
      Tree::Art(art) => { artf(&art, arg) }
    }
  }
  
  fn elim<Res,NilC,LeafC,BinC,NameC>
    (tree:Self, nil:NilC, leaf:LeafC, bin:BinC, name:NameC) -> Res
    where NilC  : FnOnce() -> Res
    ,     LeafC : FnOnce(Leaf) -> Res
    ,     BinC  : FnOnce(Self::Lev,  Self, Self) -> Res
    ,     NameC : FnOnce(Name, Self::Lev, Self, Self) -> Res
  {
    match tree {
      Tree::Nil => nil(),
      Tree::Leaf(x) => leaf(x),
      Tree::Bin(b, l, r) => bin(b, *l, *r),
      Tree::Name(nm, b, l, r) => name(nm, b, *l, *r),
      Tree::Art(art) => {
        let list = force(&art);
        Self::elim(list, nil, leaf, bin, name)
      }
    }
  }
  
  fn elim_ref<Res,NilC,LeafC,BinC,NameC>
    (tree:&Self, nil:NilC, leaf:LeafC, bin:BinC, name:NameC) -> Res
    where NilC  : FnOnce() -> Res
    ,     LeafC : FnOnce(&Leaf) -> Res
    ,     BinC  : FnOnce(&Self::Lev, &Self, &Self) -> Res
    ,     NameC : FnOnce(&Name, &Self::Lev, &Self, &Self) -> Res
  {
    match *tree {
      Tree::Nil => nil(),
      Tree::Leaf(ref x) => leaf(x),
      Tree::Bin(ref b, ref l, ref r) => bin(b, &*l, &*r),
      Tree::Name(ref nm, ref b, ref l, ref r) => name(nm, b, &*l, &*r),
      Tree::Art(ref art) => {
        let list = force(art);
        Self::elim_ref(&list, nil, leaf, bin, name)
      }
    }
  }
  
  // fn get_string(l:Self) -> String {
  //   Self::full_move
  //     (l, (),
  //      |st, _| format!("Nil"),
  //      |st, elm, _| format!("Leaf({:?})",elm),
  //      |st, lev, l, r, _| {
  //        let ls = Self::get_string(st, l);
  //        let rs = Self::get_string(st, r);
  //        format!("Bin({:?},{},{})",lev,ls,rs)},
  //      |st, lev, nm, l, r, _| {
  //        let ls = Self::get_string(st, l);
  //        let rs = Self::get_string(st, r);
  //        format!("Name({:?},{:?},{},{})",lev,nm,ls,rs)},
  //      |st,art,_| format!("Art({})", {let l = st.force(art); Self::get_string(st, l)}),
  //      )}  
}


// #[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
// pub enum CursorEdit<X,Dir> {
//     Insert  (Dir,X), // Inserts an element
//     Remove  (Dir),   // Removes an element (name-oblivious)
//     Replace (Dir,X), // Replace an element (name-oblivious)
//     Goto    (Dir),   // Move the cursor
// }

// // #[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
// // pub enum Cmd<X,Dir,Name> {
// //     Basic    (CursorEdit<X,Dir>),
// //     InsName  (Dir,Name), // Insert name
// //     RemName  (Dir),      // Remove immediately-adjacent
// //     ShowView (ListReduce),   // Show the given view
// //     HideView (ListReduce),   // Remove the given view
// // }

// #[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
// pub enum ListTransf {
//     Sort, Reverse
// }

// #[derive(Debug,Hash,PartialEq,Eq,Clone,Rand)]
// pub enum ListReduce {
//     Sum, Max, Min, Median,
//     Vec(ListTransf, Option<usize>),
//     Tree(ListTransf, Option<usize>),
// }

// impl Rand for CursorEdit<u32,Dir2>
// {
//   fn rand<R:Rng> (r: &mut R) -> Self {
//     let m : u32 = 1024 * 1024 ;
//     let n : u32 = Rand::rand(r) ;
//     match r.gen_range(0, 100) {
//       0  ... 50  => { CursorEdit::Insert (Rand::rand(r), n % m) },
//       50 ... 60  => { CursorEdit::Remove (Rand::rand(r)) },
//       60 ... 70  => { CursorEdit::Replace(Rand::rand(r), n % m) },
//       70 ... 95 =>  { CursorEdit::Goto   (Dir2::Right) },
//       95 ... 100 => { CursorEdit::Goto   (Dir2::Left)  },
//       _ => unreachable!()
//     }
//   }
// }

// pub trait ExperimentT<X,Tree:TreeT<X>,Out> {
//     type ListEdit : ListEdit<X,Tree> ;
//     fn run (Vec<CursorEdit<X,Dir2>>, view:ListReduce) -> Vec<(Out,Cnt)> ;
// }

// pub fn eval_edit<X,T:TreeT<X>,E:ListEdit<X,T>> (edit:CursorEdit<X,Dir2>, z:E::State, id:usize) -> E::State {
//     match edit {
//         CursorEdit::Insert(dir,x) => {
//             //let z = E::clr_names(st, z, dir.clone()) ;
//             let n = A::name_of_usize(st, id);
//             let m = A::name_of_string(st, format!("input{}", id)) ;
//             let z = E::ins_cell(st, z, dir.clone(), m) ;
//             let z = E::ins_name(st, z, dir.clone(), n) ;
//             let z = E::insert(st, z, dir, x) ;
//             z },
//         CursorEdit::Remove(dir) => {
//             let (z, _) = E::remove (st, z, dir.clone()) ;
//             let (z, _) = E::rem_name(st, z, dir) ;
//             z },
//         CursorEdit::Goto(dir) => {
//             let (z, _) = E::goto (st, z, dir.clone()) ;
//             z },
//         CursorEdit::Replace(dir,x) => {
//             let (z, _, _) = E::replace(st, z, dir, x) ;
//             z },
//     }
// }

// pub fn demand_vec
//     <A:Adapton
//     ,X:Ord+PartialOrd+Hash+Debug+Clone+Eq+PartialEq
//     ,L:ListT<X>
//     ,T:TreeT<X>
//     >
//     (tree:T::Tree, transform:&ListTransf, count:Option<usize>) -> Vec<X>
// {
//     match *transform {
//         ListTransf::Sort => {
//             let output = tree_merge_sort::<X,L,T>(st, tree);
//             vec_of_list::<X,L>(st, output, count)
//         },
//         ListTransf::Reverse => {
//             let output = rev_list_of_tree::<X,L,T>(st, tree);
//             vec_of_list::<X,L>(st, output, count)
//         }
//     }
// }

// pub fn demand_tree
//     <A:Adapton
//     ,X:Ord+PartialOrd+Hash+Debug+Clone+Eq+PartialEq
//     ,L:ListT<X>
//     ,T:TreeT<X>
//     >
//     (tree:T::Tree, transform:&ListTransf, count:Option<usize>) -> Vec<X>
// {
//     // Todo: Respect the `count` argument
//     match *transform {
//         ListTransf::Sort => {
//             let output = tree_merge_sort::<X,L,T>(st, tree) ;
//             let nm = st.name_of_string("tree_of_output".to_string());
//             let tree = st.ns(nm, |st| tree_of_list::<X,T,L>(st, Dir2::Left, output) );
//             vec![]
//         },
//         ListTransf::Reverse => {
//             let output = rev_list_of_tree::<X,L,T>(st, tree) ;
//             let nm = st.name_of_string("tree_of_output".to_string());
//             let tree = st.ns(nm, |st| tree_of_list::<X,T,L>(st, Dir2::Left, output) );
//             vec![]
//         }
//     }
// }

// pub fn eval_reduce
//     <A:Adapton
//     ,X:Ord+PartialOrd+Hash+Debug+Clone+Eq+PartialEq+Zero+Add<Output=X>+GMLog<A>
//     ,L:ListT<X>
//     ,T:TreeT<X>
//     >
//     (tree:T::Tree, red:&ListReduce) -> Vec<X>
// {
//     match *red {
//         ListReduce::Sum => { let x = tree_reduce_monoid::<X,T,_> (st, tree, X::zero(), &|st,x,y| x + y) ; vec![ x ] },
//         ListReduce::Max => { let x = tree_reduce_monoid::<X,T,_> (st, tree, X::zero(), &|st,x,y| if x > y {x} else {y}) ; vec![ x ] },
//         ListReduce::Min => { let x = tree_reduce_monoid::<X,T,_> (st, tree, X::zero(), &|st,x,y| if x < y {x} else {y}) ; vec![ x ] },
//         ListReduce::Median => panic!(""),
//         ListReduce::Vec (ref transform, ref n) => demand_vec:: <X,L,T> (st, tree, transform, n.clone()),
//         ListReduce::Tree(ref transform, ref n) => demand_tree::<X,L,T> (st, tree, transform, n.clone()),
//     }
// }

// /// `ListEdit<X,L>` gives a simple notion of list-editing that is
// /// generic with respect to adapton implementation `A`, list element
// /// type `X`, and list implementation `L`.
// pub trait ListEdit<X,T:TreeT<X>> {
//     /// The State of the Editor is abstract.
//     type State : Clone+Hash+Eq+PartialEq+Debug;

//     // XXX
//     // type Tree  : TreeT<X>;
        
//     fn empty    (&mut A) -> Self::State;
//     fn insert   (Self::State, Dir2, X) -> Self::State;
//     fn remove   (Self::State, Dir2)    -> (Self::State, Option<X>);
//     fn replace  (Self::State, Dir2, X) -> (Self::State, X, bool);
//     fn goto     (Self::State, Dir2)    -> (Self::State, bool);
//     fn shift    (Self::State, Dir2)    -> (Self::State, bool);
//     fn observe  (Self::State, Dir2)    -> (Self::State, Option<X>);

//     // Insert/remove names from the list content:
//     fn clr_names (Self::State, Dir2) -> Self::State;
//     fn ins_name  (Self::State, Dir2, Name) -> Self::State;
//     fn ins_cell  (Self::State, Dir2, Name) -> Self::State;
//     fn rem_name  (Self::State, Dir2) -> (Self::State, Option<Name>);

//     fn ins_tree (Self::State, Dir2, T::Tree, Dir2) -> Self::State;

//     fn clear_side (Self::State, Dir2) -> Self::State ;

//     fn get_list<L:ListT<X>> (Self::State, Dir2) -> L::List;
//     fn get_tree               (Self::State, Dir2) -> T::Tree;
//     fn print_all (Self::State);

//   fn insert_optnm (z:Self::State, dir:Dir2, nm:Option<Name>, elm:X) -> Self::State {
//     match nm {
//       None => Self::insert(st, z, dir, elm),
//       Some(nm) => {
//         let z = Self::insert(st, z, dir.clone(), elm) ;
//         let z = Self::ins_cell(st, z, dir.clone(), nm.clone()) ;
//         let z = Self::ins_name(st, z, dir, nm) ;
//         z
//       }}}
      
//   fn ins_tree_optnm (z:Self::State, dir:Dir2, nm:Option<Name>, tr:T::Tree, tdir:Dir2) -> Self::State {
//     match nm {
//       None => Self::ins_tree(st, z, dir, tr, tdir),
//       Some(nm) => {
//         let z = Self::ins_tree(st, z, dir.clone(), tr, tdir) ;
//         let z = Self::ins_cell(st, z, dir.clone(), nm.clone()) ;
//         let z = Self::ins_name(st, z, dir, nm) ;
//         z
//       }}}
      
  
//   fn move_optnm (z:Self::State, dir:Dir2, nm:Option<Name>) -> (Self::State, bool) {
//     match nm {
//       None => Self::goto(st, z, dir),
//       Some(nm) => {
//         let (z, success) = Self::goto(st, z, dir.clone()) ;
//         if success {
//           let z = Self::ins_cell(st, z, dir.clone().opp(), nm.clone()) ;
//           let z = Self::ins_name(st, z, dir.opp(), nm) ;
//           (z, true)
//         } else {
//           (z, false)
//         }
//       }}}

//   // Todo-Later: Consider adding replace_optnm
// }


// /// Lists with a focus; suitable to implement `ListEdit`.
// #[derive(Debug,Hash,PartialEq,Eq,Clone)]
// pub struct ListZipper<X,T:TreeT<X>,L:TreeListT<X,T>> {
//     /// Elements to the left of the focus, nearest to furthest.
//     pub left: L::List,
//     /// Elements to the right of the focus, nearest to furthest.
//     pub right: L::List,

//     /// Todo-Later: This phantom field should *not* be needed, but is, due to rustc being confused (?).
//     /// It complains that the parameter T is not used, though it is (by the type parameter L's trait bound).
//     phantom:PhantomData<T::Tree>,
// }

// macro_rules! zipper {
//     { $left:expr , $right:expr } => {
//         {
//             ListZipper{ left:$left, right:$right, phantom:PhantomData}
//         }
//     };
// }

// /// Implement `ListEdit` for `ListZipper` generically with respect to
// /// adapton implementation `A`, list element type `X`, and list
// /// implementation `L`.
// impl<A:Adapton
//     ,X:Debug+Hash+PartialEq+Eq+Clone
//     ,T:TreeT<X>
//     ,L:TreeListT<X,T>
//     >
//     ListEdit<X,T>
//     for
//     ListZipper<X,T,L>
// {
//     type State=ListZipper<X,T,L>;

//     // XXX
//     // type Tree=T;
    
//   fn clr_names (zip:Self::State, dir:Dir2) -> Self::State {
//     panic!("clr_names generally re-associates names");
//         // match dir {
//         //     Dir2::Left => L::elim_arg
//         //         (st, zip.left, zip.right,
//         //          |st,right| zipper!{L::nil(st), right},
//         //          |st,x,left,right| zipper!{L::cons(st,x,left), right},
//         //          |st,nm,left,right| {
//         //              let right = L::name(st,nm,right);
//         //              Self::clr_names(st, zipper!{left, right}, dir)}
//         //          ),
//         //     Dir2::Right => L::elim_arg
//         //         (st, zip.right, zip.left,
//         //          |st,left| zipper!{left, L::nil(st)},
//         //          |st,x,right,left| zipper!{left, L::cons(st,x,right)},
//         //          |st,nm,right,left| {
//         //              let left = L::name(st,nm,left);
//         //              Self::clr_names(st, zipper!{left, right}, dir)}
//         //          ),
//         // }
//     }

//   fn print_all (zip:Self::State) {
//     println!("zip-left:  {}", L::get_string(st, zip.left));
//     println!("zip-right: {}", L::get_string(st, zip.right));
//   }
  
//     fn ins_tree (zip:Self::State, ins_dir:Dir2, tree:T::Tree, tree_dir:Dir2) -> Self::State {
//         match ins_dir {
//             Dir2::Left =>
//                 zipper!{L::tree(st, tree, tree_dir, zip.left),
//                         zip.right},
//             Dir2::Right =>
//                 zipper!{zip.left,
//                         L::tree(st, tree, tree_dir, zip.right)},
//         }
//     }
    
//     fn ins_name (zip:Self::State, dir:Dir2, name:Name) -> Self::State {
//         match dir {
//             Dir2::Left =>
//                 zipper!{L::name(st, name, zip.left),
//                            zip.right},
//             Dir2::Right =>
//                 zipper!{zip.left,
//                            L::name(st, name, zip.right)},
//         }
//     }

//     fn ins_cell (zip:Self::State, dir:Dir2, name:Name) -> Self::State {
//         match dir {
//             Dir2::Left => {
//                 let art = st.cell(name, zip.left) ;
//                 let art = st.read_only(art);
//                 zipper!{L::art(st, art),
//                            zip.right}},                
//             Dir2::Right => {
//                 let art = st.cell(name, zip.right) ;
//                 let art = st.read_only(art);
//                 zipper!{zip.left,
//                            L::art(st, art)}},
//         }
//     }

//     fn rem_name  (zip:Self::State, dir:Dir2) -> (Self::State, Option<Name>) {
//         match dir {
//             Dir2::Left => L::elim_arg
//                 (st, zip.left, zip.right,
//                  |st,right|         (zipper!{L::nil(st), right}, None ),
//                  |_,x,left,right|   (zipper!{left,       right}, None ),
//                  |st,nm,left,right| (zipper!{left, right},       Some(nm))
//                  ),
//             Dir2::Right => L::elim_arg
//                 (st, zip.right, zip.left,
//                  |st,left|          (zipper!{left, L::nil(st)}, None ),
//                  |_,x,right,left|   (zipper!{left, right},      None ),
//                  |st,nm,right,left| (zipper!{left, right},      Some(nm))
//                  ),
//         }
//     }
    
//     fn empty (st: &mut A) -> Self::State {
//         let nil1 = L::nil(st);
//         let nil2 = nil1.clone();
//         zipper!{nil1, nil2}
//     }
    
//     fn insert (zip:Self::State, dir:Dir2, x:X) -> Self::State {
//         match dir {
//             Dir2::Left =>
//                 zipper!{L::cons(st, x, zip.left),
//                            zip.right},
//             Dir2::Right =>
//                 zipper!{zip.left,
//                            L::cons(st, x, zip.right)},
//         }
//     }

//     fn remove  (zip:Self::State, dir:Dir2) -> (Self::State, Option<X>) {
//         match dir {
//             Dir2::Left => L::elim_arg
//                 (st, zip.left, zip.right,
//                  |st,right|         (zipper!{L::nil(st), right}, None   ),
//                  |_,x,left,right|   (zipper!{left,       right}, Some(x)),
//                  |st,nm,left,right| {let zip = zipper!{left, right};
//                                      Self::remove (st, zip, Dir2::Left)}
//                  ),
//             Dir2::Right => L::elim_arg
//                 (st, zip.right, zip.left,
//                  |st,left|          (zipper!{left, L::nil(st)}, None   ),
//                  |_,x,right,left|   (zipper!{left, right},      Some(x)),
//                  |st,nm,right,left| {let zip = zipper!{left, right};
//                                      Self::remove (st, zip, Dir2::Right)}
//                  ),
//         }
//     }


//   // XXX rename 'move'
//     fn goto (zip:Self::State, dir:Dir2) -> (Self::State, bool) {
//       match dir {
//         Dir2::Left => L::elim_arg
//           (st, zip.left, zip.right,
//            /* Nil */  |st,right|          (zipper!{L::nil(st), right}          , false ),
//            /* Cons */ |st,elm,left,right| (zipper!{left, L::cons(st,elm,right)}, true  ),
//            /* Name */ |st,_,left,right| {let zip = zipper!{left, right};
//                                           Self::goto (st, zip, Dir2::Left)}
//            ),
//         Dir2::Right => L::elim_arg
//           (st, zip.right, zip.left,
//            /* Nil */ |st,left|           (zipper!{left,              L::nil(st)}, false ),
//            /* Cons */ |st,x,right,left|  (zipper!{L::cons(st,x,left),right}     , true  ),
//            /* Name */ |st,_,right,left| {let zip = zipper!{left, right};
//                                           Self::goto (st, zip, Dir2::Right)}
//            ),
//       }
//     }

//     fn shift (zip:Self::State, dir:Dir2) -> (Self::State, bool) {
//       match dir {
//         Dir2::Left => L::elim_arg
//           (st, zip.left, zip.right,
//            /* Nil */  |st,right|          (zipper!{L::nil(st), right}          , false ),
//            /* Cons */ |st,elm,left,right| (zipper!{left, L::cons(st,elm,right)}, true  ),
//            /* Name */ |st,nm,left,right| {let zip = zipper!{left, L::name(st,nm,right)};
//                                           Self::shift (st, zip, Dir2::Left)}
//            ),
//         Dir2::Right => L::elim_arg
//           (st, zip.right, zip.left,
//            /* Nil */ |st,left|           (zipper!{left,              L::nil(st)}, false ),
//            /* Cons */ |st,x,right,left|  (zipper!{L::cons(st,x,left),right}     , true  ),
//            /* Name */ |st,nm,right,left| {let zip = zipper!{L::name(st,nm,left), right};
//                                           Self::shift (st, zip, Dir2::Right)}
//            ),
//       }
//     }

//     fn observe (zip:Self::State, dir:Dir2) -> (Self::State,Option<X>) {
//         match dir {
//             Dir2::Left => L::elim_arg
//                 (st, zip.left, zip.right,
//                  |st,right|         (zipper!{L::nil(st), right}, None),
//                  |st,x,left,right| {let x2 = x.clone();
//                                     (zipper!{L::cons(st,x,left), right}, Some(x2))},
//                  |st,nm,left,right|{let zip = zipper!{left,right};
//                                     Self::observe (st, zip, Dir2::Left)}
//                  ),
//             Dir2::Right => L::elim_arg
//                 (st, zip.right, zip.left,
//                  |st,left|         (zipper!{left, L::nil(st)}, None),
//                  |st,x,right,left| {let x2 = x.clone();
//                                     (zipper!{left, L::cons(st,x,right)}, Some(x2))},
//                  |st,nm,right,left|{let zip = zipper!{left,right};
//                                     Self::observe (st, zip, Dir2::Right)}
//                  ),
//         }
//     }

//     fn replace (zip:Self::State, dir:Dir2, y:X) -> (Self::State, X, bool) {
//         match dir {
//             Dir2::Left => L::elim_arg
//                 (st, zip.left, (zip.right, y),
//                  |st,(right,y)|        (zipper!{L::nil(st),         right}, y, false),
//                  |st,x,left,(right,y)| (zipper!{L::cons(st,y,left), right}, x, true ),
//                  |st,nm,left,(right,y)|{let zip = zipper!{left,right};
//                                         Self::replace (st, zip, Dir2::Left, y)}
//                  ),
//             Dir2::Right => L::elim_arg
//                 (st, zip.right, (zip.left,y),
//                  |st,(left,y)|         (zipper!{left, L::nil(st)},          y, false),
//                  |st,x,right,(left,y)| (zipper!{left, L::cons(st,y,right)}, x, true ),
//                  |st,nm,right,(left,y)|{let zip = zipper!{left,right};
//                                         Self::replace (st, zip, Dir2::Right, y)}
//                  ),
//         }
//     }

//     fn clear_side
//         (zip:Self::State, dir:Dir2) -> Self::State {
//             let emp = L::nil(st) ;
//             match dir {
//                 Dir2::Right => zipper!{zip.left, emp},
//                 Dir2::Left  => zipper!{emp, zip.right},
//             }
//         }
        
//     fn get_list<N:ListT<X>>
//         (zip:Self::State, dir:Dir2) -> N::List
//     {
//         let tree = Self::get_tree(st, zip, dir);
//         list_of_tree::<X,N,T>(st, tree)
//     }

//     /// Creates a tree whose leaves hold the contents of the zipper, in order.
//     /// When `dir=Left`,  the tree's leaves are ordered from left-to-right, i.e., as (rev left) @ right.
//     /// When `dir=Right`, the tree's leaves are ordered from right-to-left, i.e., as (rev right) @ left.
//     fn get_tree(zip:Self::State, dir:Dir2) -> T::Tree
//     {
//         match dir {
//             Dir2::Left  => {
//                 let left  = tree_of_treelist::<X,T,L>(st, Dir2::Right, zip.left);
//                 let right = tree_of_treelist::<X,T,L>(st, Dir2::Left,  zip.right);
//                 tree_append::<X,T>(st, left, right)}
            
//             Dir2::Right => {
//                 let right = tree_of_treelist::<X,T,L>(st, Dir2::Right, zip.right);
//                 let left  = tree_of_treelist::<X,T,L>(st, Dir2::Left,  zip.left);
//                 tree_append::<X,T>(st, right, left)}
//         }
//     }
// }
