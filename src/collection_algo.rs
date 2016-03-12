#![feature(associated_type_defaults)]
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use gm::GMLog;
  
use macros::* ;
use adapton_sigs::* ;
use collection_traits::*;
// use quickcheck::Arbitrary;
// use quickcheck::Gen;
use std::num::Zero;


use rand::{Rng,Rand};

pub fn tree_reduce_monoid<A:Adapton,Elm:Eq+Hash+Clone+Debug+GMLog<A>,T:TreeT<A,Elm>,BinOp>
    (st:&mut A, tree:T::Tree, zero:Elm, binop:&BinOp) -> Elm
    where BinOp:Fn(&mut A, Elm, Elm) -> Elm
{
    T::fold_up(st, tree,
                        &|_| zero.clone(),
                   &|_,leaf| leaf,
                 &|st,_,l,r| binop(st,l,r),
               &|st,_,_,l,r| binop(st,l,r),
               )
}

pub fn list_reduce_monoid<A:Adapton,Elm:Eq+Hash+Clone+Debug+GMLog<A>,L:ListT<A,Elm>,BinOp,T:TreeT<A,Elm>>
    (st:&mut A, list:L::List, zero:Elm, binop:&BinOp) -> Elm
    where BinOp:Fn(&mut A, Elm, Elm) -> Elm
{
    let tree = tree_of_list::<A,Elm,T,L>(st, Dir2::Left, list);
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
               &|st| T::nil(st),
               &|st,x| { let fx = f(st,&x);
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
               &|_,_,xs| xs,
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

pub fn vec_of_list<A:Adapton,X:Clone,L:ListT<A,X>> (st:&mut A, list:L::List, limit:Option<usize>) -> Vec<X> {
    let mut out = vec![];
    let mut list = list ;
    loop {
        let (hd, rest) =
            L::elim( st, list,
                     |st| (None, None),
                     |st, x, rest| { (Some(x), Some(rest)) },
                     |st, _, rest| { (None,    Some(rest)) }
                     ) ;
        match hd { Some(x) => out.push(x), _ => () } ;
        match limit { Some(limit) if out.len() == limit => return out, _ => () } ;
        match rest { Some(rest) => { list = rest; continue }, None => return out }
    }
}

pub fn list_of_vec<A:Adapton,X:Clone,L:ListT<A,X>> (st:&mut A, v:Vec<X>) -> L::List {
    let mut l = L::nil(st);
    for x in v.iter().rev() { l = L::cons(st,x.clone(),l) }
    return l
}

pub fn list_of_vec_w_names<A:Adapton,X:Clone,L:ListT<A,X>> (st:&mut A, v:Vec<NameOrElem<A::Name,X>>) -> L::List {
  let mut l = L::nil(st);
  for x in v.iter().rev() {
    l = match *x {
      NameOrElem::Name(ref nm) => L::name(st,nm.clone(),l),
      NameOrElem::Elem(ref el) => L::cons(st,el.clone(),l),
    }}
  return l
}

pub fn rev_list_of_vec<A:Adapton,X:Clone,L:ListT<A,X>> (st:&mut A, v:Vec<X>) -> L::List {
    let mut l = L::nil(st);
    for x in v.iter() { l = L::cons(st,x.clone(), l) }
    return l
}

pub fn tree_of_list
    < A:Adapton
    , X:Hash+Clone+Debug
    , T:TreeT<A,X>
    , L:ListT<A,X>
    >
    (st:&mut A, dir_list:Dir2, list:L::List) -> T::Tree {
        let tnil = T::nil(st);
        let lnil = L::nil(st);
        let (tree, list) = tree_of_list_rec::<A,X,T,L>(st, dir_list, list, tnil, T::lev_zero(), T::lev_max_val());
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
     dir_list:Dir2, list:L::List,
     tree:T::Tree, tree_lev:T::Lev, parent_lev:T::Lev)
     -> (T::Tree, L::List)
{
    L::elim_move (
        st, list, (dir_list, tree, tree_lev, parent_lev),

        /* Nil */
        |st, (_dir_list, tree, _, _)| (tree, L::nil(st)),

        /* Cons */
        |st, hd, rest, (dir_list, tree, tree_lev, parent_lev)| {
                                                               
            let lev_hd = T::lev_inc ( &T::lev(&hd) ) ;
            if T::lev_lte ( &tree_lev , &lev_hd ) && T::lev_lte ( &lev_hd , &parent_lev ) {
                let leaf = T::leaf(st, hd) ;
                let (tree2, rest2) = {
                    tree_of_list_rec::<A,X,T,L> ( st, dir_list.clone(), rest, leaf, T::lev_zero(), lev_hd.clone() )
                };
                let tree3 = match dir_list.clone() {
                    Dir2::Left  => T::bin ( st, lev_hd.clone(), tree,  tree2 ),
                    Dir2::Right => T::bin ( st, lev_hd.clone(), tree2, tree  ),
                } ;
                tree_of_list_rec::<A,X,T,L> ( st, dir_list, rest2, tree3, lev_hd, parent_lev )
            }
            else {
                (tree, L::cons(st,hd,rest))
            }},

        /* Name */
        |st, nm, rest, (dir_list, tree, tree_lev, parent_lev)|{
            let lev_nm = T::lev_inc( &T::lev_add( &T::lev_bits() , &T::lev(&nm) ) ) ;
            if T::lev_lte ( &tree_lev , &lev_nm ) && T::lev_lte ( &lev_nm ,  &parent_lev ) {
                let nil = T::nil(st) ;
                let (nm1, nm2, nm3, nm4) = st.name_fork4(nm.clone());
                let (_, (tree2, rest)) =
                    eager!(st, nm1 =>> tree_of_list_rec::<A,X,T,L>,
                           dir_list:dir_list.clone(), list:rest,
                           tree:nil, tree_lev:T::lev_zero(), parent_lev:lev_nm.clone() ) ;
                let tree3 = match dir_list.clone() {
                    Dir2::Left  => T::name ( st, nm.clone(), lev_nm.clone(), tree,  tree2 ),
                    Dir2::Right => T::name ( st, nm.clone(), lev_nm.clone(), tree2, tree  ),
                } ;
                let art = st.cell(nm3, tree3) ;
                let art = st.read_only( art ) ;
                let tree3 = T::art( st, art ) ;
                let (_, (tree, rest)) =
                    eager!(st, nm2 =>> tree_of_list_rec::<A,X,T,L>,
                           dir_list:dir_list.clone(), list:rest,
                           tree:tree3, tree_lev:lev_nm, parent_lev:parent_lev ) ;
                //let art = st.cell(nm4, tree) ;
                //let art = st.read_only( art ) ;
                //let tree = T::art( st, art ) ;
                (tree, rest)
            }
            else {
                (tree, L::name(st,nm,rest))
            }},
        )
}

pub fn tree_of_treelist
    < A:Adapton
    , X:Hash+Clone+Eq+Debug
    , T:TreeT<A,X>
    , L:TreeListT<A,X,T>
    >
    (st:&mut A, dir_list:Dir2, list:L::List) -> T::Tree {
      let tnil = T::nil(st);
      let lnil = L::nil(st);
      let (tree, list) = tree_of_treelist_rec2::<A,X,T,L>(st, dir_list, list, tnil);
      if list == lnil { tree }
      else {
        panic!("Left over list: {:?}", list);
      }
    }

pub fn tree_of_treelist_rec2 
    < A:Adapton
    , X:Hash+Clone+Eq+Debug
    , T:TreeT<A,X>
    , L:TreeListT<A,X,T>
    >
    (st:&mut A, dir_list:Dir2, list:L::List, tree0:T::Tree)
     -> (T::Tree, L::List) {
       let tnil = T::nil(st);
       let lnil = L::nil(st);
       let (tree1, list1) = tree_of_treelist_rec::<A,X,T,L>(st, dir_list.clone(), list, tnil.clone(), T::lev_zero(), T::lev_max_val());
       if list1 == lnil {
         let tree01 = match dir_list {
           Dir2::Left  => tree_append::<A,X,T>(st, tree0, tree1),
           Dir2::Right => tree_append::<A,X,T>(st, tree1, tree0),
         } ;
         (tree01, list1)
       }
       else {
         let (tree2, list2) = tree_of_treelist_rec::<A,X,T,L>(st, dir_list.clone(), list1, tnil, T::lev_zero(), T::lev_max_val());
         let tree3 =
           match dir_list {
             Dir2::Left  => tree_append::<A,X,T>(st, tree1, tree2),
             Dir2::Right => tree_append::<A,X,T>(st, tree2, tree1),
           } ;
         return tree_of_treelist_rec2::<A,X,T,L>(st, dir_list, list2, tree3)
       }
     }

pub fn tree_of_treelist_rec
    < A:Adapton
    , X:Hash+Clone+Eq+Debug
    , T:TreeT<A,X>
    , L:TreeListT<A,X,T>
    >
    (st:&mut A,
     dir_list:Dir2, list:L::List,
     tree:T::Tree, tree_lev:T::Lev, parent_lev:T::Lev)
     -> (T::Tree, L::List)
{
  L::tree_elim_move (
    st, list, (dir_list, tree, tree_lev, parent_lev),

    /* Tree */
    |st, tree2, dir_tree2, rest,
    /* Accums: */ (dir_tree1, tree1, tree1_lev, parent_lev)| {
      assert!( dir_tree1 == dir_tree2 );
      let tree2_lev = T::lev_of_tree(st, &tree2);
      if T::lev_lte ( &tree2_lev , &parent_lev ) {
        let tree3 =
          match dir_tree1 {
            Dir2::Left  => tree_append::<A,X,T>(st, tree1, tree2),
            Dir2::Right => tree_append::<A,X,T>(st, tree2, tree1),
          } ;
        // !!! using this tree level may end up failing the invariant that rest == Nil at completion
        let tree3_lev = T::lev_of_tree(st, &tree3);
        // !!! XXX Using this tree_lev is not quite right for maintaining balance;
        // !!! XXX The level may be affected by the append on the prior line.
        let (tree4, rest2) =
          tree_of_treelist_rec::<A,X,T,L>(st, dir_tree1.clone(), rest, tree3, tree3_lev, parent_lev.clone()) ;

        let tree4_lev = T::lev_of_tree(st, &tree4);
        return tree_of_treelist_rec::<A,X,T,L>(st, dir_tree1, rest2, tree4, tree4_lev, parent_lev)
      }
      else {
        // If: tree2_lev > parent_lev
        (tree1, L::tree(st,tree2,dir_tree2,rest))
      }
    },
    
    /* Nil */
    |st, (_dir_list, tree, _, _)| (tree, L::nil(st)),

    /* Cons */
    |st, hd, rest, (dir_list, tree, tree_lev, parent_lev)| {
      let lev_hd = T::lev_inc ( &T::lev(&hd) ) ;
      if T::lev_lte ( &tree_lev , &lev_hd ) && T::lev_lte ( &lev_hd , &parent_lev ) {
        let leaf = T::leaf(st, hd) ;
        let (tree2, rest2) = {
          tree_of_treelist_rec::<A,X,T,L> ( st, dir_list.clone(), rest, leaf, T::lev_zero(), lev_hd.clone() )
        };
        let tree3 = match dir_list.clone() {
          Dir2::Left  => T::bin ( st, lev_hd.clone(), tree,  tree2 ),
          Dir2::Right => T::bin ( st, lev_hd.clone(), tree2, tree  ),
        } ;
        return tree_of_treelist_rec::<A,X,T,L> ( st, dir_list, rest2, tree3, lev_hd, parent_lev )
      }
      else {
        // If: lev_hd > parent_lev \/ tree > lev_hd
        (tree, L::cons(st,hd,rest))
      }},

    /* Name */
    |st, nm, rest, (dir_list, tree, tree_lev, parent_lev)|{
      let lev_nm = T::lev_inc( &T::lev_add( &T::lev_bits() , &T::lev(&nm) ) ) ;
      if T::lev_lte ( &tree_lev , &lev_nm ) && T::lev_lte ( &lev_nm ,  &parent_lev ) {
        let nil = T::nil(st) ;
        let (nm1, nm2, nm3, nm4) = st.name_fork4(nm.clone());
        let (tree2, rest) =
          tree_of_treelist_rec::<A,X,T,L>(st, dir_list.clone(), rest, nil, T::lev_zero(), lev_nm.clone());
        let tree3 = match dir_list.clone() {
          Dir2::Left  => T::name ( st, nm.clone(), lev_nm.clone(), tree,  tree2 ),
          Dir2::Right => T::name ( st, nm.clone(), lev_nm.clone(), tree2, tree  ),
        } ;
        let art = st.cell(nm3, tree3) ;
        let art = st.read_only( art ) ;
        let tree3 = T::art( st, art ) ;
        return tree_of_treelist_rec::<A,X,T,L>(st, dir_list.clone(), rest, tree3, lev_nm, parent_lev)
      }
      else {
        // If: lev_nm > parent_lev \/ tree_lev > lev_nm
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
    let tree = tree_of_list::<A,X,T,L>(st, Dir2::Left, list);
    tree_merge_sort::<A,X,L,T>(st, tree)
}

pub fn tree_merge_sort<A:Adapton,X:Ord+Hash+Debug+Clone,L:ListT<A,X>,T:TreeT<A,X>>
    (st:&mut A, tree:T::Tree) -> L::List
{
    T::fold_up (st, tree,
                &|st|                 L::nil(st),
                &|st, x|              L::singleton(st, x),
                &|st, _, left, right| { list_merge::<A,X,L>(st, None, left, None, right) },
                &|st, n, _, left, right| { let (n1,n2) = st.name_fork(n);
                                           list_merge::<A,X,L>(st, Some(n1), left, Some(n2), right) },
                )
}

pub fn tree_append__simple_join
    < A:Adapton
    , X:Hash+Clone+Debug
    , T:TreeT<A,X>
    >
    (st:&mut A,tree1:T::Tree,tree2:T::Tree) -> T::Tree
{
  // XXX: This is a hack. Make this balanced, a la Pugh 1989.
  if      T::is_empty(st, tree1.clone()) { tree2 }
  else if T::is_empty(st, tree2.clone()) { tree1 }
  else    { T::bin(st, T::lev_max_val(), tree1, tree2) }
}



pub fn tree_append__Pugh_join<
  A:Adapton,
  X:Clone+Hash+Eq+Debug,
  T:TreeT<A,X>
>(st:&mut A, tree1:T::Tree, tree2:T::Tree) -> T::Tree {
  fn merge4<
    A:Adapton,
    X:Clone+Hash+Eq+Debug,
    T:TreeT<A,X>
  >(
    st:&mut A,
    n1:Option<A::Name>, lev1:T::Lev, l1:T::Tree, r1:T::Tree,
    n2:Option<A::Name>, lev2:T::Lev, l2:T::Tree, r2:T::Tree
  ) -> T::Tree {
    let levl1 = T::lev_of_tree(st, &l1);
    let levr1 = T::lev_of_tree(st, &r1);
    let levl2 = T::lev_of_tree(st, &l2);
    let levr2 = T::lev_of_tree(st, &r2);
    let lev   = T::lev_max(&lev1, &lev2);
    if T::lev_lte(&levr1, &levr2) && T::lev_lte(&levl2, &levr2) {
      let tree1 = if let Some(n) = n1 {
        T::name(st, n, lev1, l1, r1)
      } else {
        T::bin(st, lev1, l1, r1)
      };
      let tree1 = tree_append::<A,X,T>(st, tree1, l2);
      if let Some(n) = n2 {
        T::name(st, n, lev, tree1, r2)
      } else {
        T::bin(st, lev, tree1, r2) 
      }
    } else if T::lev_lte(&levr1, &levl2) && !T::lev_lte(&levl2, &levr2) {
      let tree1 = if let Some(n) = n1 {
        T::name(st, n, lev1, l1, r1)
      } else {
        T::bin(st, lev1, l1, r1)
      };
      let tree2 = if let Some(n) = n2 {
        T::name(st, n, lev2, l2, r2)
      } else {
        T::bin(st, lev2, l2, r2)
      };
      T::bin(st, lev, tree1, tree2) 
    } else {
      let tree2 = if let Some(n) = n2 {
        T::name(st, n, lev2, l2, r2)
      } else {
        T::bin(st, lev2, l2, r2)
      };
      let tree2 = tree_append::<A,X,T>(st, r1, tree2);
      if let Some(n) = n1 {
        T::name(st, n, lev, l1, tree2)
      } else {
        T::bin(st, lev, l1, tree2)
      }
    }
  }; //end fn merge  
  T::elim_move(
    st, tree1, tree2,
    /* Nil */  |_, tree2| tree2,
    /* Leaf */ |st,leaf,  tree2| {
      T::elim_move(
        st, tree2, leaf,
        /* Leaf-Nil */  |st, leaf1| T::leaf(st,leaf1),
        /* Leaf-Leaf */ |st, leaf2, leaf1| {
          let lev1 = T::lev(&leaf1);
          let lev2 = T::lev(&leaf2);
          let lev = T::lev_max(&lev1,&lev2) ;
          let l1 = T::leaf(st,leaf1);
          let l2 = T::leaf(st,leaf2);
          T::bin(st, lev, l1, l2)
        },
        /* Leaf-Bin */  |st, lev2, l2, r2, leaf1| {
          let levl2 = T::lev_of_tree(st, &l2);
          let levr2 = T::lev_of_tree(st, &r2);
          if T::lev_lte(&levl2, &levr2) {
            let l1 = T::leaf(st,leaf1);
            let tree1 = tree_append::<A,X,T>(st, l1, l2);
            let tree1lev = T::lev_of_tree(st, &tree1);
            let lev = T::lev_max(&tree1lev, &levr2);
            T::bin(st, lev, tree1, r2)
          } else {
            let lev1 = T::lev(&leaf1) ;
            let lev  = T::lev_max(&lev1, &lev2);
            let l    = T::leaf(st,leaf1);
            let r    = T::bin(st, lev2, l2, r2);
            T::bin(st, lev, l, r)
          }
        },
        /* Leaf-Name */ |st, nm2, lev2, l2, r2, leaf1| {
          let levl2 = T::lev_of_tree(st, &l2);
          let levr2 = T::lev_of_tree(st, &r2);
          if T::lev_lte(&levl2, &levr2) {
            let l1 = T::leaf(st,leaf1);
            let tree1 = tree_append::<A,X,T>(st, l1, l2);
            let tree1lev = T::lev_of_tree(st, &tree1);
            let lev = T::lev_max(&tree1lev, &levr2);
            T::name(st, nm2, lev, tree1, r2)
          } else {
            let lev1 = T::lev(&leaf1) ;
            let lev  = T::lev_max(&lev1, &lev2);
            let l    = T::leaf(st,leaf1);
            let r    = T::name(st, nm2, lev2, l2, r2);
            T::bin(st, lev, l, r)
          }
        }
      )
    }, /* end Leaf */
    /* Bin */ |st,lev1,l1,r1, tree2| {
      let bin_data = (lev1,l1,r1) ;
      T::elim_move(
        st, tree2, bin_data,
        /* Bin-Nil */  |st, (lev1,l1,r1)| T::bin(st,lev1,l1,r1),
        /* Bin-Leaf */ |st, leaf2, (lev1,l1,r1)| {
          let tree2 = T::leaf(st, leaf2);
          let lev2  = T::lev_of_tree(st, &tree2);
          let levr1 = T::lev_of_tree(st, &r1);
          let lev   = T::lev_max(&lev1, &lev2);
          if T::lev_lte(&levr1, &lev2) {
            let tree1 = T::bin(st, lev1, l1, r1);
            T::bin(st, lev, tree1, tree2)
          } else {
            let tree2 = tree_append::<A,X,T>(st, r1, tree2);
            T::bin(st, lev, l1, tree2)
          }
        },
        /* Bin-Bin */  |st, lev2, l2, r2, (lev1, l1, r1)| {
          merge4::<A,X,T>(st, None, lev1, l1, r1, None, lev2, l2, r2)
        },
        /* Bin-Name */ |st, n2, lev2, l2, r2, (lev1, l1, r1) | {
          merge4::<A,X,T>(st, None, lev1, l1, r1, Some(n2), lev2, l2, r2)
        }
      )
    }, /* end Bin */
    /* Name */ |st, n1, lev1, l1, r1, tree2| {
      let name_data = (n1,lev1,l1,r1) ;
      T::elim_move(
        st, tree2, name_data,
        /* Name-Nil */  |st, (n1,lev1,l1,r1)| T::name(st,n1,lev1,l1,r1),
        /* Name-Leaf */ |st, leaf2, (n1,lev1,l1,r1)| {
          let tree2 = T::leaf(st, leaf2);
          let lev2  = T::lev_of_tree(st, &tree2);
          let levr1 = T::lev_of_tree(st, &r1);
          let lev   = T::lev_max(&lev1, &lev2);
          if T::lev_lte(&levr1, &lev2) {
            let tree1 = T::name(st, n1, lev1, l1, r1);
            T::bin(st, lev, tree1, tree2)
          } else {
            let tree2 = tree_append::<A,X,T>(st, r1, tree2);
            T::name(st, n1, lev, l1, tree2)
          }
        },
        /* Name-Bin */  |st, lev2, l2, r2, (n1, lev1, l1, r1)| {
          merge4::<A,X,T>(st, Some(n1), lev1, l1, r1, None, lev2, l2, r2)
        },
        /* Name-Name */ |st, n2, lev2, l2, r2, (n1, lev1, l1, r1) | {
          merge4::<A,X,T>(st, Some(n1), lev1, l1, r1, Some(n2), lev2, l2, r2)
        }
      )
    }
  )
}

pub fn tree_append<
  A:Adapton,
  X:Clone+Hash+Eq+Debug,
  T:TreeT<A,X>
>(st:&mut A, tree1:T::Tree, tree2:T::Tree) -> T::Tree {
  T::elim_move(
    st, tree1, tree2,
    /* Nil */  |_, tree2| tree2,
    /* Leaf */ |st,leaf,  tree2| {
      T::elim_move(
        st, tree2, leaf,
        /* Leaf-Nil */  |st, leaf1| T::leaf(st,leaf1),
        /* Leaf-Leaf */ |st, leaf2, leaf1| {
          let lev1 = T::lev(&leaf1);
          let lev2 = T::lev(&leaf2);
          let lev = T::lev_max(&lev1,&lev2) ;
          let l1 = T::leaf(st,leaf1);
          let l2 = T::leaf(st,leaf2);
          T::bin(st, lev, l1, l2)
        },
        /* Leaf-Bin */  |st, lev2, l2, r2, leaf1| {
          let lev1 = T::lev(&leaf1);
          if T::lev_lte(&lev2, &lev1) {
            let lev  = T::lev_max(&lev1, &lev2);
            let l    = T::leaf(st,leaf1);
            let r    = T::bin(st, lev2, l2, r2);
            T::bin(st, lev, l, r) // xxx count me
          } else {
            let l1 = T::leaf(st,leaf1);
            let tree1 = tree_append::<A,X,T>(st, l1, l2);
            let tree1lev = T::lev_of_tree(st, &tree1);
            let levr2 = T::lev_of_tree(st, &r2);
            let lev = T::lev_max(&tree1lev, &levr2);
            T::bin(st, lev, tree1, r2)
          }
        },
        /* Leaf-Name */ |st, n2, lev2, l2, r2, leaf1| {
          let lev1 = T::lev(&leaf1);
          let l1 = T::leaf(st,leaf1);
          if T::lev_lte(&lev2, &lev1) {
            let lev  = T::lev_max(&lev1, &lev2);
            let r    = T::name(st, n2, lev2, l2, r2);
            T::bin(st, lev, l1, r) // xxx panic
          } else {
            let tree1 = cell_call!(st, n2.clone() =>> tree_append::<A,X,T>, tree1:l1, tree2:l2);
            let tree1 = T::art(st, tree1);
            let tree1lev = T::lev_of_tree(st, &tree1);
            let levr2 = T::lev_of_tree(st, &r2);
            let lev = T::lev_max(&tree1lev, &levr2);
            T::name(st, n2, lev, tree1, r2)
          }
        }
      )
    }, /* end Leaf */
    /* Bin */ |st,lev1,l1,r1, tree2| {
      let bin_data = (lev1,l1,r1) ;
      T::elim_move(
        st, tree2, bin_data,
        /* Bin-Nil */  |st, (lev1,l1,r1)| T::bin(st,lev1,l1,r1),
        /* Bin-Leaf */ |st, leaf2, (lev1,l1,r1)| {
          let tree2 = T::leaf(st, leaf2);
          let lev2  = T::lev_of_tree(st, &tree2);
          if T::lev_lte(&lev2, &lev1) {
            let tree2 = tree_append::<A,X,T>(st, r1, tree2);
            T::bin(st, lev1, l1, tree2)
          } else {
            let lev   = T::lev_max(&lev1, &lev2);
            let tree1 = T::bin(st, lev1, l1, r1);
            T::bin(st, lev, tree1, tree2) // xxx count me
          }
        },
        /* Bin-Bin */  |st, lev2, l2, r2, (lev1, l1, r1)| {
          if T::lev_lte(&lev2, &lev1) {
            let tree2 = T::bin(st, lev2, l2, r2);
            let r = tree_append::<A,X,T>(st, r1, tree2);
            T::bin(st, lev1, l1, r)
          } else {
            let tree1 = T::bin(st, lev1, l1, r1);
            let l = tree_append::<A,X,T>(st, tree1, l2);
            T::bin(st, lev2, l, r2)
          }
        },
        /* Bin-Name */ |st, n2, lev2, l2, r2, (lev1, l1, r1) | {
          if T::lev_lte(&lev2, &lev1) {
            let tree2 = T::name(st, n2, lev2, l2, r2);
            let r = tree_append::<A,X,T>(st, r1, tree2);
            T::bin(st, lev1, l1, r)
          } else {
            let tree1 = T::bin(st, lev1, l1, r1);
            let l = cell_call!(st, n2.clone() =>> tree_append::<A,X,T>, tree1:tree1, tree2:l2);
            let l = T::art(st, l);
            T::name(st, n2, lev2, l, r2)
          }
        }
      )
    }, /* end Bin */
    /* Name */ |st, n1, lev1, l1, r1, tree2| {
      let name_data = (n1,lev1,l1,r1) ;
      T::elim_move(
        st, tree2, name_data,
        /* Name-Nil */  |st, (n1,lev1,l1,r1)| T::name(st,n1,lev1,l1,r1),
        /* Name-Leaf */ |st, leaf2, (n1,lev1,l1,r1)| {
          let tree2 = T::leaf(st, leaf2);
          let lev2  = T::lev_of_tree(st, &tree2);
          if T::lev_lte(&lev2, &lev1) {
            let tree2 = cell_call!(st, n1.clone() =>> tree_append::<A,X,T>, tree1:r1, tree2:tree2);
            let tree2 = T::art(st, tree2);
            T::name(st, n1, lev1, l1, tree2)
          } else {
            let lev   = T::lev_max(&lev1, &lev2);
            let tree1 = T::name(st, n1, lev1, l1, r1);
            panic!("Name-Leaf");
            T::bin(st, lev, tree1, tree2) // panic
          }
        },
        /* Name-Bin */  |st, lev2, l2, r2, (n1, lev1, l1, r1)| {
          if T::lev_lte(&lev2, &lev1) {
            let tree2 = T::bin(st, lev2, l2, r2);
            let r = cell_call!(st, n1.clone() =>> tree_append::<A,X,T>, tree1:r1, tree2:tree2);
            let r = T::art(st, r);
            T::name(st, n1, lev1, l1, r)
          } else {
            let tree1 = T::name(st, n1, lev1, l1, r1);
            let l = tree_append::<A,X,T>(st, tree1, l2);
            panic!("Name-Bin");
            T::bin(st, lev2, l, r2) // panic
          }
        },
        /* Name-Name */ |st, n2, lev2, l2, r2, (n1, lev1, l1, r1) | {
          if T::lev_lte(&lev2, &lev1) {
            let tree2 = T::name(st, n2, lev2, l2, r2);
            let r = cell_call!(st, n1.clone() =>> tree_append::<A,X,T>, tree1:r1, tree2:tree2);
            let r = T::art(st, r);
            T::name(st, n1, lev1, l1, r)
           } else {
            let tree1 = T::name(st, n1, lev1, l1, r1);
            let l = cell_call!(st, n2.clone() =>> tree_append::<A,X,T>, tree1:tree1, tree2:l2);
            let l = T::art(st, l);
            T::name(st, n2, lev2, l, r2) 
          }
        }
      )
    } /* end Name */
  )
}


#[cfg(test)]
mod test {
  use super::*;
  use adapton::adapton_sigs::* ;
  use adapton::collection_traits::*;
  use adapton::collection_edit::*;
  use adapton::collection::*;
  use adapton::engine;
  use adapton::naive;
  use std::fmt::Debug;
  use std::hash::Hash;
  use gm::GMLog;

  fn lev_name<A:Adapton,T:TreeT<A,usize>>(nm:&A::Name) -> T::Lev {
    T::lev_inc( &T::lev_add( &T::lev_bits() , &T::lev(nm) ) )
  }
  
  fn doit<A:Adapton>(st:&mut A) {
    let size = 32;
    let mut v1 : Vec<NameOrElem<A::Name,usize>> = vec![];
    let mut v2 : Vec<NameOrElem<A::Name,usize>> = vec![];
    for i in 0..(size/2) {
      let n = st.name_of_usize(i);
      println!("lev({:?})={:?}", &n, lev_name::<A,Tree<A,usize,u32>>(&n));
      v1.push(NameOrElem::Name(n));
      v1.push(NameOrElem::Elem(i));      
    }
    for i in (size/2)..size {
      let n = st.name_of_usize(i);
      println!("lev({:?})={:?}", &n, lev_name::<A,Tree<A,usize,u32>>(&n));
      v2.push(NameOrElem::Name(n));
      v2.push(NameOrElem::Elem(i));
    }
    println!("v1: {:?}", v1);
    println!("v2: {:?}", v2);
    let l1 = list_of_vec_w_names::<A,usize,List<A,usize>>(st, v1.clone());
    let l2 = list_of_vec_w_names::<A,usize,List<A,usize>>(st, v2.clone());

    println!("l1: {}", List::get_string(st, l1.clone()));
    println!("l2: {}", List::get_string(st, l2.clone()));

    let t1 = st.structural(|st|tree_of_list::<A,usize,Tree<A,usize,u32>,List<A,usize>>(st, Dir2::Left, l1));
    let t2 = st.structural(|st|tree_of_list::<A,usize,Tree<A,usize,u32>,List<A,usize>>(st, Dir2::Left, l2));

    println!("t1: {}", Tree::get_string(st, t1.clone()));
    println!("t2: {}", Tree::get_string(st, t2.clone()));

    v1.append(&mut v2);
    let l3 = list_of_vec_w_names::<A,usize,List<A,usize>>(st, v1);
    let t3 = st.structural(|st|tree_of_list::<A,usize,Tree<A,usize,u32>,List<A,usize>>(st, Dir2::Left, l3));
    
    let t4 = st.structural(|st|tree_append::<A,usize,Tree<A,usize,u32>>(st, t1, t2));
    
    println!("t3: {}", Tree::get_string(st, t3.clone()));
    println!("t4: {}", Tree::get_string(st, t4.clone()));

    t3.log_snapshot(st, "t3", None);
    t4.log_snapshot(st, "t4", None);
  }

  #[test]
  fn doit2() {
    let mut st = engine::Engine::new();
    doit(&mut st)
  }
}
