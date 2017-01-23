use std::fmt::Debug;
use std::hash::Hash;
//use std::marker::PhantomData;
//use std::num::Zero;
use std::rc::Rc;

//use rand::{Rng,Rand};

use macros::* ;
use adapton::engine::* ;

pub mod trie {
  pub use trie::*;
}

#[derive(Clone,Copy,Hash,Eq,PartialEq,Debug)]
pub enum Dir2 { Left, Right }

trait Invert { fn invert(&self) -> Self; }
impl Invert for Dir2 {
  fn invert(&self) -> Self {
    match *self { Dir2::Left => Dir2::Right,
                  Dir2::Right => Dir2::Left }
  }
}

/// Types that can be created like a list of `X` are `ListIntro<X>`
pub trait ListIntro<X> : Debug+Clone+Hash+PartialEq+Eq {
  /// Introduce an empty list
  fn nil  () -> Self ;
  /// Introduce a Cons cell
  fn cons (X, Self) -> Self ;  
  /// Introduce a Name "cons" cell
  fn name (Name, Self) -> Self ;
  /// Introduce a list with an articulation that holds a list
  fn art  (Art<Self>) -> Self ;

  /// Creates a singleton list. Derived from `cons` and `nil` introduction forms.
  fn singleton (hd:X) -> Self {
    let nil = Self::nil();
    Self::cons(hd, nil)
  }

  /// For `Some(nm)`, wraps the given list in `name` and `art` constructors for given name `nm`.
  /// For `None`, is the list identity function.
  fn name_art(nm:Option<Name>, rest:Self) -> Self {
    match nm {
      None => rest,
      Some(nm) => Self::name(nm.clone(), Self::art(cell(nm, rest)))
    }}
}

// Generate a list, given a generator function from naturals to list elements.
// Useful for generating test inputs.
pub fn list_gen<X,G,L:ListIntro<X>>
  (len:usize, gen_elm:G) -> L
  where G:Fn(usize) -> X {
    let mut out : L = L::nil();
    for i in len-1..0 {
      let x = gen_elm(i);
      let nm = name_of_usize(i);
      out = L::art(cell(nm.clone(), out));
      out = L::name(nm, out);
      out = L::cons(x,  out);
    }
    return out
}

/// Types that can be pattern-matched like a list of `X` are `ListElim<X>`.
/// We consider iterators to be a similar (nearly analogous) trait.
/// The key distinction here are that list elimination is a pattern-match used with (pure) recursion,
/// as opposed to an imperative for-loop, as is typical of iteration;
/// further, lists in Adapton contain data (of type `X`) and names (of type `Name`).
pub trait ListElim<X> : Debug+Clone+Hash+PartialEq+Eq {
  /// Eliminate a list with the given functions (for the pattern match
  /// arms) that handle the `nil`, `cons` and `name` cases.
  /// Eliminates the `art` case internally, by forcing the art and
  /// eliminating the resulting list with the given handler functions;
  /// forces multiple `art` cases, if need be.
  fn elim<Res,NilF,ConsF,NameF> (&Self, NilF, ConsF, NameF) -> Res
    where NilF:FnOnce(       &Self) -> Res
    ,    ConsF:FnOnce(&X,    &Self) -> Res
    ,    NameF:FnOnce(&Name, &Self) -> Res ;

  /// Like `elim`, except that the functions are given an additional
  /// argument.  This variant is needed due to the move semantics of
  /// Rust. The argument is moved into the body of the activated
  /// handler function when it is applied.
  fn elim_arg<Arg,Res,NilF,ConsF,NameF> (Self, Arg, NilF, ConsF, NameF) -> Res
    where NilF:FnOnce(      Self, Arg) -> Res
    ,    ConsF:FnOnce(X,    Self, Arg) -> Res
    ,    NameF:FnOnce(Name, Self, Arg) -> Res ;

  /// Tests if the list contains any `cons` cells. Derived from `elim`.
  fn is_empty (list:&Self) -> bool {
    Self::elim(list,
               |_|      true,
               |_,_|   false,
               |_,tl| Self::is_empty(tl))
  }

  /// Tests if the head of the list consists of a `name` constructor. Derived from `elim`.
  fn is_name (list:&Self) -> bool {
    Self::elim(list,
               |_|   false,
               |_,_| true,
               |_,_| false)
  }
}

pub fn list_nil<X, L:ListIntro<X>>()          -> L { L::nil() }
pub fn list_cons<X, L:ListIntro<X>>(x:X, l:L) -> L { L::cons(x, l) }
pub fn list_name<X, L:ListIntro<X>>(n:Name, l:L) -> L { L::name(n, l) }
pub fn list_art<X, L:ListIntro<X>>(l:Art<L>) -> L { L::art(l) }
pub fn list_name_art_op<X, L:ListIntro<X>>(n:Option<Name>, l:L) -> L {
  match n {
    None => l,
    Some(n) => {
      let c : Art<L> = cell(n.clone(), l);
      list_name(n, list_art(c))
    }
  }
}
pub fn list_name_op<X, L:ListIntro<X>>(n:Option<Name>, l:L) -> L {
  match n {
    None => l,
    Some(n) => {
      list_name(n, l)
    }
  }
}

/// Lazily maps the list, guided by names in input list.
/// Creates lazy named thunks in output for each name in input.
pub fn list_map_lazy<X, Le:'static+ListElim<X>, 
                     Y, Li:'static+ListIntro<Y>, 
                     F:'static>
  (l:Le, body:Rc<F>) -> Li
 where F:Fn(X) -> Y
{
  Le::elim_arg
    (l, body,
     |_,_| list_nil(),
     |x, tl, body| {
       let y = body.clone() (x);
       list_cons(y, list_map_lazy(tl, body))
     },
     |n, tl, body| {
       // We are lazy only when we encounter a name in the input
       // The name n is written with a recursive call to list_map_lazy, using argument tl, and additionally, passing body.
       // Note the `;;`. This means that the DCG does not check body for equality when dirtying the DCG. 
       // (We have no equality test for functions, anyway).
       list_art(thunk!( n =>> list_map_lazy =>> <X, Le, Y, Li, F>, l:tl ;; body:body.clone() ))
     })
}

/// Lazily filters the list, guided by names in input list.
/// Creates lazy named thunks in output for each name in input.
pub fn list_filter_lazy<X, Le:'static+ListElim<X>, Li:'static+ListIntro<X>, F:'static>
  (l:Le, body:Rc<F>) -> Li
 where F:Fn(&X) -> bool
{
  Le::elim_arg
    (l, body,
     |_,_| list_nil(),
     |x, tl, body| {
       if body.clone() (&x) {
         list_cons(x, list_filter_lazy(tl, body))
       } else {
         list_filter_lazy(tl, body)
       }
     },
     |n, tl, body| {
       list_art(thunk!( n =>> list_filter_lazy =>> <X, Le, Li, F>, l:tl ;; body:body.clone() ))
     })
}

/// Eagerly filters the list, guided by names in input list.
/// Memoizes recursion for each name in input.
pub fn list_filter_eager<X, Le:'static+ListElim<X>, Li:'static+ListIntro<X>, F:'static>
  (l:Le, body:Rc<F>) -> Li
 where F:Fn(&X) -> bool
{
  Le::elim_arg
    (l, body,
     |_,_| list_nil(),
     |x, tl, body| {
       if body.clone() (&x) {
         list_cons(x, list_filter_eager(tl, body))
       } else {
         list_filter_eager(tl, body)
       }
     },
     |n, tl, body| {
       let (rest, _) = eager!( 
         n.clone() =>> list_filter_eager =>> 
           <X, Le, Li, F>, l:tl ;; body:body.clone() 
       );
       list_name(n, list_art(rest))
     })
}

/// Eagerly maps the list.
/// Uses (eager) memoization for each name in `l`.
pub fn list_map_eager<X, Le:'static+ListElim<X>, 
                      Y, Li:'static+ListIntro<Y>, 
                      F:'static>
  (l:Le, body:Rc<F>) -> Li
 where F:Fn(X) -> Y
{
  Le::elim_arg
    (l, body,
     |_,_| list_nil(),
     |x, tl, body| {
       let y = body.clone() (x);
       list_cons(y, list_map_eager(tl, body))
     },
     |n, tl, body| {
       // We only memoize when we encounter a name in the input
       let (t,_) = eager!( n.clone() =>> list_map_eager =>> <X, Le, Y, Li, F>, l:tl ;; body:body.clone() );
       list_name(n, list_art(t))
     })
}


/// Eagerly maps the list.
/// Uses (eager) memoization for each name in `l`.
pub fn list_reverse<X, Le:'static+ListElim<X>, Li:'static+ListIntro<X>>
  (l:Le, rev:Li) -> Li
{
  Le::elim_arg
    (l, rev,
     |_,rev| rev,
     |x, tl, rev| { list_reverse(tl, list_cons(x, rev)) },
     |n, tl, rev| {
       // We only memoize when we encounter a name in the input
       let rev = list_art( cell(name_pair(name_of_str("rev"),n.clone()), rev) );
       let (t,_) = eager!( n.clone() =>> list_reverse <X, Le, Li>, l:tl, rev:rev );
       list_name(n, list_art(t))
     })
}

/// Ignores names; performs no memoization; use tree_fold_* for lists that could be long.
pub fn list_fold<X,L:ListElim<X>,F,Res>(l:L, res:Res, body:Rc<F>) -> Res 
  where F:Fn(X,Res) -> Res 
{ 
  L::elim_arg
    (l, res,
     |_,res| res,
     |x, tl, res| {
       let res = list_fold(tl, res, body.clone());
       body(x,res)
     },
     |_, tl, res| list_fold(tl, res, body.clone()),
  )
}

pub fn list_is_empty<X, L:ListElim<X>>(stack:&L) -> bool {
  L::is_empty(stack)
}

pub fn list_pop<X,L:ListElim<X>>(stack:L) -> (X, L) {
  L::elim_arg(stack, (),
              |_,_|    panic!("cannot pop an empty stack"),
              |h,t, _| (h, t),
              |_,t, _| list_pop(t))
}

pub fn list_push<X, L:ListIntro<X>>(stack:L, elm:X) -> L {
  L::cons(elm, stack)
}

pub fn list_append<X, L:ListIntro<X>+ListElim<X>>(l1:L, l2:L) -> L {
  list_fold(l1, l2, Rc::new(|x,r| list_cons(x,r)))
}


/// Rose Trees: A tree with arbitrary branching at each node.
/// See also, Definition 2 (page 2) of
///   [*Parallel Implementation of Tree Skeletons*, by D.B. Skillicorn 1995.]
///   (http://ftp.qucis.queensu.ca/TechReports/Reports/1995-380.pdf)
pub trait RoseIntro<Leaf,Branch> : Debug+Clone+Hash+PartialEq+Eq {
  type List: ListElim<Self>;
  /// Introduce a leaf with exactly zero children
  fn leaf (Leaf) -> Self;
  /// Introduce a branch with zero or more subtrees
  fn branch (Branch, Self::List) -> Self;
  /// Introduce a Named subtree  
  fn name (Name, Self) -> Self ;
  /// Introduce a list with an articulation that holds a list
  fn art  (Art<Self>) -> Self ;
}

/// Rose Trees: A tree with arbitrary branching at each node.
/// See also, Definition 2 (page 2) of
///   [*Parallel Implementation of Tree Skeletons*, by D.B. Skillicorn 1995.]
///   (http://ftp.qucis.queensu.ca/TechReports/Reports/1995-380.pdf)
pub trait RoseElim<Leaf,Branch> : Debug+Clone+Hash+PartialEq+Eq {
  type Children: ListElim<Self>;
  fn elim<Arg, Res, LeafFn, BranchFn, NameFn>
    (Self, Arg, LeafFn, BranchFn, NameFn) -> Res
    where LeafFn:  FnOnce(Leaf,                   Arg) -> Res
    ,     BranchFn:FnOnce(Branch, Self::Children, Arg) -> Res
    ,     NameFn:  FnOnce(Name,   Self,           Arg) -> Res
    ;    
}

/// Levels for a probabilistically-balanced trees. For more details see
/// Pugh and Teiltelbaum's POPL 1989 paper, and its "Chunky List"
/// representation (*Incremental Computation via Function Caching*).
pub trait Level : Debug+Hash+PartialEq+Eq+Clone+'static {
  fn new<X:Hash>(&X) -> Self ;
  fn bits () -> Self ;
  fn zero () -> Self ;
  fn inc (&Self) -> Self ;
  fn add (&Self, &Self) -> Self ;
  fn lte (&Self, &Self) -> bool ;
  fn max_val () -> Self ;
  fn max (a:&Self, b:&Self) -> Self {
    if Self::lte(a, b) { b.clone() } else { a.clone() }
  }
}

/// Types that can be created like a (binary) tree with leaves of type `Leaf` are `TreeIntro<Leaf>`.
/// We recognize that monoids are a nearly-analogous case;
/// the key differences with monoids are that trees contain names (see `name` fn) and articulations (see `art` fn);
/// further, the binary cases `name` and `bin` carry levels of type `Lev`, which helps establish and maintain balance.
pub trait TreeIntro<Lev:Level,Leaf> : Debug+Hash+PartialEq+Eq+Clone+'static {
  fn nil  () -> Self ;
  fn leaf (Leaf) -> Self ;
  fn bin  (Lev, Self, Self) -> Self ;

  // requisite "adaptonic" constructors: `name` and `art`:
  fn name (Name, Lev, Self, Self) -> Self ;
  fn art  (Art<Self>) -> Self ;
}
  
pub trait TreeElim<Lev:Level,Leaf> : Debug+Hash+PartialEq+Eq+Clone+'static {
  fn lev_of_tree(&Self) -> Lev ;
  
  fn elim<Res,NilC,LeafC,BinC,NameC>
    (Self, NilC, LeafC, BinC, NameC) -> Res        
    where NilC  : FnOnce() -> Res
    ,     LeafC : FnOnce(Leaf) -> Res
    ,     BinC  : FnOnce(Lev,       Self, Self) -> Res
    ,     NameC : FnOnce(Name, Lev, Self, Self) -> Res
    ;

  fn elim_ref<Res,NilC,LeafC,BinC,NameC>
    (&Self, NilC, LeafC, BinC, NameC) -> Res
    where NilC  : FnOnce() -> Res
    ,     LeafC : FnOnce(&Leaf) -> Res
    ,     BinC  : FnOnce(&Lev,        &Self, &Self) -> Res
    ,     NameC : FnOnce(&Name, &Lev, &Self, &Self) -> Res
    ;

  fn elim_arg<Arg,Res,NilC,LeafC,BinC,NameC>
    (Self, Arg, NilC, LeafC, BinC, NameC) -> Res
    where NilC  : FnOnce(Arg) -> Res
    ,     LeafC : FnOnce(Leaf, Arg) -> Res
    ,     BinC  : FnOnce(Lev,       Self, Self, Arg) -> Res
    ,     NameC : FnOnce(Name, Lev, Self, Self, Arg) -> Res
    ;

  fn full_move<Arg,Res,NilC,LeafC,BinC,NameC,ArtC>
    (Self, Arg, NilC, LeafC, BinC, NameC, ArtC) -> Res
    where NilC  : FnOnce(Arg) -> Res
    ,     LeafC : FnOnce(Leaf, Arg) -> Res
    ,     BinC  : FnOnce(Lev,       Self, Self, Arg) -> Res
    ,     NameC : FnOnce(Name, Lev, Self, Self, Arg) -> Res
    ,     ArtC  : FnOnce(&Art<Self>, Arg) -> Res
    ;

  //fn get_string(l:Self) -> String ;
  
  // Derived from `elim` above:
  fn is_empty (tree:&Self) -> bool {
    Self::elim_ref
      (tree,
       ||        true,
       |_|       false,
       |_,l,r|   Self::is_empty(l) && Self::is_empty(r),
       |_,_,l,r| Self::is_empty(l) && Self::is_empty(r)
       )
  }

  fn is_nil (tree:&Self) -> bool {
    Self::elim_ref
      (tree,
       ||        true,
       |_|       false,
       |_,_,_|   false,
       |_,_,_,_| false,
       )
  }
}

pub trait MapIntro<Dom,Cod>
  : Debug+Hash+PartialEq+Eq+Clone+'static
{
  fn empty () -> Self;
  //fn extend<F> (self:Self, d:Dom, f:F) -> (Self, Option<Cod>)
  // where F:FnOnce(Option<Cod>) -> (Option<Cod>, Option<Cod>);
  fn update (map:Self, d:Dom, c:Cod) -> Self;
  //{
  //let (map, _) = self.extend(d,move|_|{(Some(c),None)});
  //map
  //}
  //{
  //let (map, _) = self.extend(d,move|_|{(None,None)});
  //map
  //}
}

pub trait MapElim<Dom,Cod>
  : Debug+Hash+PartialEq+Eq+Clone+'static

{
  fn find(&Self, d:&Dom) -> Option<Cod>;
  fn remove (Self, d:&Dom) -> (Self, Option<Cod>);
  fn fold<Res,F>(Self, Res, Rc<F>) -> Res where
    F:Fn(Dom, Cod, Res) -> Res;
  fn append(Self, other:Self) -> Self;
}

pub fn map_empty<Dom,Cod,M:MapIntro<Dom,Cod>>() -> M { M::empty() }
pub fn map_update<Dom,Cod,M:MapIntro<Dom,Cod>>(map:M, d:Dom, c:Cod) -> M { M::update(map, d, c) }
pub fn map_find<Dom,Cod,M:MapElim<Dom,Cod>>(map:&M, d:&Dom) -> Option<Cod> { M::find(map, d) }
pub fn map_fold<Dom,Cod,M:MapElim<Dom,Cod>,F,Res>(map:M, r:Res, f:Rc<F>) -> Res where F:Fn(Dom,Cod, Res) -> Res { M::fold(map, r, f) }

pub trait SetIntro<Elm>
  : Debug+Hash+PartialEq+Eq+Clone+'static
{
  fn empty  () -> Self;
  fn add    (Self, e:Elm) -> Self;
  fn remove (Self, e:&Elm) -> Self;
  fn union  (Self, Self) -> Self;
  fn inter  (Self, Self) -> Self;
  fn diff   (Self, Self) -> Self;
}

impl<Elm,Map:MapIntro<Elm,()>+MapElim<Elm,()>> SetIntro<Elm> for Map {
  fn empty  () -> Self { Map::empty() }
  fn add    (set:Self, x:Elm) -> Self { Map::update(set, x, ()) }
  fn remove (set:Self, x:&Elm) -> Self { let (map, _) = Map::remove(set, x); map }
  fn union  (set:Self, other:Self) -> Self { Map::append(set, other) }
  fn inter (set:Self, other:Self) -> Self {
    let (out, _) =
    Map::fold
      (set, (Self::empty(), other), 
       Rc::new(|x, (), (out, other)|{
         let out = match Map::find(&other, &x) {
           Some (ref _unit) => Self::add(out, x),
           None => out
         };
         (out, other)
       })) ; 
    return out
  }
  fn diff (_set:Self, _other:Self) -> Self {
    unimplemented!() // Should be very similar to inter, above.
  }
}

pub trait SetElim<Elm>
  : Debug+Hash+PartialEq+Eq+Clone+'static  
{
  fn is_mem (set:&Self, e:&Elm) -> bool;
  fn fold<Res,F>(set:Self, Res, F) -> Res where
    F:Fn(Elm, Res) -> Res;
}

impl<Elm,Map:MapElim<Elm,()>> SetElim<Elm> for Map {
  fn is_mem (set:&Self, e:&Elm) -> bool {
    match Map::find(set, e) {
      Some (ref _unit) => true,
      None => false,
    }
  }
  fn fold<Res,F>(set:Self, res:Res, f:F) -> Res where
    F:Fn(Elm, Res) -> Res
  {
    Map::fold(set, res, Rc::new(|elm, (), res| f(elm, res)))
  }
}




  
fn bin_arts_niltest
  < Lev:Level, Leaf
  , T:TreeElim<Lev,Leaf>+TreeIntro<Lev,Leaf>+'static
  >
  (n:Option<Name>, lev:Lev, l:T, r:T) -> T
{
  if      T::is_nil(&l) { r }
  else if T::is_nil(&r) { l }
  else {
    match n {
      None    => T::bin(lev, l, r),
      Some(n) => {
        let (nl,nr) = name_fork(n.clone());
        let l = T::art(cell(nl, l));
        let r = T::art(cell(nr, r));
        T::name(n, lev, l, r)
    }}}
}

  
pub fn tree_fold_seq
  < Lev:Level, Leaf, T:TreeElim<Lev, Leaf>
  , Res:Hash+Debug+Eq+Clone+'static
  , LeafC:'static
  , BinC:'static
  , NameC:'static
  >
  (tree:T, dir:Dir2, res:Res,
   leaf:Rc<LeafC>,
   bin: Rc<BinC>,
   name:Rc<NameC>) -> Res
  where LeafC:Fn(Leaf,      Res ) -> Res
  ,      BinC:Fn(Lev,       Res ) -> Res 
  ,     NameC:Fn(Name, Lev, Res ) -> Res 
{
  T::elim_arg
    (tree, (res,(leaf,bin,name)),
     |      (res,_)              | res,
     |x,    (res,(leaf,_,_))     | leaf(x, res),
     |x,l,r,(res,(leaf,bin,name))| {
       let (l,r) = match dir { Dir2::Left => (l,r), Dir2::Right => (r, l) } ;
       let res = tree_fold_seq(l, dir, res, leaf.clone(), bin.clone(), name.clone());
       let res = (&bin)(x, res);
       let res = tree_fold_seq(r, dir, res, leaf, bin, name);
       res
     },
     |n,x,l,r,(res,(leaf,bin,name))| {
       let (l,r) = match dir { Dir2::Left => (l,r), Dir2::Right => (r, l) } ;
       let (n1,n2) = name_fork(n.clone());
       let res = memo!(n1 =>> tree_fold_seq, tree:l, dir:dir, res:res ;;
                       leaf:leaf.clone(), bin:bin.clone(), name:name.clone());
       let res = name(n, x, res);
       let res = memo!(n2 =>> tree_fold_seq, tree:r, dir:dir, res:res ;;
                       leaf:leaf, bin:bin, name:name);
       res
     }
     )
}

/// Fold over the structure of the tree, with results flowing up, from `nil` and `leaf` cases to the binary cases of `bin` and `name`.
/// This folding pattern is suitable for aggregating the leaf elements via an associative operation, such as a monoid (e.g., counting, addition, multiplication, maximum, minimum, etc.).
/// See `monoid_of_tree` for a wrapper function that offers this usage.
/// This folding pattern is also suitable for producing copies of the tree's structure.
pub fn tree_fold_up
  < Lev:Level, Leaf, T:TreeElim<Lev,Leaf>
  , Res:Hash+Debug+Eq+Clone+'static
  , NilF:'static
  , LeafF:'static
  , BinF:'static
  , NameF:'static
  >
  (tree:T,
   nil:Rc<NilF>,
   leaf:Rc<LeafF>,
   bin:Rc<BinF>,
   name:Rc<NameF>) -> Res
  where  NilF:Fn() -> Res
  ,     LeafF:Fn(Leaf                ) -> Res
  ,      BinF:Fn(Lev,       Res, Res ) -> Res
  ,     NameF:Fn(Name, Lev, Res, Res ) -> Res
{
  T::elim_arg
    (tree, (nil,leaf,bin,name),
     |(nil,_,_,_)| nil(),
     |x,(_,leaf,_,_)| leaf(x),
     |x,l,r,(nil,leaf,bin,name)| {
       let resl = tree_fold_up(l, nil.clone(), leaf.clone(), bin.clone(), name.clone());
       let resr = tree_fold_up(r, nil, leaf, bin.clone(), name);
       let res = bin(x, resl, resr);
       res
     },
     |n,x,l,r,(nil,leaf,bin,name)| {
       //let (n1,n2,n3) = name_fork3(n.clone()); // XXX/Todo: Do something better than fork3 up here
       let (n1, n2) = name_fork(n.clone());
       let resl = memo!(n1 =>> tree_fold_up, tree:l ;; nil:nil.clone(), leaf:leaf.clone(), bin:bin.clone(), name:name.clone());
       let resr = memo!(n2 =>> tree_fold_up, tree:r ;; nil:nil, leaf:leaf, bin:bin, name:name.clone());
       let res = name(n, x, resl, resr);
       res
     }
     )
}

/// Like `tree_fold_up`, except that names from `name` nodes are passed down, to the next `nil` and `leaf` cases.
/// The name from a `name` constructor associates to its right subtree, which is consistent with a left-to-right, in-order traversal of the tree.
/// The recursive argument `nm` provides the name for the left subtree, if any.
///
/// **Regarding naming effects** for the RHS functions:
/// The names consumed by the RHS functions for constructors `name`, `leaf` and `nil` overlap.
/// It is thus critical that, e.g., the RHS functions for `name` and `leaf` have disjoint write effects.
/// (Either the `name` RHS uses the given name, or the `leaf` RHS uses the name, but not both).
/// Similarly, it is thus critical that the RHS functions for `name` and `nil` have disjoint write effects.
pub fn tree_fold_up_nm_dn
  < Lev:Level, Leaf, T:TreeElim<Lev,Leaf>
  , Res:Hash+Debug+Eq+Clone+'static
  , NilF:'static
  , LeafF:'static
  , BinF:'static
  , NameF:'static
  >
  (tree:T, nm:Option<Name>,
   nil:Rc<NilF>,
   leaf:Rc<LeafF>,
   bin:Rc<BinF>,
   name:Rc<NameF>) -> Res
  where  NilF:Fn(Option<Name>        ) -> Res
  ,     LeafF:Fn(Option<Name>, Leaf  ) -> Res
  ,      BinF:Fn(Option<Name>, Lev, Res, Res ) -> Res
  ,     NameF:Fn(Option<Name>, Name, Lev, Res, Res ) -> Res
{
  T::elim_arg
    (tree, (nm,nil,leaf,bin,name),
     |(nm,nil,_,_,_)|    nil(nm),
     |x,(nm,_,leaf,_,_)| leaf(nm, x),
     |x,l,r,(nm,nil,leaf,bin,name)| {
       let resl = tree_fold_up_nm_dn(l, nm.clone(),   nil.clone(), leaf.clone(), bin.clone(), name.clone());
       let resr = tree_fold_up_nm_dn(r, None, nil,         leaf,         bin.clone(), name);
       let res = bin(nm, x, resl, resr); // TODO: Should `bin` function accept a name?
       res
     },
     |n,x,l,r,(nm,nil,leaf,bin,name)| {
       let (n1,n2) = name_fork(n.clone());
       let nm2 = Some(n.clone());
       let resl = memo!(n1 =>> tree_fold_up_nm_dn, tree:l, nm:nm.clone()  ;; nil:nil.clone(), leaf:leaf.clone(), bin:bin.clone(), name:name.clone());
       let resr = memo!(n2 =>> tree_fold_up_nm_dn, tree:r, nm:nm2 ;; nil:nil,         leaf:leaf,         bin:bin,         name:name.clone());
       let res = name(nm.clone(), n, x, resl, resr);
       res
     }
     )
}


pub fn tree_of_list
  < Lev:Level, X:Hash+Clone+Debug
  , T:TreeIntro<Lev,X>+'static
  , L:ListElim<X>+ListIntro<X>+'static
  >
  (dir_list:Dir2, list:L) -> T {
    let tnil = T::nil();
    let (tree, list) = tree_of_list_rec::<Lev,X,T,L>(dir_list, list, tnil, Lev::zero(), Lev::max_val());
    assert!(L::is_empty(&list));
    tree
  }

pub fn tree_of_list_rec
  < Lev:Level, X:Hash+Clone+Debug
  , T:TreeIntro<Lev,X>+'static
  , L:ListElim<X>+ListIntro<X>+'static
  >
  (dir_list:Dir2, list:L, tree:T, tree_lev:Lev, parent_lev:Lev) -> (T, L)
{
  L::elim_arg (
    list, (dir_list, tree, tree_lev, parent_lev),
    
    /* Nil */
    |nil,(_dir_list, tree, _, _)| (tree, nil),
    
    /* Cons */
    |hd, rest, (dir_list, tree, tree_lev, parent_lev)| {      
      let lev_hd = Lev::inc ( &Lev::new(&hd) ) ;
      if Lev::lte ( &tree_lev , &lev_hd ) && Lev::lte ( &lev_hd , &parent_lev ) {
        let leaf = T::leaf(hd) ;
        let (tree2, rest2) = {
          tree_of_list_rec( dir_list.clone(), rest, leaf, Lev::zero(), lev_hd.clone() )
        };
        let tree3 = match dir_list.clone() {
          Dir2::Left  => T::bin ( lev_hd.clone(), tree,  tree2 ),
          Dir2::Right => T::bin ( lev_hd.clone(), tree2, tree  ),
        } ;
        tree_of_list_rec( dir_list, rest2, tree3, lev_hd, parent_lev )
      }
      else {
        (tree, L::cons(hd,rest))
      }},
    
    /* Name */
    |nm:Name, rest, (dir_list, tree, tree_lev, parent_lev)|{
      let lev_nm = Lev::inc( &Lev::add( &Lev::bits() , &Lev::new(&nm) ) ) ;
      if Lev::lte ( &tree_lev , &lev_nm ) && Lev::lte ( &lev_nm ,  &parent_lev ) {
        let nil = T::nil() ;
        let (nm1, nm2) = name_fork(nm.clone());
        let (_, (tree2, rest)) =
          eager!(nm1 =>> tree_of_list_rec,
                 dir_list:dir_list.clone(), list:rest,
                 tree:nil, tree_lev:Lev::zero(), parent_lev:lev_nm.clone() ) ;
        let tree3 = match dir_list.clone() {
          Dir2::Left  => T::name ( nm.clone(), lev_nm.clone(), tree,  tree2 ),
          Dir2::Right => T::name ( nm.clone(), lev_nm.clone(), tree2, tree  ),
        } ;
        let art = cell(nm, tree3) ;
        let tree3 = T::art( art ) ;
        let (_, (tree, rest)) =
          eager!(nm2 =>> tree_of_list_rec,
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

/// List the leaf elements and names of a tree, in the given order, via a sequential, in-order traversal.
/// Direction `Dir2::Left` lists elements from left to right. (Leftmost elements are in the head of the list).
/// Direction `Dir2::Right` lists elements from right to left. (Rightmost elements are in the head of the list).
/// Preserves the order of elements, up to `dir`, and the names in the tree.
pub fn list_of_tree
  < Lev:Level,X:Hash+Clone
  , L:ListIntro<X>+'static
  , T:TreeElim<Lev,X>+'static>
  (tree:T, dir:Dir2) -> L
{
  let nil = L::nil();
  tree_fold_seq
    (tree, dir.invert(), nil,
     Rc::new(|x,xs| L::cons(x,xs)),
     Rc::new(|_,xs| xs),
     // TODO: Is this Name case OK? (Name-Art-Cons vs Name-Cons-Art patterns?)
     Rc::new(|n:Name,_,xs| L::name(n.clone(),L::art(cell(n,xs))))
     )
}

/// Filter the leaf elements of a tree using a user-provided predicate, `pred`.
/// Returns a list of the elements for which the predicate returns `true`.
/// Retains exactly one name between any two elements that, in the original sequence, were separated by a name.
/// Does not insert names that were not present in the original sequence.
pub fn filter_list_of_tree
  < Lev:Level, X:Hash+Clone+'static
  , L:ListIntro<X>+ListElim<X>+'static
  , T:TreeElim<Lev,X>+'static
  >
  (tree:T, pred:Box<Fn(&X) -> bool>) -> L
{
  let nil = L::nil();
  tree_fold_seq
    (tree, Dir2::Right, nil,
     Rc::new(move |x,xs| if pred(&x) { L::cons(x,xs) } else { xs }),
     Rc::new(|_,xs| xs),
     Rc::new(|n:Name,_,xs| if L::is_name(&xs) {xs} else {
       // TODO: Is this Name case OK? (Name-Art-Cons vs Name-Cons-Art patterns?)
       L::name(n.clone(),L::art(cell(n,xs)))
     }))
}

/// Filter the leaf elements of a tree using a user-provided predicate, `pred`.
/// Returns a tree of the elements for which the predicate returns `true`.
/// Retains all names from the original tree, even if they merely name empty sub-trees.
pub fn filter_tree_of_tree
  < Lev:Level, X:Hash+Clone+'static
  , Te:TreeElim<Lev,X>+'static
  , Ti:TreeIntro<Lev,X>+TreeElim<Lev,X>+'static
  >
  (tree:Te, pred:Box<Fn(&X) -> bool>) -> Ti
{
  tree_fold_up
    (tree,
     Rc::new(|| Ti::nil()),
     Rc::new(move |x|
             {  let fx = pred(&x);
                if !fx { Ti::nil()   }
                else   { Ti::leaf(x) } }),
     Rc::new(|lev,l,r|   ns(name_unit(),|| bin_arts_niltest(None, lev, l, r))),
     Rc::new(|n,lev,l,r| ns(name_unit(),|| bin_arts_niltest(Some(n), lev, l, r)))
     )
}

/// Aggregates the leaf elements of a tree using a user-defined
/// monoid.  The monoid consists of an identity element `id_elm` and
/// binary operation over leaf values `bin_op`.
/// Derived from `tree_fold_up`.
pub fn monoid_of_tree
  < Lev:Level, X:Debug+Eq+Hash+Clone+'static
  , Te:TreeElim<Lev,X>+'static
  >
  (tree:Te, id_elm:X, bin_op:Rc<Fn(X,X) -> X>) -> X
{
  let bin_op2 = bin_op.clone();
  tree_fold_up
    (tree,
     Rc::new(move ||        id_elm.clone()),
     Rc::new(     |x|       x),
     Rc::new(move |_,l,r|   bin_op (l,r)),
     Rc::new(move |_,_,l,r| bin_op2(l,r))
     )
}

/// Produces a tree with the same structure as its input, but without
/// any articulations.  Useful for `println`-style debugging, and for
/// equality comparisons across distinct engine implementations (e.g.,
/// to verify the DCG-based engine).
pub fn eager_tree_of_tree
  < Lev:Level, X:Hash+Clone+'static
  , Te:TreeElim<Lev,X>+'static
  , Ti:TreeIntro<Lev,X>+'static
  >
  (tree:Te) -> Ti
{
  tree_fold_up
    (tree,
     Rc::new(||          Ti::nil()),
     Rc::new(|x|         Ti::leaf(x)),
     Rc::new(|lev,l,r|   Ti::bin(lev, l, r)),
     Rc::new(|n,lev,l,r| Ti::name(n, lev, l, r))
     )
}

/// Produces a tree with the same structure as its input, but without
/// any empty subtrees, and with articulations placed around the
/// subtrees of named binary nodes.
pub fn prune_tree_of_tree
  < Lev:Level, X:Hash+Clone+'static
  , Te:TreeElim<Lev,X>+'static
  , Ti:TreeElim<Lev,X>+TreeIntro<Lev,X>+'static
  >
  (tree:Te) -> Ti
{
  tree_fold_up
    (tree,
     Rc::new(||          Ti::nil()),
     Rc::new(|x|         Ti::leaf(x)),
     Rc::new(|lev,l,r|   bin_arts_niltest(None, lev, l, r)),
     Rc::new(|n,lev,l,r| bin_arts_niltest(Some(n), lev, l, r))
     )
}

/// Calls `vec_of_list` with the given `demand`
pub fn list_demand<X:Clone,L:ListElim<X>+'static>
 (list:L, demand:usize) -> Vec<NameElse<X>>
{ 
  vec_of_list(list, Some(demand))
}

/// Attempts to force `limit` number of `Cons` cells of the list,
/// gathering these elements and any interposed `Name`s.
pub fn vec_of_list<X:Clone,L:ListElim<X>+'static>
  (list:L, limit:Option<usize>) -> Vec<NameElse<X>>
{
  let mut out = vec![];
  let mut list = list ;
  loop {
    let (hd, rest) =
      L::elim_arg(
        list, (),
        |_, _|         (None, None),
        |x, rest, _| { (Some(NameElse::Else(x.clone())), Some(rest)) },
        |n, rest, _| { (Some(NameElse::Name(n.clone())), Some(rest)) }
        ) ;
    match hd { Some(x) => out.push(x), _ => () } ;
    match limit { Some(limit) if out.len() == limit => return out, _ => () } ;
    match rest { Some(rest) => { list = rest; continue }, None => return out }
  }
}

// pub fn list_of_vec<X:Clone,L:ListT<X>> (v:Vec<X>) -> L::List {
//     let mut l = L::nil(st);
//     for x in v.iter().rev() { l = L::cons(st,x.clone(),l) }
//     return l
// }

/// Constructs a linked list that consists of elements and names, as
/// given by the input vector (in that order).
/// Not incremental; used only for setting up inputs for tests.
pub fn list_of_vec<X:Clone,L:ListIntro<X>>
  (v:&Vec<NameElse<X>>) -> L
{   
  let mut l = L::nil();
  for x in v.iter().rev() {
    l = match *x {
      // TODO: Is this Name case OK? (Name-Art-Cons vs Name-Cons-Art patterns?)
      NameElse::Name(ref nm) => L::name(nm.clone(),L::art(cell(nm.clone(),l))), 
      NameElse::Else(ref el) => L::cons(el.clone(),l),
    }}
  return l
}

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
//      tree:T::Tree, tree_lev:Lev, parent_lev:T::Lev)
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

/// Produce a lazy list that consists of merging two input lists.
/// The output is lazy to the extent that the input lists contain `name`s.
/// When the input lists are each sorted according to `Ord`; the output is sorted.
pub fn list_merge<X:Ord+Clone+Debug,L:ListIntro<X>+ListElim<X>+'static>
  (n1:Option<Name>, l1:L,
   n2:Option<Name>, l2:L ) -> L
{
  L::elim_arg
    (l1, (n1,n2,l2),
     /* Nil */  |_,(_n1, n2,l2)| list_name_op(n2, l2),
     /* Cons */ |h1,t1,(n1,n2,l2)|
     L::elim_arg
     (l2, (h1,t1,n1,n2),
      /* Nil */  |_,(h1, t1, n1, _n2 )| list_name_op(n1, L::cons(h1,t1)),
      /* Cons */ |h2, t2, (h1, t1, n1, n2)| {
        if &h1 <= &h2 {
          let l2 = L::cons(h2,t2);
          match n1 {
            None => {
              let rest = list_merge::<X,L>(None, t1, n2, l2);
              L::cons(h1, rest)
            }
            Some(n1) => {
              let (n1a, n1b) = name_fork(n1);
              let rest = thunk!(n1a =>>
                                list_merge::<X,L>,
                                n1:None, l1:t1, n2:n2, l2:l2);
              let rest = L::art(rest);
              let rest = L::cons(h1, rest);
              let rest = L::name(n1b, rest);
              rest
            }
          }
        }
        else {
          let l1 = L::cons(h1,t1);
          match n2 {
            None => {
              let rest = list_merge::<X,L>(n1, l1, None, t2);
              let l = L::cons(h2, rest);
              l
            }
            Some(n2) => {
              let (n2a, n2b) = name_fork(n2);
              let rest = thunk!(n2a =>>
                                list_merge::<X,L>,
                                n1:n1, l1:l1, n2:None, l2:t2);
              let rest = L::art(rest);
              let rest = L::cons(h2, rest);
              let rest = L::name(n2b, rest);
              rest
            }
          }
        }},
      |m2,t2,(h1,t1,n1,_n2)| {
        let l1 = L::cons(h1,t1);
        list_merge::<X,L>(n1, l1, Some(m2), t2)
      }
      ),
     |m1,t1,(_n1,n2,l2)| {
       list_merge::<X,L>(Some(m1), t1, n2, l2)
     }
     )
}

// pub fn list_merge_sort<X:Ord+Hash+Debug+Clone,L:ListT<X>,T:TreeT<X>>
//     (list:L::List) -> L::List
// {
//     let tree = tree_of_list::<X,T,L>(st, Dir2::Left, list);
//     tree_merge_sort::<X,L,T>(st, tree)
// }

/// Demand-driven sort over a tree's leaves, whose elements are `Ord`.
/// To the extent that the tree contains `name`s, the output is lazy, and thus sorts on-demand.
/// Demanding the first element is `O(n)` for a tree with `n` leaves.
/// Demanding the next element requires more comparisons, but fewer than the first element.
/// Demanding the last element requires only `O(1)` comparisons.
/// In total, the number of comparisons to demand the entire output is, as usual, `O(n ° log(n))`.
pub fn mergesort_list_of_tree
  < X:Ord+Hash+Debug+Clone
  , Lev:Level
  , T:TreeElim<Lev,X>
  , L:ListIntro<X>+ListElim<X>+'static
  >
  (tree:T) -> L
{
  tree_fold_up
    (tree,
     Rc::new(||           L::nil()),
     Rc::new(|x|          L::singleton(x)),
     Rc::new(|_, l, r|    { list_merge(None, l, None, r) }),
     Rc::new(|n, _, l, r| { let (n1,n2) = name_fork(n);
                            list_merge(Some(n1), l, Some(n2), r) }),
     )
}

/// Demand-driven sort over a tree's leaves, whose elements are `Ord`.
/// To the extent that the tree contains `name`s, the output is lazy, and thus sorts on-demand.
/// Demanding the first element is `O(n)` for a tree with `n` leaves.
/// Demanding the next element requires more comparisons, but fewer than the first element.
/// Demanding the last element requires only `O(1)` comparisons.
/// In total, the number of comparisons to demand the entire output is, as usual, `O(n ° log(n))`.
pub fn mergesort_list_of_tree2
  < X:Ord+Hash+Debug+Clone
  , Lev:Level
  , T:TreeElim<Lev,X>
  , L:ListIntro<X>+ListElim<X>+'static
  >
  (tree:T, nm:Option<Name>) -> L
{
  tree_fold_up_nm_dn
    (tree, nm,
     Rc::new(|m,|         L::nil()),
     Rc::new(|m,x|        list_name_op(m, L::singleton(x))),
     Rc::new(|m,    _, l, r| { list_merge_wrapper(None, l, None, r) }),
     Rc::new(|m, n, _, l, r| { list_merge_wrapper(None, l, Some(n), r) }),
     )
}

/// Demand-driven sort over a tree's leaves, whose elements are `Ord`.
/// To the extent that the tree contains `name`s, the output is lazy, and thus sorts on-demand.
/// Demanding the first element is `O(n)` for a tree with `n` leaves.
/// Demanding the next element requires more comparisons, but fewer than the first element.
/// Demanding the last element requires only `O(1)` comparisons.
/// In total, the number of comparisons to demand the entire output is, as usual, `O(n ° log(n))`.
pub fn mergesort_list_of_tree3
  < X:Ord+Hash+Debug+Clone
  , Lev:Level
  , T:TreeElim<Lev,X>
  , L:ListIntro<X>+ListElim<X>+'static
  >
  (tree:T, nm:Option<Name>) -> L
{
  tree_fold_up_nm_dn
    (tree, nm,
     Rc::new(|m,|         L::nil()),
     Rc::new(|m,x|        list_name_op(m, L::singleton(x))),
     Rc::new(|m,    _, l, r| { list_merge_wrapper(None, l, None, r) }),
     Rc::new(|m, n, _, l, r| { list_merge_wrapper(None, l, None, r) }),
     )
}

pub fn list_merge_wrapper<X:Ord+Clone+Debug,L:ListIntro<X>+ListElim<X>+'static>
  (n1:Option<Name>, l1:L,
   n2:Option<Name>, l2:L ) -> L
{
  ns(name_of_str("merge"), || list_merge(n1, l1, n2, l2))
}

// /// Returns the length of the longest run of non-name list elements.
// /// Useful for tests that assert that lists have well-spaced names.
// pub fn list_max_run<X,L:ListElim<X>+'static>
//   (l:L) {
//     L::elim_arg(l, 0,
//                 |_, max_run| max_run,
//                 |h, t| list_max_run + 1,
                

//   }


#[test]
pub fn test_mergesort1 () {
  fn doit() -> Vec<NameElse<usize>> {
    let l : List<usize> = list_gen(1000, |x|x);
    let i = vec_of_list(l.clone(), None);
    println!("input vec: {:?}", i);
    let t = ns(name_of_str("tree_of_list"),
               ||tree_of_list::<_,_,Tree<_>,_>(Dir2::Right, l));
    let s = ns(name_of_str("mergesort"),
               ||mergesort_list_of_tree::<_,_,_,List<_>>(t));
    let o = vec_of_list(s, None);
    println!("output vec: {:?}", o);
    o
  }
  init_naive();
  let o1 = doit();
  init_dcg();
  let o2 = doit();
  assert_eq!(o1, o2);
}

#[test]
pub fn test_mergesort2 () {
  fn doit() -> Vec<NameElse<usize>> {
    let l : List<usize> = list_gen(1000, |x|x);
    let i = vec_of_list(l.clone(), None);
    println!("input vec: {:?}", i);
    let t = ns(name_of_str("tree_of_list"),
               ||tree_of_list::<_,_,Tree<_>,_>(Dir2::Right, l));
    let t = ns(name_of_str("prune"),
               ||prune_tree_of_tree::<_,_,_,Tree<_>>(t));
    println!("input tree: {:?}", eager_tree_of_tree::<_,_,_,Tree<_>>(t.clone()));
    let s = ns(name_of_str("mergesort"),
               ||mergesort_list_of_tree2::<_,_,_,List<_>>(t, (Some(name_of_usize(666)))));
    let o = vec_of_list(s, None);
    println!("output vec: {:?}", o);
    o
  }
  init_naive();
  let o1 = doit();
  init_dcg();
  let o2 = doit();
  assert_eq!(o1, o2);
}

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

#[derive(Debug,Clone,PartialEq,Eq,Hash)]
pub enum NameElse<X> {
  Name(Name),
  Else(X),
}

impl<X:Debug+Hash+PartialEq+Eq+Clone> ListIntro<X> for List<X>
{
  fn nil  ()                 -> Self { List::Nil }
  fn cons (hd:X, tl:Self)    -> Self { List::Cons(hd,Box::new(tl)) }
  fn name (nm:Name, tl:Self) -> Self { List::Name(nm, Box::new(tl)) }
  fn art  (art:Art<List<X>>) -> Self { List::Art(art) }
}

impl<X:Debug+Hash+PartialEq+Eq+Clone> ListElim<X> for List<X>
{
  fn elim<Res,NilF,ConsF,NameF>
    (list:&Self, nilf:NilF, consf:ConsF, namef:NameF) -> Res
    where NilF:FnOnce(       &Self) -> Res
    ,    ConsF:FnOnce(&X,    &Self) -> Res
    ,    NameF:FnOnce(&Name, &Self) -> Res
  {
    match *list {
      List::Nil => nilf(&List::Nil),
      List::Cons(ref hd, ref tl) => consf(hd, tl),
      List::Name(ref nm, ref tl) => namef(nm, tl),
      List::Art(ref art) => {
        let list = force(art);
        Self::elim(&list, nilf, consf, namef)
      },
      List::Tree(ref _tree, ref _dir, ref _tl) => {
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
    where NilF:FnOnce(      Self, Arg) -> Res
    ,    ConsF:FnOnce(X,    Self, Arg) -> Res
    ,    NameF:FnOnce(Name, Self, Arg) -> Res
  {
    match list {
      List::Nil => nilf(List::Nil,arg),
      List::Cons(hd, tl) => consf(hd, *tl, arg),
      List::Name(nm, tl) => namef(nm, *tl, arg),
      List::Art(ref art) => {
        let list = force(art);
        Self::elim_arg(list, arg, nilf, consf, namef)
      },
      List::Tree(_tree, _dir, _tl) => {
        //let tree = *tree;
        //let tl = *tl;
        let (res, rest) = structural(|| panic!("List::next_leaf_rec(tree, dir, tl)")) ;
        match res {
          None => Self::elim_arg(rest, arg, nilf, consf, namef),
          Some(elm) => consf(elm, rest, arg),
        }
      }
    }
  }
}

impl<Dom:Debug+Hash+PartialEq+Eq+Clone+'static,
     Cod:Debug+Hash+PartialEq+Eq+Clone+'static> 
  MapIntro<Dom,Cod> 
  for 
  List<(Dom,Cod)> 
{
  fn empty () -> Self { List::Nil }
  fn update (map:Self, d:Dom, c:Cod) -> Self { List::Cons((d,c),Box::new(map)) }
}

impl<Dom:Debug+Hash+PartialEq+Eq+Clone+'static,
     Cod:Debug+Hash+PartialEq+Eq+Clone+'static> 
  MapElim<Dom,Cod> 
  for 
  List<(Dom,Cod)> 
{
  fn find<'a> (map:&'a Self, d:&Dom) -> Option<Cod> { 
    match *map {
      List::Nil => None,
      List::Cons((ref d2, ref c), ref tl) => {
        if d == d2 { Some(c.clone()) }
        else { Self::find(&*tl, d) }
      },
      List::Tree(_,_,_) => unimplemented!(),
      List::Name(_, ref list) => Self::find(&*list, d),
      List::Art(ref a) => { Self::find(&(force(a)), d) }
    }
  }
  fn remove (_map:Self, _d:&Dom) -> (Self, Option<Cod>) { 
    unimplemented!()
  }
  fn fold<Res,F> (map:Self, res:Res, body:Rc<F>) -> Res
    where F:Fn(Dom, Cod, Res) -> Res 
  {
    list_fold(map, res, Rc::new(move |(d,c),r|(*body)(d,c,r)) )
  }
  fn append(_map:Self, _other:Self) -> Self {
    unimplemented!()    
  }
}


/// Demonstrates how to write performance and correctness tests that
/// compare the measurements and outputs of running a common piece of
/// code under a common sequence of input changes.
#[test]
fn test_engine_alternation () {
  
  // The code that we want to compare/measure under naive versus DCG engines:
  fn doit(l:List<usize>) -> Tree<usize> {
    let t1 = ns(name_of_str("tree_of_list"),
                ||tree_of_list::<_,_,Tree<_>,_>(Dir2::Left, l));
    let t2 = ns(name_of_str("eager_tree"),
                ||eager_tree_of_tree::<_,_,_,Tree<_>>(t1));
    t2
  };

  // The common way to change the input over time: by prepending elements of type usize:
  fn push_input(i:usize, l:List<usize>) -> List<usize> {
    let l = List::art(cell(name_of_usize(i), l)); // Important in DCG version for hash-consing input list; O(1)-time equality checks
    let l = List::name(name_of_usize(i), l);
    let l = List::cons(i, l);
    l
  };
  
  let mut naive_input : List<usize> = List::nil(); // the naive input, which we will prepend in the loop below
  let mut   dcg_input : List<usize> = List::nil(); // the DCG   input, which we will prepend in the loop below
  
  init_dcg(); // Initialize the current engine with an empty DCG instance
  let mut dcg = init_naive(); // Current engine is naive; save DCG for later
  
  for i in vec![1,2,3,4,5,6,7,8,9].iter()
  {
    assert!(engine_is_naive()); // Sanity check
    naive_input = push_input(*i, naive_input); // Prepend Naive input
    let naive_out = doit(naive_input.clone()); // MEASURE ME!

    use_engine(dcg); // Switch to DCG engine    
    assert!(engine_is_dcg()); // Sanity check
    dcg_input = push_input(*i, dcg_input); // Prepend DCG input
    let dcg_out = doit(dcg_input.clone()); // MEASURE ME!
    dcg = init_naive(); // Switch back to naive; save DCG engine for later

    assert_eq!(naive_out, dcg_out);
    
  }
}

#[test]
fn test_tree_of_list () {
  fn test_code() -> (Tree<usize>, Tree<usize>, usize) {
    let max = 1000;
    let l : List<usize> = list_gen(max, |x|x);

    let t1 = ns(name_of_str("tree_of_list"),
                ||tree_of_list::<_,_,Tree<_>,_>(Dir2::Left, l.clone()));

    let s1 = ns(name_of_str("eager_tree"),
                ||eager_tree_of_tree::<_,_,_,Tree<_>>(t1));
    
    let l = List::cons(max + 1,l);
    let n = name_of_usize(max + 1);
    let l = List::art(cell(n.clone(), l));
    let l = List::name(n, l);
    
    let t1 = ns(name_of_str("tree_of_list"),
                ||tree_of_list::<_,_,Tree<_>,_>(Dir2::Left, l));
    
    let s2 = ns(name_of_str("eager_tree"),
                ||eager_tree_of_tree::<_,_,_,Tree<_>>(t1.clone()));

    let max = ns(name_of_str("max"),
                 ||monoid_of_tree(t1, usize::max_value(),
                                  Rc::new(|x,y| if x > y { x } else { y }))) ;
    (s1,s2,max)
  };

  init_naive();
  let (s1,s2,m) = test_code();
  println!("naive: s1={:?}", s1);
  println!("naive: s2={:?}", s2);
  println!("naive: m ={:?}", m);
  
  init_dcg();
  let (t1,t2,n) = test_code();
  println!("dcg:   t1={:?}", t1);
  println!("dcg:   t2={:?}", t2);
  println!("dcg:   n ={:?}", n);

  assert_eq!(s1, t1); // first trees are equal
  assert_eq!(s2, t2); // second trees are equal
  assert_eq!(m,  n);  // max value in leaves of second trees are equal
}


#[test]
fn test_tree_filter () {
  fn test_code() -> (Tree<usize>, Tree<usize>, usize) {
    let max = 1000;
    let l : List<usize> = list_gen(max, |x|x);
    
    let t1 = ns(name_of_str("tree_of_list"),
                ||tree_of_list::<_,_,Tree<_>,_>(Dir2::Left, l.clone()));
    
    let t1 = ns(name_of_str("filter_tree"),
                ||filter_tree_of_tree::<_,_,_,Tree<_>>(t1,Box::new(|n| *n != 2 )));
                                                       
    let s1 = ns(name_of_str("eager_tree"),
                ||eager_tree_of_tree::<_,_,_,Tree<_>>(t1));
    
    let l = List::cons(max + 1,l);
    let n = name_of_usize(max + 1);
    let l = List::art(cell(n.clone(), l));
    let l = List::name(n, l);
    
    let t1 = ns(name_of_str("tree_of_list"),
                ||tree_of_list::<_,_,Tree<_>,_>(Dir2::Left, l));

    let t1 = ns(name_of_str("filter_tree"),
                ||filter_tree_of_tree::<_,_,_,Tree<_>>(t1,Box::new(|n| *n != 2 )));
    
    let s2 = ns(name_of_str("eager_tree"),
                ||eager_tree_of_tree::<_,_,_,Tree<_>>(t1.clone()));

    let max = ns(name_of_str("max"),
                 ||monoid_of_tree(t1, usize::max_value(),
                                  Rc::new(|x,y| if x > y { x } else { y }))) ;
    (s1,s2,max)
  };

  init_naive();
  let (s1,s2,m) = test_code();
  println!("filter naive: s1={:?}", s1);
  println!("filter naive: s2={:?}", s2);
  println!("max    naive: m ={:?}", m);
  
  init_dcg();
  let (t1,t2,n) = test_code();
  println!("filter dcg:   t1={:?}", t1);
  println!("filter dcg:   t2={:?}", t2);
  println!("max    dcg:   n ={:?}", n);


  assert_eq!(s1, t1);
  assert_eq!(s2, t2);  
  assert_eq!(m,  n);
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

impl Level for usize {
  //  See Pugh+Teiltelbaum in POPL 1989 for an explanation of this notion of "level":
  fn new<X:Hash>(x:&X) -> Self  {
    my_hash_n(&x,1).trailing_zeros() as Self
  }
  fn bits () -> Self { 32 }
  fn zero () -> Self { 0 }
  fn max_val () -> Self { usize::max_value() }
  fn add (x:&Self,y:&Self) -> Self { x + y }
  fn inc (x:&Self) -> Self { x + 1 }
  fn lte (x:&Self,y:&Self) -> bool { x <= y }
}

impl <Leaf:Debug+Hash+PartialEq+Eq+Clone+'static>
  TreeIntro<usize,Leaf>
  for Tree<Leaf>
{ 
  fn nil  ()                                  -> Self { Tree::Nil }
  fn leaf (x:Leaf)                            -> Self { Tree::Leaf(x) }
  fn bin  (lev:usize, l:Self, r:Self)         -> Self { Tree::Bin(lev,Box::new(l),Box::new(r)) }
  fn name (nm:Name, lev:usize, l:Self,r:Self) -> Self { Tree::Name(nm, lev, Box::new(l),Box::new(r)) }
  fn art  (art:Art<Self>)                     -> Self { Tree::Art(art) }
}

impl <Leaf:Debug+Hash+PartialEq+Eq+Clone+'static>
  TreeElim<usize,Leaf>
  for Tree<Leaf>
{  
  fn elim_arg<Arg,Res,NilC,LeafC,BinC,NameC>
    (tree:Self, arg:Arg, nil:NilC, leaf:LeafC, bin:BinC, name:NameC) -> Res
    where NilC  : FnOnce(Arg) -> Res
    ,     LeafC : FnOnce(Leaf, Arg) -> Res
    ,     BinC  : FnOnce(usize,  Self, Self, Arg) -> Res
    ,     NameC : FnOnce(Name, usize,  Self, Self, Arg) -> Res
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
    ,     BinC  : FnOnce(usize,  Self, Self, Arg) -> Res
    ,     NameC : FnOnce(Name, usize,  Self, Self, Arg) -> Res
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
    ,     BinC  : FnOnce(usize,  Self, Self) -> Res
    ,     NameC : FnOnce(Name, usize, Self, Self) -> Res
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
    ,     BinC  : FnOnce(&usize, &Self, &Self) -> Res
    ,     NameC : FnOnce(&Name, &usize, &Self, &Self) -> Res
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

  fn lev_of_tree (tree:&Self) -> usize {
    Self::elim_ref
      (tree,
       || 0,
       |leaf|      <usize as Level>::new(leaf),
       |lev,_,_|   lev.clone(),
       |_,lev,_,_| lev.clone(),
       )
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

/// Random Access Zipper (RAZ).
/// Purely functional sequences with global access (via a balanced
/// tree structure) and simple local edits (via a zipper structure).
///
/// See also: [RAZ in OCaml](http://github.com/cuplv/raz.ocaml.git)
pub mod raz {
  use std::rc::Rc;
  use std::fmt::Debug;
  use std::hash::Hash;

  use macros::* ;
  use adapton::engine::* ; 

  /// Punctuation: Information that is interposed between each run of
  /// elements, and sometimes before and after them.  The RAZ uses
  /// user-chosen levels to probabilistically balance the Tree form.
  /// The RAZ uses the Name to identify cached computations in
  /// Adapton.
  #[derive(PartialEq,Eq,Hash,Debug,Clone)]
  pub struct Punc{ pub level:usize, pub name:Name }
  
  /// A binary tree with vector leaves (aka, a "rope"), balanced
  /// probabilistically.
  #[derive(PartialEq,Eq,Hash,Debug,Clone)]
  pub enum Tree<X> { 
    /// Invariant: Nil only appears in certain edge positions
    Nil, 
    /// Invariant: Each element run in a Leaf is interposed between two `Punc`s
    Leaf(Vec<X>),
    /// (Non-trivial) Inductive case: Two balanced sub-trees, interposed with punctuation.
    /// Invariant: For balance, every Bin node in left/right subtrees has a equal-or-lower level.
    Bin(Box<Tree<X>>,Punc,Box<Tree<X>>),
    /// (Trivial) Inductive case: An Adapton articulation.
    Art(Art<Tree<X>>),
  }
  
  /// Runs of contiguous element sequences, interposed with
  /// punctuation information of type `Punc`.
  #[derive(PartialEq,Eq,Hash,Debug,Clone)]
  enum Elms<X> {
    /// Base case: No more Cons cells, all remaining elements are formed into a sequence of Trees.
    Trees(Vec<Tree<X>>), 
    /// (Non-trivial) Inductive case: Run of elements, followed by more elements.
    Cons(Vec<X>,Option<ConsTl<X>>),
    /// (Trivial) Inductive case: an Adapton articulation.
    Art(Art<Elms<X>>),
  }
  #[derive(PartialEq,Eq,Hash,Debug,Clone)]
  struct ConsTl<X> {
    /// Invariant: Punctuation separates each run of elements.
    punc:Punc,
    /// Inductive structure: (possibly empty) sequence of more element runs.
    elms:Box<Elms<X>>,
  }

  /// The Zipper form consists of element sequences before (to the
  /// left of) and after (to the right of) a distinguished punctuation
  /// point in the sequence.
  #[derive(PartialEq,Eq,Hash,Debug,Clone)]
  pub struct Zip<X> {
    left:  Elms<X>,
    curs:  Punc,
    right: Elms<X>
  }

  /// Distinguishes left (`L`) and right (`R`) traversal orders for various operations involving trees.
  #[derive(PartialEq,Eq,Hash,Debug,Clone,Copy)]
  pub enum Dir { L, R }
  
  /// An O(1)-sized change to the zipper: a single element insertion,
  /// replacement, removal; or, a local cursor movement by one element
  /// in either direction.
  #[derive(PartialEq,Eq,Hash,Debug,Clone)]
  pub enum Edit<X> {
    Insert(Dir,X,Option<Punc>),
    Replace(Dir,X),
    Remove(Dir),
    Move(Dir),
  }

  /// A zipper that consists of zero elements (and exactly one punctuation `p`).
  pub fn zip_empty<X>(p:Punc) -> Zip<X> {
    return Zip{left:Elms::Cons(vec![], None),
               curs:p,
               right:Elms::Cons(vec![], None)}
  }

  /// A tree that consists of zero elements (and exactly one punctuation `p`).
  pub fn tree_empty<X>(p:Punc) -> Tree<X> {
    return Tree::Bin(Box::new(Tree::Nil), p, Box::new(Tree::Nil))
  }

  /// Transform the zipper, inserting element `x` in the given
  /// direction `d`. The optional punctuation `p` follows the inserted
  /// element, in the given direction.
  pub fn zip_insert<X:Eq+Clone+Hash+Debug>(z:Zip<X>, d:Dir, x:X, p:Option<Punc>) -> Zip<X> {
    match &d {
      &Dir::L => {
        match p {
          Some(p) => { // Punctuate the insertion with the given p; creates new run of elements, consisting only of element x
            let a = cell(p.name.clone(), z.left);
            Zip{left:Elms::Cons(vec![x], Some(ConsTl{punc:p, elms:Box::new(Elms::Art(a))})), ..z}
          }
          None => {
            match z.left { // Mutate the current run of elements, by pushing element x onto it
              Elms::Cons(mut xs,tl) => { xs.push(x); Zip{left:Elms::Cons(xs, tl), ..z} },
              _ => panic!("bad arguments: need punctuation")
            }
          }
        }
      }
      &Dir::R => {
        // TODO-Now
        panic!("symmetric to above")
      }
    }
  }
    
  /// Perform the given edit, in the given direction, at the current focus of the given zipper.
  /// The zipper is taken because its head vectors may be mutated, e.g., to insert or remove elements.
  pub fn zip_edit<X:Eq+Clone+Hash+Debug>(z:Zip<X>, edit:Edit<X>) -> Zip<X> {
    match edit {
      Edit::Insert(d,x,p) => { zip_insert(z, d, x, p) },
      _ => unimplemented!() // TODO-Later
    }
  }

  fn cons_trampoline<X:Eq+Clone+Hash+Debug+'static>
    (dir:Dir, parent_lev:usize, tr_lev:usize, tr:Tree<X>, es:Elms<X>) 
     -> (Tree<X>, Elms<X>)
  {
    match es {
      Elms::Trees(trs) => { (tr, Elms::Trees(trs)) }
      Elms::Cons(v, None) => { if v.len() > 0 { panic!("bad") } else { (tr, Elms::Cons(v, None)) }},
      Elms::Art(a) => cons_trampoline(dir, parent_lev, tr_lev, tr, force(&a)),
      Elms::Cons(v, Some(ConsTl{punc:p, elms:es})) => {
        if tr_lev <= p.level && p.level <= parent_lev {
          let (n1, n2) = name_fork(p.name.clone());
          let (_, (rtr, es)) = eager!(n1 =>> cons_trampoline, dir:dir, parent_lev:p.level, tr_lev:0, tr:Tree::Leaf(vec![]), es:*es);
          let tr = match &dir {
            & Dir::L => Tree::Bin(Box::new(tr),  p.clone(), Box::new(rtr)),
            & Dir::R => Tree::Bin(Box::new(rtr), p.clone(), Box::new(tr)),
          };
          let tr = Tree::Art(cell(p.name.clone(), tr));
          let (_, (tr, es)) = eager!(n2 =>> cons_trampoline, dir:dir, parent_lev:parent_lev, tr_lev:p.level, tr:tr, es:es);
          (tr, es)
        }
        else {
          (tr, Elms::Cons(v, Some(ConsTl{punc:p, elms:es})))
        }
      }
    }
  } 

  /// Compute a vector of trees, building any leading Cons cells into Tree structure
  fn trees_of_elms<X:Eq+Clone+Hash+Debug+'static>(dir:Dir, es:Elms<X>) -> Vec<Tree<X>> {
    match cons_trampoline(dir, usize::max_value(), 0, Tree::Nil, es) {
      (tr, Elms::Trees(mut trs)) => { trs.push(tr); return trs }
      _ => unreachable!(),
    }
  }

  /// Appends the sequences of two trees
  pub fn tree_append<X>(tr1:Tree<X>, tr2:Tree<X>) -> Tree<X> {
    panic!("TODO")    
  }

  /// Unfocuses the zipper into a tree form
  pub fn tree_of_zip<X:Eq+Clone+Hash+Debug+'static>(z:Zip<X>) -> Tree<X> {
    let mut tree = tree_empty(z.curs);
    let ltrees = trees_of_elms(Dir::L, z.left);
    for t in ltrees.iter() { // TODO: Take iterator; Reverse iteration here
      tree = tree_append(t.clone(), tree); // TODO Avoid clone
    }
    let rtrees = trees_of_elms(Dir::R, z.right);
    for t in rtrees.iter() {
      tree = tree_append(tree, t.clone()); // TODO Avoid clone
    }
    return tree
  }

  fn merge_elms<X:Ord+Hash+Eq+Debug+Clone+'static>
  (es1:Elms<X>, es2:Elms<X>) -> Elms<X> {
    let (mut v1, tl1, 
         mut v2, tl2) = match (es1, es2) {
      (Elms::Cons(v1, tl1), 
       Elms::Cons(v2, tl2)) => (v1.clone(), tl1, v2.clone(), tl2), // TODO Avoid these clones.
      (_, _)                => panic!("illegal argument(s)")
    };
    let mut merged = Vec::new();
    loop {
      match (v1.pop(), v2.pop()) {
        (None, None)     => { return Elms::Cons(Vec::new(), None) },
        (None, Some(e2)) => { // First vector "wins" race to empty.
          v2.push(e2); let es2 = Elms::Cons(v2, tl2);
          match tl1 { 
            None => return es2,
            Some(tl1) => { // since v1 is out of elements, use its punctuation (aka, tl1.punc)
              let rest = thunk!(tl1.punc.name.clone() =>> merge_elms, es1:*(tl1.elms), es2:es2);
              return Elms::Cons(merged, Some(ConsTl{punc:tl1.punc, elms:Box::new(Elms::Art(rest))}))
            }
          }
        },
        (Some(e1), None) => { // Second vector "wins" race to empty.
          v1.push(e1); let es1 = Elms::Cons(v1, tl1);
          match tl2 { 
            None      => return es1,
            Some(tl2) => { // since v2 is out of elements, use its punctuation (aka, tl2.punc)
              let rest = thunk!(tl2.punc.name.clone() =>> merge_elms, es1:es1, es2:*(tl2.elms));
              return Elms::Cons(merged, Some(ConsTl{punc:tl2.punc, elms:Box::new(Elms::Art(rest))}))
          }}
        }
        (Some(e1), Some(e2)) => { // Neither vector is empty; determine the next element, replace the other.
          if e1 <= e2 { merged.push(e1); v2.push(e2) } 
          else        { merged.push(e2); v1.push(e1) }
        }
      }
    }
  }

  /// Merge-sort the elements of the given tree, producing another
  /// tree, in ascending order (when `d = Dir::L`) or descending order
  /// (when `d = Dir::R`).
  pub fn mergesort_eager<X:Ord+Hash+Eq+Debug+Clone+'static>(n:Option<Name>, tr:Tree<X>, d:Dir) -> Tree<X> {
    let elms = merge_sort_tree(n, tr, None);
    let mut trees = trees_of_elms(d, elms);
    assert!( trees.len() == 1 );
    match trees.pop() {
      None       => unreachable!(),
      Some(tree) => tree
    }
  }

  fn merge_sort_tree<X:Ord+Hash+Eq+Debug+Clone+'static>(n:Option<Name>, tr:Tree<X>, rp:Option<Punc>) -> Elms<X> {
    match n { Some(n) => {let (_,ans) = eager!(n =>> merge_sort_tree, n:None, tr:tr, rp:rp); ans}, None => { // <<< EXPLICIT-IC
      fn tl_of_opp<X>(opp:Option<Punc>) -> Option<ConsTl<X>> {
        let emp = Box::new(Elms::Cons(Vec::new(), None));
        match opp { 
          None    => None, 
          Some(p) => Some(ConsTl{punc:p, elms:emp})
        }
      }
      match tr {
        Tree::Nil => { Elms::Cons(Vec::new(), None) }
        Tree::Leaf(es) => { 
          let last = es.last().clone() ; // Unavoidable clone; need to find last element below
          let mut sorted = es.clone(); /* TODO: Avoid this Vec clone(). */
          sorted.sort();
          match rp {
            None    => Elms::Cons(sorted, None),
            Some(p) => {
              let (sorted1, sorted2) = { (sorted, Vec::new()) /* TODO: Partition sorted based on position of `last` */ };
              Elms::Cons(sorted1, Some(ConsTl{punc:p, elms:Box::new(Elms::Cons(sorted2, None))}))
            }}}
        Tree::Bin(l,lp,r) => { 
          let (n1,n2) = name_fork(lp.name.clone()); // <<< EXPLICIT-IC
          let ms_l = merge_sort_tree(Some(n1), *l, Some(lp));
          let ms_r = merge_sort_tree(Some(n2), *r, rp);
          merge_elms(ms_l, ms_r)
        }
        Tree::Art(ref a) => merge_sort_tree(None, force(a), rp)
      }
    }}
  }
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
