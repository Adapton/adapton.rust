use std::fmt::Debug;
use std::hash::{Hash,Hasher};
use std::collections::hash_map::DefaultHasher;
use std::rc::Rc;

use macros::* ;
use adapton::engine::* ;

pub mod trie {
  pub use catalog::trie::*;
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

fn my_hash_n<T>(obj: T, n:usize) -> u64
  where T: Hash
{
  let mut hasher = DefaultHasher::new();
  for _ in 0..n {
    obj.hash(&mut hasher);
  }
  hasher.finish()
}

/// Types that can be created like a list of `X` are `ListIntro<X>`
pub trait ListIntro<X:'static> : Debug+Clone+Hash+PartialEq+Eq+'static {
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
pub fn list_gen<X:'static,G,L:ListIntro<X>>
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

pub fn list_nil<X:'static, L:ListIntro<X>>()          -> L { L::nil() }
pub fn list_cons<X:'static, L:ListIntro<X>>(x:X, l:L) -> L { L::cons(x, l) }
pub fn list_name<X:'static, L:ListIntro<X>>(n:Name, l:L) -> L { L::name(n, l) }
pub fn list_art<X:'static, L:ListIntro<X>>(l:Art<L>) -> L { L::art(l) }
pub fn list_name_art_op<X:'static, L:ListIntro<X>>(n:Option<Name>, l:L) -> L {
  match n {
    None => l,
    Some(n) => {
      let c : Art<L> = cell(n.clone(), l);
      list_name(n, list_art(c))
    }
  }
}
pub fn list_name_op<X:'static, L:ListIntro<X>>(n:Option<Name>, l:L) -> L {
  match n {
    None => l,
    Some(n) => {
      list_name(n, l)
    }
  }
}

/// Lazily maps the list, guided by names in input list.
/// Creates lazy named thunks in output for each name in input.
pub fn list_map_lazy<X:'static, Le:'static+ListElim<X>, 
                     Y:'static, Li:'static+ListIntro<Y>, 
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
pub fn list_filter_lazy<X:'static, Le:'static+ListElim<X>, Li:'static+ListIntro<X>, F:'static>
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
pub fn list_filter_eager<X:'static, Le:'static+ListElim<X>, Li:'static+ListIntro<X>, F:'static>
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
pub fn list_map_eager<X:'static, Le:'static+ListElim<X>, 
                      Y:'static, Li:'static+ListIntro<Y>, 
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

/// Eagerly maps the list.  Uses (eager) memoization for each name in
/// `l`.  Unlike list_map_eager, it allocates a reference cell for
/// each name, separate from the memoized thunk for the recursive
/// call.
pub fn list_map_eager2<X:'static, Le:'static+ListElim<X>, 
                       Y:'static, Li:'static+ListIntro<Y>, 
                       F:'static>
  (l:Le, body:Rc<F>) -> Li
 where F:Fn(X) -> Y
{
  Le::elim_arg
    (l, body,
     |_,_| list_nil(),
     |x, tl, body| {
       let y = body.clone() (x);
       list_cons(y, list_map_eager2(tl, body))
     },
     |n, tl, body| {
       // We only memoize when we encounter a name in the input
       let (nm1, nm2, nm3) = name_fork3(n.clone());
       let t = memo!( nm2 =>> list_map_eager2 =>> <X, Le, Y, Li, F>, l:tl ;; body:body.clone() );
       list_name(nm1, list_art( cell( nm3 , t) ) )
     })
}


/// Eagerly maps the list.
/// Uses (eager) memoization for each name in `l`.
pub fn list_reverse<X:'static, Le:'static+ListElim<X>, Li:'static+ListIntro<X>>
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

pub fn list_push<X:'static, L:ListIntro<X>>(stack:L, elm:X) -> L {
  L::cons(elm, stack)
}

pub fn list_append<X:'static, L:ListIntro<X>+ListElim<X>>(l1:L, l2:L) -> L {
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
  fn update (map:Self, d:Dom, c:Cod) -> Self;
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
  < Lev:Level, X:'static+Hash+Clone+Debug
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
  < Lev:Level, X:'static+Hash+Clone+Debug
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

/// List the leaf elements and names of a tree, in the given order, via a sequential, in-order traversal.
/// Direction `Dir2::Left` lists elements from left to right. (Leftmost elements are in the head of the list).
/// Direction `Dir2::Right` lists elements from right to left. (Rightmost elements are in the head of the list).
/// Preserves the order of elements, up to `dir`, and the names in the tree.
pub fn list_of_tree
  < Lev:Level,X:'static+Hash+Clone
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

/// Constructs a linked list that consists of elements and names, as
/// given by the input vector (in that order).
/// Not incremental; used only for setting up inputs for tests.
pub fn list_of_vec<X:'static+Clone,L:ListIntro<X>>
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

/// Produce a lazy list that consists of merging two input lists.
/// The output is lazy to the extent that the input lists contain `name`s.
/// When the input lists are each sorted according to `Ord`; the output is sorted.
pub fn list_merge<X:'static+Ord+Clone+Debug,L:ListIntro<X>+ListElim<X>+'static>
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

/// Demand-driven sort over a tree's leaves, whose elements are `Ord`.
/// To the extent that the tree contains `name`s, the output is lazy, and thus sorts on-demand.
/// Demanding the first element is `O(n)` for a tree with `n` leaves.
/// Demanding the next element requires more comparisons, but fewer than the first element.
/// Demanding the last element requires only `O(1)` comparisons.
/// In total, the number of comparisons to demand the entire output is, as usual, `O(n ° log(n))`.
pub fn mergesort_list_of_tree
  < X:'static+Ord+Hash+Debug+Clone
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
  < X:'static+Ord+Hash+Debug+Clone
  , Lev:Level
  , T:TreeElim<Lev,X>
  , L:ListIntro<X>+ListElim<X>+'static
  >
  (tree:T, nm:Option<Name>) -> L
{
  tree_fold_up_nm_dn
    (tree, nm,
     Rc::new(|_,|         L::nil()),
     Rc::new(|m,x|        list_name_op(m, L::singleton(x))),
     Rc::new(|_,    _, l, r| { list_merge_wrapper(None, l, None, r) }),
     Rc::new(|_, n, _, l, r| { list_merge_wrapper(None, l, Some(n), r) }),
     )
}

/// Demand-driven sort over a tree's leaves, whose elements are `Ord`.
/// To the extent that the tree contains `name`s, the output is lazy, and thus sorts on-demand.
/// Demanding the first element is `O(n)` for a tree with `n` leaves.
/// Demanding the next element requires more comparisons, but fewer than the first element.
/// Demanding the last element requires only `O(1)` comparisons.
/// In total, the number of comparisons to demand the entire output is, as usual, `O(n ° log(n))`.
pub fn mergesort_list_of_tree3
  < X:'static+Ord+Hash+Debug+Clone
  , Lev:Level
  , T:TreeElim<Lev,X>
  , L:ListIntro<X>+ListElim<X>+'static
  >
  (tree:T, nm:Option<Name>) -> L
{
  tree_fold_up_nm_dn
    (tree, nm,
     Rc::new(|_,|         L::nil()),
     Rc::new(|m,x|        list_name_op(m, L::singleton(x))),
     Rc::new(|_,    _, l, r| { list_merge_wrapper(None, l, None, r) }),
     Rc::new(|_, _, _, l, r| { list_merge_wrapper(None, l, None, r) }),
     )
}

pub fn list_merge_wrapper<X:'static+Ord+Clone+Debug,L:ListIntro<X>+ListElim<X>+'static>
  (n1:Option<Name>, l1:L,
   n2:Option<Name>, l2:L ) -> L
{
  ns(name_of_str("merge"), || list_merge(n1, l1, n2, l2))
}

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
  manage::init_naive();
  let o1 = doit();
  manage::init_dcg();
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
  manage::init_naive();
  let o1 = doit();
  manage::init_dcg();
  let o2 = doit();
  assert_eq!(o1, o2);
}

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

impl<X:'static+Debug+Hash+PartialEq+Eq+Clone> ListIntro<X> for List<X>
{
  fn nil  ()                 -> Self { List::Nil }
  fn cons (hd:X, tl:Self)    -> Self { List::Cons(hd,Box::new(tl)) }
  fn name (nm:Name, tl:Self) -> Self { List::Name(nm, Box::new(tl)) }
  fn art  (art:Art<List<X>>) -> Self { List::Art(art) }
}

impl<X:'static+Debug+Hash+PartialEq+Eq+Clone> ListElim<X> for List<X>
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
  
  manage::init_dcg(); // Initialize the current engine with an empty DCG instance
  let mut dcg = manage::init_naive(); // Current engine is naive; save DCG for later
  
  for i in vec![1,2,3,4,5,6,7,8,9].iter()
  {
    assert!(manage::engine_is_naive()); // Sanity check
    naive_input = push_input(*i, naive_input); // Prepend Naive input
    let naive_out = doit(naive_input.clone()); // MEASURE ME!

    manage::use_engine(dcg); // Switch to DCG engine    
    assert!(manage::engine_is_dcg()); // Sanity check
    dcg_input = push_input(*i, dcg_input); // Prepend DCG input
    let dcg_out = doit(dcg_input.clone()); // MEASURE ME!
    dcg = manage::init_naive(); // Switch back to naive; save DCG engine for later

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

  manage::init_naive();
  let (s1,s2,m) = test_code();
  println!("naive: s1={:?}", s1);
  println!("naive: s2={:?}", s2);
  println!("naive: m ={:?}", m);
  
  manage::init_dcg();
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

  manage::init_naive();
  let (s1,s2,m) = test_code();
  println!("filter naive: s1={:?}", s1);
  println!("filter naive: s2={:?}", s2);
  println!("max    naive: m ={:?}", m);
  
  manage::init_dcg();
  let (t1,t2,n) = test_code();
  println!("filter dcg:   t1={:?}", t1);
  println!("filter dcg:   t2={:?}", t2);
  println!("max    dcg:   n ={:?}", n);


  assert_eq!(s1, t1);
  assert_eq!(s2, t2);  
  assert_eq!(m,  n);
}

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
}

