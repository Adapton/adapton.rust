use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;
use std::rc::Rc;
use std::cmp::min;

use adapton::catalog::collections::{ListIntro, ListElim, list_fold};
use adapton::catalog::bitstring::*;
use adapton::engine::*;
use macros::*;

/// Probablistically Balanced Trie
/// Rough implementation of probabilistic tries from OOPSLA 2015 paper.
///
/// See also: [Tries in OCaml](http://github.com/plum-umd/adapton.ocaml)
#[derive(Debug,PartialEq,Eq,Clone)]
pub enum Trie<X> {
    Nil(BS),
    Leaf(BS, X),
    Bin(BS, Box<Trie<X>>, Box<Trie<X>>),
    Root(Meta, Box<Trie<X>>),
    Name(Name, Box<Trie<X>>),
    Art(Art<Trie<X>>),
}

pub const PLACEMENT_SEED: u64 = 42;

/// Metadata held by the root node.
#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub struct Meta {
    pub min_depth: i64,
}

pub trait MetaT {
    fn hash_seeded(&self, u64);
}

impl MetaT for Meta {
    fn hash_seeded(&self, seed: u64) {
        let mut hasher = DefaultHasher::new();
        seed.hash(&mut hasher);
        "Adapton.Trie.Meta".hash(&mut hasher);
        self.min_depth.hash(&mut hasher);
    }
}

// impl<X: Debug + Hash + PartialEq + Eq + Clone + 'static> PartialEq for Trie<X> {
//     fn eq(&self, other: &Trie<X>) -> bool {
//         match (self, other) {
//             (&Trie::Nil(ref bs_self), &Trie::Nil(ref bs_other)) => bs_self == bs_other,
//             (&Trie::Leaf(ref bs_self, ref e_self), &Trie::Leaf(ref bs_other, ref e_other)) => {
//                 let bs_equal = bs_self == bs_other;
//                 let b = bs_equal && e_self == e_other;
//                 // println!("{:?}\n{}\n{:?}", self, b, other);
//                 b
//             }
//             (&Trie::Bin(ref bs, ref left, ref right),
//              &Trie::Bin(ref bs_other, ref left_other, ref right_other)) => {
//                 let b = bs == bs_other && left == left_other && right == right_other;
//                 // println!("{:?}\n{}\n{:?}", self, b, other);
//                 b
//             }
//             (&Trie::Root(ref md, ref t), &Trie::Root(ref md_other, ref t_other)) => {
//                 let b = md == md_other && t == t_other;
//                 // println!("{:?}\n{}\n{:?}", t, b, t_other);
//                 b
//             }
//             (&Trie::Name(ref nm, ref t), &Trie::Name(ref nm_other, ref t_other)) => {
//                 let b = nm == nm_other && t == t_other;
//                 // println!("{:?}\n{}\n{:?}", t, b, t_other);
//                 b
//             }
//             (&Trie::Art(ref a), &Trie::Art(ref a_other)) => {
//                 let b = a == a_other;
//                 // println!("{:?}\n{}\n{:?}", a, b, a_other);
//                 b
//             }
//             (t, t_other) => {
//                 // println!("{:?}\n!=\n{:?}", t, t_other);
//                 false
//             }
//         }
//     }
// }

// impl<X: Debug + Hash + PartialEq + Eq + Clone + 'static> Eq for Trie<X> {}

pub trait TrieIntro<X>: Debug + Hash + PartialEq + Eq + Clone + 'static {
    fn nil(BS) -> Self;
    fn leaf(BS, X) -> Self;
    fn bin(BS, Self, Self) -> Self;
    fn root(Meta, Self) -> Self;

    // requisite "adaptonic" constructors: `name` and `art`:
    fn name(Name, Self) -> Self;
    fn art(Art<Self>) -> Self;

    fn empty(Meta) -> Self;
    fn singleton(Meta, Name, X) -> Self;
    fn extend(Name, Self, X) -> Self;
}

pub trait TrieElim<X>: Debug + Hash + PartialEq + Eq + Clone + 'static {
    fn find(&Self, &X, i64) -> Option<X>;
    fn is_empty(&Self) -> bool;
    fn split_atomic(Self) -> Self;

    fn elim<Res, NilC, LeafC, BinC, RootC, NameC>(Self, NilC, LeafC, BinC, RootC, NameC) -> Res
        where NilC: FnOnce(BS) -> Res,
              LeafC: FnOnce(BS, X) -> Res,
              BinC: FnOnce(BS, Self, Self) -> Res,
              RootC: FnOnce(Meta, Self) -> Res,
              NameC: FnOnce(Name, Self) -> Res;

    fn elim_arg<Arg, Res, NilC, LeafC, BinC, RootC, NameC>(Self,
                                                           Arg,
                                                           NilC,
                                                           LeafC,
                                                           BinC,
                                                           RootC,
                                                           NameC)
                                                           -> Res
        where NilC: FnOnce(BS, Arg) -> Res,
              LeafC: FnOnce(BS, X, Arg) -> Res,
              BinC: FnOnce(BS, Self, Self, Arg) -> Res,
              RootC: FnOnce(Meta, Self, Arg) -> Res,
              NameC: FnOnce(Name, Self, Arg) -> Res;

    fn elim_ref<Res, NilC, LeafC, BinC, RootC, NameC>(&Self,
                                                      NilC,
                                                      LeafC,
                                                      BinC,
                                                      RootC,
                                                      NameC)
                                                      -> Res
        where NilC: FnOnce(&BS) -> Res,
              LeafC: FnOnce(&BS, &X) -> Res,
              BinC: FnOnce(&BS, &Self, &Self) -> Res,
              RootC: FnOnce(&Meta, &Self) -> Res,
              NameC: FnOnce(&Name, &Self) -> Res;
}

impl<X: Debug + Hash + PartialEq + Eq + Clone + 'static> Trie<X> {
    fn mfn(nm: Name, meta: Meta, trie: Self, bs: BS, elt: X, hash: u64) -> Self {
        match trie {
            Trie::Nil(_) if BS::length(bs) < meta.min_depth => {
                let h_ = hash >> 1;
                let bs0 = BS::prepend(0, bs);
                let bs1 = BS::prepend(1, bs);
                let mt0 = Self::nil(bs0);
                let mt1 = Self::nil(bs1);
                if hash % 2 == 0 {
                    Self::bin(bs, Self::mfn(nm, meta, mt0, bs0, elt, h_), mt1)
                } else {
                    Self::bin(bs, mt0, Self::mfn(nm, meta, mt1, bs1, elt, h_))
                }
            }
            Trie::Nil(_) => Trie::Leaf(bs, elt),
            Trie::Leaf(_, e) => {
                let depth = BS::length(bs);
                if depth >= BS::MAX_LEN || e == elt {
                    Self::leaf(bs, e)
                } else if depth < BS::MAX_LEN {
                    Self::mfn(nm,
                              meta,
                              Self::split_atomic(Self::leaf(bs, e)),
                              bs,
                              elt,
                              hash)
                } else {
                    panic!("Bad value found in nadd:\nLeaf(bs, e)\n{:?}",
                           Self::leaf(bs, e));
                }
            }
            Trie::Bin(bs, left, right) => {
                let h_ = hash >> 1;
                if hash % 2 == 0 {
                    let l = Self::mfn(nm, meta, *left, BS::prepend(0, bs), elt, h_);
                    Self::bin(bs, l, *right)
                } else {
                    let r = Self::mfn(nm, meta, *right, BS::prepend(1, bs), elt, h_);
                    Self::bin(bs, *left, r)
                }
            }
            Trie::Name(_, box Trie::Art(a)) => Self::mfn(nm, meta, force(&a), bs, elt, hash),
            t => panic!("Bad value found in nadd:\n{:?}\n", t),
        }
    }

    fn root_mfn(_: Name, nm: Name, trie: Self, elt: X) -> Self {
        match trie {
            Trie::Name(_, box Trie::Art(a)) => {
                match force(&a) {
                    Trie::Root(meta, t) => {
                        let (nm, nm_) = name_fork(nm);
                        let mut hasher = DefaultHasher::new();
                        elt.hash(&mut hasher);
                        let a = Self::mfn(nm_,
                                          meta.clone(),
                                          *t,
                                          BS {
                                              length: 0,
                                              value: 0,
                                          },
                                          elt,
                                          hasher.finish());
                        Self::root(meta, Self::name(nm, Self::art(put(a))))
                    }
                    t @ Trie::Name(_, box Trie::Art(_)) => Self::root_mfn(nm.clone(), nm, t, elt),
                    t => panic!("Non-root node entry to `Trie.extend': {:?}", t),
                }
            }
            _ => panic!("None-name node at entry to `Trie.extend'"),
        }
    }
}

impl<X: Debug + Hash + PartialEq + Eq + Clone + 'static> TrieIntro<X> for Trie<X> {
    fn nil(bs: BS) -> Self {
        Trie::Nil(bs)
    }
    fn leaf(bs: BS, x: X) -> Self {
        Trie::Leaf(bs, x)
    }
    fn bin(bs: BS, l: Self, r: Self) -> Self {
        Trie::Bin(bs, Box::new(l), Box::new(r))
    }
    fn root(meta: Meta, trie: Self) -> Self {
        Trie::Root(meta, Box::new(trie))
    }
    fn name(nm: Name, trie: Self) -> Self {
        Trie::Name(nm, Box::new(trie))
    }
    fn art(art: Art<Self>) -> Self {
        Trie::Art(art)
    }

    fn empty(meta: Meta) -> Self {
        if meta.min_depth > BS::MAX_LEN {
            println!("Cannot make Adapton.Trie with min_depth > {} (given {})",
                     BS::MAX_LEN,
                     meta.min_depth);
        }
        let min = min(meta.min_depth, BS::MAX_LEN);
        let meta = Meta { min_depth: min };
        let nm = name_of_str("empty");
        let (nm1, nm2) = name_fork(nm);
        let mtbs = BS {
            length: 0,
            value: 0,
        };
        let nil_art = thunk!(nm2.clone() =>> Self::nil, bs:mtbs);
        let root_art = thunk!(nm1.clone() =>> Self::root, meta:meta,
                              trie:Self::name(nm2, Self::art(nil_art)));
        Self::name(nm1.clone(), Self::art(root_art))
    }

    fn singleton(meta: Meta, nm: Name, elt: X) -> Self {
        Self::extend(nm, TrieIntro::empty(meta), elt)
    }

    fn extend(nm: Name, trie: Self, elt: X) -> Self {
        let (nm, nm_) = name_fork(nm);
        // let a = Self::root_mfn(nm.clone(), nm_, trie, elt);
        let root_mfn_art = put(Self::root_mfn(nm.clone(), nm_, trie, elt));
        Self::name(nm, Self::art(root_mfn_art))
    }
}

impl<X: Debug + Hash + PartialEq + Eq + Clone + 'static> Hash for Trie<X> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Trie::Nil(bs) => bs.hash(state),
            Trie::Leaf(bs, ref x) => {
                x.hash(state);
                bs.hash(state)
            }
            Trie::Bin(bs, ref left, ref right) => {
                right.hash(state);
                left.hash(state);
                bs.hash(state)
            }
            Trie::Root(ref md, ref t) => {
                t.hash(state);
                md.hash_seeded(state.finish());
            }
            Trie::Name(ref nm, ref t) => {
                t.hash(state);
                nm.hash(state)
            }
            Trie::Art(ref art_t) => art_t.hash(state),
        }
    }
}

impl<X: Debug + Hash + PartialEq + Eq + Clone + 'static> TrieElim<X> for Trie<X> {
    fn find(trie: &Self, elt: &X, i: i64) -> Option<X> {
        Self::elim_ref(trie,
                       |_| None,
                       |_, x| if *elt == *x { Some(x.clone()) } else { None },
                       |_, left, right| if i % 2 == 0 {
                           Self::find(left, elt, i >> 1)
                       } else {
                           Self::find(right, elt, i >> 1)
                       },
                       |_, t| Self::find(t, elt, i),
                       |_, t| Self::find(t, elt, i))
    }

    fn is_empty(trie: &Self) -> bool {
        Self::elim_ref(trie,
                       |_| true,
                       |_, _| false,
                       |_, _, _| false,
                       |_, t| Self::is_empty(t),
                       |_, t| Self::is_empty(t))
    }

    fn split_atomic(trie: Self) -> Self {
        fn suffix(bs: BS, k: i64) -> bool {
            bs.value & k == bs.value
        }
        match trie {
            t @ Trie::Nil(_) |
            t @ Trie::Bin(_, _, _) => t,
            Trie::Leaf(bs, e) => {
                let bs0 = BS::prepend(0, bs);
                let bs1 = BS::prepend(1, bs);
                let mut hasher = DefaultHasher::new();
                e.hash(&mut hasher);
                if suffix(bs1, hasher.finish() as i64) {
                    Self::bin(bs, Self::nil(bs0), Self::leaf(bs1, e))
                } else {
                    Self::bin(bs, Self::leaf(bs0, e), Self::nil(bs1))
                }
            }
            _ => panic!("Bad split_atomic(t)"),
        }
    }

    fn elim<Res, NilC, LeafC, BinC, RootC, NameC>(trie: Self,
                                                  nil: NilC,
                                                  leaf: LeafC,
                                                  bin: BinC,
                                                  root: RootC,
                                                  name: NameC)
                                                  -> Res
        where NilC: FnOnce(BS) -> Res,
              LeafC: FnOnce(BS, X) -> Res,
              BinC: FnOnce(BS, Self, Self) -> Res,
              RootC: FnOnce(Meta, Self) -> Res,
              NameC: FnOnce(Name, Self) -> Res
    {
        match trie {
            Trie::Nil(bs) => nil(bs),
            Trie::Leaf(bs, x) => leaf(bs, x),
            Trie::Bin(bs, l, r) => bin(bs, *l, *r),
            Trie::Name(nm, t) => name(nm, *t),
            Trie::Root(meta, t) => root(meta, *t),
            Trie::Art(art) => {
                let trie = force(&art);
                Self::elim(trie, nil, leaf, bin, root, name)
            }
        }
    }

    fn elim_arg<Arg, Res, NilC, LeafC, BinC, RootC, NameC>(trie: Self,
                                                           arg: Arg,
                                                           nil: NilC,
                                                           leaf: LeafC,
                                                           bin: BinC,
                                                           root: RootC,
                                                           name: NameC)
                                                           -> Res
        where NilC: FnOnce(BS, Arg) -> Res,
              LeafC: FnOnce(BS, X, Arg) -> Res,
              BinC: FnOnce(BS, Self, Self, Arg) -> Res,
              RootC: FnOnce(Meta, Self, Arg) -> Res,
              NameC: FnOnce(Name, Self, Arg) -> Res
    {
        match trie {
            Trie::Nil(bs) => nil(bs, arg),
            Trie::Leaf(bs, x) => leaf(bs, x, arg),
            Trie::Bin(bs, l, r) => bin(bs, *l, *r, arg),
            Trie::Name(nm, t) => name(nm, *t, arg),
            Trie::Root(meta, t) => root(meta, *t, arg),
            Trie::Art(art) => {
                let trie = force(&art);
                Self::elim_arg(trie, arg, nil, leaf, bin, root, name)
            }
        }
    }

    fn elim_ref<Res, NilC, LeafC, BinC, RootC, NameC>(trie: &Self,
                                                      nil: NilC,
                                                      leaf: LeafC,
                                                      bin: BinC,
                                                      root: RootC,
                                                      name: NameC)
                                                      -> Res
        where NilC: FnOnce(&BS) -> Res,
              LeafC: FnOnce(&BS, &X) -> Res,
              BinC: FnOnce(&BS, &Self, &Self) -> Res,
              RootC: FnOnce(&Meta, &Self) -> Res,
              NameC: FnOnce(&Name, &Self) -> Res
    {
        match *trie {
            Trie::Nil(ref bs) => nil(bs),
            Trie::Leaf(ref bs, ref x) => leaf(bs, x),
            Trie::Bin(ref bs, ref l, ref r) => bin(bs, &*l, &*r),
            Trie::Name(ref nm, ref t) => name(nm, &*t),
            Trie::Root(ref meta, ref t) => root(meta, &*t),
            Trie::Art(ref art) => {
                let trie = force(art);
                Self::elim_ref(&trie, nil, leaf, bin, root, name)
            }
        }
    }
}

pub trait SetIntro<X>: Debug + Hash + PartialEq + Eq + Clone + 'static {
    fn empty() -> Self;
    fn add(Self, e: X) -> Self;
    // fn remove(Self, e: &X) -> Self;
    // fn union(Self, Self) -> Self;
    // fn inter(Self, Self) -> Self;
    // fn diff(Self, Self) -> Self;
}

pub trait SetElim<X>: Debug + Hash + PartialEq + Eq + Clone + 'static {
    fn mem(&Self, &X) -> bool;
    fn fold<Res, F>(Self, Res, Rc<F>) -> Res where F: Fn(X, Res) -> Res;
}

impl<X, Set: TrieIntro<X> + TrieElim<X>> SetIntro<X> for Set {
    fn empty() -> Self {
        let meta = Meta { min_depth: 1 };
        Self::empty(meta)
    }

    fn add(set: Self, elt: X) -> Self {
        Self::extend(name_unit(), set, elt)
    }
}

impl<X: Hash, Set: TrieIntro<X> + TrieElim<X>> SetElim<X> for Set {
    fn mem(set: &Self, elt: &X) -> bool {
        let mut hasher = DefaultHasher::new();
        elt.hash(&mut hasher);
        match Set::find(set, elt, hasher.finish() as i64) {
            Some(_) => true,
            None => false,
        }
    }

    fn fold<Res, F>(set: Self, res: Res, f: Rc<F>) -> Res
        where F: Fn(X, Res) -> Res
    {
        Self::elim_arg(set,
                       res,
                       |_, arg| arg,
                       |_, x, arg| f(x, arg),
                       |_, left, right, arg| {
                           Self::fold(right, Self::fold(left, arg, f.clone()), f.clone())
                       },
                       |_, t, arg| Self::fold(t, arg, f.clone()),
                       |_, t, arg| Self::fold(t, arg, f.clone()))
    }
}

pub type Set<X> = Trie<X>;
pub fn trie_fold
    <X, T:TrieElim<X>, Res:Hash+Debug+Eq+Clone+'static, F:'static>
    (t: T, res:Res, f: Rc<F>) -> Res
    where F: Fn(X, Res) -> Res {
    T::elim_arg(t,
                (res, f),
                |_, (arg, _)| arg,
                |_, x, (arg, f)| f(x, arg),
                |_, left, right, (arg, f)| trie_fold(right, trie_fold(left, arg, f.clone()), f),
                |_, t, (arg, f)| trie_fold(t, arg, f),
                |nm, t, (arg, f)| memo!(nm =>> trie_fold, t:t, res:arg ;; f:f))
}

pub fn trie_of_list<X: Hash + Clone + Debug + 'static,
                    T: TrieIntro<X> + 'static,
                    L: ListElim<X> + ListIntro<X> + 'static>
    (list: L)
     -> T {
    list_fold(list,
              T::empty(Meta { min_depth: 1 }),
              Rc::new(|x, trie_acc| T::extend(name_unit(), trie_acc, x)))
}
