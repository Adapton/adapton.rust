use std::fmt::Debug;
use std::hash::{Hash, Hasher, SipHasher};
use std::collections::hash_map::DefaultHasher;
use std::cmp::min;

use adapton::bitstring::*;
use adapton::engine::{Art, Name, force, name_fork, name_of_str, name_unit, put};

/// Probablistically Balanced Trie
/// Rough implementation of probabilistic tries from OOPSLA 2015 paper.
///
/// See also: [Tries in OCaml](http://github.com/plum-umd/adapton.ocaml)
#[derive(Debug,Clone)]
pub enum Trie<X> {
    Nil(BS),
    Leaf(BS, X),
    Bin(BS, Box<Trie<X>>, Box<Trie<X>>),
    Root(Meta, Box<Trie<X>>),
    Name(Name, Box<Trie<X>>),
    Art(Art<Trie<X>>),
}

#[derive(Debug,PartialEq,Eq,Clone)]
pub enum IFreq {
    Never,
    Depth(u64, u64),
    First(u64),
    Const(u64),
}

impl Hash for IFreq {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            IFreq::Never => {
                "Adapton.Trie.Meta.Freq#Never".hash(state);
            }
            IFreq::Depth(i, j) => {
                "Adapton.Trie.Meta.Freq#Depth".hash(state);
                i.hash(state);
                j.hash(state);
            }
            IFreq::First(n) => {
                "Adapton.Trie.Meta.Freq#First".hash(state);
                n.hash(state);
            }
            IFreq::Const(i) => {
                "Adapton.Trie.Meta.Freq#Const".hash(state);
                i.hash(state);
            }
        }
    }
}

/// Metadata held by the root node.
#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub struct Meta {
    min_depth: i64,
    ifreq: IFreq,
}

pub trait MetaT {
    fn hash_seeded(&self, u64);
}

impl MetaT for Meta {
    fn hash_seeded(&self, seed: u64) {
        let mut hasher = SipHasher::new_with_keys(0, seed);
        "Adapton.Trie.Meta".hash(&mut hasher);
        self.min_depth.hash(&mut hasher);
        self.ifreq.hash(&mut hasher);
    }
}

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
    fn find(&Self, i64) -> Option<X>;
    fn is_empty(&Self) -> bool;
    fn split_atomic(Self) -> Self;

    fn elim<Res, NilC, LeafC, BinC, RootC, NameC>(Self, NilC, LeafC, BinC, RootC, NameC) -> Res
        where NilC: FnOnce(BS) -> Res,
              LeafC: FnOnce(BS, X) -> Res,
              BinC: FnOnce(BS, Self, Self) -> Res,
              RootC: FnOnce(Meta, Self) -> Res,
              NameC: FnOnce(Name, Self) -> Res;

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
                let h_ = hash << 1;
                let bs0 = BS::prepend(0, bs);
                let bs1 = BS::prepend(1, bs);
                let mt0 = Self::nil(bs0);
                let mt1 = Self::nil(bs1);
                if BS::is_set(0, bs.value) {
                    Self::bin(bs, mt0, Self::mfn(nm, meta, mt1, bs1, elt, h_))
                } else {
                    Self::bin(bs, Self::mfn(nm, meta, mt0, bs0, elt, h_), mt1)
                }
            }
            Trie::Nil(_) => Trie::Leaf(bs, elt),
            Trie::Leaf(bs_, e) => {
                let depth = BS::length(bs);
                if depth >= BS::MAX_LEN || e == elt {
                    Self::leaf(bs, elt)
                } else if depth < BS::MAX_LEN {
                    Self::mfn(nm,
                              meta,
                              Self::split_atomic(Self::leaf(bs_, e)),
                              bs,
                              elt,
                              hash)
                } else {
                    panic!("Bad value found in nadd:\nLeaf(bs, e)\n");
                }
            }
            Trie::Bin(bs, left, right) => {
                let h_ = hash << 1;
                if BS::is_set(0, bs.value) {
                    let r = Self::mfn(nm, meta, *right, BS::prepend(1, bs), elt, h_);
                    Self::bin(bs, *left, r)
                } else {
                    let l = Self::mfn(nm, meta, *left, BS::prepend(0, bs), elt, h_);
                    Self::bin(bs, l, *right)
                }
            }
            Trie::Name(_, box Trie::Art(a)) => Self::mfn(nm, meta, force(&a), bs, elt, hash),
            t => panic!("Bad value found in nadd:\n{:?}\n", t),
        }
    }

    fn root_mfn(_: Name, nm:Name, trie: Self, elt: X) -> Self {
        match trie {
            Trie::Name(_, box Trie::Art(a)) =>
                match force(&a) {
                    Trie::Root (meta, t) => {
                        let (nm, nm_) = name_fork(nm);
                        let mut hasher = DefaultHasher::new();
                        elt.hash(&mut hasher);
                        let a = Self::mfn(nm_, meta.clone(), *t, BS { length:0, value:0 }, elt, hasher.finish());
                        Self::root(meta, Self::name(nm, Self::art(put(a))))
                    }
                    _ => panic!("Non-root node entry to `Trie.extend'")
                },
            _ => panic!("None-name node at entry to `Trie.extend'")
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

    fn empty(meta:Meta) -> Self {
        if meta.min_depth > BS::MAX_LEN {
            println!("Cannot make Adapton.Trie with min_depth > {} (given {})", BS::MAX_LEN, meta.min_depth);
        }
        let min = min(meta.min_depth, BS::MAX_LEN);
        let meta = Meta { min_depth:min, ifreq:meta.ifreq };
        let nm = name_of_str("empty");
        let (nm1, nm2) = name_fork(nm);
        let mtbs = BS { length:0, value:0 };
        Self::name(nm1, Self::root(meta, Self::name(nm2, Self::nil(mtbs))))
    }

    fn singleton(meta: Meta, nm: Name, elt: X) -> Self {
        Self::extend(nm, Self::empty(meta), elt)
    }

    fn extend(nm: Name, trie: Self, elt: X) -> Self {
        let (nm, nm_) = name_fork(nm);
        let a = Self::root_mfn(nm.clone(), nm_, trie, elt);
        Self::name(nm, Self::art(put(a)))
    }
}

impl<X: Debug + Hash + PartialEq + Eq + Clone + 'static> PartialEq for Trie<X> {
    fn eq(&self, other: &Trie<X>) -> bool {
        match (self, other) {
            (&Trie::Nil(bs_self), &Trie::Nil(bs_other)) => bs_self == bs_other,
            (&Trie::Leaf(bs_self, ref e_self), &Trie::Leaf(bs_other, ref e_other)) => {
                let bs_equal = bs_self == bs_other;
                bs_equal && e_self == e_other
            }
            (&Trie::Bin(bs, ref left, ref right),
             &Trie::Bin(bs_other, ref left_other, ref right_other)) => {
                bs == bs_other && left == left_other && right == right_other
            }
            (&Trie::Root(ref md, ref t), &Trie::Root(ref md_other, ref t_other)) => {
                md == md_other && t == t_other
            }
            (&Trie::Name(ref nm, ref t), &Trie::Name(ref nm_other, ref t_other)) => {
                nm == nm_other && t == t_other
            }
            _ => false,
        }
    }
}

impl<X: Debug + Hash + PartialEq + Eq + Clone + 'static> Eq for Trie<X> {}

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
    fn find(trie: &Self, i: i64) -> Option<X> {
        Self::elim_ref(trie,
                       |_| None,
                       |_, x| Some(x.clone()),
                       |_, left, right| if i % 2 == 0 {
                           Self::find(left, i >> 1)
                       } else {
                           Self::find(right, i >> 1)
                       },
                       |_, t| Self::find(t, i),
                       |_, t| Self::find(t, i))
    }

    fn is_empty(trie: &Self) -> bool {
        Self::elim_ref(trie,
                       |_| true,
                       |_,_| false,
                       |_,_,_| false,
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

#[test]
fn test_is_empty() {
    let meta = Meta { min_depth:1, ifreq:IFreq::Const(1) };
    let empty = Trie::<usize>::empty(meta.clone());
    let singleton = Trie::<usize>::singleton(meta.clone(), name_unit(), 7);
    assert!(Trie::is_empty(&Trie::<usize>::nil(BS { length:0, value:0 })));
    assert!(Trie::is_empty(&empty));

    assert!(!Trie::is_empty(&Trie::<usize>::leaf(BS { length:0, value:0 }, 0)));
    assert!(!Trie::is_empty(&singleton));
}
