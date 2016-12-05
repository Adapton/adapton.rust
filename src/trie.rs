use std::fmt::Debug;
use std::hash::{Hash, Hasher, SipHasher};

use adapton::bitstring::*;
use adapton::engine::{Art, Name, force};

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
            },
            IFreq::Depth(i, j) => {
                "Adapton.Trie.Meta.Freq#Depth".hash(state);
                i.hash(state);
                j.hash(state);
            },
            IFreq::First(n) => {
                "Adapton.Trie.Meta.Freq#First".hash(state);
                n.hash(state);
            },
            IFreq::Const(i) => {
                "Adapton.Trie.Meta.Freq#Const".hash(state);
                i.hash(state);
            },
        }
    }
}

/// Metadata held by the root node.
#[derive(Debug,PartialEq,Eq,Hash,Clone)]
pub struct Meta {
    min_depth: u64,
    ifreq: IFreq,
}

pub trait MetaT {
    fn hash_seeded(&self, u64);
}

impl MetaT for Meta {
    fn hash_seeded(&self, seed:u64) {
        let mut hasher = SipHasher::new_with_keys(0, seed);
        "Adapton.Trie.Meta".hash(&mut hasher);
        self.min_depth.hash(&mut hasher);
        self.ifreq.hash(&mut hasher);
    }
}

pub trait TrieIntro<X> : Debug+Hash+PartialEq+Eq+Clone+'static {
    fn nil() -> Self;
    fn leaf(BS, X) -> Self;
    fn bin(BS, Self, Self) -> Self;
    fn root(Meta, Self) -> Self;

    // requisite "adaptonic" constructors: `name` and `art`:
    fn name(Name, Self) -> Self;
    fn art(Art<Self>) -> Self;
}

pub trait TrieElim<X> : Debug+Hash+PartialEq+Eq+Clone+'static {
    fn find(Self, i64) -> Option<X>;

    fn elim<Res,NilC,LeafC,BinC,RootC,NameC>
        (Self, NilC, LeafC, BinC, RootC, NameC) -> Res
        where NilC : FnOnce(BS) -> Res
        ,LeafC : FnOnce(BS, X) -> Res
        ,BinC : FnOnce(BS, Self, Self) -> Res
        ,RootC : FnOnce(Meta, Self) -> Res
        ,NameC : FnOnce(Name, Self) -> Res;

    fn elim_ref<Res,NilC,LeafC,BinC,RootC,NameC>
        (&Self, NilC, LeafC, BinC, RootC, NameC) -> Res
        where NilC : FnOnce(&BS) -> Res
        ,LeafC : FnOnce(&BS, &X) -> Res
        ,BinC : FnOnce(&BS, &Self, &Self) -> Res
        ,RootC : FnOnce(&Meta, &Self) -> Res
        ,NameC : FnOnce(&Name, &Self) -> Res;
}

impl <X:Debug+Hash+PartialEq+Eq+Clone+'static>
    TrieIntro<X>
    for Trie<X> {
        fn nil() -> Self {
            Trie::Nil(BS { length: 0, value: 0 })
        }
        fn leaf(bs:BS, x:X) -> Self {
            Trie::Leaf(bs, x)
        }
        fn bin(bs:BS, l:Self, r:Self) -> Self {
            Trie::Bin(bs, Box::new(l), Box::new(r))
        }
        fn root(meta:Meta, trie:Self) -> Self {
            Trie::Root(meta, Box::new(trie))
        }
        fn name(nm:Name, trie:Self) -> Self {
            Trie::Name(nm, Box::new(trie))
        }
        fn art(art:Art<Self>) -> Self {
            Trie::Art(art)
        }
}

impl <X:Debug+Hash+PartialEq+Eq+Clone+'static>
    Hash
    for Trie<X> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            match *self {
                Trie::Nil(bs) => bs.hash(state),
                Trie::Leaf(bs, ref x) => {
                    x.hash(state);
                    bs.hash(state)
                },
                Trie::Bin(bs, ref left, ref right) => {
                    right.hash(state);
                    left.hash(state);
                    bs.hash(state)
                },
                Trie::Root(ref md, ref t) => {
                    t.hash(state);
                    md.hash_seeded(state.finish());
                },
                Trie::Name(ref nm, ref t) => {
                    t.hash(state);
                    nm.hash(state)
                },
                Trie::Art(ref art_t) => art_t.hash(state),
            }
        }
}

impl <X:Debug+Hash+PartialEq+Eq+Clone+'static>
    TrieElim<X>
    for Trie<X> {
        fn find(trie: Self, i:i64) -> Option<X> {
            fn _loop<X:Debug+Hash+PartialEq+Eq+Clone+'static>(h: i64, t: Trie<X>) -> Option<X> {
                match t {
                    Trie::Nil(_) => None,
                    Trie::Leaf(_, x) => Some(x),
                    Trie::Bin(_, left, right) => {
                        _loop(h >> 1, if h % 2 == 0 { *left } else { *right })
                    },
                    Trie::Root(_, t) => _loop(h, *t),
                    Trie::Name(_, t) => _loop(h, *t),
                    Trie::Art(art_t) => _loop(h, force(&art_t)),
                }
            };
            _loop(i,trie)
        }

        fn elim<Res,NilC,LeafC,BinC,RootC,NameC>
            (trie:Self, nil:NilC, leaf:LeafC, bin:BinC, root:RootC, name:NameC) -> Res
            where NilC : FnOnce(BS) -> Res
            ,LeafC : FnOnce(BS, X) -> Res
            ,BinC : FnOnce(BS, Self, Self) -> Res
            ,RootC : FnOnce(Meta, Self) -> Res
            ,NameC : FnOnce(Name, Self) -> Res
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

        fn elim_ref<Res,NilC,LeafC,BinC,RootC,NameC>
            (trie:&Self, nil:NilC, leaf:LeafC, bin:BinC, root:RootC, name:NameC) -> Res
            where NilC : FnOnce(&BS) -> Res
            ,LeafC : FnOnce(&BS, &X) -> Res
            ,BinC : FnOnce(&BS, &Self, &Self) -> Res
            ,RootC : FnOnce(&Meta, &Self) -> Res
            ,NameC : FnOnce(&Name, &Self) -> Res
        {
            match *trie {
                Trie::Nil(ref bs) => nil(bs),
                Trie::Leaf(ref bs, ref x) => leaf(bs, x),
                Trie::Bin(ref bs, ref l, ref r) => bin(bs, &*l, &*r),
                Trie::Name(ref nm, ref t) => name(nm, &*t),
                Trie::Root(ref meta, ref t) => root(meta, &*t),
                Trie::Art(ref art) => {
                    let trie = force(&art);
                    Self::elim_ref(&trie, nil, leaf, bin, root, name)
                }
            }
        }
}
