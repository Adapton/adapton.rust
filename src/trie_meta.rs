use std::fmt::Debug;
use std::hash::{Hash, Hasher, SipHasher};

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
