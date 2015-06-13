// Adapton uses memoization under the covers, which needs an efficient
// mechanism to search for function pointers and them for equality.
//
// Meanwhile, Rust does not provide Eq and Hash implementations for
// trait Fn.  So, to identify Rust functions as values that we can
// hash and compare, we need to bundle additional static information
// along with the function pointer as use this data as a proxy for the
// function itself.  The idea is that this information uniquely
// identifies the function pointer (i.e., two distinct functions will
// always have two distinct identities).
//
// TODO: make a macro for wrapping Fn's as FnObj's, defined below:
use std::hash::{Hash,Hasher,SipHasher};

#[derive(PartialEq,Eq,Clone,Hash,Debug)]
pub struct ProgPt {
    pub hash:u64, // hash of all fields below:

    // Symbolic identity, in Rust semantics:
    pub symbol:&'static str, // via stringify!(...)
    // module:Rc<String>, // via module!()

    // Location in local filesystem:
    pub file:&'static str,   // via file!()
    pub line:u32,        // via line!()
    pub column:u32,      // via column!()
}

pub fn my_hash<T>(obj: T) -> u64
    where T: Hash
{
    let mut hasher = SipHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}
