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

use std::fmt::{Debug,Formatter,Result};
use std::hash::{Hash,Hasher};
use std::rc::Rc;
use adapton_sigs::Adapton;

#[derive(Hash,Debug)]
pub struct ProgPt {
    hash:u64, // hash of all fields below:

    // Symbolic identity, in Rust semantics:
    symbol:Rc<String>, // via stringify!(...)
    module:Rc<String>, // via module!()

    // Location in local filesystem:
    file:Rc<String>,   // via file!()
    line:usize,        // via line!()
    column:usize,      // via column!()
}

pub struct FnObj<A:Adapton,Arg,Res> {
    prog_pt:ProgPt,
    fn_val:Box<Fn(&mut A, Rc<Arg>) -> Rc<Res>>,
}

impl<A:Adapton,Arg,Res> Debug for FnObj<A,Arg,Res> {
    fn fmt(&self, f: &mut Formatter) -> Result { self.prog_pt.fmt(f) }
}

impl<A:Adapton,Arg,Res> Hash for FnObj<A,Arg,Res> {
    fn hash<H>(&self, state: &mut H) where H: Hasher { self.prog_pt.hash(state) }
    //fn hash_slice<H>(data: &[Self], state: &mut H) where H: Hasher { panic!("TODO") }
}
