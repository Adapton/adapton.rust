use std::fmt;
use std::hash;
use name::*;
pub use lazy::single::Thunk;

struct ArtThunk<T> {
    // TODO: Memo table identity, for comparing thunks with same names
    // TODO: Access to args, for equality check, hashing
    thunk : Thunk<T>
}

impl<T> fmt::Show for ArtThunk<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(ArtThunk)")
    }
}

#[deriving(Show)]
struct ArtCon<T> {
    name : Name,
    thunk : ArtThunk<T>
}

impl<T> PartialEq for ArtCon<T> {
    fn eq(&self, other: &ArtCon<T>) -> bool {
        self.name.eq( &other.name )
    }
    fn ne(&self, other: &ArtCon<T>) -> bool {
        !(self.eq(other))
    }
}

impl<T> Eq for ArtCon<T> {
    fn assert_receiver_is_total_eq(&self) {  }
}

impl<S: hash::Writer, T> hash::Hash<S> for ArtCon<T> {
    fn hash(&self, state: &mut S) {
        self.name.hash( state )
    }
}

pub type Art<T> = ArtCon<T>;

#[allow(dead_code)]
// Create a named cell
pub fn cell<T> (n:Name, x:T) -> Art<T> {
    //! TODO: Cache the art based on the name n
    ArtCon { name  : n,
             thunk : ArtThunk{thunk:Thunk::evaluated(x)} }
}
