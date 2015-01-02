use std::fmt;
use std::hash;
use std::thunk;

use name::*;
// pub use lazy::single::Thunk;

struct ArtThunk<'x,T> {
    // TODO: Memo table identity, for comparing thunks with same names
    // TODO: Access to args, for equality check, hashing    
    thunk : Box<thunk::Invoke<(),T> + 'x>,
    value : Option<T>
}

impl<'x,T> fmt::Show for ArtThunk<'x,T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(ArtThunk)")
    }
}

#[deriving(Show)]
struct ArtCon<'x,T> {
    name : Name,
    thunk : ArtThunk<'x,T>
}

impl<'x,T> PartialEq for ArtCon<'x,T> {
    fn eq(&self, other: &ArtCon<T>) -> bool {
        self.name.eq( &other.name )
    }
    fn ne(&self, other: &ArtCon<T>) -> bool {
        !(self.eq(other))
    }
}

impl<'x,T> Eq for ArtCon<'x,T> {
    fn assert_receiver_is_total_eq(&self) {  }
}

impl<'x,S: hash::Writer, T> hash::Hash<S> for ArtCon<'x,T> {
    fn hash(&self, state: &mut S) {
        self.name.hash( state )
    }
}

pub type Art<'x,T> = ArtCon<'x,T>;

// Create a named cell
pub fn cell<'x,T:'x> (n:Name, x:T) -> Art<'x,T> {
    //! TODO: Cache the art based on the name n
    ArtCon { name  : n,
             thunk : ArtThunk{
                 value:None,
                 thunk:box () (move |:() |{
                     x
                 } ) as Box<thunk::Invoke<(),T>>
             }
    }
}


// Create a named cell
pub fn nart<'x,T:'x> (n:Name, body:Box<thunk::Invoke<(),T>+'x>) -> Art<'x,T> {
    //! TODO: Cache the art based on the name n
    ArtCon { name  : n,
             thunk : ArtThunk{
                 value:None,
                 thunk:body
             }
    }
}

#[macro_export]
macro_rules! nart (
    ($name:expr, $body:expr) => {
        nart($name,
             box () (move |:() |{ $body }))
    }
);

// pub fn force<'x,T> (art:Art<'x,T>) -> T {
//     match art {
//         ArtCon {name:_,thunk:t} => t ()
//     }
// }

pub fn force<'x,T> (art:Art<'x,T>) -> T {
    panic!("Oh no something bad has happened!")
}
