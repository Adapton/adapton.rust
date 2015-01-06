use std::fmt;
use std::hash;
use std::thunk::Invoke;

use name::*;
// pub use lazy::single::Thunk;

pub type UAr<'x,S,T> = Box<Fn<S,T> + 'x>;
// arrow-typed value (in CBPV speak),
// with explicit lifetime 'x, input tuple S, and return value of type T.
//
// Read in CBPV notation has "U(S -> T) with lifetime 'x".
//
// In rust, can be introduced by: 
//   box () (move |&: args:S |{ $body }))


pub type UF<'x,T> = Box<Invoke<(),T> + 'x>;
// value-producing value (in CBPV speak).
// with explicit lifetime 'x and return value of type T.
//
// Read in CBPV notation has "U(F(T)) with lifetime 'x".
//
// In Rust, can be introduced by:
//   box () (move |: () |{ $body }))


/// rustc says that I need an explicit lifetime parameter; I name it `'x`.
struct ArtThunk<'x,T> {
    // TODO: Memo table identity, for comparing thunks with same names
    // TODO: Access to args, for equality check, hashing    
    thunk : UF<'x,T>,
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
                 thunk:box () (move|:()|{
                     x
                 } ) as Box<Invoke()->T>
             }
    }
}

// Create a named thunk
pub fn nart<'x,T:'x> (n:Name, body:Box<Invoke()->T+'x>) -> Art<'x,T> {
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
             box () (move|:()|{ $body }))
    }
);

// pub fn force<'x,T> (art:Art<'x,T>) -> T {
//     match art {
//         ArtCon {name:_,thunk:t} => {
//             let result = (*t.thunk).invoke(()) ;
//             result
//         }
//     }
// }

pub fn force<'x,T> (art:Art<'x,T>) -> T {
   panic!("Oh no something bad has happened!")
}

/// Needed this form to get List Iterator to borrow-check.
pub fn force_ref<'x,T> (art:&'x Art<'x,T>) -> &'x T {
   panic!("Oh no something bad has happened!")
}
