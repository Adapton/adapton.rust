// Adapton uses memoization under the covers, which needs an efficient
// mechanism to search for function pointers and compare them for
// equality.
//
// Meanwhile, Rust does not provide Eq and Hash implementations for
// trait Fn.  So, to identify Rust functions as values that we can
// hash and compare, we need to bundle additional static information
// along with the function pointer as use this data as a proxy for the
// function itself.  The idea is that this information uniquely
// identifies the function pointer (i.e., two distinct functions will
// always have two distinct identities).
//
use std::hash::{Hash,Hasher,SipHasher};
use std::fmt::{Formatter,Result,Debug};
//use std::mem::replace;

#[derive(PartialEq,Eq,Clone,Hash)]
pub struct ProgPt {
    // Symbolic identity, in Rust semantics:
    pub symbol:&'static str, // via stringify!(...)
    // module:Rc<String>, // via module!()

    // Location in local filesystem:
    //pub file:&'static str,   // via file!()
    //pub line:u32,        // via line!()
    //pub column:u32,      // via column!()
}

impl Debug for ProgPt {
    fn fmt(&self, f: &mut Formatter) -> Result { self.symbol.fmt(f) }
}

pub fn my_hash<T>(obj: T) -> u64
    where T: Hash
{
    let mut hasher = SipHasher::new();
    obj.hash(&mut hasher);
    hasher.finish()
}

#[macro_export]
macro_rules! prog_pt {
    ($symbol:expr) => {{
        ProgPt{
            symbol:$symbol,
            //file:file!(),
            //line:line!(),
            //column:column!(),
        }
    }}
}

#[macro_export]
macro_rules! thunk {
    ( $st:expr , $nm:expr =>> $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
        ($st).thunk
            (ArtIdChoice::Nominal($nm),
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*, _) = args ;
                     $f :: < $( $ty ),* >( st, $( $lab ),* )
                 })),
             ( $( $arg ),*, ()),
             ()
             )
    }}
    ;
    ( $st:expr , $nm:expr =>> $f:ident , $( $lab:ident : $arg:expr ),* ) => {{
        ($st).thunk
            (ArtIdChoice::Nominal($nm),
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*, _) = args ;
                     $f ( st, $( $lab ),* )
                 })),
             ( $( $arg ),*, () ),
             ()
             )
    }}
    ;
    ( $st:expr , $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
        ($st).thunk
            (ArtIdChoice::Structural,
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*, _) = args ;
                     $f :: < $( $ty ),* >( st, $( $lab ),* )
                 })),
             ( $( $arg ),*, () ),
             ()
             )
    }}
    ;
    ( $st:expr , $f:path , $( $lab:ident : $arg:expr ),* ) => {{
        ($st).thunk
            (ArtIdChoice::Structural,
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*, _) = args ;
                     $f ( st, $( $lab ),* )
                 })),
             ( $( $arg ),*, () ),
             ()
             )        
    }}
    ;
}

#[macro_export]
macro_rules! memo {
    ( $st:expr , $nm:expr =>> $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
        let t = ($st).thunk
            (ArtIdChoice::Nominal($nm),
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*) = args ;
                     $f :: < $( $ty ),* >( st, $( $lab ),* )
                 })),
             ( $( $arg ),*, ),
             ()
             );
        ($st).force(&t)
    }}
    ;
    ( $st:expr , $nm:expr =>> $f:path , $( $lab:ident : $arg:expr ),* ) => {{
        let t = ($st).thunk
            (ArtIdChoice::Nominal($nm),
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*) = args ;
                     $f ( st, $( $lab ),* )
                 })),
             ( $( $arg ),* ),
             ()
             );
        ($st).force(&t)
    }}
    ;
    ( $st:expr , $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
        let t = ($st).thunk
            (ArtIdChoice::Structural,
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*) = args ;
                     $f :: < $( $ty ),* >( st, $( $lab ),* )
                 })),
             ( $( $arg ),* ),
             ()
             );
        ($st).force(&t)
    }}
    ;
    ( $st:expr , $f:path , $( $lab:ident : $arg:expr ),* ) => {{
        let t = ($st).thunk
            (ArtIdChoice::Structural,
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*, _) = args ;
                     $f ( st, $( $lab ),* )
                 })),
             ( $( $arg ),*, () ),
             ()
             );
        ($st).force(&t)
    }}
    ;
    ( $st:expr , $nm:expr =>> $f:path , $( $lab1:ident : $arg1:expr ),* ;; $( $lab2:ident : $arg2:expr ),* ) => {{
        let t = ($st).thunk
            (ArtIdChoice::Nominal($nm),
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args1, args2|{
                     let ($( $lab1 ),*, _) = args1 ;
                     let ($( $lab2 ),*, _) = args2 ;
                     $f ( st, $( $lab1 ),* , $( $lab2 ),* )
                 })),
             ( $( $arg1 ),*, () ),
             ( $( $arg2 ),*, () ),
             );
        ($st).force(&t)
    }}
    ;
}


#[macro_export]
macro_rules! eager {
    ( $st:expr , $nm:expr =>> $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
        let t = ($st).thunk
            (ArtIdChoice::Nominal($nm),
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*) = args ;
                     $f :: < $( $ty ),* >( st, $( $lab ),* )
                 })),
             ( $( $arg ),*, ),
             ()
             );
        let res = ($st).force(&t) ;
        (t, res)
    }}
    ;
    ( $st:expr , $nm:expr =>> $f:path , $( $lab:ident : $arg:expr ),* ) => {{
        let t = ($st).thunk
            (ArtIdChoice::Nominal($nm),
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*) = args ;
                     $f ( st, $( $lab ),* )
                 })),
             ( $( $arg ),* ),
             ()
             );
        let res = ($st).force(&t) ;
        (t, res)
    }}
    ;
    ( $st:expr , $f:ident :: < $( $ty:ty ),* > , $( $lab:ident : $arg:expr ),* ) => {{
        let t = ($st).thunk
            (ArtIdChoice::Structural,
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*) = args ;
                     $f :: < $( $ty ),* >( st, $( $lab ),* )
                 })),
             ( $( $arg ),* ),
             ()
             );
        let res = ($st).force(&t) ;
        (t, res)
    }}
    ;
    ( $st:expr , $f:path , $( $lab:ident : $arg:expr ),* ) => {{
        let t = ($st).thunk
            (ArtIdChoice::Structural,
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args, _|{
                     let ($( $lab ),*, _) = args ;
                     $f ( st, $( $lab ),* )
                 })),
             ( $( $arg ),*, () ),
             ()
             );
        let res = ($st).force(&t) ;
        (t, res)
    }}
    ;
    ( $st:expr , $nm:expr =>> $f:path , $( $lab1:ident : $arg1:expr ),* ;; $( $lab2:ident : $arg2:expr ),* ) => {{
        let t = ($st).thunk
            (ArtIdChoice::Nominal($nm),
             prog_pt!(stringify!($f)),
             Rc::new(Box::new(
                 |st, args1, args2|{
                     let ($( $lab1 ),*, _) = args1 ;
                     let ($( $lab2 ),*, _) = args2 ;
                     $f ( st, $( $lab1 ),* , $( $lab2 ),* )
                 })),
             ( $( $arg1 ),*, () ),
             ( $( $arg2 ),*, () ),
             );
        let res = ($st).force(&t) ;
        (t, res)
    }}
    ;
}


// https://doc.rust-lang.org/book/macros.html
//
// macro_rules! o_O {
//     (
//         $(
//             $x:expr; [ $( $y:expr ),* ]
//          );*
//     ) => {
//         &[ $($( $x + $y ),*),* ]
//     }
// }
//
// fn main() {
//     let a: &[i32]
//         = o_O!(10; [1, 2, 3];
//                20; [4, 5, 6]);
//
//     assert_eq!(a, [11, 12, 13, 24, 25, 26]);
// }

// TODO: Need to gensym a variable for each argument below:
//
// macro_rules! thunk {
//     ( $f:ident , $st:expr , $( $arg:expr ),* ) => {
//         let fval = Rc::new(Box::new(
//             |st, args|{
//                 let ($( $arg ),*) = args ;
//                 f( st, $( $arg ),* )
//             })) ;
//         ($st).thunk (ArtId::Eager, prog_pt!($f), fval, $( $arg ),* )
//     }}
