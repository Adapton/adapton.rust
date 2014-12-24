use name::*;
use art::*;
//use self::List::{Cons, Nil, NomArt};


/// A functional, shareable, persistent singly linked list.
//#[deriving(Show)]
pub enum List<T> {
    Cons(T, Box<List<T>>),
    Nil,
    Name(Name,Box<List<T>>),
    Art(Art<Box<List<T>>>)
}


#[test]
pub fn sizeofstuff () {
//    println!("{} {} {}", 
//             List::Nil, 
//             List::Cons(1,box List::Nil), 
//             List::Nil
//             List::Name(symbol(format!("one")),List::Nil)
//             );
}

// impl<T> List<T> {
//     /// Construct a new, empty list.
//     #[inline]
//     pub fn new() -> List<T> { Nil }
// }

// impl<T: Send + Sync> List<T> {
//     /// Create a list with one element in it.
//     #[inline]
//     pub fn singleton(n:Name,val: T) -> List<T> { Cons(val, Art::new(Nil)) }

//     /// Get the head of a list.
//     pub fn head(&self) -> Option<&T> {
//         match *self {
//             Nil => None,
//             Cons(ref head, _) => Some(head)
//         }
//     }

//     /// Get the tail of a list.
//     pub fn tail(&self) -> Option<Art<List<T>>> {
//         match *self {
//             Nil => None,
//             Cons(_, ref tail) => Some(tail.clone())
//         }
//     }

//     /// Get an iterator over the items in a list.
//     pub fn iter<'a>(&'a self) -> ListItems<'a, T> {
//         ListItems {
//             list: self
//         }
//     }
// }

// /// An iterator over the items in a list.
// pub struct ListItems<'a, T: 'a> {
//     list: &'a List<T>
// }

// impl<'a, T: Send + Sync> Iterator<&'a T> for ListItems<'a, T> {
//     fn next(&mut self) -> Option<&'a T> {
//         match *self.list {
//             Cons(ref head, ref tail) => {
//                 self.list = &**tail;
//                 Some(head)
//             },
//             Nil => None
//         }
//     }
// }

