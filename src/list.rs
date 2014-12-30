use name::*;
use art::*;

/// Nominal, artful lists: Lists with names and articulation points
#[deriving(Show,Hash,PartialEq,Eq)]
pub enum List<T> {
    Nil,
    Cons(T, Box<List<T>>),
    Name(Name, Box<List<T>>),
    Art(Art<Box<List<T>>>)
}

pub fn copy<T>(list:List<T>) -> List<T>
where T:'static {
    match list {
        List::Nil         => List::Nil,
        List::Cons(hd,tl) => List::Cons(hd, box copy(*tl)),
        List::Art(art)    => copy(*force(art)),
        List::Name(nm,tl) => {
            let (nm1,nm2) = fork(nm) ;
            let art = nart!(nm1, box copy(*tl)) ;
            List::Name(nm2, box List::Art(art))
        }
    }
}

pub fn map<T,S,F>(f:F,list:List<T>) -> List<S>
where T:'static, S:'static,
      F:Fn(T) -> S + 'static
{
    match list {
        List::Nil         => List::Nil,
        List::Cons(hd,tl) => List::Cons(f(hd), box map(f,*tl)),
        List::Art(art)    => map(f,*force(art)),
        List::Name(nm,tl) => {
            let (nm1,nm2) = fork(nm) ;
            let art = nart!(nm1, box map(f,*tl)) ;
            List::Name(nm2, box List::Art(art))
        }
    }
}

#[test]
pub fn construct_list () {
    let z : List<int> = List::Nil;
    let y : List<int> = List::Cons(1, box z);
    let x : List<int> = List::Art(cell(symbol(format!("two")), box y));
    let l : List<int> = List::Name(symbol(format!("one")), box x);
    println!("constructed list: {}", l);
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

