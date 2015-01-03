use name::*;
use art::*;

/// Nominal, artful lists: Lists with names and articulation points
#[deriving(Show,Hash,PartialEq,Eq)]
pub enum List<'x,T> {
    Nil,
    Cons(T, Box<List<'x,T>>),
    Name(Name, Box<List<'x,T>>),
    Art(Art<'x, Box<List<'x,T>>>)
}

pub fn copy<'x,T:'x>(list:List<'x,T>) -> List<'x,T> {
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

pub fn map<'x, T:'x, S:'x>
(f:UAr<'x,T,S>, list:List<'x,T>) -> List<'x,S>
{
    match list {
        List::Nil         => List::Nil,
        List::Cons(hd,tl) => List::Cons((*f).call(hd), box map(f,*tl)),
        // - - - - - boilerplate cases - - - - - -
        List::Art(art)    => map(f,*force(art)),
        List::Name(nm,tl) => {
            let (nm1,nm2) = fork(nm) ;
            let art = nart!(nm1, box map(f,*tl));
            List::Name(nm2, box List::Art(art))
        }
    }
}

pub fn contract<'x,F,G,T:'x>
(f:&'x F, g:&'x G, list:List<'x,T>) -> List<'x,T>
where F:Fn(&T,&T) -> bool + 'x,
      G:Fn(T,T) -> T + 'x
{
    match list {
        List::Nil => List::Nil,
        List::Cons(hd1, box list) => {
            match list {
                List::Nil => List::Cons(hd1, box List::Nil),
                List::Cons(hd2, tl) => {
                    if (*f)(&hd1,&hd2) {
                        List::Cons((*g)(hd1,hd2), box contract(f,g,*tl))
                    } else {
                        List::Cons(hd1, box contract(f, g, List::Cons(hd2, tl)))
                    } },
                // - - - - - boilerplate cases - - - - - -
                List::Art(art) => contract(f,g,List::Cons(hd1,force(art))),
                List::Name(nm, tl) => {
                    let (nm1,nm2) = fork(nm) ;
                    let art = nart!(nm1, box contract(f,g,List::Cons(hd1,tl))) ;
                    List::Name(nm2, box List::Art(art))
                }
            }
        },
        // - - - - - boilerplate cases - - - - - -
        List::Art(art)    => contract(f,g,*force(art)),
        List::Name(nm,tl) => {
            let (nm1,nm2) = fork(nm) ;
            let art = nart!(nm1, box contract(f,g,*tl)) ;
            List::Name(nm2, box List::Art(art))
        },
    }
}

pub fn reduce<'x,F:'x,G:'x,T:'x> 
(f:&'x F, g:&'x G, list:List<'x,T>) -> Option<T>
where F: Fn(&T,&T) -> bool,
      G: Fn(T, T) -> T,
{
    match list {
        List::Nil => None,
        List::Cons(hd, box list) => {
            match list {
                List::Nil => Some(hd),
                List::Cons(hd2, tl) => {
                    let hd3 = (*g)(hd,hd2) ;
                    let list = contract(f, g, List::Cons(hd3, tl)) ;
                    reduce (f, g, list)
                },
                // - - - - - boilerplate cases - - - - - -
                List::Name(_,tl) => reduce (f, g, *tl),
                List::Art(art)   => reduce (f, g, *force(art)),
            }},
        // - - - - - boilerplate cases - - - - - -
        List::Name(_,tl) => reduce(f, g, *tl),
        List::Art(art)   => reduce(f, g, *force(art)),
    }
}

pub fn merge<'x,T:'x,Ord:'x> 
(ord:&'x Ord, list1:List<'x,T>, list2:List<'x,T>) -> List<'x,T>
where Ord: Fn(&T,&T) -> bool
{
    match (list1, list2) {
        (List::Nil, list2) => list2,
        (list1, List::Nil) => list1,
        (List::Cons(hd1,tl1),
         List::Cons(hd2,tl2)) =>
            if (*ord)(&hd1,&hd2) {
                List::Cons(hd1, box merge(ord, *tl1, List::Cons(hd2,tl2)))
            } else {
                List::Cons(hd2, box merge(ord, List::Cons(hd1,tl1), *tl2))
            },
        // - - - - - boilerplate cases - - - - -
        (List::Name(nm,tl), list2) => {
            let (nm1,nm2) = fork(nm) ;
            let art = nart!(nm1, box merge(ord, *tl, list2)) ;
            List::Name(nm2, box List::Art(art))
        },
        (list1, List::Name(nm,tl)) => {
            let (nm1,nm2) = fork(nm) ;
            let art = nart!(nm1, box merge(ord, list1, *tl)) ;
            List::Name(nm2, box List::Art(art))
        },
        (List::Art(art), list2) => merge(ord, *force(art), list2),
        (list1, List::Art(art)) => merge(ord, list1, *force(art)),
    }
}

pub fn singletons<'x,T:'x> 
(list:List<'x,T>) -> List<'x,List<'x,T>>
{
    match list {
        List::Nil => List::Nil,
        List::Cons(hd, tl) => {
            List::Cons( List::Cons(hd, box List::Nil),
                        box singletons(*tl) )
        },
        List::Art(art) => singletons(*force(art)),
        List::Name(nm, tl) => {
            let (nm1, nm2) = fork(nm) ;
            let art = nart!(nm2, box singletons(*tl)) ;
            List::Name(nm1, box List::Art(art))
        }
    }
}

pub fn mergesort<'x,T:'x,Ord:'x>
(ord:&'x Ord, list:List<'x,T>) -> List<'x,T>
where Ord: Fn(&T,&T) -> bool
{
    let c = move |&: list1:&List<'x,T>,list2:&List<'x,T>| false ; // TODO
    let m = move |&: list1:List<'x,T>,list2:List<'x,T>| merge(ord,list1,list2) ;
    
    let r = reduce(&c, &m, singletons(list)) ;
     match r {
         None => List::Nil,
         Some(list) => list
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

