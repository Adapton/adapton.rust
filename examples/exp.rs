use std::hash;
use std::hash::Hash;

#[derive(Show,Clone)]
pub enum Exp<'x> {
    Value(int),
    Plus(Box<Exp<'x>>, Box<Exp<'x>>),
    Ref(&'x Exp<'x>) // Refs allow for (non-affine) sharing.
}

pub fn step<'x> (e:&'x Exp<'x>) -> Option<Exp<'x>> {
    match *e {
        Exp::Value(_) => None,
        Exp::Ref(e) => step(e),
        Exp::Plus(ref e1, ref e2) =>
            match **e1 {
                Exp::Value(n) =>
                    match **e2 {
                        Exp::Value(m) => Some(Exp::Value(n+m)),
                        _ => {
                            let e2_ = step(&**e2) ;
                            match e2_ {
                                None => panic!("impossible"),
                                Some(e2_) => Some(Exp::Plus(box Exp::Value(n), box e2_))
                            }
                        }
                    },

                _ => {
                    let e1_ = step(&**e1) ;
                    match e1_ {
                        None => panic!("impossible"),
                        Some(e1_) => Some(Exp::Plus(box e1_, box Exp::Ref(&**e2)))
                    }
                }
            }
    }
}

#[test]
pub fn print() {
    let e0 = Exp::Plus(box Exp::Value(1),
                       box Exp::Plus(box Exp::Value(2),
                                     box Exp::Plus(box Exp::Value(3),
                                                   box Exp::Plus(box Exp::Value(4),
                                                                 box Exp::Value(5))))) ;
    let e1 = step(e0) ;
    let e2 = step(e1) ;
    let e3 = step(e2) ;
    print!("{} {} {} {}", e0, e1, e2, e3);
}

pub fn test1 () {
    let e0 = Exp::Plus(box Exp::Value(1),
                       box Exp::Plus(box Exp::Value(2),
                                     box Exp::Plus(box Exp::Value(3),
                                                   box Exp::Plus(box Exp::Value(4),
                                                                 box Exp::Value(5))))) ;
    let e1 = step(&e0) ;
    match e1 {
        None => (),
        Some(ref e1) => {
            println!("{} --> {}", e0, e1) ;
            let e2 = step( e1 ) ;
            match e2 {
                None => (),
                Some(ref e2) => {
                    println!("{} --> {}", e1, e2) ;
                    let e3 = step( e2 ) ;
                    match e3 {
                        None => (),
                        Some(ref e3) => {
                            println!("{} --> {}", e2, e3) ;
                            let e4 = step( e3 ) ;
                            match e4 {
                                None => (),
                                Some(ref e4) => {
                                    println!("{} --> {}", e3, e4) ;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn test2 () {
    let e0 = Exp::Plus(box Exp::Value(1),
                       box Exp::Plus(box Exp::Value(2),
                                     box Exp::Plus(box Exp::Value(3),
                                                   box Exp::Plus(box Exp::Value(4),
                                                                 box Exp::Value(5))))) ;
    let mut exp = & e0 ;
    loop {
        let s = step(&e0) ;
        match s {
            None => break,
            Some(ref exp_) => {
                println!("{} --> {}", exp, exp_) ;
                // exp = exp_.clone () ;
                continue
            }
        }
    }
}

pub fn main () {
    if true {
        test1 ()
    }
    else {
        test2 ()
    }
}

//     let mut e = e0.clone () ;
// loop {
//         match step(&e) {
//             None => break,
//             Some(e2) => {
//                 println!("{} --> {}", e, e2) ;
//                 //let e3 = e2.clone() ;
//                 //e = e3 ;
//                 continue ;
//             }
//     }}
// }
