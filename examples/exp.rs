use std::hash;
use std::hash::Hash;

#[derive(Show)]
pub enum Exp<'x> {
    Value(int),
    Plus(Box<Exp<'x>>, Box<Exp<'x>>),
    Ref(&'x Exp<'x>)
}

#[test]
pub fn print() {
    let e = Exp::Plus(box Exp::Value(1),
                      box Exp::Plus(box Exp::Value(2),
                                    box Exp::Plus(box Exp::Value(3),
                                                  box Exp::Plus(box Exp::Value(4),
                                                                box Exp::Value(5))))) ;
    print!("{}", e);
}

pub fn main () {
    let e = Exp::Plus(box Exp::Value(1),
                      box Exp::Plus(box Exp::Value(2),
                                    box Exp::Plus(box Exp::Value(3),
                                                  box Exp::Plus(box Exp::Value(4),
                                                                box Exp::Value(5))))) ;
    print!("{}", e);
}
