#[deriving(Clone,Show,Hash)]
enum NameS { 
    Symbol (Box<String>),
    ForkL (Box<Name>),
    ForkR (Box<Name>),
    Pair (Box<Name>,Box<Name>),
}
pub type Name = NameS;
#[allow(dead_code)]
pub fn symbol (s:String) -> Name {
    NameS::Symbol(box s)
}
#[allow(dead_code)]
pub fn fork (n:Name) -> (Name,Name) {
    let m = n.clone () ;
    (NameS::ForkL(box n), NameS::ForkR(box m))
}    
#[allow(dead_code)]
pub fn pair (n:Name, m:Name) -> Name {
    NameS::Pair(box n, box m)
}
#[allow(dead_code)]
pub fn printstuff () {
    let n = symbol (format!("one")) ;
    let m = symbol (format!("two")) ;
    println!("Adapton: {}, {}", n, m);
    println!("Adapton: {}, {}", fork(n), m);
}
#[test] 
fn test_printstuff () {
    printstuff();
}
