mod name {
    #[deriving(Clone,Show,Hash)]
    enum Name { 
        Symbol (Box<String>),
        ForkL (Box<Name>),
        ForkR (Box<Name>),
        Pair (Box<Name>,Box<Name>),
    }
    #[allow(dead_code)]
    pub fn symbol (s:String) -> Name {
        Name::Symbol(box s)
    }
    #[allow(dead_code)]
    pub fn fork (n:Name) -> (Name,Name) {
        let m = n.clone () ;
        (Name::ForkL(box n), Name::ForkR(box m))
    }    
    #[allow(dead_code)]
    pub fn pair (n:Name, m:Name) -> Name {
        Name::Pair(box n, box m)
    }
    #[test]
    fn printstuff () {
        let n = symbol (format!("one")) ;
        let m = symbol (format!("two")) ;
        println!("Adapton: {}, {}", n, m);
        println!("Adapton: {}, {}", fork(n), m);
    }
}
