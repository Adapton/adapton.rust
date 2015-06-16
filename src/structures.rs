//#[macro_use]
//use adapton_syntax::* ;
use adapton_sigs::* ;
// use adapton_state::AdaptonState ;

trait List {
    type Hd ;
    type List ;
    fn nil <A:Adapton> (&mut A) -> Self::List ;
    fn cons<A:Adapton> (&mut A, Self::Hd, Self::List) -> Self::List ;
    
    // requisite "adaptonic" constructors: `name` and `art`:
    fn name<A:Adapton> (&mut A, A::Name, Self::List) -> Self::List ;
    fn art<A:Adapton>  (&mut A, Art<Self::List,A::Loc>) -> Self::List ;

    fn fold<A:Adapton,Res,Cons> (&mut A, Self::List, Res, Cons) -> Res
        where Cons:Fn(&mut A, Res, Self::Hd) -> Res ;
}

trait Tree {
    type Leaf ;
    type Bin ;
    type Tree ;
    fn nil <A:Adapton> (&mut A) -> Self::Tree ;
    fn leaf<A:Adapton> (&mut A, Self::Leaf) -> Self::Tree ;
    fn bin<A:Adapton>  (&mut A, Self::Bin, Self::Tree, Self::Tree) -> Self::Tree ;

    // requisite "adaptonic" constructors: `name` and `art`:
    fn name<A:Adapton> (&mut A, A::Name, Self::Tree, Self::Tree) -> Self::Tree ;
    fn art<A:Adapton>  (&mut A, Art<Self::Tree,A::Loc>) -> Self::Tree ;

    fn fold<A:Adapton,Res,Leaf,Bin> (&mut A, Self::Tree, Res, Leaf, Bin) -> Res
        where Leaf:Fn(&mut A, Res, Self::Leaf) -> Res
        ,      Bin:Fn(&mut A, Res, Self::Bin ) -> Res ;

    fn fold2<A:Adapton,Arg,Res,Leaf,Bin> (&mut A, Self::Tree, Arg, Leaf, Bin) -> Res
        where Leaf:Fn(&mut A, Arg, Self::Leaf) -> Res
        ,      Bin:Fn(&mut A, Arg, Self::Bin, Res, Res ) -> Res ;
}
