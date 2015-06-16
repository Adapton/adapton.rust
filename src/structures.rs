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
}
