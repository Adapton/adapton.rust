//#[macro_use]
//use adapton_syntax::* ;
use adapton_sigs::* ;
// use adapton_state::AdaptonState ;

trait List {
    type Hd ;
    type List ;
    fn nil <A:Adapton> (&mut A) -> Self::List ;
    fn cons<A:Adapton> (&mut A, Self::Hd) -> Self::List ;
    fn name<A:Adapton> (&mut A, A::Name) -> Self::List ;
    fn art<A:Adapton>  (&mut A, Art<Self::List,A::Loc>) -> Self::List ;
}
