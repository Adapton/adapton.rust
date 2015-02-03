
struct AdaptonState {
    foo : bool,
}

trait Computer<Res> {
    type Arg;
    fn compute(self:&Self, st:&mut AdaptonState, arg:Self::Arg) -> Res;
    //fn size_hint(&self) -> (usize, Option<usize>) { ... }
}

struct ComputeNode<Res,Comp:Computer<Res>> {
    arg : Comp::Arg,
    res : Option<Res>,
    comp : Box<Comp>,
}

trait Nd where Self::Comp : Computer<Self::Res> {
    type Res;
    type Comp;
    fn get_node<'x>(self:&'x mut Self) -> &'x mut ComputeNode<Self::Res,Self::Comp>;
}


fn main () {

}
