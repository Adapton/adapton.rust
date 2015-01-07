use std::thunk::Invoke;

// Consume a closure
// Notice: Use Box<...>.
fn elim_invoke_object (f:Box<Invoke<(), int>>) {
    print!("{}", f.invoke(()) );
}

// Produce a closure
// Notice: Use Box<...> and explicit lifetime of 'x.
pub fn intro_invoke_object <'x> () -> Box<Invoke<(), int> + 'x> {
    let f : Box<Invoke<(),int>> = box move |:()| 1 ;
    f
}

#[test]
pub fn my_test () {
    let o = intro_invoke_object () ;
    elim_invoke_object( o )
}

pub fn main () {
    let o = intro_invoke_object () ;
    elim_invoke_object( o )
}
