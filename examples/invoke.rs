use std::thunk::Invoke;

// Consume a closure
fn foo (f:Box<Invoke<(), int>>) {
    print!("{}", f.invoke(()) );
}

// Produce a closure
pub fn main () {    
    let f : Box<Invoke<(),int>> = box move |:()| 1 ;
    foo (f);
}
