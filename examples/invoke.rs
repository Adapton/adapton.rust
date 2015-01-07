use std::thunk::Invoke;

fn foo (f:Box<Invoke<(), int>>) {
    print!("{}", f.invoke(()) );
}

pub fn main () {    
    let f : Box<Invoke<(),int>> = box move |:()| 1 ;
    foo (f);
}
