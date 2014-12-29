extern crate adapton;
extern crate lazy;
extern crate memoirs;
extern crate adamantium;

pub use lazy::single::Thunk;
pub use adapton::list::List;
pub use adapton::art::cell;
pub use adapton::name::symbol;
pub use adapton::name::fork;

#[allow(dead_code)]
fn main () {
    let x = Thunk::new(move |:| { 666u });
    println!("hello world, the answer is {}.", x.unwrap());

    let z : List<int> = List::Nil;
    let y : List<int> = List::Cons(1, box z);
    let x : List<int> = List::Art(cell(symbol(format!("two")), box y));
    let l : List<int> = List::Name(symbol(format!("one")), box x);
    println!("constructed list: {}", l);
}
