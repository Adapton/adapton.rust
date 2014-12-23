extern crate adapton;
extern crate lazy;
extern crate memoirs;

pub use lazy::single::Thunk;
pub use adapton::name::printstuff;

fn main () {
    printstuff();
    let x = Thunk::new(move |:| { 666u });
    println!("hello world, the answer is {}.", x.unwrap())
}
