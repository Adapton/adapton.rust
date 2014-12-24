extern crate adapton;
extern crate lazy;
extern crate memoirs;
extern crate adamantium;

pub use lazy::single::Thunk;

#[allow(dead_code)]
fn main () {
    let x = Thunk::new(move |:| { 666u });
    println!("hello world, the answer is {}.", x.unwrap())
}
