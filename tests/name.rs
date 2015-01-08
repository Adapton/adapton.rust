extern crate adapton;
extern crate test;

use adapton::name::*;
use test::Bencher;

#[test]
pub fn printstuff () {
    let n = symbol (format!("one")) ;
    let m = symbol (format!("two")) ;
    println!("Adapton: {}, {}", n, m);
    println!("Adapton: {}, {}", fork(n), m);
}

#[bench]
pub fn printstuff_bench (b:&mut Bencher) {
    b.iter(|| {
        let n = symbol (format!("one")) ;
        let m = symbol (format!("two")) ;
        println!("Adapton: {}, {}", n, m);
        println!("Adapton: {}, {}", fork(n), m);
    })
}
