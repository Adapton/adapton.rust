use std::rc::Rc;

use macros::* ;
use adapton_sigs::* ;

pub fn fact<A:Adapton> (st:&mut A, x:u64 ) -> u64 {
    if x == 0 { 1 } else { x * (memo!(st, fact, x:x-1 )) }
}

pub fn run_fact<A:Adapton> (x:u64) -> u64 {
    let mut st = &mut (A::new()) ;
    memo!(st, fact, x:x )
}

pub fn run_fact_repeat<A:Adapton> (x:u64, n:u64) -> u64 {
    let mut st = &mut (A::new()) ;
    for _ in 1..(n-1) {
        memo!(st, fact, x:x );
    }
    memo!(st, fact, x:x )
}
