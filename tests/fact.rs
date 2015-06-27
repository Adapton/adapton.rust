use std::rc::Rc;
use std::mem::replace;

extern crate adapton;

use adapton::adapton_syntax::* ;
use adapton::adapton_sigs::* ;
use adapton::adapton_state::* ;

pub fn fact<'r> (st:&'r mut AdaptonState, x:Rc<u64>) -> Rc<u64> {
    if *x == 0 { Rc::new(1) } else {
        let res = fact(st, Rc::new(*x-1));
        Rc::new(*x * *res)
    }
}

pub fn run_fact (x:u64) -> u64 {
    let mut st = AdaptonState::new() ;
    let t = st.thunk(ArtIdChoice::Eager,
                     panic!("prog_pt!(fact)"),
                     Rc::new(Box::new(fact)),
                     Rc::new(x)) ;
    *(st.force(&t))
}

#[cfg(test)]
mod tests {

    extern crate test;
    use super::*;
    use self::test::Bencher;

    #[test]
    fn it_works() {
        assert_eq!(125 as u64, run_fact(5));
    }

    #[bench]
    fn bench_fact_5(b: &mut Bencher) {
        b.iter(|| run_fact(5));
    }
}
