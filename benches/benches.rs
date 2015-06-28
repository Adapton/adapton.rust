#![feature(test)]
#[macro_use]
extern crate adapton ;

mod fact {
    #[cfg(test)]
    mod no_caching {
        extern crate test;
        use self::test::Bencher;
        
        pub fn fact (x:u64) -> u64 {
            if x == 0 { 1 } else { test::black_box(x) * fact(x-1) }
        }
        
        pub fn fact_repeat (x:u64, n:u64) -> u64 {
            for _ in 1..(n-1) {
                test::black_box(fact(x));
            }
            fact(x)
        }

        #[test]
        fn it_works() {
            assert_eq!(120 as u64, fact(5));
        }
        
        #[bench]
        fn bench_fact(b: &mut Bencher) {
            b.iter(|| test::black_box(fact(100)));
        }

        #[bench]
        fn bench_fact_repeat(b: &mut Bencher) {
            b.iter(|| test::black_box(fact_repeat(100, 100)));
        }
    }

    #[cfg(test)]
    mod pure_caching {
        extern crate test;
        use self::test::Bencher;

        use std::mem::replace;
        use std::rc::Rc;
        
        use adapton::adapton_syntax::* ;
        use adapton::adapton_sigs::* ;
        use adapton::adapton_state::* ;
        
        pub fn fact<A:Adapton> (st:&mut A, x:u64, _n:() ) -> u64 {
            if x == 0 { 1 } else { x * (memo!(st, fact, x:x-1, _n:())) }
        }
        
        pub fn run_fact (x:u64) -> u64 {
            let mut st = &mut (AdaptonState::new()) ;
            memo!(st, fact, x:x, _n:())
        }

        pub fn run_fact_repeat (x:u64, n:u64) -> u64 {
            let mut st = &mut (AdaptonState::new()) ;
            for _ in 1..(n-1) {
                memo!(st, fact, x:x, _n:());
            }
            memo!(st, fact, x:x, _n:())
        }

        #[test]
        fn it_works() {
            assert_eq!(120 as u64, run_fact(5));
        }
        
        #[bench]
        fn bench_fact(b: &mut Bencher) {
            b.iter(|| test::black_box(run_fact(100)));
        }

        #[bench]
        fn bench_fact_repeat(b: &mut Bencher) {
            b.iter(|| test::black_box(run_fact_repeat(100, 100)));
        }
    }
}
