#![feature(test)]
#[macro_use]
extern crate adapton ;

mod fib {
    const INPUT_SIZE:u64 = 21;
    const REPEAT_COUNT:u64 = 100;
    
    #[cfg(test)]
    mod no_caching {
        use super::INPUT_SIZE;
        use super::REPEAT_COUNT;
        
        extern crate test;
        use self::test::Bencher;
        
        pub fn fib (x:u64) -> u64 {
            test::black_box(
                match x {
                    0 => 1,
                    1 => 1,
                    x => fib(x-1) + fib(x-2)
                })
        }
        
        pub fn fib_repeat (x:u64, n:u64) -> u64 {
            for _ in 1..(n-1) {
                test::black_box(fib(x));
            }
            fib(x)
        }

        #[test]
        fn it_works() {
            assert_eq!(120 as u64, fib(5));
        }
        
        #[bench]
        fn bench_fib(b: &mut Bencher) {
            b.iter(|| test::black_box(fib(INPUT_SIZE)));
        }

        #[bench]
        fn bench_fib_repeat(b: &mut Bencher) {
            b.iter(|| test::black_box(fib_repeat(INPUT_SIZE, REPEAT_COUNT)));
        }
    }

    #[cfg(test)]
    mod pure_caching {
        use super::INPUT_SIZE;
        use super::REPEAT_COUNT;

        extern crate test;
        use self::test::Bencher;

        use std::mem::replace;
        use std::rc::Rc;
        
        use adapton::adapton_syntax::* ;
        use adapton::adapton_sigs::* ;
        use adapton::adapton_state::* ;
        
        pub fn fib<A:Adapton> (st:&mut A, x:u64, _n:() ) -> u64 {
            match x {
                0 => 1,
                1 => 1,
                x => { memo!(st, fib, x:x-1, _n:())
                       +
                       memo!(st, fib, x:x-2, _n:()) }}
        }
        
        pub fn run_fib (x:u64) -> u64 {
            let mut st = &mut (AdaptonState::new()) ;
            memo!(st, fib, x:x, _n:())
        }

        pub fn run_fib_repeat (x:u64, n:u64) -> u64 {
            let mut st = &mut (AdaptonState::new()) ;
            for _ in 1..(n-1) {
                memo!(st, fib, x:x, _n:());
            }
            memo!(st, fib, x:x, _n:())
        }

        #[test]
        fn it_works() {
            assert_eq!(120 as u64, run_fib(5));
        }
        
        #[bench]
        fn bench_fib(b: &mut Bencher) {
            b.iter(|| test::black_box(run_fib(INPUT_SIZE)));
        }

        #[bench]
        fn bench_fib_repeat(b: &mut Bencher) {
            b.iter(|| test::black_box(run_fib_repeat(INPUT_SIZE, REPEAT_COUNT)));
        }
    }
}


mod fact {
    const INPUT_SIZE:u64 = 100;
    const REPEAT_COUNT:u64 = 100;
    
    #[cfg(test)]
    mod no_caching {
        use super::INPUT_SIZE;
        use super::REPEAT_COUNT;

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
            b.iter(|| test::black_box(fact(INPUT_SIZE)));
        }

        #[bench]
        fn bench_fact_repeat(b: &mut Bencher) {
            b.iter(|| test::black_box(fact_repeat(INPUT_SIZE, REPEAT_COUNT)));
        }
    }

    #[cfg(test)]
    mod pure_caching {
        use super::INPUT_SIZE;
        use super::REPEAT_COUNT;

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
            b.iter(|| test::black_box(run_fact(INPUT_SIZE)));
        }

        #[bench]
        fn bench_fact_repeat(b: &mut Bencher) {
            b.iter(|| test::black_box(run_fact_repeat(INPUT_SIZE, REPEAT_COUNT)));
        }
    }
}
