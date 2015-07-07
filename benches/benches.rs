#![feature(test)]
#![feature(alloc)]
#![feature(heap_api)]
#[macro_use]
extern crate adapton ;

mod fib {
    const INPUT_SIZE:u64 = 20;
    const REPEAT_COUNT:u64 = 100;
    
    #[cfg(test)]
    mod no_caching {
        use super::INPUT_SIZE;
        use super::REPEAT_COUNT;
        
        extern crate test;
        use self::test::Bencher;
        
        pub fn fib (x:u64) -> u64 { test::black_box (
            match x {
                0 => 0,
                1 => 1,
                x => fib(x-1) + fib(x-2)
            })
        }
        
        pub fn fib_repeat (x:u64, n:u64) -> u64 {
            for _ in 1..(n-1) {
                test::black_box(fib(x));
            }
            test::black_box(fib(x))
        }

        #[test]
        fn it_works() {
            assert_eq!(5 as u64, fib(5));
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

        extern crate alloc;
        use std::rc::Rc;
        use adapton::adapton_syntax::* ;
        use adapton::adapton_sigs::* ;
        use adapton::adapton_state::* ;
        
        pub fn fib<A:Adapton> (st:&mut A, x:u64 ) -> u64 {
            match x {
                0 => 0,
                1 => 1,
                x => { memo!(st, fib, x:x-1)
                       +
                       memo!(st, fib, x:x-2) }}
        }
        
        pub fn run_fib (x:u64) -> u64 {
            let mut st = &mut (AdaptonState::new()) ;
            memo!(st, fib, x:x )                
        }

        pub fn run_fib_repeat (x:u64, n:u64) -> u64 {
            let mut st = &mut (AdaptonState::new()) ;
            for _ in 1..(n-1) {
                memo!(st, fib, x:x );
            }
            memo!(st, fib, x:x )
        }

        #[test]
        fn it_works() {
            assert_eq!(5 as u64, run_fib(5));
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

        use std::rc::Rc;
        
        use adapton::adapton_syntax::* ;
        use adapton::adapton_sigs::* ;
        use adapton::adapton_state::* ;
        
        pub fn fact<A:Adapton> (st:&mut A, x:u64 ) -> u64 {
            if x == 0 { 1 } else { x * (memo!(st, fact, x:x-1 )) }
        }
        
        pub fn run_fact (x:u64) -> u64 {
            let mut st = &mut (AdaptonState::new()) ;
            memo!(st, fact, x:x )
        }

        pub fn run_fact_repeat (x:u64, n:u64) -> u64 {
            let mut st = &mut (AdaptonState::new()) ;
            for _ in 1..(n-1) {
                memo!(st, fact, x:x );
            }
            memo!(st, fact, x:x )
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

mod hourglass {
    const INPUT_ARRAY:Vec<u64> = vec![  4, 7, 3, 0,   2, 7, 2, 5, // mam: 28*24= 672
                                        1, 5, 7, 8,   3, 2, 9, 5, // mam: 61*51=3111
                                        8, 7, 1, 4,   8, 7, 4, 7, // mam: 60*84=5040
                                        2, 5, 7, 2,   9, 3, 0, 3, // mam: 24*27= 648
                                                                  // am: 21517704
                                        2, 7, 8, 3,   3, 8, 4, 2, // mam: 38*32=1216
                                        9, 3, 7, 3,   2, 7, 2, 8, // mam: 48*30=1440
                                        2, 5, 7, 3,   2, 9, 8, 6, // mam: 31*66=2046
                                        3, 3, 3, 0,   3, 7, 5, 9  // mam:  9*66= 594
                                                                  // am: 7144640
                                    ];                            // a: 28662344
    const REPEAT_COUNT:u64 = 100;
    
    #[cfg(test)]
    mod eager_ids {
        use super::INPUT_ARRAY;
        use super::REPEAT_COUNT;

        extern crate test;
        use self::test::Bencher;

        use std::rc::Rc;
        use std::slice::Iter;
        
        use adapton::adapton_syntax::* ;
        use adapton::adapton_sigs::* ;
        use adapton::adapton_state::* ;

        fn ident<A:Adapton,T>(st:A,x:T) -> T {x}
        fn depend_mult<A:Adapton>(st:&mut A, x: &Art<u64,A::Loc>,y: &Art<u64,A::Loc>) -> u64 { 
            st.force(x) * st.force(y)
        }
        fn depend_add<A:Adapton>(st:&mut A, x: &Art<u64,A::Loc>,y: &Art<u64,A::Loc>) -> u64 {
            st.force(x) + st.force(y)
        }

        pub fn hourglass<A:Adapton> (st:&mut A, x:[u64;64]) -> u64 {
            let cells = INPUT_ARRAY.iter().map(|x| thunk!(st, ident, x:x));
            let nodes32 = cells.take(32).zip(cells.skip(32)).map(
                |(x,y)| thunk!(st, depend_mult, x:x, y:y)
            );
            let nodes16 = nodes32.take(16).zip(nodes32.skip(16)).map(
                |(x,y)| thunk!(st, depend_add, x:x, y:y)
            );
            let nodes8 = nodes16.take(8).zip(nodes16.skip(8)).map(
                |(x,y)| thunk!(st, depend_mult, x:x, y:y)
            );
            let nodes4 = nodes8.take(4).zip(nodes8.skip(4)).map(
                |(x,y)| thunk!(st, depend_add, x:x, y:y)
            );
            let nodes2 = nodes4.take(2).zip(nodes4.skip(2)).map(
                |(x,y)| thunk!(st, depend_mult, x:x, y:y)
            );
            let pivot = nodes2.take(1).zip(nodes2.skip(1)).map(
                |(x,y)| thunk!(st, depend_add, x:x, y:y)
            );
            st.force(pivot[1])
            //now do second half, deriving multiple results from this one
        }

        pub fn run_hourglass (x:[u64;64]) -> u64 {
            let mut st = &mut (AdaptonState::new());
            hourglass(st,x)
        }
        
        pub fn hourglass_repeat (x:[u64;64], n:u64) -> u64 {
            let mut st = &mut (AdaptonState::new());
            for _ in 1..(n-1) {
                test::black_box(hourglass(st,x));
            }
            hourglass(st,x)
        }

    }

}
