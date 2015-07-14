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
    //const INPUT_ARRAY:Vec<u64> = vec![2; 64]; 
    const REPEAT_COUNT:u64 = 100;
    
    #[cfg(test)]
    mod eager_ids {
        //use super::INPUT_ARRAY;
        use super::REPEAT_COUNT;

        extern crate test;
        use self::test::Bencher;

        use std::rc::Rc;
        
        use adapton::adapton_syntax::* ;
        use adapton::adapton_sigs::* ;
        use adapton::adapton_fromscratch::* ;

        fn ident<A:Adapton,T>(st:&mut A, x:T) -> T {x}

        fn depend_mult<A:Adapton>(st:&mut A, x: &Art<u64,A::Loc>,y: &Art<u64,A::Loc>) -> u64 { 
            st.force(x) * st.force(y)
        }

        fn depend_add<A:Adapton>(st:&mut A, x: &Art<u64,A::Loc>,y: &Art<u64,A::Loc>) -> u64 {
            st.force(x) + st.force(y)
        }

/*
        pub fn hourglass_old<A:Adapton> (st:&mut A) -> u64 {
            let cells = vec![2;64];
            let nodes64 = cells.iter().map(|x| thunk!(st, ident, x:*x));
            let nodes32 = nodes64.take(32).zip(nodes64.skip(32)).map(
                |(x,y)| thunk!(st, depend_mult, x:&x, y:&y)
            );
            let nodes16 = nodes32.take(16).zip(nodes32.skip(16)).map(
                |(x,y)| thunk!(st, depend_add, x:&x, y:&y)
            );
            let nodes8 = nodes16.take(8).zip(nodes16.skip(8)).map(
                |(x,y)| thunk!(st, depend_mult, x:&x, y:&y)
            );
            let nodes4 = nodes8.take(4).zip(nodes8.skip(4)).map(
                |(x,y)| thunk!(st, depend_add, x:&x, y:&y)
            );
            let nodes2 = nodes4.take(2).zip(nodes4.skip(2)).map(
                |(x,y)| thunk!(st, depend_mult, x:&x, y:&y)
            );
            let pivot = nodes2.take(1).zip(nodes2.skip(1)).map(
                |(x,y)| thunk!(st, depend_add, x:&x, y:&y)
            );
            let fake_result = vec!(thunk!(st, ident, x:2));
            let mut pivot = nodes2;
            let default = thunk!(st, ident, x:2);
            let chokepoint = match pivot.next() {
                None => default,
                Some(a) => a
            };
            st.force(&chokepoint)
            //now do second half, deriving multiple results from this one
        }
*/

        pub fn hourglass<A:Adapton> (st:&mut A) -> u64 {
            let cells = vec![2;64];
            let mut nodes = vec![];
            for i in 0..64 {
                nodes.push(thunk!(st, ident, x:cells[i]));
            }
            let mut item_num = 0;
            loop {
                if nodes.len() - item_num < 2 { break; }
                let first = nodes[item_num].clone();
                let second = nodes[item_num+1].clone();
                //why don't we have lifetime issues here?
                nodes.push(thunk!(st, depend_add, x:&first, y:&second));
                item_num += 2;
                //println!("At: {}", item_num);
            }
            //this line gives an index out of bounds error!!
            //let thk = &nodes[nodes.len()-1];
            let thk = &nodes[&nodes.len()-2];
            println!("past checkpoint");
            st.force(thk)
        }

        pub fn run_hourglass () -> u64 {
            let mut st = &mut (AdaptonFromScratch::new());
            hourglass(st)
        }

        pub fn run_hourglass_repeat (n:u64) -> u64 {
            let mut st = &mut (AdaptonFromScratch::new());
            for _ in 1..(n-1) {
                test::black_box(hourglass(st));
            }
            hourglass(st)
        }

        #[test]
        fn it_works() {
            assert_eq!(2 as u64, run_hourglass());
        }
        
        #[bench]
        fn bench_hourglass(b: &mut Bencher) {
            b.iter(|| test::black_box(run_hourglass()));
        }

        #[bench]
        fn bench_hourglass_repeat(b: &mut Bencher) {
            b.iter(|| test::black_box(run_hourglass_repeat(REPEAT_COUNT)));
        }
        
    }

}
