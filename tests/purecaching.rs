#![feature(test)]

#[cfg(test)]
mod pure_caching {
    extern crate test;
    use self::test::Bencher;
    use std::rc::Rc;

    extern crate adapton;
    use self::adapton::simple::{run_fact,run_fact_repeat};
    use self::adapton::adapton_state::AdaptonState;
    
    #[test]
    fn it_works() {
        assert_eq!(120 as u64, run_fact::<AdaptonState>(5));
    }
    
    #[bench]
    fn bench_fact_5(b: &mut Bencher) {
        b.iter(|| run_fact::<AdaptonState>(5));
    }

    #[bench]
    fn bench_fact_5_repeat_100(b: &mut Bencher) {
        b.iter(|| run_fact_repeat::<AdaptonState>(5, 100));
    }
}
