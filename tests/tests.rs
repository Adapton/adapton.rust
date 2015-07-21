#![feature(test)]
#[macro_use]
extern crate adapton ;

mod zipper {
    extern crate test;
    use adapton::adapton_syntax::* ;
    use adapton::adapton_sigs::* ;
    //use adapton::adapton_state::* ;
    use adapton::adapton_fromscratch::* ;
    use adapton::structures::* ;

    
    pub fn zipper_get_tree<A:Adapton,L:ListT<A,u64>> (st:&mut A, nil:L) {
        let z = ListZipper::<A,u64,L>::empty(st);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  1);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 2);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  3);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 4);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  5);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 6);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  7);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 8);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  9);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 10);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  11);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 12);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  13);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 14);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  15);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 16);
        println!("z = {:?}\n", z);
        {
            let t = ListZipper::get_tree::<Tree<A,u64,()>>(st, z.clone(), ListEditDir::Left);
            println!("t = {:?}\n", t);
            let l = list_of_tree::<A,u64,L,Tree<A,u64,()>>(st, &t);
            println!("l = {:?}\n", l);
            let l_spec = list_of_vec::<A,u64,L>(st, vec![1,3,5,7,9,11,13,15, /*cursor*/ 16,14,12,10,8,6,4,2]);
            //assert_eq!(l, l_spec);
        }
        {
            let t = ListZipper::get_tree::<Tree<A,u64,()>>(st, z.clone(), ListEditDir::Right);
            let l = list_of_tree::<A,u64,L,Tree<A,u64,()>>(st, &t);
            let l_spec = list_of_vec::<A,u64,L>(st, vec![2,4,6,8,10,12,14,16,1,3,5,7,9,11,13,15]);
            //assert_eq!(l, l_spec);
        }
    }
    
    #[test]
    pub fn zipper_test () {
        let mut st = AdaptonFromScratch::new();
        let nil = List::nil(&mut st);
        zipper_get_tree (&mut st, nil);
    }
}


#[cfg(test)]
mod pure_caching {
    extern crate test;
    use self::test::Bencher;
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
    fn bench_fact_5(b: &mut Bencher) {
        b.iter(|| run_fact(5));
    }

    #[bench]
    fn bench_fact_5_repeat_100(b: &mut Bencher) {
        b.iter(|| run_fact_repeat(5, 100));
    }
}
