#![feature(test)]
#[macro_use]
extern crate adapton ;

mod zipper {
    extern crate test;
    //use adapton::adapton_syntax::* ;
    use adapton::adapton_sigs::* ;
    //use adapton::adapton_state::* ;
    use adapton::adapton_fromscratch::* ;
    use adapton::structures::* ;

    
    pub fn zipper_get_tree<A:Adapton,L:ListT<A,u64>> (st:&mut A, _nil1:L) {
        let z = ListZipper::<A,u64,L>::empty(st);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  1);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 2);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  3);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 4);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  5);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 6);
        let z = ListZipper::insert(st, z, ListEditDir::Left,  7);
        let z = ListZipper::insert(st, z, ListEditDir::Right, 8);
        println!("z = {:?}\n", z);

        for get_tree_dir in vec![ListEditDir::Left,ListEditDir::Right].iter()
        {
            let t = ListZipper::get_tree::<Tree<A,u64,()>>(st, z.clone(), get_tree_dir.clone());
            println!("t = get_tree z = {:?}\n", t);

            let l_spec = match *get_tree_dir {
                ListEditDir::Left  =>     list_of_vec::<A,u64,L>(st, vec![1,3,5,7, /*cursor*/ 8,6,4,2]),
                ListEditDir::Right => rev_list_of_vec::<A,u64,L>(st, vec![1,3,5,7, /*cursor*/ 8,6,4,2]),
            };

            println!("l_spec  = {:?}", l_spec);
            // let l_spec_sorted = list_merge_sort::<A,u64,L,Tree<A,u64,()>>(st, l_spec.clone());
            // println!("l_spec_sorted  = list_merge_sort l_spec = {:?}\n", l_spec_sorted);
            
            let l = list_of_tree::<A,u64,L,Tree<A,u64,()>>(st, &t);
            println!("l  = list_of_tree t = {:?}\n", l);

            println!("l == l_spec = {}\n", l == l_spec.clone());
            assert_eq!(l, l_spec);
            
            let t2 = tree_of_list::<A,u64,Tree<A,u64,()>,L>(st, ListEditDir::Left, l.clone());
            println!("t2 = tree_of_list l = {:?}", t2);
            println!("t2 == t = {}\n", t2 == t);
            //assert_eq!(t2, t); // FIXME: tree_append needs to follow Pugh's algorithm (POPL 1989).

            let t_spec = tree_of_list::<A,u64,Tree<A,u64,()>,L>(st, ListEditDir::Left, l_spec.clone());
            println!("t_spec = tree_of_list l_spec = {:?}\n", t_spec);
            
            let l2 = list_of_tree::<A,u64,L,Tree<A,u64,()>>(st, &t2);
            println!("l2 = list_of_tree t2 = {:?}", l2);
            println!("l2 == l = {}\n", l2 == l); // Tests `list_of_tree o tree_of_list = id`.
            assert_eq!(l2, l);

            let t3 = tree_of_list::<A,u64,Tree<A,u64,()>,L>(st, ListEditDir::Left, l2);
            println!("t3 = tree_of_list l2 = {:?}", t3);
            println!("t3 == l = {}\n", t3 == t2); // Tests `tree_of_list o list_of_tree = id`.
            assert_eq!(t3, t2);            

            assert_eq!(l, l_spec);
        }
    }
    
    #[test]
    pub fn zipper_test () {
        let mut st = AdaptonFromScratch::new();
        let nil1 = List::nil(&mut st);
        zipper_get_tree (&mut st, nil1);
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
