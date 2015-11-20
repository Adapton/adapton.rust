#![feature(test)]

extern crate adapton ;
extern crate test;

//use adapton::adapton_syntax::* ;
use adapton::adapton_sigs::* ;
//use adapton::adapton_state::* ;
use adapton::naive::* ;
use adapton::collection::* ;
    
pub fn zipper_get_tree<A:Adapton,L:ListT<A,u64>,T:TreeT<A,u64>> (st:&mut A, _nil1:L) {
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
        let t = ListZipper::get_tree::<T>(st, z.clone(), get_tree_dir.clone());
        println!("t = get_tree z = {:?}\n", t);
        
        let l_spec = match *get_tree_dir {
            ListEditDir::Left  =>     list_of_vec::<A,u64,L>(st, vec![1,3,5,7, /*cursor*/ 8,6,4,2]),
            ListEditDir::Right => rev_list_of_vec::<A,u64,L>(st, vec![1,3,5,7, /*cursor*/ 8,6,4,2]),
        };
        
        println!("l_spec  = {:?}", l_spec);
        let l_spec_sorted = list_merge_sort::<A,u64,L,T>(st, l_spec.clone());
        println!("l_spec_sorted  = list_merge_sort l_spec = {:?}\n", l_spec_sorted);
        
        let l = list_of_tree::<A,u64,L,T>(st, t.clone());
        println!("l  = list_of_tree t = {:?}\n", l);
        
        println!("l == l_spec = {}\n", l == l_spec.clone());
        assert_eq!(l, l_spec);
        
        let t2 = tree_of_list::<A,u64,T,L>(st, ListEditDir::Left, l.clone());
        println!("t2 = tree_of_list l = {:?}", t2);
        println!("t2 == t = {}\n", t2 == t);
        // assert_eq!(t2, t); // FIXME: tree_append needs to follow Pugh's algorithm (POPL 1989).
        
        let t_spec = tree_of_list::<A,u64,T,L>(st, ListEditDir::Left, l_spec.clone());
        println!("t_spec = tree_of_list l_spec = {:?}\n", t_spec);
        
        let l2 = list_of_tree::<A,u64,L,T>(st, t2.clone());
        println!("l2 = list_of_tree t2 = {:?}", l2);
        println!("l2 == l = {}\n", l2 == l); // Tests `list_of_tree o tree_of_list = id`.
        assert_eq!(l2, l);
        
        let t3 = tree_of_list::<A,u64,T,L>(st, ListEditDir::Left, l2);
        println!("t3 = tree_of_list l2 = {:?}", t3);
        println!("t3 == l = {}\n", t3 == t2); // Tests `tree_of_list o list_of_tree = id`.
        assert_eq!(t3, t2);            
    }
    //assert!(false)
}

#[test]
pub fn zipper_test () {
    let mut st = AdaptonFromScratch::new();
    let nil1 = List::nil(&mut st);
    zipper_get_tree::< AdaptonFromScratch,
    List<AdaptonFromScratch,u64>,
    Tree<AdaptonFromScratch,u64,u32> > (&mut st, nil1);
}
