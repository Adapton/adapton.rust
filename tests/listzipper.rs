// #![feature(test)]

// extern crate adapton ;
// extern crate test;

// //use adapton::adapton_syntax::* ;
// use adapton::adapton_sigs::* ;
// use adapton::naive::* ;
// use adapton::collection_traits::* ;
// use adapton::collection_edit::* ;
// use adapton::collection::{List,Tree} ;
// use adapton::collection_algo::* ;
    
// pub fn zipper_get_tree<A:Adapton,T:TreeT<A,u64>,L:TreeListT<A,u64,T>> (st:&mut A, _nil1:L) {
//     let z = ListZipper::<A,u64,T,L>::empty(st);
//     let z = ListZipper::insert(st, z, Dir2::Left,  1);
//     let z = ListZipper::insert(st, z, Dir2::Right, 2);
//     let z = ListZipper::insert(st, z, Dir2::Left,  3);
//     let z = ListZipper::insert(st, z, Dir2::Right, 4);
//     let z = ListZipper::insert(st, z, Dir2::Left,  5);
//     let z = ListZipper::insert(st, z, Dir2::Right, 6);
//     let z = ListZipper::insert(st, z, Dir2::Left,  7);
//     let z = ListZipper::insert(st, z, Dir2::Right, 8);
//     println!("z = {:?}\n", z);
    
//     for get_tree_dir in vec![Dir2::Left,Dir2::Right].iter()
//     {
//         let t = ListZipper::get_tree(st, z.clone(), get_tree_dir.clone());
//         println!("t = get_tree z = {:?}\n", t);
        
//         let l_spec = match *get_tree_dir {
//             Dir2::Left  =>     list_of_vec::<A,u64,L>(st, vec![1,3,5,7, /*cursor*/ 8,6,4,2]),
//             Dir2::Right => rev_list_of_vec::<A,u64,L>(st, vec![1,3,5,7, /*cursor*/ 8,6,4,2]),
//         };
        
//         println!("l_spec  = {:?}", l_spec);
//         let l_spec_sorted = list_merge_sort::<A,u64,L,T>(st, l_spec.clone());
//         println!("l_spec_sorted  = list_merge_sort l_spec = {:?}\n", l_spec_sorted);
        
//         let l = list_of_tree::<A,u64,L,T>(st, t.clone());
//         println!("l  = list_of_tree t = {:?}\n", l);
        
//         println!("l == l_spec = {}\n", l == l_spec.clone());
//         assert_eq!(l, l_spec);
        
//         let t2 = tree_of_list::<A,u64,T,L>(st, Dir2::Left, l.clone());
//         println!("t2 = tree_of_list l = {:?}", t2);
//         println!("t2 == t = {}\n", t2 == t);
//         // assert_eq!(t2, t); // FIXME: tree_append needs to follow Pugh's algorithm (POPL 1989).
        
//         let t_spec = tree_of_list::<A,u64,T,L>(st, Dir2::Left, l_spec.clone());
//         println!("t_spec = tree_of_list l_spec = {:?}\n", t_spec);
        
//         let l2 = list_of_tree::<A,u64,L,T>(st, t2.clone());
//         println!("l2 = list_of_tree t2 = {:?}", l2);
//         println!("l2 == l = {}\n", l2 == l); // Tests `list_of_tree o tree_of_list = id`.
//         assert_eq!(l2, l);
        
//         let t3 = tree_of_list::<A,u64,T,L>(st, Dir2::Left, l2);
//         println!("t3 = tree_of_list l2 = {:?}", t3);
//         println!("t3 == l = {}\n", t3 == t2); // Tests `tree_of_list o list_of_tree = id`.
//         assert_eq!(t3, t2);            
//     }
//     //assert!(false)
// }

// #[test]
// pub fn zipper_test () {
//     let mut st = AdaptonFromScratch::new();
//     let nil1 = List::nil(&mut st);
//     zipper_get_tree::< AdaptonFromScratch,
//     Tree<AdaptonFromScratch,u64,u32>,
//     List<AdaptonFromScratch,u64>,
//     > (&mut st, nil1);
// }

