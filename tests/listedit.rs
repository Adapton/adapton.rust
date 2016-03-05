// #![feature(test)]
// #![feature(plugin)]
// #![feature(zero_one)]
// // #![plugin(quickcheck_macros)]

// #[macro_use] extern crate log;


// //
// // cargo test listedit::experiments -- --nocapture
// //

// extern crate adapton ;
// extern crate test;
// // extern crate quickcheck;
// extern crate rand;

// use std::num::Zero;
// use std::ops::Add;    
// use rand::Rand;

// use adapton::adapton_sigs::* ;
// use adapton::collection_traits::*;
// use adapton::collection_edit::*;
// use adapton::collection::*;
// use adapton::engine;
// use adapton::naive;
// use std::fmt::Debug;
// use std::hash::Hash;

// type Edits = Vec<CursorEdit<u32, Dir2>>;

// fn has_consecutive_names<A:Adapton,X,L:ListT<A,X>> (st:&mut A, list:L::List) -> bool {
//     L::elim(st, list,
//             |st| false,
//             |st,x,xs| has_consecutive_names::<A,X,L> (st, xs),
//             |st,n,xs|
//             L::elim(st, xs,
//                     |st| false,
//                     |st,y,ys| has_consecutive_names::<A,X,L> (st, ys),
//                     |st,m,ys| true))
// }

// pub struct Experiment ;
// impl<A:Adapton,X:Ord+Add<Output=X>+Zero+Hash+Debug+PartialEq+Eq+Clone+PartialOrd> ExperimentT<A,X,Tree<A,X,u32>,Vec<X>>
//     for Experiment
// {
//     type ListEdit = ListZipper<A,X,Tree<A,X,u32>,List<A,X>> ;
//     fn run (st:&mut A, edits:Vec<CursorEdit<X,Dir2>>, view:ListReduce) -> Vec<(Vec<X>,Cnt)> {
//         debug!("run");
//         let mut outs : Vec<(Vec<X>,Cnt)> = Vec::new();
//         let mut z : ListZipper<A,X,Tree<A,X,u32>,List<A,X>> = Self::ListEdit::empty(st) ;
//         let mut loop_cnt = 0 as usize;
//         for edit in edits.into_iter() {
//             debug!("\n----------------------- Loop head; count={}", loop_cnt);
//             debug!("zipper: {:?}", z);
//             if false {
//                 let consecutive_left  = has_consecutive_names::<A,X,List<A,X>>(st, z.left.clone());
//                 let consecutive_right = has_consecutive_names::<A,X,List<A,X>>(st, z.right.clone());
//                 debug!("zipper names: consecutive left: {}, consecutive right: {}",
//                        consecutive_left, consecutive_right);
//                 assert!(!consecutive_left);  // Todo-Later: This assertion generally fails for random interactions
//                 assert!(!consecutive_right); // Todo-Later: This assertion generally fails for random interactions
//             }
//             debug!("edit:   {:?}", edit);
//             let (out, cnt) = st.cnt(|st|{
//                 let z_next = eval_edit::<A,X,Tree<A,X,u32>,Self::ListEdit>(st, edit, z.clone(), loop_cnt);
//                 let tree = Self::ListEdit::get_tree(st, z_next.clone(), Dir2::Left);
//                 debug!("tree:   {:?}", tree);
//                 let nm = st.name_of_string("eval_reduce".to_string());
//                 let out = st.ns(nm, |st|eval_reduce::<A,X,List<A,X>,Tree<A,X,u32>>(st, tree, &view) );
//                 z = z_next;
//                 loop_cnt = loop_cnt + 1;
//                 out
//             }) ;
//             debug!("out:    {:?}", out);
//             debug!("cnt:    {:?}", cnt);
//             outs.push((out,cnt));
//         } outs
//     }
// }

// fn compare_naive_and_cached(edits: &Edits, view:&ListReduce) -> bool {
//     let mut n_st = naive::AdaptonFromScratch::new();
//     let mut e_st = engine::Engine::new();
//     // e_st.flags.ignore_nominal_use_structural = true;
    
//     let results_1 = Experiment::run(&mut n_st, edits.clone(), view.clone());
//     let results_2 = Experiment::run(&mut e_st, edits.clone(), view.clone());
    
//     let mut idx = 0;
//     let mut a_cost : Cnt = Cnt::zero();
//     let mut b_cost : Cnt = Cnt::zero();
//     for (a, b) in results_1.iter().zip(results_2.iter()) {
//         a_cost = &a_cost + &a.1 ;
//         b_cost = &b_cost + &b.1 ;
//         if a.0 != b.0 {
//             debug!("After edit {}, {:?}, expected {:?} to be {:?}, but found {:?}.\nEdits:\n{:?}",
//                      idx, edits[idx], &view, a.0, b.0, edits);
//             return false;
//         }
//         idx += 1;
//     }
//     {
//         let naive_total = a_cost.eval ;
//         let engine_total = b_cost.dirty + b_cost.eval + b_cost.change_prop ;
//         if false {
//         debug!("{:16} for {:5} edits, Naive/Engine:{:5} = {:8} / {:8}. Naive/EngineEval:{:5}. In Engine, eval is {:.2} of {:?}",
//                  format!("{:?}", view),
//                  edits.len(),
//                  (naive_total as f32) / (engine_total as f32),
//                  naive_total, engine_total,
//                  (naive_total as f32) / (b_cost.eval as f32),
//                  (b_cost.eval as f32) / (engine_total as f32),
//                  b_cost);
//         } ;
//         println!("{:24} For {:5} edits, Naive/Engine:{:5}, Naive/EngineEval:{:5} \t==> Per-edit ==> Naive:{:8}, Engine:{:6}, EngineEval:{:5},   Naive/Engine:{:5}, Naive/EngineEval:{:5}",
//                  format!("{:?}", view),
//                  edits.len(),
//                  format!("{:.2}", (naive_total as f32) / (engine_total as f32)),
//                  format!("{:.2}", (naive_total as f32) / (b_cost.eval as f32)),
//                  // Per-edit metrics:
//                  format!("{:.2}", (naive_total as f32) / (edits.len() as f32)),
//                  format!("{:.2}", (engine_total as f32) / (edits.len() as f32)),
//                  format!("{:.2}", (b_cost.eval as f32) / (edits.len() as f32)),
//                  format!("{:.2}", ((naive_total as f32) / (engine_total as f32)) / (edits.len() as f32)),
//                  format!("{:.2}", ((naive_total as f32) / (b_cost.eval as f32)) / (edits.len() as f32)),
//                  );
//     }
//     true
// }

// fn ensure_consistency_randomly(size:usize, iterations:usize, view:&ListReduce) {
//   let mut rng = rand::thread_rng();
//   let mut testv = vec![ ];
//   for i in 0..size {
//     testv.push(<CursorEdit<u32,Dir2> as Rand>::rand(&mut rng))
//   };  
//   for _ in 0..iterations {
//     assert!( compare_naive_and_cached(&testv, view) )
//   }
// }

// //#[ignore]
// #[test]
// fn ensure_consistency_randomly_100_x_100() {
//     ensure_consistency_randomly(100, 100, &ListReduce::Sum) ;
//     ensure_consistency_randomly(100, 100, &ListReduce::Max) ;
//     ensure_consistency_randomly(100, 100, &ListReduce::Vec(ListTransf::Reverse, None)) ;
//     ensure_consistency_randomly(100, 100, &ListReduce::Vec(ListTransf::Sort, None)) ;
//     ensure_consistency_randomly(100, 100, &ListReduce::Tree(ListTransf::Sort, None)) ;
//     ensure_consistency_randomly(100, 100, &ListReduce::Tree(ListTransf::Reverse, None)) ;
// }

// //#[ignore]
// #[test]
// fn ensure_consistency_randomly_300_x_100() {
//     ensure_consistency_randomly(300, 100, &ListReduce::Sum) ;
//     ensure_consistency_randomly(300, 100, &ListReduce::Max) ;
//     ensure_consistency_randomly(300, 100, &ListReduce::Vec(ListTransf::Reverse, None)) ;
//     ensure_consistency_randomly(300, 100, &ListReduce::Vec(ListTransf::Sort, None)) ;
//     ensure_consistency_randomly(300, 100, &ListReduce::Tree(ListTransf::Sort, None)) ;
//     ensure_consistency_randomly(300, 100, &ListReduce::Tree(ListTransf::Reverse, None)) ;
// }

// #[ignore]
// #[test]
// fn ensure_consistency_randomly_1k_x_20() {
//     ensure_consistency_randomly(1000, 20, &ListReduce::Sum) ;
//     ensure_consistency_randomly(1000, 20, &ListReduce::Max) ;
//     ensure_consistency_randomly(1000, 20, &ListReduce::Vec(ListTransf::Reverse, None)) ;
//     ensure_consistency_randomly(1000, 20, &ListReduce::Vec(ListTransf::Sort, None)) ;
// }

// #[ignore]
// #[test]
// fn ensure_consistency_randomly_5k_x_5() {
//     ensure_consistency_randomly(5000, 5, &ListReduce::Sum) ;
//     ensure_consistency_randomly(5000, 5, &ListReduce::Max) ;
//     ensure_consistency_randomly(5000, 5, &ListReduce::Vec(ListTransf::Reverse, None)) ;
//     ensure_consistency_randomly(5000, 5, &ListReduce::Vec(ListTransf::Sort, None)) ;
// }

// #[ignore]
// #[test]
// fn ensure_consistency_randomly_10k_x_5() {
//     ensure_consistency_randomly(10000, 5, &ListReduce::Sum) ;
//     ensure_consistency_randomly(10000, 5, &ListReduce::Max) ;
//     ensure_consistency_randomly(10000, 5, &ListReduce::Vec(ListTransf::Reverse, None)) ;
//     ensure_consistency_randomly(10000, 5, &ListReduce::Vec(ListTransf::Sort, None)) ;
// }


// #[test]
// fn ensure_consistency_regression_sort1() { assert!( compare_naive_and_cached(&testcase_sort_1(), &ListReduce::Vec(ListTransf::Sort, None))) }

// #[test]
// fn ensure_consistency_regression_sort2() { assert!( compare_naive_and_cached(&testcase_sort_2(), &ListReduce::Vec(ListTransf::Sort, None))) }

// #[test]
// fn ensure_consistency_regression_sort3() { assert!( compare_naive_and_cached(&testcase_sort_3(), &ListReduce::Vec(ListTransf::Sort, None))) }

// #[test]
// fn ensure_consistency_regression_sort4() { assert!( compare_naive_and_cached(&testcase_sort_4(), &ListReduce::Vec(ListTransf::Sort, None))) }

// #[test]
// fn ensure_consistency_regression_sort5() { assert!( compare_naive_and_cached(&testcase_sort_5(), &ListReduce::Vec(ListTransf::Sort, None))) }

// #[test]
// fn ensure_consistency_regression_sort6() { assert!( compare_naive_and_cached(&testcase_sort_6(), &ListReduce::Vec(ListTransf::Sort, None))) }

// fn testcase_sort_1 () -> Edits {
//     vec![CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 44), CursorEdit::Insert(Dir2::Left, 9), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 91), CursorEdit::Insert(Dir2::Right, 29), CursorEdit::Insert(Dir2::Right, 62), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 71), CursorEdit::Remove(Dir2::Right), CursorEdit::Insert(Dir2::Right, 87), CursorEdit::Insert(Dir2::Left, 4), CursorEdit::Remove(Dir2::Right), CursorEdit::Insert(Dir2::Right, 19), CursorEdit::Replace(Dir2::Left, 21), CursorEdit::Insert(Dir2::Left, 16), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 6), CursorEdit::Insert(Dir2::Right, 10), CursorEdit::Goto(Dir2::Left), CursorEdit::Remove(Dir2::Right), CursorEdit::Insert(Dir2::Left, 58), CursorEdit::Insert(Dir2::Right, 60), CursorEdit::Insert(Dir2::Right, 86), CursorEdit::Insert(Dir2::Right, 36), CursorEdit::Insert(Dir2::Right, 20), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 94)]
// }


// fn testcase_sort_2 () -> Edits {
//     vec![CursorEdit::Insert(Dir2::Left, 92), CursorEdit::Insert(Dir2::Left, 99), CursorEdit::Insert(Dir2::Right, 56), CursorEdit::Insert(Dir2::Right, 64), CursorEdit::Remove(Dir2::Left), CursorEdit::Insert(Dir2::Right, 82), CursorEdit::Insert(Dir2::Right, 10), CursorEdit::Insert(Dir2::Left, 22), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Right, 13), CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 19), CursorEdit::Replace(Dir2::Right, 10), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 68), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 21), CursorEdit::Insert(Dir2::Right, 61), CursorEdit::Insert(Dir2::Right, 48), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 75), CursorEdit::Replace(Dir2::Right, 75), CursorEdit::Insert(Dir2::Right, 75), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 10), CursorEdit::Replace(Dir2::Left, 96), CursorEdit::Insert(Dir2::Left, 86), CursorEdit::Insert(Dir2::Right, 42), CursorEdit::Insert(Dir2::Right, 82), CursorEdit::Replace(Dir2::Right, 38), CursorEdit::Remove(Dir2::Left), CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 9), CursorEdit::Replace(Dir2::Left, 8), CursorEdit::Remove(Dir2::Right), CursorEdit::Replace(Dir2::Left, 4), CursorEdit::Insert(Dir2::Right, 69), CursorEdit::Insert(Dir2::Right, 40), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 31), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 31), CursorEdit::Insert(Dir2::Left, 26), CursorEdit::Insert(Dir2::Right, 92), CursorEdit::Insert(Dir2::Left, 46), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 98), CursorEdit::Insert(Dir2::Left, 53), CursorEdit::Insert(Dir2::Left, 0), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 56), CursorEdit::Insert(Dir2::Left, 32), CursorEdit::Insert(Dir2::Left, 0), CursorEdit::Replace(Dir2::Right, 20), CursorEdit::Insert(Dir2::Right, 95), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 38), CursorEdit::Insert(Dir2::Left, 81), CursorEdit::Insert(Dir2::Right, 79), CursorEdit::Insert(Dir2::Left, 40), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 27), CursorEdit::Insert(Dir2::Left, 27), CursorEdit::Insert(Dir2::Left, 88), CursorEdit::Insert(Dir2::Left, 37), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 69)]
// }

// fn testcase_sort_3 () -> Edits {
//     vec![CursorEdit::Insert(Dir2::Left, 60), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 25), CursorEdit::Remove(Dir2::Left), CursorEdit::Replace(Dir2::Left, 86), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Remove(Dir2::Right), CursorEdit::Replace(Dir2::Left, 81), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Remove(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 85), CursorEdit::Insert(Dir2::Right, 10), CursorEdit::Insert(Dir2::Right, 53), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Left, 64), CursorEdit::Replace(Dir2::Right, 23), CursorEdit::Insert(Dir2::Left, 66), CursorEdit::Remove(Dir2::Left), CursorEdit::Insert(Dir2::Left, 1), CursorEdit::Insert(Dir2::Right, 17), CursorEdit::Insert(Dir2::Left, 9), CursorEdit::Replace(Dir2::Left, 31), CursorEdit::Insert(Dir2::Right, 32), CursorEdit::Insert(Dir2::Left, 76), CursorEdit::Replace(Dir2::Left, 58), CursorEdit::Replace(Dir2::Left, 55), CursorEdit::Remove(Dir2::Right), CursorEdit::Remove(Dir2::Right), CursorEdit::Replace(Dir2::Right, 26), CursorEdit::Insert(Dir2::Right, 88), CursorEdit::Insert(Dir2::Left, 79), CursorEdit::Insert(Dir2::Left, 65), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 32)]
// }


// fn testcase_sort_4 () -> Edits {
//     vec![CursorEdit::Insert(Dir2::Left, 35), CursorEdit::Insert(Dir2::Left, 94), CursorEdit::Insert(Dir2::Left, 2), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 56), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Left, 43), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 66), CursorEdit::Insert(Dir2::Right, 22), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Left, 6), CursorEdit::Insert(Dir2::Right, 26), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 35), CursorEdit::Insert(Dir2::Right, 36), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 31), CursorEdit::Insert(Dir2::Right, 6), CursorEdit::Insert(Dir2::Right, 18), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Right, 77), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 72), CursorEdit::Insert(Dir2::Right, 69), CursorEdit::Insert(Dir2::Right, 80), CursorEdit::Insert(Dir2::Right, 72), CursorEdit::Insert(Dir2::Left, 72), CursorEdit::Insert(Dir2::Left, 21), CursorEdit::Remove(Dir2::Right), CursorEdit::Insert(Dir2::Left, 36), CursorEdit::Remove(Dir2::Left), CursorEdit::Insert(Dir2::Right, 63), CursorEdit::Insert(Dir2::Left, 30), CursorEdit::Remove(Dir2::Right), CursorEdit::Remove(Dir2::Left), CursorEdit::Insert(Dir2::Left, 14)]
// }

// fn testcase_sort_5 () -> Edits {
//     vec![CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 93), CursorEdit::Remove(Dir2::Left), CursorEdit::Insert(Dir2::Left, 38), CursorEdit::Insert(Dir2::Left, 70), CursorEdit::Remove(Dir2::Right), CursorEdit::Remove(Dir2::Left), CursorEdit::Replace(Dir2::Right, 26), CursorEdit::Remove(Dir2::Left), CursorEdit::Insert(Dir2::Right, 53), CursorEdit::Insert(Dir2::Left, 54), CursorEdit::Insert(Dir2::Right, 8), CursorEdit::Insert(Dir2::Left, 56), CursorEdit::Insert(Dir2::Left, 57), CursorEdit::Remove(Dir2::Right), CursorEdit::Insert(Dir2::Left, 67), CursorEdit::Remove(Dir2::Right), CursorEdit::Insert(Dir2::Left, 67), CursorEdit::Insert(Dir2::Left, 41), CursorEdit::Insert(Dir2::Right, 56), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 63), CursorEdit::Insert(Dir2::Right, 24), CursorEdit::Remove(Dir2::Right), CursorEdit::Remove(Dir2::Right), CursorEdit::Insert(Dir2::Left, 21), CursorEdit::Insert(Dir2::Right, 28), CursorEdit::Insert(Dir2::Right, 81), CursorEdit::Insert(Dir2::Left, 72), CursorEdit::Insert(Dir2::Left, 66), CursorEdit::Insert(Dir2::Right, 29), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 52), CursorEdit::Insert(Dir2::Right, 80), CursorEdit::Insert(Dir2::Left, 3), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 75), CursorEdit::Remove(Dir2::Left), CursorEdit::Replace(Dir2::Left, 69), CursorEdit::Insert(Dir2::Right, 47), CursorEdit::Insert(Dir2::Left, 75), CursorEdit::Insert(Dir2::Right, 28), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 68), CursorEdit::Insert(Dir2::Right, 5), CursorEdit::Goto(Dir2::Right)]
// }

// fn testcase_sort_6 () -> Edits {
//     vec![CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Right, 93), CursorEdit::Insert(Dir2::Right, 50), CursorEdit::Insert(Dir2::Right, 82), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 79), CursorEdit::Insert(Dir2::Right, 79), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Remove(Dir2::Right), CursorEdit::Insert(Dir2::Right, 6), CursorEdit::Insert(Dir2::Right, 89), CursorEdit::Insert(Dir2::Left, 45), CursorEdit::Replace(Dir2::Right, 89), CursorEdit::Insert(Dir2::Right, 19), CursorEdit::Insert(Dir2::Left, 55), CursorEdit::Insert(Dir2::Right, 47), CursorEdit::Insert(Dir2::Right, 41), CursorEdit::Insert(Dir2::Left, 83), CursorEdit::Insert(Dir2::Right, 40), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 84), CursorEdit::Insert(Dir2::Right, 90), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 95), CursorEdit::Insert(Dir2::Right, 60), CursorEdit::Insert(Dir2::Left, 96), CursorEdit::Insert(Dir2::Right, 80), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 33), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Left), CursorEdit::Replace(Dir2::Right, 11), CursorEdit::Insert(Dir2::Left, 94), CursorEdit::Insert(Dir2::Left, 0), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 91), CursorEdit::Insert(Dir2::Left, 24), CursorEdit::Replace(Dir2::Left, 8), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Right, 0), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 91), CursorEdit::Remove(Dir2::Left), CursorEdit::Replace(Dir2::Left, 93), CursorEdit::Insert(Dir2::Right, 23), CursorEdit::Insert(Dir2::Right, 38), CursorEdit::Insert(Dir2::Right, 3), CursorEdit::Insert(Dir2::Right, 51), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Right, 58), CursorEdit::Insert(Dir2::Left, 53), CursorEdit::Insert(Dir2::Left, 90), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 16), CursorEdit::Replace(Dir2::Right, 9), CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Remove(Dir2::Right), CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Remove(Dir2::Left), CursorEdit::Insert(Dir2::Left, 48), CursorEdit::Insert(Dir2::Left, 39), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 75), CursorEdit::Insert(Dir2::Right, 26), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Left, 92), CursorEdit::Replace(Dir2::Right, 5), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 97), CursorEdit::Insert(Dir2::Right, 53), CursorEdit::Remove(Dir2::Right)]
// }


// // Nominal
// // After edit 185, Insert(Left, 120), expected Sum to be [7978], but found [7858].
// // thread 'ensure_consistency_randomly_300_x_100' panicked at '[Insert(Left, 119), Goto(Right), Remove(Left), Replace(Right, 213), Goto(Right), Replace(Left, 209), Insert(Right, 245), Insert(Right, 66), Insert(Left, 281), Remove(Right), Insert(Right, 29), Insert(Right, 4), Goto(Right), Insert(Left, 237), Insert(Left, 57), Goto(Right), Goto(Right), Insert(Left, 112), Remove(Right), Insert(Left, 61), Goto(Right), Remove(Right), Goto(Right), Replace(Left, 108), Insert(Right, 132), Goto(Right), Remove(Left), Remove(Left), Goto(Right), Replace(Left, 151), Goto(Right), Insert(Left, 127), Goto(Left), Goto(Left), Insert(Left, 91), Insert(Right, 115), Goto(Right), Insert(Right, 189), Insert(Right, 261), Insert(Right, 5), Insert(Left, 267), Goto(Right), Insert(Left, 269), Goto(Right), Insert(Right, 9), Insert(Right, 104), Goto(Right), Remove(Left), Insert(Left, 209), Goto(Right), Insert(Left, 29), Insert(Left, 196), Goto(Right), Insert(Left, 92), Goto(Left), Replace(Right, 28), Insert(Left, 260), Replace(Right, 289), Goto(Left), Insert(Left, 164), Goto(Right), Goto(Left), Insert(Left, 236), Remove(Right), Remove(Right), Replace(Left, 47), Insert(Right, 111), Insert(Right, 59), Insert(Right, 167), Goto(Right), Goto(Right), Insert(Left, 129), Replace(Right, 46), Remove(Left), Replace(Right, 109), Insert(Left, 266), Remove(Left), Replace(Left, 257), Remove(Right), Goto(Right), Insert(Left, 92), Insert(Left, 202), Insert(Left, 184), Insert(Left, 98), Insert(Left, 217), Insert(Right, 197), Insert(Right, 219), Remove(Right), Insert(Right, 9), Goto(Right), Goto(Right), Goto(Right), Goto(Left), Insert(Left, 147), Goto(Right), Insert(Right, 141), Insert(Right, 171), Replace(Right, 138), Insert(Right, 38), Insert(Right, 27), Remove(Right), Remove(Right), Goto(Right), Goto(Right), Remove(Left), Insert(Right, 266), Goto(Right), Goto(Right), Insert(Left, 22), Insert(Right, 93), Goto(Right), Insert(Left, 117), Goto(Right), Insert(Right, 233), Remove(Left), Insert(Left, 7), Insert(Left, 103), Insert(Right, 111), Goto(Right), Goto(Right), Insert(Left, 254), Remove(Right), Remove(Left), Goto(Right), Insert(Left, 297), Goto(Right), Remove(Left), Goto(Right), Replace(Right, 97), Insert(Left, 206), Goto(Right), Insert(Left, 121), Insert(Left, 80), Insert(Left, 63), Insert(Left, 145), Insert(Right, 156), Insert(Left, 80), Insert(Left, 224), Remove(Left), Insert(Right, 82), Remove(Right), Goto(Right), Remove(Right), Goto(Right), Goto(Right), Insert(Right, 85), Insert(Left, 294), Replace(Right, 37), Remove(Right), Replace(Left, 71), Goto(Right), Goto(Right), Insert(Right, 35), Goto(Right), Insert(Left, 172), Goto(Right), Goto(Right), Remove(Right), Goto(Right), Goto(Right), Insert(Left, 205), Insert(Right, 0), Replace(Left, 181), Insert(Right, 133), Remove(Right), Insert(Left, 108), Remove(Right), Insert(Right, 171), Insert(Left, 247), Insert(Left, 259), Goto(Left), Insert(Left, 218), Insert(Left, 12), Goto(Right), Remove(Right), Insert(Right, 198), Replace(Right, 96), Goto(Right), Remove(Right), Goto(Right), Goto(Right), Insert(Left, 83), Replace(Right, 37), Insert(Left, 214), Remove(Left), Insert(Left, 120), Replace(Right, 279), Insert(Right, 78), Replace(Left, 243), Remove(Left), Replace(Left, 216), Goto(Right), Goto(Right), Replace(Right, 293), Goto(Right), Goto(Right), Remove(Right), Remove(Right), Remove(Left), Goto(Right), Replace(Left, 115), Goto(Right), Insert(Left, 274), Remove(Right), Insert(Right, 76), Goto(Left), Remove(Right), Insert(Right, 233), Insert(Right, 167), Goto(Right), Remove(Left), Insert(Right, 41), Insert(Right, 138), Insert(Right, 25), Goto(Left), Insert(Right, 251), Insert(Right, 22), Goto(Right), Remove(Left), Insert(Right, 250), Goto(Right), Insert(Right, 139), Goto(Left), Replace(Right, 147), Insert(Left, 56), Insert(Right, 240), Insert(Left, 125), Insert(Left, 46), Goto(Right), Insert(Left, 116), Insert(Right, 203), Insert(Right, 132), Insert(Right, 188), Goto(Right), Insert(Left, 244), Remove(Left), Insert(Left, 263), Replace(Right, 80), Insert(Left, 299), Insert(Right, 212), Goto(Left), Replace(Right, 241), Insert(Left, 291), Goto(Right), Goto(Right)]', tests/listedit.rs:138
// //    test ensure_consistency_randomly_300_x_100 ... FAILED

// // Structural
// // After edit 45, Goto(Right), expected Sum to be [952], but found [865].
// // thread 'ensure_consistency_randomly_100_x_100' panicked at '[Insert(Right, 37), Goto(Right), Remove(Right), Replace(Right, 99), Insert(Left, 16), Goto(Right), Insert(Left, 76), Goto(Right), Insert(Left, 60), Goto(Right), Replace(Left, 33), Insert(Right, 60), Insert(Left, 80), Insert(Right, 93), Insert(Left, 49), Replace(Right, 68), Insert(Left, 27), Insert(Left, 82), Insert(Left, 11), Goto(Right), Insert(Left, 37), Insert(Right, 7), Goto(Right), Replace(Right, 28), Goto(Right), Insert(Left, 3), Replace(Left, 32), Goto(Right), Goto(Right), Insert(Left, 69), Replace(Right, 11), Insert(Right, 67), Replace(Right, 31), Insert(Left, 83), Replace(Right, 61), Replace(Right, 60), Goto(Right), Goto(Right), Insert(Right, 69), Insert(Right, 70), Goto(Right), Remove(Right), Insert(Left, 66), Remove(Left), Insert(Right, 87), Goto(Right), Replace(Left, 71), Insert(Left, 34), Replace(Right, 93), Insert(Right, 91), Remove(Right), Insert(Left, 69), Goto(Right), Goto(Right), Insert(Right, 49), Insert(Left, 82), Insert(Left, 8), Replace(Left, 51), Insert(Right, 54), Insert(Left, 19), Goto(Right), Replace(Right, 18), Replace(Right, 63), Remove(Left), Remove(Left), Remove(Left), Goto(Right), Replace(Left, 28), Insert(Left, 88), Insert(Left, 18), Insert(Left, 76), Remove(Left), Goto(Right), Goto(Right), Replace(Left, 83), Goto(Right), Goto(Right), Insert(Left, 83), Insert(Left, 55), Insert(Right, 71), Insert(Left, 1), Goto(Right), Goto(Right), Goto(Right), Insert(Left, 5), Insert(Left, 54), Insert(Right, 29), Insert(Left, 9), Replace(Right, 81), Insert(Right, 55), Insert(Left, 81), Insert(Left, 5), Insert(Right, 13), Goto(Right), Goto(Right), Insert(Left, 49), Insert(Right, 82), Insert(Left, 11)]', tests/listedit.rs:136

// // Structural
// // After edit 53, Goto(Right), expected Sum to be [1110], but found [1066].
// // thread 'ensure_consistency_randomly_100_x_100' panicked at '[Goto(Right), Insert(Right, 18), Insert(Right, 22), Insert(Left, 47), Insert(Right, 32), Insert(Right, 96), Replace(Left, 37), Goto(Left), Goto(Right), Remove(Left), Insert(Left, 88), Insert(Left, 45), Insert(Left, 12), Insert(Left, 6), Insert(Right, 55), Insert(Right, 14), Insert(Right, 34), Replace(Right, 70), Insert(Left, 73), Insert(Left, 56), Insert(Left, 35), Insert(Left, 19), Remove(Right), Insert(Left, 73), Goto(Right), Replace(Right, 12), Goto(Right), Insert(Left, 5), Insert(Right, 50), Remove(Left), Remove(Left), Replace(Left, 44), Goto(Right), Insert(Left, 37), Goto(Right), Insert(Right, 12), Goto(Right), Goto(Right), Goto(Right), Replace(Left, 66), Insert(Left, 50), Insert(Left, 69), Insert(Right, 73), Goto(Right), Insert(Left, 45), Insert(Left, 18), Goto(Right), Replace(Right, 98), Insert(Left, 49), Goto(Right), Insert(Left, 66), Remove(Left), Insert(Right, 44), Goto(Right), Goto(Right), Insert(Left, 20), Insert(Right, 58), Insert(Left, 28), Insert(Right, 82), Goto(Right), Insert(Right, 7), Insert(Right, 87), Insert(Left, 97), Replace(Right, 16), Insert(Left, 79), Insert(Left, 82), Insert(Left, 92), Insert(Right, 91), Insert(Right, 6), Goto(Left), Replace(Right, 44), Insert(Left, 63), Insert(Right, 50), Goto(Left)]', tests/listedit.rs:136

// // Structural
// // After edit 43, Goto(Right), expected Sum to be [732], but found [717].
// // thread 'ensure_consistency_randomly_100_x_100' panicked at '[Goto(Right), Insert(Left, 73), Remove(Left), Replace(Right, 2), Insert(Right, 62), Goto(Right), Remove(Right), Goto(Right), Goto(Right), Insert(Right, 75), Goto(Right), Goto(Left), Goto(Right), Goto(Right), Replace(Right, 40), Goto(Right), Remove(Right), Insert(Left, 6), Insert(Left, 95), Remove(Right), Remove(Right), Goto(Right), Insert(Right, 58), Insert(Left, 0), Insert(Left, 12), Goto(Right), Insert(Left, 41), Insert(Left, 93), Insert(Left, 81), Replace(Left, 47), Insert(Right, 42), Insert(Right, 67), Goto(Right), Goto(Right), Insert(Left, 3), Insert(Right, 42), Insert(Left, 74), Goto(Right), Goto(Right), Replace(Right, 60), Goto(Left), Goto(Right), Insert(Right, 15), Goto(Right), Insert(Right, 21), Goto(Right), Insert(Left, 90), Goto(Left), Insert(Left, 84), Insert(Right, 34), Goto(Left), Goto(Right), Goto(Right), Remove(Right), Goto(Right), Insert(Right, 66), Insert(Left, 49), Insert(Right, 62), Goto(Right), Insert(Right, 82), Insert(Right, 87), Insert(Right, 43), Goto(Right), Goto(Right), Insert(Right, 69)]', tests/listedit.rs:136

// // -----------------------------------------------------------------------------------------------------
// // Max Regression tests
// //

// #[test]
// fn ensure_consistency_regression_testcase1() { assert!( compare_naive_and_cached(&testcase1(), &ListReduce::Max)) }

// #[test]
// fn ensure_consistency_regression_testcase2() { assert!( compare_naive_and_cached(&testcase2(), &ListReduce::Max)) }

// #[test]
// fn ensure_consistency_regression_testcase3() { assert!( compare_naive_and_cached(&testcase3(), &ListReduce::Max)) }

// #[test]
// fn ensure_consistency_regression_testcase4() { assert!( compare_naive_and_cached(&testcase4(), &ListReduce::Max)) }

// #[test]
// fn ensure_consistency_regression_testcase5() { assert!( compare_naive_and_cached(&testcase5(), &ListReduce::Max)) }

// fn testcase1 () -> Edits {
//     vec![
//         CursorEdit::Insert(Dir2::Left, 36), CursorEdit::Insert(Dir2::Right, 44), CursorEdit::Remove(Dir2::Right), CursorEdit::Remove(Dir2::Right),
//         CursorEdit::Insert(Dir2::Left, 86), CursorEdit::Insert(Dir2::Left, 11), CursorEdit::Insert(Dir2::Right, 22), CursorEdit::Insert(Dir2::Right, 23),
//         CursorEdit::Insert(Dir2::Left, 41), CursorEdit::Remove(Dir2::Right),
//         CursorEdit::Insert(Dir2::Right, 13), CursorEdit::Insert(Dir2::Left, 21), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 41),
//         CursorEdit::Insert(Dir2::Left, 71), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 22), CursorEdit::Replace(Dir2::Left, 11), CursorEdit::Goto(Dir2::Right),
//         CursorEdit::Insert(Dir2::Right, 76), CursorEdit::Insert(Dir2::Left, 45), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 12), CursorEdit::Insert(Dir2::Right, 14),
//         CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 35), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 39),
//         CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Right, 43), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 36), CursorEdit::Insert(Dir2::Left, 85),
//         CursorEdit::Insert(Dir2::Left, 11), CursorEdit::Insert(Dir2::Left, 93), CursorEdit::Insert(Dir2::Right, 52), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Right)
//        ]
// }

// fn testcase2 () -> Edits {
//     vec![CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 86), CursorEdit::Insert(Dir2::Left, 76), CursorEdit::Insert(Dir2::Right, 39), CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Insert(Dir2::Right, 63), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Left, 54), CursorEdit::Remove(Dir2::Left),
//          CursorEdit::Insert(Dir2::Right, 77), CursorEdit::Insert(Dir2::Right, 32), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Replace(Dir2::Right, 57), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Left, 6), CursorEdit::Remove(Dir2::Right),
//          CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 81), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Remove(Dir2::Right), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Insert(Dir2::Right, 76), CursorEdit::Replace(Dir2::Left, 72), CursorEdit::Insert(Dir2::Left, 51), CursorEdit::Remove(Dir2::Right), CursorEdit::Insert(Dir2::Left, 49),
//          CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 6),
//          CursorEdit::Insert(Dir2::Right, 82), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 89), CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Insert(Dir2::Left, 9), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 26), CursorEdit::Replace(Dir2::Left, 35),
//          CursorEdit::Goto(Dir2::Left), CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 5), CursorEdit::Insert(Dir2::Left, 75),
//          CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 32), CursorEdit::Replace(Dir2::Right, 74), CursorEdit::Insert(Dir2::Left, 77),
//          CursorEdit::Insert(Dir2::Left, 71), CursorEdit::Insert(Dir2::Left, 44), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Right, 81),
//          CursorEdit::Insert(Dir2::Left, 61), CursorEdit::Insert(Dir2::Right, 92), CursorEdit::Insert(Dir2::Left, 68), CursorEdit::Replace(Dir2::Right, 42),
//          CursorEdit::Insert(Dir2::Right, 81), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 31),
//          CursorEdit::Insert(Dir2::Right, 72), CursorEdit::Insert(Dir2::Right, 70), CursorEdit::Insert(Dir2::Left, 87), CursorEdit::Insert(Dir2::Right, 95),
//          CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Left), CursorEdit::Replace(Dir2::Right, 96), CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Goto(Dir2::Left), CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Left)]
// }

// fn testcase3 () -> Edits {
//     vec![CursorEdit::Insert(Dir2::Left, 86),
//          CursorEdit::Insert(Dir2::Right, 37),
//          CursorEdit::Remove(Dir2::Left),
//          CursorEdit::Insert(Dir2::Left, 42),
//          CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Insert(Dir2::Left, 68),
//          CursorEdit::Insert(Dir2::Right, 18),
//          CursorEdit::Remove(Dir2::Left),
//          CursorEdit::Remove(Dir2::Right),
//          CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Replace(Dir2::Left, 68),
//          CursorEdit::Insert(Dir2::Left, 82),
//          CursorEdit::Insert(Dir2::Right, 30),
//          CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Insert(Dir2::Left, 78),
//          CursorEdit::Insert(Dir2::Right, 88),
//          CursorEdit::Insert(Dir2::Left, 38),
//          CursorEdit::Insert(Dir2::Right, 91),
//          CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Insert(Dir2::Right, 67),
//          CursorEdit::Insert(Dir2::Right, 3),
//          CursorEdit::Remove(Dir2::Right),
//          CursorEdit::Insert(Dir2::Right, 16),
//          CursorEdit::Insert(Dir2::Left, 4),
//          CursorEdit::Insert(Dir2::Right, 29),
//          CursorEdit::Insert(Dir2::Right, 92),
//          CursorEdit::Insert(Dir2::Left, 79),
//          CursorEdit::Replace(Dir2::Left, 88),
//          CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Goto(Dir2::Left),
//          // CursorEdit::Insert(Dir2::Left, 25), CursorEdit::Insert(Dir2::Left, 46), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Left),
//          // CursorEdit::Insert(Dir2::Left, 18), CursorEdit::Insert(Dir2::Left, 1), CursorEdit::Insert(Dir2::Right, 43), CursorEdit::Goto(Dir2::Left), CursorEdit::Remove(Dir2::Left),
//          // CursorEdit::Insert(Dir2::Right, 93), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Right, 10), CursorEdit::Remove(Dir2::Left),
//          // CursorEdit::Insert(Dir2::Right, 34), CursorEdit::Remove(Dir2::Left), CursorEdit::Replace(Dir2::Left, 47), CursorEdit::Goto(Dir2::Right),
//          // CursorEdit::Insert(Dir2::Left, 56), CursorEdit::Insert(Dir2::Left, 36), CursorEdit::Replace(Dir2::Right, 99), CursorEdit::Insert(Dir2::Right, 19),
//          // CursorEdit::Insert(Dir2::Right, 35), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 94), CursorEdit::Replace(Dir2::Right, 58), CursorEdit::Goto(Dir2::Right),
//          // CursorEdit::Insert(Dir2::Left, 71)
//          ]
// }

// fn testcase4 () -> Edits {
//     vec![
//         CursorEdit::Replace(Dir2::Left, 29),
//         CursorEdit::Replace(Dir2::Left, 34),
//         CursorEdit::Goto(Dir2::Right),
//         CursorEdit::Goto(Dir2::Right),
//         CursorEdit::Insert(Dir2::Right, 30),
//         CursorEdit::Insert(Dir2::Left, 26),
//         CursorEdit::Insert(Dir2::Right, 26),
//         CursorEdit::Insert(Dir2::Left, 7),
//         CursorEdit::Remove(Dir2::Left),
//         CursorEdit::Goto(Dir2::Left),
//         CursorEdit::Insert(Dir2::Left, 19),
//         CursorEdit::Insert(Dir2::Left, 16),
//         CursorEdit::Goto(Dir2::Right),
//         CursorEdit::Goto(Dir2::Left),
//         CursorEdit::Insert(Dir2::Right, 27),
//         CursorEdit::Insert(Dir2::Right, 3),
//         CursorEdit::Insert(Dir2::Left, 13),
//         CursorEdit::Goto(Dir2::Left),
//         CursorEdit::Insert(Dir2::Left, 26),
//         CursorEdit::Insert(Dir2::Left, 10),
//         CursorEdit::Insert(Dir2::Right, 2),
//         CursorEdit::Insert(Dir2::Right, 38),
//         CursorEdit::Insert(Dir2::Left, 36),
//         CursorEdit::Replace(Dir2::Left, 36),
//         CursorEdit::Insert(Dir2::Left, 8),
//         CursorEdit::Insert(Dir2::Left, 39),
//         CursorEdit::Replace(Dir2::Right, 7),
//         CursorEdit::Insert(Dir2::Right, 30),
//         CursorEdit::Goto(Dir2::Left),
//         CursorEdit::Goto(Dir2::Right),
//         //CursorEdit::Goto(Dir2::Right),
//         //CursorEdit::Insert(Dir2::Right, 25),
//         //CursorEdit::Insert(Dir2::Left, 0),
//         //CursorEdit::Insert(Dir2::Left, 0),
//         //CursorEdit::Goto(Dir2::Right),
//         //CursorEdit::Goto(Dir2::Right)
//       ]
// }

// fn testcase5 () -> Edits {
//     vec![CursorEdit::Insert(Dir2::Left, 5),
//          CursorEdit::Insert(Dir2::Right, 5),
//          CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Insert(Dir2::Left, 25),
//          CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Insert(Dir2::Right, 37),
//          CursorEdit::Replace(Dir2::Left, 1),
//          CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Remove(Dir2::Right),
//          CursorEdit::Replace(Dir2::Left, 20),
//          CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Replace(Dir2::Right, 4),
//          CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Insert(Dir2::Right, 25),
//          CursorEdit::Remove(Dir2::Left),
//          CursorEdit::Insert(Dir2::Right, 5),
//          CursorEdit::Replace(Dir2::Left, 11),
//          CursorEdit::Insert(Dir2::Left, 30),
//          CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Insert(Dir2::Left, 1),
//          CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Insert(Dir2::Right, 3),
//          CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Insert(Dir2::Right, 16),
//          CursorEdit::Insert(Dir2::Right, 31),
//          CursorEdit::Insert(Dir2::Left, 24),
//          CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Insert(Dir2::Left, 22),
//          CursorEdit::Insert(Dir2::Left, 33),
//          CursorEdit::Goto(Dir2::Right),
//          CursorEdit::Insert(Dir2::Left, 3),
//          CursorEdit::Insert(Dir2::Left, 1),
//          CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Insert(Dir2::Right, 17),
//          CursorEdit::Goto(Dir2::Left),
//          CursorEdit::Insert(Dir2::Left, 34),
//          CursorEdit::Replace(Dir2::Right, 9)]
// }

// // ---- ensure_consistency stdout ----
// //     after edit 29: Replace(Dir2::Dir2::Right, 47): expected Max to be [47], but found [45]
// //     thread 'ensure_consistency' panicked at '[Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 45), Remove(Dir2::Dir2::Right), Remove(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 19), Goto(Dir2::Dir2::Left), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 23), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 14), Insert(Dir2::Dir2::Left, 28), Goto(Dir2::Dir2::Right), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 37), Replace(Dir2::Dir2::Left, 35), Insert(Dir2::Dir2::Right, 0), Insert(Dir2::Dir2::Right, 23), Insert(Dir2::Dir2::Left, 27), Goto(Dir2::Dir2::Left), Insert(Dir2::Dir2::Right, 11), Insert(Dir2::Dir2::Right, 30), Insert(Dir2::Dir2::Right, 25), Insert(Dir2::Dir2::Right, 38), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 24), Insert(Dir2::Dir2::Left, 45), Replace(Dir2::Dir2::Left, 31), Replace(Dir2::Dir2::Right, 43), Replace(Dir2::Dir2::Right, 2), Replace(Dir2::Dir2::Right, 47), Insert(Dir2::Dir2::Right, 46), Insert(Dir2::Dir2::Left, 27), Goto(Dir2::Dir2::Right), Remove(Dir2::Dir2::Right), Goto(Dir2::Dir2::Left), Insert(Dir2::Dir2::Left, 44), Goto(Dir2::Dir2::Left), Insert(Dir2::Dir2::Right, 27), Goto(Dir2::Dir2::Left)]', tests/listedit.rs:52

// // ---- ensure_consistency stdout ----
// //     after edit 41: Goto(Dir2::Dir2::Right): expected Max to be [49], but found [44]
// //     thread 'ensure_consistency' panicked at '[Replace(Dir2::Dir2::Right, 45), Insert(Dir2::Dir2::Right, 0), Insert(Dir2::Dir2::Left, 12), Insert(Dir2::Dir2::Right, 22), Goto(Dir2::Dir2::Right), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 16), Replace(Dir2::Dir2::Right, 7), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 16), Insert(Dir2::Dir2::Right, 27), Insert(Dir2::Dir2::Left, 0), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 36), Goto(Dir2::Dir2::Right), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 11), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 32), Remove(Dir2::Dir2::Left), Insert(Dir2::Dir2::Right, 14), Remove(Dir2::Dir2::Right), Goto(Dir2::Dir2::Right), Replace(Dir2::Dir2::Right, 23), Insert(Dir2::Dir2::Left, 34), Replace(Dir2::Dir2::Right, 49), Insert(Dir2::Dir2::Left, 0), Insert(Dir2::Dir2::Left, 16), Remove(Dir2::Dir2::Left), Insert(Dir2::Dir2::Left, 4), Goto(Dir2::Dir2::Right), Replace(Dir2::Dir2::Right, 23), Insert(Dir2::Dir2::Left, 44), Goto(Dir2::Dir2::Left), Insert(Dir2::Dir2::Left, 23), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 18), Insert(Dir2::Dir2::Right, 15), Replace(Dir2::Dir2::Left, 49), Insert(Dir2::Dir2::Right, 16), Goto(Dir2::Dir2::Left), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 46), Goto(Dir2::Dir2::Left)]', tests/listedit.rs:52
