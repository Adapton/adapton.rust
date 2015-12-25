#![feature(test)]
#![feature(plugin)]
#![feature(zero_one)]
// #![plugin(quickcheck_macros)]

#[macro_use] extern crate log;


//
// cargo test listedit::experiments -- --nocapture
//

extern crate adapton ;
extern crate test;
extern crate quickcheck;
extern crate rand;

use std::num::Zero;

use adapton::adapton_sigs::* ;
use adapton::collection_traits::*;
use adapton::collection_edit::*;
use adapton::collection::*;
use adapton::engine;
use adapton::naive;
use std::fmt::Debug;
use std::hash::Hash;

type Edits = Vec<CursorEdit<u32, Dir2>>;

fn has_consecutive_names<A:Adapton,X,L:ListT<A,X>> (st:&mut A, list:L::List) -> bool {
    L::elim(st, list,
            |st| false,
            |st,x,xs| has_consecutive_names::<A,X,L> (st, xs),
            |st,n,xs|
            L::elim(st, xs,
                    |st| false,
                    |st,y,ys| has_consecutive_names::<A,X,L> (st, ys),
                    |st,m,ys| true))
}

pub struct Experiment ;
impl<A:Adapton,X:Zero+Hash+Debug+PartialEq+Eq+Clone+PartialOrd> ExperimentT<A,X,Vec<X>>
    for Experiment
{
    type ListEdit = ListZipper<A,X,List<A,X>> ;
    fn run (st:&mut A, edits:Vec<CursorEdit<X,Dir2>>, view:ListReduce) -> Vec<(Vec<X>,Cnt)> {
        debug!("run");
        let mut outs : Vec<(Vec<X>,Cnt)> = Vec::new();
        let mut z : ListZipper<A,X,List<A,X>> = Self::ListEdit::empty(st) ;
        let mut loop_cnt = 0 as usize;
        for edit in edits.into_iter() {
            debug!("\n----------------------- Loop head; count={}", loop_cnt);
            debug!("zipper: {:?}", z);
            if false {
                let consecutive_left  = has_consecutive_names::<A,X,List<A,X>>(st, z.left.clone());
                let consecutive_right = has_consecutive_names::<A,X,List<A,X>>(st, z.right.clone());
                debug!("zipper names: consecutive left: {}, consecutive right: {}",
                       consecutive_left, consecutive_right);
                assert!(!consecutive_left);  // Todo-Later: This assertion generally fails for random interactions
                assert!(!consecutive_right); // Todo-Later: This assertion generally fails for random interactions
            }
            debug!("edit:   {:?}", edit);
            let (out, cnt) = st.cnt(|st|{
                let z_next = eval_edit::<A,X,Self::ListEdit>(st, edit, z.clone(), loop_cnt);
                let tree = Self::ListEdit::get_tree::<Tree<A,X,u32>>(st, z_next.clone(), Dir2::Left);
                debug!("tree:   {:?}", tree);
                let nm = st.name_of_string("eval_reduce".to_string());
                let out = st.ns(nm, |st|eval_reduce::<A,X,Tree<A,X,u32>>(st, tree, &view) );
                z = z_next;
                loop_cnt = loop_cnt + 1;
                out
            }) ;
            debug!("out:    {:?}", out);
            debug!("cnt:    {:?}", cnt);
            outs.push((out,cnt));
        } outs
    }
}

fn compare_naive_and_cached(edits: &Edits) -> bool {
    let reduction = ListReduce::Max;
    let mut n_st = naive::AdaptonFromScratch::new();
    let mut e_st = engine::Engine::new();
    
    let results_1 = Experiment::run(&mut n_st, edits.clone(), reduction.clone());
    let results_2 = Experiment::run(&mut e_st, edits.clone(), reduction.clone());
    
    let mut idx = 0;
    let mut a_cost : Cnt = Cnt::zero();
    let mut b_cost : Cnt = Cnt::zero();
    for (a, b) in results_1.iter().zip(results_2.iter()) {
        a_cost = &a_cost + &a.1 ;
        b_cost = &b_cost + &b.1 ;
        if a.0 != b.0 {
            println!("After edit {}, {:?}, expected {:?} to be {:?}, but found {:?}.",
                     idx, edits[idx], &reduction, a.0, b.0);
            return false;
        }
        idx += 1;
    }
    {
        let naive_total = a_cost.eval ;
        let engine_total = b_cost.dirty + b_cost.eval + b_cost.change_prop ;
        if false {
        println!("For {:5} edits, Naive/Engine:{:5} = {:8} / {:8}. Naive/EngineEval:{:5}. In Engine, eval is {:.2} of {:?}",
                 edits.len(),
                 (naive_total as f32) / (engine_total as f32),
                 naive_total, engine_total,
                 (naive_total as f32) / (b_cost.eval as f32),
                 (b_cost.eval as f32) / (engine_total as f32),
                 b_cost);
        } ;
        println!("For {:5} edits, Naive/Engine:{:.2}, Naive/EngineEval:{:.2}.  Per-edit: Naive:{:.2} Engine:{:.2} EngineEval:{:.2}",
                 edits.len(),
                 (naive_total as f32) / (engine_total as f32),
                 (naive_total as f32) / (b_cost.eval as f32),
                 // Per-edit metrics:
                 (naive_total as f32) / (edits.len() as f32),
                 (engine_total as f32) / (edits.len() as f32),
                 (b_cost.eval as f32) / (edits.len() as f32),
                 );
    }
    true
}

fn ensure_consistency_randomly(size:usize, iterations:usize) {
    let rng = rand::thread_rng();
    let mut gen = quickcheck::StdGen::new(rng, size);
    for _ in 0..iterations {
        let testv = Box::new(<Edits as quickcheck::Arbitrary>::arbitrary(&mut gen));
        if !compare_naive_and_cached(&*testv) {
            panic!("{:?}", testv);
        }
    }
}

#[test]
fn ensure_consistency_randomly_100_x_100() { ensure_consistency_randomly(100, 100) }

#[test]
fn ensure_consistency_randomly_1k_x_20() { ensure_consistency_randomly(1000, 20) }

#[test]
fn ensure_consistency_randomly_5k_x_5() { ensure_consistency_randomly(5000, 5) }

#[test]
fn ensure_consistency_randomly_10k_x_5() { ensure_consistency_randomly(10000, 5) }

#[test]
fn ensure_consistency_regression_testcase1() { assert!( compare_naive_and_cached(&testcase1())) }

#[test]
fn ensure_consistency_regression_testcase2() { assert!( compare_naive_and_cached(&testcase2())) }

#[test]
fn ensure_consistency_regression_testcase3() { assert!( compare_naive_and_cached(&testcase3())) }

#[test]
fn ensure_consistency_regression_testcase4() { assert!( compare_naive_and_cached(&testcase4())) }

#[test]
fn ensure_consistency_regression_testcase5() { assert!( compare_naive_and_cached(&testcase5())) }

fn testcase1 () -> Edits {
    vec![
        CursorEdit::Insert(Dir2::Left, 36), CursorEdit::Insert(Dir2::Right, 44), CursorEdit::Remove(Dir2::Right), CursorEdit::Remove(Dir2::Right),
        CursorEdit::Insert(Dir2::Left, 86), CursorEdit::Insert(Dir2::Left, 11), CursorEdit::Insert(Dir2::Right, 22), CursorEdit::Insert(Dir2::Right, 23),
        CursorEdit::Insert(Dir2::Left, 41), CursorEdit::Remove(Dir2::Right),
        CursorEdit::Insert(Dir2::Right, 13), CursorEdit::Insert(Dir2::Left, 21), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 41),
        CursorEdit::Insert(Dir2::Left, 71), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 22), CursorEdit::Replace(Dir2::Left, 11), CursorEdit::Goto(Dir2::Right),
        CursorEdit::Insert(Dir2::Right, 76), CursorEdit::Insert(Dir2::Left, 45), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 12), CursorEdit::Insert(Dir2::Right, 14),
        CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 35), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 39),
        CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Right, 43), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 36), CursorEdit::Insert(Dir2::Left, 85),
        CursorEdit::Insert(Dir2::Left, 11), CursorEdit::Insert(Dir2::Left, 93), CursorEdit::Insert(Dir2::Right, 52), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Right)
       ]
}

fn testcase2 () -> Edits {
    vec![CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 86), CursorEdit::Insert(Dir2::Left, 76), CursorEdit::Insert(Dir2::Right, 39), CursorEdit::Goto(Dir2::Right),
         CursorEdit::Insert(Dir2::Right, 63), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Left, 54), CursorEdit::Remove(Dir2::Left),
         CursorEdit::Insert(Dir2::Right, 77), CursorEdit::Insert(Dir2::Right, 32), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Left),
         CursorEdit::Replace(Dir2::Right, 57), CursorEdit::Goto(Dir2::Right), CursorEdit::Replace(Dir2::Left, 6), CursorEdit::Remove(Dir2::Right),
         CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 81), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Left),
         CursorEdit::Remove(Dir2::Right), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Right),
         CursorEdit::Insert(Dir2::Right, 76), CursorEdit::Replace(Dir2::Left, 72), CursorEdit::Insert(Dir2::Left, 51), CursorEdit::Remove(Dir2::Right), CursorEdit::Insert(Dir2::Left, 49),
         CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 6),
         CursorEdit::Insert(Dir2::Right, 82), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 89), CursorEdit::Goto(Dir2::Right),
         CursorEdit::Insert(Dir2::Left, 9), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 26), CursorEdit::Replace(Dir2::Left, 35),
         CursorEdit::Goto(Dir2::Left), CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Left),
         CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Left, 5), CursorEdit::Insert(Dir2::Left, 75),
         CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Left, 32), CursorEdit::Replace(Dir2::Right, 74), CursorEdit::Insert(Dir2::Left, 77),
         CursorEdit::Insert(Dir2::Left, 71), CursorEdit::Insert(Dir2::Left, 44), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Right, 81),
         CursorEdit::Insert(Dir2::Left, 61), CursorEdit::Insert(Dir2::Right, 92), CursorEdit::Insert(Dir2::Left, 68), CursorEdit::Replace(Dir2::Right, 42),
         CursorEdit::Insert(Dir2::Right, 81), CursorEdit::Goto(Dir2::Right), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 31),
         CursorEdit::Insert(Dir2::Right, 72), CursorEdit::Insert(Dir2::Right, 70), CursorEdit::Insert(Dir2::Left, 87), CursorEdit::Insert(Dir2::Right, 95),
         CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Left), CursorEdit::Replace(Dir2::Right, 96), CursorEdit::Goto(Dir2::Right),
         CursorEdit::Goto(Dir2::Left), CursorEdit::Remove(Dir2::Left), CursorEdit::Goto(Dir2::Left)]
}

fn testcase3 () -> Edits {
    vec![CursorEdit::Insert(Dir2::Left, 86),
         CursorEdit::Insert(Dir2::Right, 37),
         CursorEdit::Remove(Dir2::Left),
         CursorEdit::Insert(Dir2::Left, 42),
         CursorEdit::Goto(Dir2::Right),
         CursorEdit::Insert(Dir2::Left, 68),
         CursorEdit::Insert(Dir2::Right, 18),
         CursorEdit::Remove(Dir2::Left),
         CursorEdit::Remove(Dir2::Right),
         CursorEdit::Goto(Dir2::Right),
         CursorEdit::Replace(Dir2::Left, 68),
         CursorEdit::Insert(Dir2::Left, 82),
         CursorEdit::Insert(Dir2::Right, 30),
         CursorEdit::Goto(Dir2::Right),
         CursorEdit::Insert(Dir2::Left, 78),
         CursorEdit::Insert(Dir2::Right, 88),
         CursorEdit::Insert(Dir2::Left, 38),
         CursorEdit::Insert(Dir2::Right, 91),
         CursorEdit::Goto(Dir2::Left),
         CursorEdit::Insert(Dir2::Right, 67),
         CursorEdit::Insert(Dir2::Right, 3),
         CursorEdit::Remove(Dir2::Right),
         CursorEdit::Insert(Dir2::Right, 16),
         CursorEdit::Insert(Dir2::Left, 4),
         CursorEdit::Insert(Dir2::Right, 29),
         CursorEdit::Insert(Dir2::Right, 92),
         CursorEdit::Insert(Dir2::Left, 79),
         CursorEdit::Replace(Dir2::Left, 88),
         CursorEdit::Goto(Dir2::Right),
         CursorEdit::Goto(Dir2::Left),
         // CursorEdit::Insert(Dir2::Left, 25), CursorEdit::Insert(Dir2::Left, 46), CursorEdit::Goto(Dir2::Left), CursorEdit::Goto(Dir2::Left),
         // CursorEdit::Insert(Dir2::Left, 18), CursorEdit::Insert(Dir2::Left, 1), CursorEdit::Insert(Dir2::Right, 43), CursorEdit::Goto(Dir2::Left), CursorEdit::Remove(Dir2::Left),
         // CursorEdit::Insert(Dir2::Right, 93), CursorEdit::Goto(Dir2::Left), CursorEdit::Insert(Dir2::Right, 10), CursorEdit::Remove(Dir2::Left),
         // CursorEdit::Insert(Dir2::Right, 34), CursorEdit::Remove(Dir2::Left), CursorEdit::Replace(Dir2::Left, 47), CursorEdit::Goto(Dir2::Right),
         // CursorEdit::Insert(Dir2::Left, 56), CursorEdit::Insert(Dir2::Left, 36), CursorEdit::Replace(Dir2::Right, 99), CursorEdit::Insert(Dir2::Right, 19),
         // CursorEdit::Insert(Dir2::Right, 35), CursorEdit::Goto(Dir2::Right), CursorEdit::Insert(Dir2::Right, 94), CursorEdit::Replace(Dir2::Right, 58), CursorEdit::Goto(Dir2::Right),
         // CursorEdit::Insert(Dir2::Left, 71)
         ]
}

fn testcase4 () -> Edits {
    vec![
        CursorEdit::Replace(Dir2::Left, 29),
        CursorEdit::Replace(Dir2::Left, 34),
        CursorEdit::Goto(Dir2::Right),
        CursorEdit::Goto(Dir2::Right),
        CursorEdit::Insert(Dir2::Right, 30),
        CursorEdit::Insert(Dir2::Left, 26),
        CursorEdit::Insert(Dir2::Right, 26),
        CursorEdit::Insert(Dir2::Left, 7),
        CursorEdit::Remove(Dir2::Left),
        CursorEdit::Goto(Dir2::Left),
        CursorEdit::Insert(Dir2::Left, 19),
        CursorEdit::Insert(Dir2::Left, 16),
        CursorEdit::Goto(Dir2::Right),
        CursorEdit::Goto(Dir2::Left),
        CursorEdit::Insert(Dir2::Right, 27),
        CursorEdit::Insert(Dir2::Right, 3),
        CursorEdit::Insert(Dir2::Left, 13),
        CursorEdit::Goto(Dir2::Left),
        CursorEdit::Insert(Dir2::Left, 26),
        CursorEdit::Insert(Dir2::Left, 10),
        CursorEdit::Insert(Dir2::Right, 2),
        CursorEdit::Insert(Dir2::Right, 38),
        CursorEdit::Insert(Dir2::Left, 36),
        CursorEdit::Replace(Dir2::Left, 36),
        CursorEdit::Insert(Dir2::Left, 8),
        CursorEdit::Insert(Dir2::Left, 39),
        CursorEdit::Replace(Dir2::Right, 7),
        CursorEdit::Insert(Dir2::Right, 30),
        CursorEdit::Goto(Dir2::Left),
        CursorEdit::Goto(Dir2::Right),
        //CursorEdit::Goto(Dir2::Right),
        //CursorEdit::Insert(Dir2::Right, 25),
        //CursorEdit::Insert(Dir2::Left, 0),
        //CursorEdit::Insert(Dir2::Left, 0),
        //CursorEdit::Goto(Dir2::Right),
        //CursorEdit::Goto(Dir2::Right)
      ]
}

fn testcase5 () -> Edits {
    vec![CursorEdit::Insert(Dir2::Left, 5),
         CursorEdit::Insert(Dir2::Right, 5),
         CursorEdit::Goto(Dir2::Right),
         CursorEdit::Insert(Dir2::Left, 25),
         CursorEdit::Goto(Dir2::Left),
         CursorEdit::Insert(Dir2::Right, 37),
         CursorEdit::Replace(Dir2::Left, 1),
         CursorEdit::Goto(Dir2::Right),
         CursorEdit::Remove(Dir2::Right),
         CursorEdit::Replace(Dir2::Left, 20),
         CursorEdit::Goto(Dir2::Left),
         CursorEdit::Replace(Dir2::Right, 4),
         CursorEdit::Goto(Dir2::Left),
         CursorEdit::Goto(Dir2::Right),
         CursorEdit::Insert(Dir2::Right, 25),
         CursorEdit::Remove(Dir2::Left),
         CursorEdit::Insert(Dir2::Right, 5),
         CursorEdit::Replace(Dir2::Left, 11),
         CursorEdit::Insert(Dir2::Left, 30),
         CursorEdit::Goto(Dir2::Left),
         CursorEdit::Insert(Dir2::Left, 1),
         CursorEdit::Goto(Dir2::Right),
         CursorEdit::Goto(Dir2::Left),
         CursorEdit::Insert(Dir2::Right, 3),
         CursorEdit::Goto(Dir2::Left),
         CursorEdit::Insert(Dir2::Right, 16),
         CursorEdit::Insert(Dir2::Right, 31),
         CursorEdit::Insert(Dir2::Left, 24),
         CursorEdit::Goto(Dir2::Right),
         CursorEdit::Insert(Dir2::Left, 22),
         CursorEdit::Insert(Dir2::Left, 33),
         CursorEdit::Goto(Dir2::Right),
         CursorEdit::Insert(Dir2::Left, 3),
         CursorEdit::Insert(Dir2::Left, 1),
         CursorEdit::Goto(Dir2::Left),
         CursorEdit::Insert(Dir2::Right, 17),
         CursorEdit::Goto(Dir2::Left),
         CursorEdit::Insert(Dir2::Left, 34),
         CursorEdit::Replace(Dir2::Right, 9)]
}

// ---- ensure_consistency stdout ----
//     after edit 29: Replace(Dir2::Dir2::Right, 47): expected Max to be [47], but found [45]
//     thread 'ensure_consistency' panicked at '[Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 45), Remove(Dir2::Dir2::Right), Remove(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 19), Goto(Dir2::Dir2::Left), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 23), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 14), Insert(Dir2::Dir2::Left, 28), Goto(Dir2::Dir2::Right), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 37), Replace(Dir2::Dir2::Left, 35), Insert(Dir2::Dir2::Right, 0), Insert(Dir2::Dir2::Right, 23), Insert(Dir2::Dir2::Left, 27), Goto(Dir2::Dir2::Left), Insert(Dir2::Dir2::Right, 11), Insert(Dir2::Dir2::Right, 30), Insert(Dir2::Dir2::Right, 25), Insert(Dir2::Dir2::Right, 38), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 24), Insert(Dir2::Dir2::Left, 45), Replace(Dir2::Dir2::Left, 31), Replace(Dir2::Dir2::Right, 43), Replace(Dir2::Dir2::Right, 2), Replace(Dir2::Dir2::Right, 47), Insert(Dir2::Dir2::Right, 46), Insert(Dir2::Dir2::Left, 27), Goto(Dir2::Dir2::Right), Remove(Dir2::Dir2::Right), Goto(Dir2::Dir2::Left), Insert(Dir2::Dir2::Left, 44), Goto(Dir2::Dir2::Left), Insert(Dir2::Dir2::Right, 27), Goto(Dir2::Dir2::Left)]', tests/listedit.rs:52

// ---- ensure_consistency stdout ----
//     after edit 41: Goto(Dir2::Dir2::Right): expected Max to be [49], but found [44]
//     thread 'ensure_consistency' panicked at '[Replace(Dir2::Dir2::Right, 45), Insert(Dir2::Dir2::Right, 0), Insert(Dir2::Dir2::Left, 12), Insert(Dir2::Dir2::Right, 22), Goto(Dir2::Dir2::Right), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 16), Replace(Dir2::Dir2::Right, 7), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 16), Insert(Dir2::Dir2::Right, 27), Insert(Dir2::Dir2::Left, 0), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 36), Goto(Dir2::Dir2::Right), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 11), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Right, 32), Remove(Dir2::Dir2::Left), Insert(Dir2::Dir2::Right, 14), Remove(Dir2::Dir2::Right), Goto(Dir2::Dir2::Right), Replace(Dir2::Dir2::Right, 23), Insert(Dir2::Dir2::Left, 34), Replace(Dir2::Dir2::Right, 49), Insert(Dir2::Dir2::Left, 0), Insert(Dir2::Dir2::Left, 16), Remove(Dir2::Dir2::Left), Insert(Dir2::Dir2::Left, 4), Goto(Dir2::Dir2::Right), Replace(Dir2::Dir2::Right, 23), Insert(Dir2::Dir2::Left, 44), Goto(Dir2::Dir2::Left), Insert(Dir2::Dir2::Left, 23), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 18), Insert(Dir2::Dir2::Right, 15), Replace(Dir2::Dir2::Left, 49), Insert(Dir2::Dir2::Right, 16), Goto(Dir2::Dir2::Left), Goto(Dir2::Dir2::Right), Insert(Dir2::Dir2::Left, 46), Goto(Dir2::Dir2::Left)]', tests/listedit.rs:52

