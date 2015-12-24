#![feature(test)]
#![feature(plugin)]
#![plugin(quickcheck_macros)]

//
// cargo test listedit::experiments -- --nocapture
//

extern crate adapton ;
extern crate test;
extern crate quickcheck;
extern crate rand;

use adapton::adapton_sigs::* ;
use adapton::collection::*;
use adapton::engine;
use adapton::naive;
   
type Edits = Vec<CursorEdit<u32, Dir2>>;

fn compare_naive_and_cached(edits: &Edits) -> bool {
    let reduction = ListReduce::Max;
    let mut n_st = naive::AdaptonFromScratch::new();
    let mut e_st = engine::Engine::new();
    
    let results_1 = Experiment::run(&mut n_st, edits.clone(), reduction.clone());
    let results_2 = Experiment::run(&mut e_st, edits.clone(), reduction.clone());
    
    let mut idx = 0;
    for (a, b) in results_1.iter().zip(results_2.iter()) {
        if a.0 != b.0 {
            println!("after edit {}: {:?}: expected {:?} to be {:?}, but found {:?}",
                     idx, edits[idx], &reduction, a.0, b.0);
            return false;
        }
        idx += 1;
    }
    
    true
}

#[test]
fn ensure_consistency_randomly() {
    let rng = rand::thread_rng();
    let mut gen = quickcheck::StdGen::new(rng, 100);
    for _ in 0..100 {
        let testv = <Edits as quickcheck::Arbitrary>::arbitrary(&mut gen);        
        if !compare_naive_and_cached(&testv) {
            panic!("{:?}", testv);
        }
    }
}

#[test]
fn ensure_consistency_regression_testcase1() { assert!( compare_naive_and_cached(&testcase1())) }

#[test]
fn ensure_consistency_regression_testcase2() { assert!( compare_naive_and_cached(&testcase2())) }

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
