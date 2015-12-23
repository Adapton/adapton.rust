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
use adapton::collection::* ;
use adapton::engine;
use adapton::naive;


type Edits = Vec<CursorEdit<u32, Dir2>>;

fn compare_naive_and_cached(edits: &Edits) -> bool {
      let mut n_st = naive::AdaptonFromScratch::new();
      let mut e_st = engine::Engine::new();

      let results_1 = Experiment::run(&mut n_st, edits.clone(), ListReduce::Max);
      let results_2 = Experiment::run(&mut e_st, edits.clone(), ListReduce::Max);
      
      for (a, b) in results_1.iter().zip(results_2.iter()) {
            if a.0 != b.0 {
                  return false;
            }
      }

      true
}

#[test]
fn ensure_consistency() {
      let rng = rand::thread_rng();
      let mut gen = quickcheck::StdGen::new(rng, 100);

      for _ in 0..100 {
            let testv = <Edits as quickcheck::Arbitrary>::arbitrary(&mut gen);

            if !compare_naive_and_cached(&testv) {
                  panic!("{:?}", testv);
            }
      }
}
