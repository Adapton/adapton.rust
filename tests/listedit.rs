#![feature(test)]

//
// cargo test listedit::experiments -- --nocapture
//

extern crate adapton ;
extern crate test;

use adapton::adapton_sigs::* ;
use adapton::collection::* ;
use adapton::engine::* ;

#[test]
pub fn experiments () {
    let mut st = Engine::new();
    let edits : Vec<BasicEditCmd<u32,_>> =
        vec![BasicEditCmd::Insert(ListEditDir::Left, 0),
             BasicEditCmd::Insert(ListEditDir::Left, 1),
             BasicEditCmd::Insert(ListEditDir::Left, 2),
             BasicEditCmd::Insert(ListEditDir::Left, 3),
             BasicEditCmd::Insert(ListEditDir::Left, 4),
             BasicEditCmd::Insert(ListEditDir::Left, 5),
             BasicEditCmd::Insert(ListEditDir::Left, 6),
             BasicEditCmd::Insert(ListEditDir::Left, 7),
             BasicEditCmd::Insert(ListEditDir::Left, 8),
             BasicEditCmd::Insert(ListEditDir::Left, 9),
             BasicEditCmd::Insert(ListEditDir::Left, 10),
             BasicEditCmd::Insert(ListEditDir::Left, 11),
             BasicEditCmd::Insert(ListEditDir::Left, 12),
             BasicEditCmd::Insert(ListEditDir::Left, 13),
             BasicEditCmd::Insert(ListEditDir::Left, 14),
             BasicEditCmd::Insert(ListEditDir::Left, 15),
             ];
    let trace = Experiment::run(&mut st, edits, ListReduce::Max);
    ()
}
