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
pub fn engine () {
    let mut st = Engine::new();
    let edits : Vec<CursorEdit<u32,_>> =
        vec![CursorEdit::Insert(Dir2::Left, 0),
             CursorEdit::Insert(Dir2::Left, 1),
             CursorEdit::Insert(Dir2::Left, 2),
             CursorEdit::Insert(Dir2::Left, 3),
             CursorEdit::Insert(Dir2::Left, 4),
             CursorEdit::Insert(Dir2::Left, 5),
             CursorEdit::Insert(Dir2::Left, 6),
             CursorEdit::Insert(Dir2::Left, 7),
             CursorEdit::Insert(Dir2::Left, 8),
             CursorEdit::Insert(Dir2::Left, 9),
             CursorEdit::Insert(Dir2::Left, 10),
             CursorEdit::Insert(Dir2::Left, 11),
             CursorEdit::Insert(Dir2::Left, 12),
             CursorEdit::Insert(Dir2::Left, 13),
             CursorEdit::Insert(Dir2::Left, 14),
             CursorEdit::Insert(Dir2::Left, 15),             
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Remove(Dir2::Left),
             CursorEdit::Insert(Dir2::Left, 0), CursorEdit::Goto(Dir2::Left),
             CursorEdit::Insert(Dir2::Right, 1),
             CursorEdit::Insert(Dir2::Right, 2),
             CursorEdit::Insert(Dir2::Left, 3), CursorEdit::Goto(Dir2::Left),
             CursorEdit::Insert(Dir2::Left, 4), 
             CursorEdit::Insert(Dir2::Left, 5), 
             CursorEdit::Insert(Dir2::Right, 6),
             CursorEdit::Insert(Dir2::Right, 7),
             CursorEdit::Insert(Dir2::Left, 8), CursorEdit::Goto(Dir2::Left),
             CursorEdit::Insert(Dir2::Left, 9),
             CursorEdit::Insert(Dir2::Right, 10),
             CursorEdit::Insert(Dir2::Right, 11),
             CursorEdit::Insert(Dir2::Right, 12), CursorEdit::Goto(Dir2::Left),
             CursorEdit::Insert(Dir2::Left, 13),
             CursorEdit::Insert(Dir2::Left, 14),
             CursorEdit::Insert(Dir2::Left, 15), CursorEdit::Goto(Dir2::Left),
             CursorEdit::Remove(Dir2::Right)
             ];
    let trace = Experiment::run(&mut st, edits, ListReduce::Max);
    ()
}
