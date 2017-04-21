#[macro_use]
extern crate adapton;

mod engine {
    #[test] 
    fn force_cell () {
        use adapton::engine::*;
        manage::init_dcg();   
        let a : u32      = 1234;
        let b : Art<u32> = cell(name_of_usize(0), a);
        let c : u32      = force(&b);    
        assert_eq!(a, c);
    }

    #[test] 
    fn force_map_cell () {
        use adapton::engine::*;
        manage::init_dcg();    
        let a : u32      = 1234;
        let b : Art<u32> = cell(name_of_usize(0), a);
        let c : u64      = force_map(&b, |_,x| x as u64);    
        assert_eq!(a as u64, c);
    }

    #[test] 
    fn force_map_cell_project () {
        use adapton::engine::*;
        manage::init_dcg();    
        let pair = (1234 as usize, 5678 as usize);
        let c    = cell(name_of_usize(0), pair);
        let fst  = force_map(&c, |_,x| x.0);
        let snd  = force_map(&c, |_,x| x.1);
        assert_eq!(pair.0, fst);
        assert_eq!(pair.1, snd);
    }

    #[test] 
    fn force_map_prunes_dirty_traversal () {
        // Test whether using force_map correctly prunes dirtying;
        // this test traces the engine, counts the number of dirtying
        // steps, and ensures that this count is zero, as expected.
        use std::rc::Rc;
        use adapton::macros::*;
        use adapton::engine::*;
        manage::init_dcg();    
        reflect::dcg_reflect_begin();
        let pair = (1234 as usize, 5678 as usize);
        let c    = cell(name_of_usize(0), pair);
        let t = thunk![ name_of_usize(1) =>> {    
            let fst = force_map(&c, |_,x| x.0);
            fst + 100
        }];
        assert_eq!(force(&t), 1334);
        let pair = (1234 as usize, 8765 as usize);
        let _    = cell(name_of_usize(0), pair);      
        assert_eq!(force(&t), 1334);        
        let traces = reflect::dcg_reflect_end();
        let counts = reflect::trace::trace_count(&traces, Some(1));
        assert_eq!(counts.dirty.0, 0);
        assert_eq!(counts.dirty.1, 0);
    }

    #[test] 
    fn force_map_thunk () {
        use std::rc::Rc;
        use adapton::macros::*;
        use adapton::engine::*;
        manage::init_dcg();    
        let a : u32      = 1234;
        let b : Art<u32> = thunk![ name_of_usize(0) =>> a];
        let c : u64      = force_map(&b, |_,x| x as u64);    
        assert_eq!(a as u64, c);
    }
}
