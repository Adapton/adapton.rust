#[macro_use]
extern crate adapton;

mod engine_is_typesafe {


    #[test]
    #[should_panic]
    fn engine_dynamic_type_error_as_editor () {
        //use adapton::macros::*;
        use adapton::engine::*;
        manage::init_dcg();
        let n = name_of_str("cell");
        let _x : Art<usize> = cell(n.clone(), 1);
        let _y : Art<(usize,usize)> = cell(n, (2,3));
    }

    #[test]
    #[should_panic]
    fn engine_dynamic_type_error_as_archivist () {
        use adapton::macros::*;
        use adapton::engine::*;
        manage::init_dcg();
        let t = thunk![{
            let t1 = thunk![ name_of_str("t1") =>> { 
                let n = name_of_str("cell");
                let _x : Art<usize> = cell(n, 1);
            }];
            let t2 = thunk![ name_of_str("t2") =>> {
                let n = name_of_str("cell");
                let _y : Art<(usize,usize)> = cell(n, (2,3));
            }];
            get!(t1);
            get!(t2);
        }];
        let _ = get!(t);
    }
}

mod engine_is_from_scratch_consistent {
    //! This module tests that change propagation is from-scratch consistent

    /// Compare to this sound behavior to the unsound behavior from
    /// JaneStreet 'Incremental' libary.  In particular, Yit's gist
    /// shows the problematic change propagation behavior for a
    /// similar example program (dividing by zero when it should not):
    /// https://gist.github.com/khooyp/98abc0e64dc296deaa48

    #[test]
    fn avoid_divide_by_zero () {
        use adapton::macros::*;
        use adapton::engine::*;
        manage::init_dcg();

        // Construct two mutable inputs, `num` and `den`, a
        // computation `div` that divides the numerator in `num` by
        // the denominator in `den`, and a thunk `check` that first
        // checks whether the denominator is zero (returning None if
        // so) and if non-zero, returns the value of the division.
       
        let num  = cell!(42);
        let den  = cell!(2);
        
        // In Rust, cloning is explicit:
        let den2 = den.clone(); // here, we clone the _global reference_ to the cell.
        let den3 = den.clone(); // here, we clone the _global reference_ to the cell, again.

        let div   = thunk![ get!(num) / get!(den) ];
        let check = thunk![ if get!(den2) == 0 { None } else { Some(get!(div)) } ];
        
        assert_eq!(get!(check), Some(21));
        set(&den3, 0); assert_eq!(get!(check), None);
        set(&den3, 2); assert_eq!(get!(check), Some(21));
    }

    #[test]
    fn avoid_expensive_subcomp () {
        use adapton::macros::*;
        use adapton::engine::*;
        use std::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;

        manage::init_dcg();
      
        let flag = cell!(true); // flag: whether or not to do "iterative hash"
        let iter = cell!(3);    // iter: iterations for hashing
        let inp  = cell!(42);   // inp1: input value to hash        

        let inp1  = inp.clone();  // we use inp non-linearly below
        let inp2  = inp.clone();  // we use inp non-linearly below
        let flag1 = flag.clone(); // we use flag non-linearly below
        let iter1 = iter.clone(); // we use iter non-linearly below

        let iter_hash = thunk![{
            let inp_val = get!(inp); // value to hash iteratively
            let mut hash_state = DefaultHasher::new();
            for _ in 0..get!(iter) { // hash iteratively, `iter` number of times
                inp_val.hash(&mut hash_state);
            };
            assert!(get!(iter) < 100); // For Testing: Too many iters is an error!
            hash_state.finish() // returns final hash value
        }];
        let root = thunk![ // switches between hashing iteratively, or not
            if get!(flag) { get!(iter_hash) }
            else { get!(inp1) }
        ];

        assert!(get!(root) != get!(inp2));
        set(&flag1, false);
        set(&iter1, 999999999);
        assert!(get!(root) == get!(inp2));
    }
}

mod engine_api {
    //! This module tests gives unit tests for the engine's core API

    #[test] 
    fn force_cell () {
        use adapton::macros::*;
        use adapton::engine::*;
        manage::init_dcg();   
        let a : u32      = 1234;
        let b : Art<u32> = cell!(a);
        let c : u32      = force(&b);    
        assert_eq!(a, c);
    }

    #[test] 
    fn force_map_cell () {
        use adapton::macros::*;
        use adapton::engine::*;
        manage::init_dcg();    
        let a : u32      = 1234;
        let b : Art<u32> = cell!(a);
        let c : u64      = force_map(&b, |_,x| x as u64);    
        assert_eq!(a as u64, c);
    }

    #[test] 
    fn force_map_cell_project () {
        use adapton::macros::*;
        use adapton::engine::*;
        manage::init_dcg();    
        let pair = (1234 as usize, 5678 as usize);
        let c    = cell!(pair);
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
        use adapton::macros::*;
        use adapton::engine::*;
        use adapton::reflect;
        manage::init_dcg();    
        reflect::dcg_reflect_begin();
        let c : Art<(usize,usize)> = cell(name_of_str("pair"), (1234, 5678));
        let t : Art<usize> = thunk![{    
            let fst = force_map(&c, |_,x| x.0);
            fst + 100
        }];
        assert_eq!(force(&t), 1334);
        let _ : Art<(usize,usize)> = cell(name_of_str("pair"), (1234, 8765));
        assert_eq!(force(&t), 1334);        
        let traces = reflect::dcg_reflect_end();
        let counts = reflect::trace::trace_count(&traces, Some(1));
        assert_eq!(counts.dirty.0, 0);
        assert_eq!(counts.dirty.1, 0);
    }

    #[test] 
    fn force_map_thunk () {
        use adapton::macros::*;
        use adapton::engine::*;
        manage::init_dcg();    
        let a : u32      = 1234;
        let b : Art<u32> = thunk![a];
        let c : u64      = force_map(&b, |_,x| x as u64);    
        assert_eq!(a as u64, c);
    }
}
