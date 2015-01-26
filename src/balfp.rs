use std::hash;
use std::hash::Hash;
use std::int::MAX;
use std::num::Int;
use std::result::Result;

use art::*;

// The idea of a "balanced fixed-point" computation is to do two
// things simultaneously:
//
// (1) compute the fixed-point value of a function by running an
// (abstracted) iterative algorithm.
//
// (2) structure the evaluation of this algorithm as a balanced binary
// tree of Adapton thunks, where each thunk in the tree stores an
// intermediate state of the algorithm.
//
// The second goal is motivated by Adapton's need for a shallow
// call-graph.  If one were to write a loop naively, the call-graph
// may not be shallow.  By using this fixed-point combinator in place
// of a while loop (or some tail-recursive loop), we can guarantee
// (probabilistically) that the call-graph is shallow.

trait FpFn {
    // Fixed-point Function.
    //
    /// The Self type need not be stateful at all.
    ///
    /// Instead, a FpFn exposes its state (abstractly) to the loop
    /// that contains it.  There are two parts:
    //
    ///  - The loop iteration's current state (type `St`)
    //
    ///  - Any "meta-state" needed to explore the space of states
    ///  (type `Sys`).  This meta state may consist of nothing, or it
    ///  may consist of queues, stacks, and/or sets of
    ///  previously-visited states.
    //
    /// Our fixed-point loop below needs a way of hashing the "current
    /// state" (type `St`).  It builds a binary tree of stepped states
    /// (type `St_stepped`).

    type St : Hash ;
    type St_stepped ;
    type Sys ;
    fn step (self:& Self, st:Self::St, sys:Self::Sys) -> (Self::St_stepped, Self::Sys) ;
    fn next (self:& Self, sys:Self::Sys) -> Result<(Self::St, Self::Sys), Self::Sys> ;
}

trait BinTree<T> {
    // Our fixed-point loop builds a balanced binary tree of stepped states.
    type Tree;
    fn leaf(self:& Self) -> Self::Tree;
    fn bin(self:& Self, left:Self::Tree, T, right:Self::Tree) -> Self::Tree;
}

trait FpLp<F:FpFn, BT:BinTree<F::St_stepped>> {

    // We leave this abstract here.
    // It can either be a simple wrapper around work_tree, or
    // something that uses memoization.
    fn work_recur (self:&Self, f:&F, bt:&BT,
                  left:BT::Tree, left_lev:int,
                  sys:F::Sys, parent_lev:int) -> (F::Sys, BT::Tree) ;

    // Our fixed-point loop.  It uses a hashing and probabilistic
    // reasoning to build a balanced binary tree of stepped states.
    //
    // Key assumption: Each state (of type `F::St`) has a distinct hash.
    fn work_tree (self:&Self, f:&F, bt:&BT,
                  left:BT::Tree, left_lev:int,
                  sys:F::Sys, parent_lev:int) -> (F::Sys, BT::Tree)
    {
        let nxt = f.next(sys) ;
        match nxt {
            Err(sys) => (sys, left),
            Ok((st,sys)) => {
                let st_lev = Int::trailing_zeros(hash::hash(&st)) as int ;
                if left_lev <= st_lev &&
                    st_lev  <= parent_lev
                {
                    let (st, sys) = f.step(st, sys) ;
                    let (sys,right) = self.work_recur (f, bt, bt.leaf(), -1, sys, st_lev );
                    let tree = bt.bin(left,st,right) ;
                    let (sys,tree) = self.work_recur (f, bt, tree, st_lev, sys, parent_lev );
                    (sys,tree)
                }
                else {
                    (sys,left)
                }
            }
        }
    }
    fn compute (self:&Self, f:&F, bt:&BT, sys:F::Sys) -> (F::Sys,BT::Tree) {
        self.work_recur (f, bt, bt.leaf(), -1, sys, MAX )
    }
}

struct FpDirect ;
impl<F:FpFn, BT:BinTree<F::St_stepped>> FpLp<F,BT> for FpDirect {
    fn work_recur (self:&Self, f:&F, bt:&BT,
                   left:BT::Tree, left_lev:int,
                   sys:F::Sys, parent_lev:int) -> (F::Sys, BT::Tree) {
        self.work_tree (f, bt, left, left_lev, sys, parent_lev )
    }
}

struct FpArt ;
impl<F:FpFn, BT:BinTree<F::St_stepped>> FpLp<F,BT> for FpArt {
    fn work_recur (self:&Self, f:&F, bt:&BT,
                   left:BT::Tree, left_lev:int,
                   sys:F::Sys, parent_lev:int) -> (F::Sys, BT::Tree) {
        let n = panic!("");
        let a = nart!(n, self.work_tree (f, bt, left, left_lev, sys, parent_lev));
        force( a )
    }
}

