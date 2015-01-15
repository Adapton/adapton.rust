use std::hash;
use std::hash::Hash;
use std::int::MAX;
use std::num::Int;
use std::result::Result;

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
    ///  (type `Sys`)
    //
    /// Our fixed-point loop needs a way of hashing the "current
    /// state" (type `St`).  The details of what states are and how
    /// they relate need not be revealed.  We hide all of these
    /// details in a "state system" (type `Sys`).  `next` checks for
    /// unstepped states to become the "current state", or returns Err
    /// if no unexplored states exist.  `step` updates the system at
    /// the "current state".
    
    type St : Hash ;
    type Sys ;
    fn step (self:& Self, st:Self::St, sys:Self::Sys) -> Self::Sys ;
    fn next (self:& Self, sys:Self::Sys) -> Result<(Self::St, Self::Sys), Self::Sys> ;
}

trait BuildTree<T> {
    fn leaf(data:&T) -> Self;
    fn bin(left:Self,data:&T,right:Self) -> Self;
}

trait FpLp<F:FpFn, BT:BuildTree<F::St>> {
    fn work_tree (self:&Self, f:&F, left_lev:int, sys:F::Sys, parent_lev:int) -> F::Sys {
        let nxt = f.next(sys) ;
        match nxt {
            Err(sys) => sys,
            Ok((st,sys)) => {
                let st_lev = Int::trailing_zeros(hash::hash(&st)) as int ;
                if ( left_lev <= st_lev &&
                     st_lev   <= parent_lev )
                {
                    let sys : F::Sys = f.step(st, sys) ;
                    let sys : F::Sys = self.work_tree (f, -1,     sys, st_lev );
                    let sys : F::Sys = self.work_tree (f, st_lev, sys, parent_lev );
                    sys
                }
                else {
                    sys
                }
            }
        }    
    }    
    fn compute (self:&Self, f:&F, sys:F::Sys) -> F::Sys {
        self.work_tree (f, -1, sys, MAX )
    }
}
