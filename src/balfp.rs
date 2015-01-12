use std::hash;
use std::hash::Hash;
use std::int::MAX;
use std::num::Int;

/// Our fixed-point loop needs a way of hashing the "current state" (type `St).
/// The details of what states are and how they relate need not be revealed.
/// We hide all of these details in a "state system" (type `Sys`).
/// `next` checks for unstepped states to become the "current state".
/// `step` updates the system at the "current state".
trait Step {
    type St : Hash ;
    type Sys ;
    pub fn step (st:Self::St, sys:Self::Sys) -> Self::Sys ;
    pub fn next (sys:Self::Sys) -> Option<Self::St> ;
}

trait Fp<S:Step> {
    pub fn work_tree (left_lev:int, sys:S::Sys, parent_lev:int) -> S::Sys ;
    pub fn fp ( st:S::Sys ) -> S::Sys ;
}

struct FpS;

impl<S:Step> Fp<S> for FpS {
    
    fn work_tree (left_lev:int, sys:S::Sys, parent_lev:int) -> S::Sys {
        let nxt = Step::next(sys) ;
        match nxt {
            None => sys,
            Some(st) => {
                let st_lev = Int::trailing_zeros(hash::hash(&st)) as int ;
                if ( left_lev <= st_lev &&
                     st_lev   <= parent_lev )
                {
                    let sys = Step::step(st, sys) ;
                    let sys = Fp::work_tree ( -1,     sys, st_lev );
                    let sys = Fp::work_tree ( st_lev, sys, parent_lev );
                    sys
                }
                else {
                    sys
                }
            }
        }
    }
    
    fn fp (sys:S::Sys) -> S::Sys {
        Fp::work_tree (-1, sys, MAX );
    }
}
