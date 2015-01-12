use std::hash;
use std::hash::Hash;

trait FpStep {
    type St : Hash ;
    type Sys ;
    fn step (st:Self::St, sys:Self::Sys) -> Self::Sys ;
    fn next (sys:Self::Sys) -> Option<Self::St> ;
}

trait Fp<S:FpStep> {
    fn fp ( st:S::St ) -> S::St ;
}


impl<S:FpStep> Fp<S> for () {
    fn fp (st:S::St) -> S::St {
        panic!("!")
    }
}
