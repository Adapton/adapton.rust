use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use std::marker::PhantomData;

use macros::* ;
use adapton::engine::* ;

// -------------------------------------------------------------------------
// Experimental API stuff below:

pub fn thunk_codata<Arg:Hash+Eq+Debug+Clone,Res:Hash+Eq+Debug+Clone>
  (_prog_pt:ProgPt,
   _fn_box:Rc<Box< Fn(Arg, bool, &(Fn(Arg) -> Res)) -> Res >>,
   _arg:Arg)
   -> Art<Res>
{
  panic!("")
}

pub struct Trip {
  _todo:(),
}

pub struct Set<T> {
  phantom:PhantomData<T>,
}

pub fn thunk_codata2<Arg:Hash+Eq+Debug+Clone,Res:Hash+Eq+Debug+Clone>
  (_prog_pt:ProgPt,
   _fn_box:Rc<Box< Fn(Arg, bool, Trip, &(Fn(Arg,Trip) -> (Res,Trip))) -> (Res,Trip) >>,
   _arg:Arg)
   -> Art<Res>
{
  fn _rec<Arg,Trip,Res>(_arg:Arg,_trip:Trip) -> (Res,Trip) {
    // extend trip with arg (for later), return extended trip;
    // call fn_box with visited=false; return the result
    panic!("")
  };
  fn _visit<Arg>(_visited:Set<Arg>,_frontier:Set<Arg>) -> (Set<Arg>,Set<Arg>) {
    //let (node:Arg, frontier:Set<Arg>) = frontier.choose();
    panic!("");
  }
  panic!("")
}
