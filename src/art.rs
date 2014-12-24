use lazy::single::*;
use name::*;

//#[deriving(Show)]
pub struct Art<T> {
    name : Name,
    thunk : Thunk<T>
}
