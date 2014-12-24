use lazy::single::*;

//#[deriving(Show)]
pub struct Art<T> {
    thunk : Thunk<T>
}
