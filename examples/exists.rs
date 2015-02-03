// Filed issue:
// https://github.com/rust-lang/rust/issues/21909

trait A<X> { }
trait B { type X; type Y : A<Self::X>; }
fn main () { }
