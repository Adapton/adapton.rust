// Traits can have associated types:

// I thought that associated types could be used to encode existential
// types, as attempted below.  The trait T has associated type t.  I
// define two implementations for this trait, with different
// definitions of t.  Then, I try to create a packed up pair where the
// first and second components of the pair look "the same" (both just
// mention trait T, and each leaves t abstract, via an (implicit)
// existential quantifier.

// Apparently, this is not allowed:

// examples/assoctypes.rs:24:16: 24:17 error: the value of the associated type `t` (from the trait `T`) must be specified [E0191]
//    examples/assoctypes.rs:24     two : (Box<T>,Box<T>)
//
// examples/assoctypes.rs:24:23: 24:24 error: the value of the associated type `t` (from the trait `T`) must be specified [E0191]
//    examples/assoctypes.rs:24     two : (Box<T>,Box<T>)
    
trait T {
    type t;
    fn get (self:&Self) -> Self::t ;
    fn foo ( Self::t ) -> String ;
}

pub enum Foo { Foo }
pub enum Bar { Bar }

impl T for Foo {
    type t = String;
    fn get (self) { "foo" }
    fn foo (s:Self::t) { s }
}

impl T for Bar {
    type t = u32;
    fn get (self) { 0 }
    fn foo (s:Self::t) { "bar" }
}

struct Packed {
    two : (Box<T>,Box<T>)
}

pub fn main () {
    let a = Packed { two : (Box::new(Foo::Foo),
                            Box::new(Bar::Bar)) } ;
    ()
}
