// Traits can have associated types:
#![feature(collections)]

use std::fmt::Debug;

// Help from @ogeon:
//   http://users.rust-lang.org/t/trait-objects-with-associated-types/746/19

trait T : Debug {
    type t;
    fn get (self:&Self) -> Self::t ;
    fn doit (self:&Self, Self::t ) -> String ;
}

trait Tabs : Debug {
    fn doit_abs (self:&Self) -> String ;
}

impl <A, B: T<t=A>> Tabs for B {
    fn doit_abs (self:&Self) -> String {
        self.doit ( self.get () )
    }
}

#[derive(Debug)]
pub enum Foo { Foo }

#[derive(Debug)]
pub enum Bar { Bar }

impl T for Foo {
    type t = String;
    fn get (self:&Self) -> String { String::from_str("foo") }
    fn doit (self:&Self, s:Self::t) -> String { s }
}

impl T for Bar {
    type t = u32;
    fn get (self:&Self) -> u32 { 0 }
    fn doit (self:&Self, _s:Self::t) -> String { String::from_str("bar") }
}

#[derive(Debug)]
struct Packed {
    two : (Box<Tabs>,Box<Tabs>)
}

pub fn main () {
    let a = Packed { two : (Box::new(Foo::Foo),
                            Box::new(Bar::Bar)) } ;
     println!("{:?}", a);
}
