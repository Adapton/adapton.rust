#![feature(test)]
#[macro_use]
extern crate adapton ;

pub mod fact ;

mod adapton_test {
    pub use super::*;
}
