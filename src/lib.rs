// Adapton for Rust

#![feature(associated_consts)]
#![feature(box_patterns)]
#![feature(box_syntax)]

#![crate_name = "adapton"]
#![crate_type = "lib"]
    
extern crate core;

#[macro_use]
pub mod macros ;
pub mod engine ;
pub mod catalog ;
pub mod parse_val;

mod reflect;


mod adapton {
    pub use super::*;
}
