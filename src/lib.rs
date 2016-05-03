// Adapton for Rust

#![feature(zero_one)]
//#![feature(associated_type_defaults)]
//#![feature(custom_derive)]
//#![feature(type_ascription)]
//#![cfg_attr(test, feature(plugin))]
//#![cfg_attr(test, plugin(quickcheck_macros))]

#![crate_name = "adapton"]
#![crate_type = "lib"]
    
// extern crate quickcheck;
// extern crate rand;

// extern crate time;

// #[macro_use]
// extern crate log;

#[macro_use]
pub mod macros ;
pub mod engine ;
pub mod collections ;
pub mod arith_parse ;

mod adapton {
    pub use super::*;
}

