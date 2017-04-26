// Adapton for Rust

// #![feature(field_init_shorthand)]
// #![feature(rustc_private)]

//#![feature(zero_one)]
#![feature(associated_consts)]
#![feature(box_patterns)]
#![feature(box_syntax)]

//#![feature(associated_type_defaults)]
//#![feature(custom_derive)]
//#![feature(type_ascription)]
//#![cfg_attr(test, feature(plugin))]
//#![cfg_attr(test, plugin(quickcheck_macros))]

#![crate_name = "adapton"]
#![crate_type = "lib"]
    
// extern crate quickcheck;
// extern crate rand;
// extern crate serialize;
// extern crate time;

// #[macro_use]
// extern crate log;

extern crate core;

#[macro_use]
pub mod macros ;
pub mod engine ;
pub mod collections ;

// various sub-modules of the public modules above:
mod bitstring ;
mod trie ;
mod reflect ;

mod adapton {
    pub use super::*;
}
