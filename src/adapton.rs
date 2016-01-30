// Adapton for Rust
#![feature(associated_type_defaults)]
#![feature(custom_derive)]
#![feature(zero_one)]
#![feature(type_ascription)]
    
#![cfg_attr(test, feature(plugin))]
// #![cfg_attr(test, plugin(quickcheck_macros))]

#![crate_name = "adapton"]
#![crate_type = "lib"]
    
// extern crate quickcheck;
extern crate rand;

#[macro_use]
extern crate log;

#[macro_use]
pub mod macros ;
pub mod adapton_sigs ;
pub mod engine ;
pub mod naive ;

pub mod simple ;
pub mod collection_traits ;
pub mod collection_algo ;
pub mod collection ;
pub mod collection_edit ;


// pub mod fact ;

mod adapton {
    pub use super::*;
}

