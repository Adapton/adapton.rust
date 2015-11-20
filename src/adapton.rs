// Adapton for Rust
#![feature(associated_type_defaults)]

#![crate_name = "adapton"]
#![crate_type = "lib"]

#[macro_use]
pub mod macros ;
pub mod adapton_sigs ;
pub mod engine ;
pub mod naive ;

pub mod simple ;
pub mod collection ;

// pub mod fact ;

mod adapton {
    pub use super::*;
}
