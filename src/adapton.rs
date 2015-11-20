// Adapton for Rust
#![feature(associated_type_defaults)]

#![crate_name = "adapton"]
#![crate_type = "lib"]

#[macro_use]
pub mod adapton_syntax ;
pub mod adapton_sigs ;
pub mod engine ;
pub mod adapton_fromscratch ;

pub mod simple ;
pub mod structures ;

// pub mod fact ;

mod adapton {
    pub use super::*;
}
