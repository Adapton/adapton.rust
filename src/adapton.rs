// Adapton for Rust

#![crate_name = "adapton"]
#![crate_type = "lib"]

#[macro_use]
pub mod adapton_syntax ;
pub mod adapton_sigs ;
pub mod adapton_state ;

pub mod structures ;

// pub mod fact ;

mod adapton {
    pub use super::*;
}
