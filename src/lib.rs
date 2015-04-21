// #![feature(unsafe_destructor, macro_rules, phase, globs, default_type_params)]
#![feature(unboxed_closures)]
#![feature(box_syntax)]
#![feature(box_patterns)]
// #![feature(old_orphan_check)]

//! Adapton for Rust

pub mod adapton_sigs ;
pub mod adapton_impl ;

mod adapton {
    pub use super::*;
}
