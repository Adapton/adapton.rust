#![feature(unsafe_destructor, macro_rules, phase, globs, default_type_params)]

//! Adapton for Rust

/// A Thunk safe for single-threaded access.
extern crate lazy;

pub mod name;

mod adapton {
    pub use super::*;
}
