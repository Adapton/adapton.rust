#!/bin/sh
cargo build --verbose
cargo test --verbose -- --nocapture
cargo bench --verbose

