//! Intent Core - Zero unwrap, idiomatic Rust
//!
//! Core functionality for Intent CLI using Railway-Oriented Programming
//! and functional patterns.
//!
//! # Examples
//!
//! Using `IntentResult` type alias:
//! ```rust
//! use intent_core::IntentResult;
//!
//! fn example() -> IntentResult<i32> {
//!     Ok(42)
//! }
//! ```

pub mod cli;
pub mod config;
pub mod cue;
pub mod error;
pub mod http;
pub mod http_body_encoding;
pub mod http_client;
pub mod prelude;
pub mod runner;
pub mod types;

// Re-export common types for convenience
pub use error::{IntentError, IntentResult};
