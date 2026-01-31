//! Prelude module - common imports for Intent CLI
//!
//! Import this module to get all common types and traits:
//! ```rust
//! use intent_core::prelude::*;
//! ```

// Re-export functional utilities
pub use itertools::Itertools;
pub use tap::{Pipe, Tap};

pub use crate::error::{IntentError, IntentResult};
