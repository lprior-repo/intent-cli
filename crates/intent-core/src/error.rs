//! Error handling module using thiserror

use thiserror::Error;

/// Core error type for Intent CLI
#[derive(Error, Debug)]
pub enum IntentError {
    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// Serialization error
    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    /// Configuration error
    #[error("Configuration error: {0}")]
    Config(String),

    /// Not found error
    #[error("{0} not found: {1}")]
    NotFound(String, String),
}

/// Result type alias for Intent operations
pub type IntentResult<T> = Result<T, IntentError>;
