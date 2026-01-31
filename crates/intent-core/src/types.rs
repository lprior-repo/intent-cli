//! Domain types for Intent CLI
//!
//! All types use newtype pattern for type safety and validation.
//! No public constructors - use try_new() for validated construction.
//!
//! # Philosophy
//!
//! - **Type-driven design**: Make invalid states unrepresentable
//! - **Validated construction**: All types validate their invariants on creation
//! - **Zero panics**: Use `Result` for fallible construction
//! - **Newtype pattern**: Wrap primitives for type safety

use crate::error::IntentError;
use std::fmt;
use std::str::FromStr;

// =============================================================================
// HTTP Types
// =============================================================================

/// HTTP request method
///
/// Standard HTTP methods used in API testing.
/// Supports case-insensitive parsing from strings.
///
/// # Examples
///
/// ```
/// use intent_core::types::HttpMethod;
/// use std::str::FromStr;
///
/// let method = HttpMethod::from_str("GET").unwrap();
/// assert_eq!(method, HttpMethod::Get);
/// assert_eq!(method.to_string(), "GET");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HttpMethod {
    Get,
    Post,
    Put,
    Patch,
    Delete,
    Head,
    Options,
}

impl fmt::Display for HttpMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Get => write!(f, "GET"),
            Self::Post => write!(f, "POST"),
            Self::Put => write!(f, "PUT"),
            Self::Patch => write!(f, "PATCH"),
            Self::Delete => write!(f, "DELETE"),
            Self::Head => write!(f, "HEAD"),
            Self::Options => write!(f, "OPTIONS"),
        }
    }
}

impl FromStr for HttpMethod {
    type Err = IntentError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "GET" => Ok(Self::Get),
            "POST" => Ok(Self::Post),
            "PUT" => Ok(Self::Put),
            "PATCH" => Ok(Self::Patch),
            "DELETE" => Ok(Self::Delete),
            "HEAD" => Ok(Self::Head),
            "OPTIONS" => Ok(Self::Options),
            _ => Err(IntentError::validation(
                "http_method",
                format!("Invalid HTTP method: '{}'. Must be GET, POST, PUT, PATCH, DELETE, HEAD, or OPTIONS", s),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn types_module_compiles() {
        // Smoke test - module exists and compiles
    }

    // =========================================================================
    // HttpMethod Tests
    // =========================================================================

    #[test]
    fn http_method_get_display() {
        assert_eq!(HttpMethod::Get.to_string(), "GET");
    }

    #[test]
    fn http_method_post_display() {
        assert_eq!(HttpMethod::Post.to_string(), "POST");
    }

    #[test]
    fn http_method_all_variants_display() {
        assert_eq!(HttpMethod::Get.to_string(), "GET");
        assert_eq!(HttpMethod::Post.to_string(), "POST");
        assert_eq!(HttpMethod::Put.to_string(), "PUT");
        assert_eq!(HttpMethod::Patch.to_string(), "PATCH");
        assert_eq!(HttpMethod::Delete.to_string(), "DELETE");
        assert_eq!(HttpMethod::Head.to_string(), "HEAD");
        assert_eq!(HttpMethod::Options.to_string(), "OPTIONS");
    }

    #[test]
    fn http_method_from_str_valid() {
        assert_eq!("GET".parse::<HttpMethod>().unwrap(), HttpMethod::Get);
        assert_eq!("POST".parse::<HttpMethod>().unwrap(), HttpMethod::Post);
        assert_eq!("PUT".parse::<HttpMethod>().unwrap(), HttpMethod::Put);
        assert_eq!("PATCH".parse::<HttpMethod>().unwrap(), HttpMethod::Patch);
        assert_eq!("DELETE".parse::<HttpMethod>().unwrap(), HttpMethod::Delete);
        assert_eq!("HEAD".parse::<HttpMethod>().unwrap(), HttpMethod::Head);
        assert_eq!("OPTIONS".parse::<HttpMethod>().unwrap(), HttpMethod::Options);
    }

    #[test]
    fn http_method_from_str_case_insensitive() {
        assert_eq!("get".parse::<HttpMethod>().unwrap(), HttpMethod::Get);
        assert_eq!("Post".parse::<HttpMethod>().unwrap(), HttpMethod::Post);
        assert_eq!("pUt".parse::<HttpMethod>().unwrap(), HttpMethod::Put);
    }

    #[test]
    fn http_method_from_str_invalid() {
        let result = "INVALID".parse::<HttpMethod>();
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Invalid HTTP method"));
    }

    #[test]
    fn http_method_equality() {
        assert_eq!(HttpMethod::Get, HttpMethod::Get);
        assert_ne!(HttpMethod::Get, HttpMethod::Post);
    }
}
