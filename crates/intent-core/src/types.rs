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
use std::path::Path;
use std::str::FromStr;

// =============================================================================
// Spec Types
// =============================================================================

/// Validated specification name
///
/// Must be non-empty, alphanumeric + hyphens/underscores only, and end with .cue
///
/// # Examples
///
/// ```
/// use intent_core::types::SpecName;
///
/// let name = SpecName::try_new("user-api.cue").unwrap();
/// assert_eq!(name.as_str(), "user-api.cue");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpecName(String);

impl SpecName {
    /// Create a new SpecName with validation
    ///
    /// # Errors
    ///
    /// Returns `IntentError::Validation` if:
    /// - Name is empty
    /// - Name doesn't end with .cue
    /// - Name contains invalid characters
    pub fn try_new(name: impl Into<String>) -> Result<Self, IntentError> {
        let name = name.into();

        if name.is_empty() {
            return Err(IntentError::validation(
                "spec_name",
                "Spec name cannot be empty",
            ));
        }

        if !name.ends_with(".cue") {
            return Err(IntentError::validation(
                "spec_name",
                format!("Spec name must end with .cue: '{}'", name),
            ));
        }

        // Check for valid characters (alphanumeric, hyphens, underscores, dots, slashes for paths)
        if !name
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, '-' | '_' | '.' | '/'))
        {
            return Err(IntentError::validation(
                "spec_name",
                format!(
                    "Spec name contains invalid characters: '{}'. Only alphanumeric, hyphens, underscores, dots, and slashes allowed",
                    name
                ),
            ));
        }

        Ok(Self(name))
    }

    /// Get the spec name as a string slice
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Convert to PathBuf
    #[must_use]
    pub fn as_path(&self) -> &Path {
        Path::new(&self.0)
    }

    /// Get the base name without .cue extension
    #[must_use]
    pub fn base_name(&self) -> &str {
        self.0.strip_suffix(".cue").unwrap_or(&self.0)
    }
}

impl fmt::Display for SpecName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for SpecName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AsRef<Path> for SpecName {
    fn as_ref(&self) -> &Path {
        Path::new(&self.0)
    }
}

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

    // =========================================================================
    // SpecName Tests
    // =========================================================================

    #[test]
    fn spec_name_valid() {
        let name = SpecName::try_new("test.cue").unwrap();
        assert_eq!(name.as_str(), "test.cue");
    }

    #[test]
    fn spec_name_with_hyphens_underscores() {
        let name = SpecName::try_new("user-api_v2.cue").unwrap();
        assert_eq!(name.as_str(), "user-api_v2.cue");
    }

    #[test]
    fn spec_name_with_path() {
        let name = SpecName::try_new("specs/user-api.cue").unwrap();
        assert_eq!(name.as_str(), "specs/user-api.cue");
    }

    #[test]
    fn spec_name_empty_fails() {
        let result = SpecName::try_new("");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be empty"));
    }

    #[test]
    fn spec_name_missing_extension_fails() {
        let result = SpecName::try_new("test");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("must end with .cue"));
    }

    #[test]
    fn spec_name_invalid_chars_fails() {
        let result = SpecName::try_new("test@spec.cue");
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("invalid characters"));
    }

    #[test]
    fn spec_name_base_name() {
        let name = SpecName::try_new("user-api.cue").unwrap();
        assert_eq!(name.base_name(), "user-api");
    }

    #[test]
    fn spec_name_display() {
        let name = SpecName::try_new("test.cue").unwrap();
        assert_eq!(name.to_string(), "test.cue");
    }

    #[test]
    fn spec_name_as_path() {
        let name = SpecName::try_new("specs/test.cue").unwrap();
        assert_eq!(name.as_path(), Path::new("specs/test.cue"));
    }
}
