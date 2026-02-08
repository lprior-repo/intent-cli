/// Case-insensitive string operations
/// Minimal implementation to support bead_templates
import gleam/list
import gleam/string

/// Check if a string contains any of the given substrings (case-insensitive)
pub fn contains_any_ignore_case(text: String, substrings: List(String)) -> Bool {
  let lower_text = string.lowercase(text)

  list.any(substrings, fn(substring) {
    string.contains(lower_text, string.lowercase(substring))
  })
}
