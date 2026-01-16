/// JSON validation for DOS protection (JSON bomb attacks)

import gleam/int
import gleam/list
import gleam/string

/// Maximum allowed JSON payload size in bytes (10MB)
pub const max_json_size_bytes = 10_485_760

/// Maximum allowed JSON nesting depth
pub const max_json_depth = 1000

/// JSON validation errors
pub type JsonValidationError {
  PayloadTooLarge(size: Int, max: Int)
  NestingTooDeep(depth: Int, max: Int)
}

/// Validate JSON string for size and depth limits before parsing
/// Prevents JSON bomb attacks (deeply nested or huge payloads)
pub fn validate_json_safety(
  json_str: String,
) -> Result(Nil, JsonValidationError) {
  // 1. Check size in bytes
  let size = string.byte_size(json_str)
  case size > max_json_size_bytes {
    True -> Error(PayloadTooLarge(size, max_json_size_bytes))
    False -> {
      // 2. Check nesting depth
      let depth = count_max_nesting_depth(json_str)
      case depth > max_json_depth {
        True -> Error(NestingTooDeep(depth, max_json_depth))
        False -> Ok(Nil)
      }
    }
  }
}

/// Count maximum nesting depth of JSON by scanning for { and [ characters
/// This is a fast pre-parse check that doesn't handle escaped characters in strings
/// but provides protection against deeply nested JSON bombs
fn count_max_nesting_depth(json_str: String) -> Int {
  let #(_current, max) =
    json_str
    |> string.to_graphemes
    |> list.fold(#(0, 0), fn(acc, char) {
      let #(current, max) = acc
      case char {
        "{" | "[" -> #(current + 1, int.max(max, current + 1))
        "}" | "]" -> #(int.max(current - 1, 0), max)
        _ -> acc
      }
    })
  max
}
