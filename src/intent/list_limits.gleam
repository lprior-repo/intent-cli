/// List Limits - AI guardrails for bounded list operations
/// Default limit: 100 items to prevent overwhelming AI context
import gleam/list

/// Default maximum items for list operations (AI guardrail)
pub const default_max_items: Int = 100

/// Apply a maximum items limit to a list
/// Returns at most `limit` items from the list
///
/// Behavior:
/// - If limit <= 0: returns the full list (no limit)
/// - If limit > 0: returns at most `limit` items
///
/// ## Examples
///
/// ```gleam
/// apply_limit([1, 2, 3, 4, 5], 3)
/// // -> [1, 2, 3]
///
/// apply_limit([1, 2, 3], 100)
/// // -> [1, 2, 3]
///
/// apply_limit([1, 2, 3], 0)
/// // -> [1, 2, 3] (no limit)
/// ```
pub fn apply_limit(items: List(a), limit: Int) -> List(a) {
  case limit <= 0 {
    True -> items
    False -> list.take(items, limit)
  }
}

/// Apply the default limit (100 items) to a list
///
/// ## Examples
///
/// ```gleam
/// apply_default_limit(list.range(1, 150))
/// // -> returns first 100 items
/// ```
pub fn apply_default_limit(items: List(a)) -> List(a) {
  apply_limit(items, default_max_items)
}

/// Returns the effective limit value, using default if input is 0 or negative
/// This is useful for converting flag values to actual limits
///
/// ## Examples
///
/// ```gleam
/// effective_limit(50)
/// // -> 50
///
/// effective_limit(0)
/// // -> 100 (default)
///
/// effective_limit(-1)
/// // -> 100 (default)
/// ```
pub fn effective_limit(limit: Int) -> Int {
  case limit <= 0 {
    True -> default_max_items
    False -> limit
  }
}

/// Indicates if a list was truncated by the limit
/// Returns True if the original list had more items than the limit
///
/// ## Examples
///
/// ```gleam
/// was_truncated([1, 2, 3, 4, 5], 3)
/// // -> True
///
/// was_truncated([1, 2, 3], 100)
/// // -> False
/// ```
pub fn was_truncated(items: List(a), limit: Int) -> Bool {
  case limit <= 0 {
    True -> False
    False -> list.length(items) > limit
  }
}
