/// Tests for list_limits module - AI guardrails for bounded list operations
import gleam/list
import gleeunit
import gleeunit/should
import intent/list_limits

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Tests for default_max_items constant
// =============================================================================

pub fn default_max_items_is_100_test() {
  list_limits.default_max_items
  |> should.equal(100)
}

// =============================================================================
// Tests for apply_limit function
// =============================================================================

pub fn apply_limit_empty_list_test() {
  []
  |> list_limits.apply_limit(100)
  |> should.equal([])
}

pub fn apply_limit_under_limit_test() {
  [1, 2, 3]
  |> list_limits.apply_limit(100)
  |> should.equal([1, 2, 3])
}

pub fn apply_limit_at_limit_test() {
  [1, 2, 3, 4, 5]
  |> list_limits.apply_limit(5)
  |> should.equal([1, 2, 3, 4, 5])
}

pub fn apply_limit_over_limit_test() {
  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  |> list_limits.apply_limit(5)
  |> should.equal([1, 2, 3, 4, 5])
}

pub fn apply_limit_one_test() {
  [1, 2, 3]
  |> list_limits.apply_limit(1)
  |> should.equal([1])
}

pub fn apply_limit_zero_returns_all_test() {
  [1, 2, 3, 4, 5]
  |> list_limits.apply_limit(0)
  |> should.equal([1, 2, 3, 4, 5])
}

pub fn apply_limit_negative_returns_all_test() {
  [1, 2, 3, 4, 5]
  |> list_limits.apply_limit(-1)
  |> should.equal([1, 2, 3, 4, 5])
}

// =============================================================================
// Tests for apply_default_limit function
// =============================================================================

pub fn apply_default_limit_under_100_test() {
  list.range(1, 50)
  |> list_limits.apply_default_limit
  |> list.length
  |> should.equal(50)
}

pub fn apply_default_limit_over_100_test() {
  list.range(1, 150)
  |> list_limits.apply_default_limit
  |> list.length
  |> should.equal(100)
}

// =============================================================================
// Tests for effective_limit function
// =============================================================================

pub fn effective_limit_positive_unchanged_test() {
  list_limits.effective_limit(50)
  |> should.equal(50)
}

pub fn effective_limit_zero_returns_default_test() {
  list_limits.effective_limit(0)
  |> should.equal(100)
}

pub fn effective_limit_negative_returns_default_test() {
  list_limits.effective_limit(-1)
  |> should.equal(100)
}

// =============================================================================
// Tests for was_truncated function
// =============================================================================

pub fn was_truncated_true_when_over_limit_test() {
  list_limits.was_truncated([1, 2, 3, 4, 5], 3)
  |> should.be_true
}

pub fn was_truncated_false_when_at_limit_test() {
  list_limits.was_truncated([1, 2, 3], 3)
  |> should.be_false
}

pub fn was_truncated_false_when_under_limit_test() {
  list_limits.was_truncated([1, 2], 100)
  |> should.be_false
}

pub fn was_truncated_false_when_no_limit_test() {
  list_limits.was_truncated([1, 2, 3, 4, 5], 0)
  |> should.be_false
}
