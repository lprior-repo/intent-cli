import gleam/dict
import gleam/json
import gleam/list
import gleeunit/should
import intent/types

/// CUPID-DRIVEN TEST: Forces composable, pure factory design
///
/// This test enforces:
/// - C: Composability - Factory must compose with list.range and list.map
/// - U: Unix philosophy - Factory does ONE thing: creates valid Behavior
/// - P: Purity - Same input produces same Behavior (deterministic)
/// - I: Idiomatic - Uses |> pipelines and pattern matching
/// - D: Domain - Speaks "Behavior", "Feature", "Spec" language
///
/// DESIGN PRESSURE: If implementer can't pass this with a pure function
/// that composes via |>, they're not thinking functionally enough.

pub fn generate_large_spec_via_composition_test() {
  // Test that factory composes cleanly with standard library
  let behaviors =
    list.range(0, 999)
    |> list.map(behavior_factory)

  // Verify composability: Should produce exactly 1000 behaviors
  list.length(behaviors)
  |> should.equal(1000)

  // Verify purity: Same index always produces same behavior name
  let b0 = behavior_factory(0)
  let b0_again = behavior_factory(0)
  should.equal(b0.name, b0_again.name)

  // Verify domain correctness: Each behavior must be structurally valid
  list.each(behaviors, fn(b) {
    // Must have non-empty name
    should.not_equal(b.name, "")
    // Must have intent
    should.not_equal(b.intent, "")
    // Must have valid request
    should.not_equal(b.request.path, "")
  })

  // Verify uniqueness: Factory must produce distinct behaviors
  let unique_names =
    behaviors
    |> list.map(fn(b) { b.name })
    |> list.unique

  list.length(unique_names)
  |> should.equal(1000)
}

/// PURE FUNCTION CONTRACT: behavior_factory(i) -> Behavior
///
/// A factory is pure if:
/// 1. behavior_factory(i) == behavior_factory(i) always
/// 2. No I/O, no randomness, no external state
/// 3. Composes with list.map, list.range, etc.
///
/// This signature FORCES the implementer to think about:
/// - How to generate unique but deterministic names
/// - How to create valid Request/Response without external data
/// - How to keep it composable and testable
fn behavior_factory(index: Int) -> types.Behavior {
  // IMPLEMENTER: You must create a valid Behavior here
  // CHALLENGE: How do you make 1000 unique, valid behaviors purely?
  // HINT: Think about string interpolation, index arithmetic, patterns
  panic as "Factory not implemented"
}
