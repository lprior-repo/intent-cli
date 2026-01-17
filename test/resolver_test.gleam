//// Tests for the resolver module
//// Tests dependency resolution and topological sorting of behaviors
//// The resolver determines execution order based on `requires` dependencies

import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import intent/resolver
import test_helpers.{make_test_behavior, make_test_feature, make_test_spec}

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Happy Path Tests - Valid Resolution
// ============================================================================

/// Test resolving behaviors with no dependencies
/// All behaviors should be returned in original order since no ordering is required
pub fn resolve_no_dependencies_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", [])
  let c = make_test_behavior("c", [])
  let spec = make_test_spec([make_test_feature("Test", [a, b, c])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(3)
      // All behaviors should be present
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      list.contains(names, "a")
      |> should.be_true
      list.contains(names, "b")
      |> should.be_true
      list.contains(names, "c")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Test resolving a simple linear dependency chain: A → B → C
/// C requires B, B requires A, so order should be A, B, C
pub fn resolve_simple_chain_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  let c = make_test_behavior("c", ["b"])
  let spec = make_test_spec([make_test_feature("Chain", [a, b, c])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(3)
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      // A must come before B
      let a_idx = list_index_of(names, "a")
      let b_idx = list_index_of(names, "b")
      let c_idx = list_index_of(names, "c")

      case a_idx < b_idx && b_idx < c_idx {
        True -> Nil
        False -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test resolving parallel dependencies
/// Both B and C depend on A, so A must come first, but B and C can be in any order
pub fn resolve_parallel_dependencies_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  let c = make_test_behavior("c", ["a"])
  let spec = make_test_spec([make_test_feature("Parallel", [a, b, c])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(3)
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      let a_idx = list_index_of(names, "a")
      let b_idx = list_index_of(names, "b")
      let c_idx = list_index_of(names, "c")

      // A must come before both B and C
      case a_idx < b_idx && a_idx < c_idx {
        True -> Nil
        False -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test resolving diamond dependencies
/// D depends on B and C, both B and C depend on A
/// Order should be: A, then B and C (any order), then D
pub fn resolve_diamond_dependencies_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  let c = make_test_behavior("c", ["a"])
  let d = make_test_behavior("d", ["b", "c"])
  let spec = make_test_spec([make_test_feature("Diamond", [a, b, c, d])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(4)
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      let a_idx = list_index_of(names, "a")
      let b_idx = list_index_of(names, "b")
      let c_idx = list_index_of(names, "c")
      let d_idx = list_index_of(names, "d")

      // A must come first
      // B and C must come after A
      // D must come after both B and C
      case
        a_idx < b_idx
        && a_idx < c_idx
        && b_idx < d_idx
        && c_idx < d_idx
      {
        True -> Nil
        False -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test resolving behaviors across multiple features
/// All behaviors should be resolved together, respecting cross-feature dependencies
pub fn resolve_multiple_features_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  let feature1 = make_test_feature("Feature1", [a])
  let feature2 = make_test_feature("Feature2", [b])
  let spec = make_test_spec([feature1, feature2])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(2)
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      let a_idx = list_index_of(names, "a")
      let b_idx = list_index_of(names, "b")

      case a_idx < b_idx {
        True -> Nil
        False -> should.fail()
      }

      // Check feature names are preserved
      let a_resolved =
        list.find(resolved, fn(rb) { rb.behavior.name == "a" })
      let b_resolved =
        list.find(resolved, fn(rb) { rb.behavior.name == "b" })

      case a_resolved, b_resolved {
        Ok(a_rb), Ok(b_rb) -> {
          a_rb.feature_name
          |> should.equal("Feature1")
          b_rb.feature_name
          |> should.equal("Feature2")
        }
        _, _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test resolving empty behavior list
/// Should succeed with empty result
pub fn resolve_empty_behaviors_test() {
  let spec = make_test_spec([])
  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

/// Test resolving complex dependency graph
/// Multiple levels of dependencies with various branching patterns
pub fn resolve_complex_graph_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  let c = make_test_behavior("c", ["a"])
  let d = make_test_behavior("d", ["b", "c"])
  let e = make_test_behavior("e", ["b"])
  let f = make_test_behavior("f", ["d", "e"])
  let spec = make_test_spec([make_test_feature("Complex", [a, b, c, d, e, f])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(6)
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      let a_idx = list_index_of(names, "a")
      let b_idx = list_index_of(names, "b")
      let c_idx = list_index_of(names, "c")
      let d_idx = list_index_of(names, "d")
      let e_idx = list_index_of(names, "e")
      let f_idx = list_index_of(names, "f")

      // Verify all ordering constraints
      case
        a_idx < b_idx
        && a_idx < c_idx
        && b_idx < d_idx
        && c_idx < d_idx
        && b_idx < e_idx
        && d_idx < f_idx
        && e_idx < f_idx
      {
        True -> Nil
        False -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Error Path Tests - Circular Dependencies
// ============================================================================

/// Test detecting simple circular dependency: A → B → A
pub fn resolve_simple_circular_dependency_test() {
  let a = make_test_behavior("a", ["b"])
  let b = make_test_behavior("b", ["a"])
  let spec = make_test_spec([make_test_feature("Circular", [a, b])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Error(resolver.CyclicDependency(behaviors)) -> {
      list.length(behaviors)
      |> should.equal(2)
      list.contains(behaviors, "a")
      |> should.be_true
      list.contains(behaviors, "b")
      |> should.be_true
    }
    _ -> should.fail()
  }
}

/// Test detecting three-way circular dependency: A → B → C → A
pub fn resolve_three_way_circular_dependency_test() {
  let a = make_test_behavior("a", ["c"])
  let b = make_test_behavior("b", ["a"])
  let c = make_test_behavior("c", ["b"])
  let spec = make_test_spec([make_test_feature("Circular", [a, b, c])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Error(resolver.CyclicDependency(behaviors)) -> {
      list.length(behaviors)
      |> should.equal(3)
      list.contains(behaviors, "a")
      |> should.be_true
      list.contains(behaviors, "b")
      |> should.be_true
      list.contains(behaviors, "c")
      |> should.be_true
    }
    _ -> should.fail()
  }
}

/// Test detecting self-dependency: A → A
pub fn resolve_self_dependency_test() {
  let a = make_test_behavior("a", ["a"])
  let spec = make_test_spec([make_test_feature("SelfDep", [a])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Error(resolver.CyclicDependency(behaviors)) -> {
      list.contains(behaviors, "a")
      |> should.be_true
    }
    _ -> should.fail()
  }
}

/// Test circular dependency in complex graph
/// Some behaviors are valid, but a cycle exists among others
pub fn resolve_circular_in_complex_graph_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  // Create circular dependency between c and d
  let c = make_test_behavior("c", ["d"])
  let d = make_test_behavior("d", ["c"])
  let spec = make_test_spec([make_test_feature("Mixed", [a, b, c, d])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Error(resolver.CyclicDependency(behaviors)) -> {
      // The cycle should involve c and d
      list.contains(behaviors, "c")
      |> should.be_true
      list.contains(behaviors, "d")
      |> should.be_true
    }
    _ -> should.fail()
  }
}

// ============================================================================
// Error Path Tests - Missing Dependencies
// ============================================================================

/// Test detecting missing dependency
pub fn resolve_missing_dependency_test() {
  let a = make_test_behavior("a", ["nonexistent"])
  let spec = make_test_spec([make_test_feature("Missing", [a])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Error(resolver.MissingDependency("a", "nonexistent")) -> Nil
    _ -> should.fail()
  }
}

/// Test detecting multiple missing dependencies
pub fn resolve_multiple_missing_dependencies_test() {
  let a = make_test_behavior("a", ["missing1", "missing2"])
  let spec = make_test_spec([make_test_feature("Missing", [a])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Error(resolver.MissingDependency("a", missing)) -> {
      // Should fail on first missing dependency
      case missing == "missing1" || missing == "missing2" {
        True -> Nil
        False -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

/// Test missing dependency in middle of chain
pub fn resolve_missing_in_chain_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a", "missing"])
  let c = make_test_behavior("c", ["b"])
  let spec = make_test_spec([make_test_feature("BrokenChain", [a, b, c])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Error(resolver.MissingDependency("b", "missing")) -> Nil
    _ -> should.fail()
  }
}

// ============================================================================
// Error Path Tests - Duplicate Behavior Names
// ============================================================================

/// Test detecting duplicate behavior names within same feature
pub fn resolve_duplicate_behavior_names_same_feature_test() {
  let a1 = make_test_behavior("duplicate", [])
  let a2 = make_test_behavior("duplicate", [])
  let spec = make_test_spec([make_test_feature("Test", [a1, a2])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Error(resolver.DuplicateBehaviorName("duplicate")) -> Nil
    _ -> should.fail()
  }
}

/// Test detecting duplicate behavior names across features
pub fn resolve_duplicate_behavior_names_across_features_test() {
  let a1 = make_test_behavior("duplicate", [])
  let a2 = make_test_behavior("duplicate", [])
  let feature1 = make_test_feature("Feature1", [a1])
  let feature2 = make_test_feature("Feature2", [a2])
  let spec = make_test_spec([feature1, feature2])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Error(resolver.DuplicateBehaviorName("duplicate")) -> Nil
    _ -> should.fail()
  }
}

// ============================================================================
// Filter Functions Tests
// ============================================================================

/// Test filtering resolved behaviors by feature name
pub fn filter_by_feature_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", [])
  let feature1 = make_test_feature("Feature1", [a])
  let feature2 = make_test_feature("Feature2", [b])
  let spec = make_test_spec([feature1, feature2])

  case resolver.resolve_execution_order(spec) {
    Ok(resolved) -> {
      let filtered = resolver.filter_by_feature(resolved, "Feature1")
      list.length(filtered)
      |> should.equal(1)
      let assert [rb] = filtered
      rb.behavior.name
      |> should.equal("a")
      rb.feature_name
      |> should.equal("Feature1")
    }
    Error(_) -> should.fail()
  }
}

/// Test filtering by feature returns empty list for non-existent feature
pub fn filter_by_feature_nonexistent_test() {
  let a = make_test_behavior("a", [])
  let spec = make_test_spec([make_test_feature("Feature1", [a])])

  case resolver.resolve_execution_order(spec) {
    Ok(resolved) -> {
      let filtered = resolver.filter_by_feature(resolved, "NonExistent")
      list.length(filtered)
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

/// Test filtering resolved behaviors by behavior name
pub fn filter_by_name_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", [])
  let c = make_test_behavior("c", [])
  let spec = make_test_spec([make_test_feature("Test", [a, b, c])])

  case resolver.resolve_execution_order(spec) {
    Ok(resolved) -> {
      let filtered = resolver.filter_by_name(resolved, "b")
      list.length(filtered)
      |> should.equal(1)
      let assert [rb] = filtered
      rb.behavior.name
      |> should.equal("b")
    }
    Error(_) -> should.fail()
  }
}

/// Test filtering by name returns empty list for non-existent behavior
pub fn filter_by_name_nonexistent_test() {
  let a = make_test_behavior("a", [])
  let spec = make_test_spec([make_test_feature("Test", [a])])

  case resolver.resolve_execution_order(spec) {
    Ok(resolved) -> {
      let filtered = resolver.filter_by_name(resolved, "nonexistent")
      list.length(filtered)
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

/// Test getting dependents of a behavior
pub fn get_dependents_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", ["a"])
  let c = make_test_behavior("c", ["a"])
  let d = make_test_behavior("d", ["b"])
  let spec = make_test_spec([make_test_feature("Test", [a, b, c, d])])

  case resolver.resolve_execution_order(spec) {
    Ok(resolved) -> {
      let dependents_of_a = resolver.get_dependents(resolved, "a")
      list.length(dependents_of_a)
      |> should.equal(2)
      let names = list.map(dependents_of_a, fn(rb) { rb.behavior.name })
      list.contains(names, "b")
      |> should.be_true
      list.contains(names, "c")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Test getting dependents when behavior has no dependents
pub fn get_dependents_none_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", [])
  let spec = make_test_spec([make_test_feature("Test", [a, b])])

  case resolver.resolve_execution_order(spec) {
    Ok(resolved) -> {
      let dependents = resolver.get_dependents(resolved, "a")
      list.length(dependents)
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

// ============================================================================
// Format Error Tests
// ============================================================================

/// Test formatting cyclic dependency error
pub fn format_error_cyclic_dependency_test() {
  let error = resolver.CyclicDependency(["a", "b", "c"])
  let formatted = resolver.format_error(error)
  formatted
  |> string.contains("Cyclic dependency")
  |> should.be_true
  formatted
  |> string.contains("a")
  |> should.be_true
  formatted
  |> string.contains("b")
  |> should.be_true
}

/// Test formatting missing dependency error
pub fn format_error_missing_dependency_test() {
  let error = resolver.MissingDependency("behavior1", "missing_dep")
  let formatted = resolver.format_error(error)
  formatted
  |> string.contains("behavior1")
  |> should.be_true
  formatted
  |> string.contains("missing_dep")
  |> should.be_true
  formatted
  |> string.contains("does not exist")
  |> should.be_true
}

/// Test formatting duplicate behavior name error
pub fn format_error_duplicate_name_test() {
  let error = resolver.DuplicateBehaviorName("duplicate")
  let formatted = resolver.format_error(error)
  formatted
  |> string.contains("Duplicate")
  |> should.be_true
  formatted
  |> string.contains("duplicate")
  |> should.be_true
}

// ============================================================================
// Edge Cases
// ============================================================================

/// Test behavior with empty string dependency
/// Should be treated as missing dependency
pub fn resolve_empty_string_dependency_test() {
  let a = make_test_behavior("a", [""])
  let spec = make_test_spec([make_test_feature("Test", [a])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Error(resolver.MissingDependency("a", "")) -> Nil
    _ -> should.fail()
  }
}

/// Test large dependency graph
/// Ensures algorithm scales reasonably
pub fn resolve_large_graph_test() {
  // Create a chain of 50 behaviors
  let behaviors =
    list.range(0, 49)
    |> list.map(fn(i) {
      case i {
        0 -> make_test_behavior("b0", [])
        _ -> {
          let name = "b" <> string.inspect(i)
          let prev = "b" <> string.inspect(i - 1)
          make_test_behavior(name, [prev])
        }
      }
    })

  let spec = make_test_spec([make_test_feature("Large", behaviors)])
  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(50)
      // Verify order is correct
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      let assert ["b0", ..] = names
      Nil
    }
    Error(_) -> should.fail()
  }
}

/// Test behavior depending on multiple behaviors in specific order
pub fn resolve_multiple_dependencies_order_test() {
  let a = make_test_behavior("a", [])
  let b = make_test_behavior("b", [])
  let c = make_test_behavior("c", ["a", "b"])
  let spec = make_test_spec([make_test_feature("Test", [a, b, c])])

  let result = resolver.resolve_execution_order(spec)

  case result {
    Ok(resolved) -> {
      list.length(resolved)
      |> should.equal(3)
      let names = list.map(resolved, fn(rb) { rb.behavior.name })
      let a_idx = list_index_of(names, "a")
      let b_idx = list_index_of(names, "b")
      let c_idx = list_index_of(names, "c")

      // Both a and b must come before c
      case a_idx < c_idx && b_idx < c_idx {
        True -> Nil
        False -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test ResolvedBehavior type construction
pub fn resolved_behavior_type_test() {
  let behavior = make_test_behavior("test", [])
  let resolved = resolver.ResolvedBehavior("Feature1", behavior)

  resolved.feature_name
  |> should.equal("Feature1")
  resolved.behavior.name
  |> should.equal("test")
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Find the index of an element in a list
/// Returns the index or -1 if not found (for comparison purposes)
fn list_index_of(list: List(String), item: String) -> Int {
  list_index_of_helper(list, item, 0)
}

fn list_index_of_helper(list: List(String), item: String, idx: Int) -> Int {
  case list {
    [] -> -1
    [head, ..tail] ->
      case head == item {
        True -> idx
        False -> list_index_of_helper(tail, item, idx + 1)
      }
  }
}
