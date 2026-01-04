/// Pattern extraction and library learning
/// Learns from existing specs and suggests patterns for new specs

import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import intent/types.{type Spec, type Behavior}

/// A reusable pattern extracted from specs
pub type Pattern {
  Pattern(
    name: String,
    description: String,
    category: String,
    frequency: Int,
    behaviors: List(Behavior),
  )
}

/// Pattern library containing learned patterns
pub type PatternLibrary {
  PatternLibrary(
    patterns: List(Pattern),
    total_specs_analyzed: Int,
    categories: List(String),
  )
}

/// Create an empty pattern library
pub fn new_library() -> PatternLibrary {
  PatternLibrary(patterns: [], total_specs_analyzed: 0, categories: [])
}

/// Extract patterns from a single spec and add to library
pub fn analyze_and_add_spec(
  library: PatternLibrary,
  spec: Spec,
) -> PatternLibrary {
  let behaviors =
    spec.features
    |> list.flat_map(fn(f) { f.behaviors })

  let extracted_patterns = extract_patterns_from_behaviors(behaviors)

  let updated_patterns = merge_patterns(library.patterns, extracted_patterns)

  let categories =
    updated_patterns
    |> list.map(fn(p) { p.category })
    |> list.unique

  PatternLibrary(
    patterns: updated_patterns,
    total_specs_analyzed: library.total_specs_analyzed + 1,
    categories: categories,
  )
}

/// Extract patterns from a list of behaviors
fn extract_patterns_from_behaviors(behaviors: List(Behavior)) -> List(Pattern) {
  []
  |> append_error_handling_patterns(behaviors)
  |> append_validation_patterns(behaviors)
  |> append_authentication_patterns(behaviors)
  |> append_workflow_patterns(behaviors)
}

/// Extract error handling patterns
fn append_error_handling_patterns(
  patterns: List(Pattern),
  behaviors: List(Behavior),
) -> List(Pattern) {
  let error_behaviors =
    behaviors
    |> list.filter(fn(b) { b.response.status >= 400 })

  case list.length(error_behaviors) {
    0 -> patterns
    count -> {
      list.append(
        patterns,
        [
          Pattern(
            name: "Error Handling",
            description: "API returns appropriate error status codes and messages",
            category: "error-handling",
            frequency: count,
            behaviors: error_behaviors,
          ),
        ],
      )
    }
  }
}

/// Extract validation patterns
fn append_validation_patterns(
  patterns: List(Pattern),
  behaviors: List(Behavior),
) -> List(Pattern) {
  let behaviors_with_checks =
    behaviors
    |> list.filter(fn(b) { !dict.is_empty(b.response.checks) })

  case list.length(behaviors_with_checks) {
    0 -> patterns
    count -> {
      list.append(
        patterns,
        [
          Pattern(
            name: "Response Validation",
            description: "Validates response fields against specific formats and rules",
            category: "validation",
            frequency: count,
            behaviors: behaviors_with_checks,
          ),
        ],
      )
    }
  }
}

/// Extract authentication patterns
fn append_authentication_patterns(
  patterns: List(Pattern),
  behaviors: List(Behavior),
) -> List(Pattern) {
  let auth_behaviors =
    behaviors
    |> list.filter(fn(b) {
      string.contains(string.lowercase(b.name), "auth")
      || string.contains(string.lowercase(b.intent), "auth")
    })

  case list.length(auth_behaviors) {
    0 -> patterns
    count -> {
      list.append(
        patterns,
        [
          Pattern(
            name: "Authentication",
            description: "Tests authentication and authorization scenarios",
            category: "authentication",
            frequency: count,
            behaviors: auth_behaviors,
          ),
        ],
      )
    }
  }
}

/// Extract workflow/sequencing patterns
fn append_workflow_patterns(
  patterns: List(Pattern),
  behaviors: List(Behavior),
) -> List(Pattern) {
  let workflow_behaviors =
    behaviors
    |> list.filter(fn(b) { !list.is_empty(b.requires) })

  case list.length(workflow_behaviors) {
    0 -> patterns
    count -> {
      list.append(
        patterns,
        [
          Pattern(
            name: "Workflow Sequencing",
            description: "Tests behaviors with dependencies and ordering constraints",
            category: "workflow",
            frequency: count,
            behaviors: workflow_behaviors,
          ),
        ],
      )
    }
  }
}

/// Merge newly extracted patterns with existing ones, updating frequency
fn merge_patterns(
  existing: List(Pattern),
  new_patterns: List(Pattern),
) -> List(Pattern) {
  new_patterns
  |> list.fold(existing, fn(acc, new_pattern) {
    case
      acc
      |> list.find(fn(p) { p.name == new_pattern.name })
    {
      Error(_) -> list.append(acc, [new_pattern])
      Ok(found) -> {
        let updated =
          Pattern(
            ..found,
            frequency: found.frequency + new_pattern.frequency,
            behaviors: list.append(found.behaviors, new_pattern.behaviors),
          )
        acc
        |> list.filter(fn(p) { p.name != found.name })
        |> list.append([updated])
      }
    }
  })
}

/// Find patterns matching a query string
pub fn search_patterns(
  library: PatternLibrary,
  query: String,
) -> List(Pattern) {
  let query_lower = string.lowercase(query)
  library.patterns
  |> list.filter(fn(p) {
    string.contains(string.lowercase(p.name), query_lower)
    || string.contains(string.lowercase(p.description), query_lower)
    || string.contains(string.lowercase(p.category), query_lower)
  })
}

/// Get patterns by category
pub fn get_patterns_by_category(
  library: PatternLibrary,
  category: String,
) -> List(Pattern) {
  library.patterns
  |> list.filter(fn(p) { p.category == category })
}

/// Get most frequent patterns
pub fn get_top_patterns(
  library: PatternLibrary,
  limit: Int,
) -> List(Pattern) {
  library.patterns
  |> list.sort(fn(a, b) { int.compare(b.frequency, a.frequency) })
  |> list.take(limit)
}

/// Get pattern library statistics
pub fn library_stats(library: PatternLibrary) -> String {
  let pattern_count = list.length(library.patterns)
  let total_behaviors =
    library.patterns
    |> list.flat_map(fn(p) { p.behaviors })
    |> list.length

  "Pattern Library Statistics:
  Total patterns: " <> int.to_string(pattern_count) <> "
  Categories: " <> int.to_string(list.length(library.categories)) <> "
  Specs analyzed: " <> int.to_string(library.total_specs_analyzed) <> "
  Total behaviors indexed: " <> int.to_string(total_behaviors) <> "
"
}

/// Format patterns for display
pub fn format_patterns(patterns: List(Pattern)) -> String {
  case list.length(patterns) {
    0 -> "No patterns found."
    count -> {
      let header =
        "Found " <> int.to_string(count) <> " pattern(s):\\n\\n"

      let pattern_lines =
        patterns
        |> list.sort(fn(a, b) { int.compare(b.frequency, a.frequency) })
        |> list.map(format_pattern)
        |> string.join("\\n\\n")

      header <> pattern_lines
    }
  }
}

/// Format a single pattern
fn format_pattern(pattern: Pattern) -> String {
  "Pattern: " <> pattern.name <> "
Category: " <> pattern.category <> "
Description: " <> pattern.description <> "
Found in: " <> int.to_string(pattern.frequency) <> " behavior(s)
"
}

/// Suggest patterns for a new spec based on its content
pub fn suggest_patterns_for_spec(
  library: PatternLibrary,
  spec: Spec,
) -> List(Pattern) {
  let behaviors =
    spec.features
    |> list.flat_map(fn(f) { f.behaviors })

  let has_errors =
    list.any(behaviors, fn(b) { b.response.status >= 400 })
  let has_auth =
    list.any(behaviors, fn(b) {
      string.contains(string.lowercase(b.name), "auth")
    })
  let has_workflow =
    list.any(behaviors, fn(b) { !list.is_empty(b.requires) })

  []
  |> append_if(
    has_errors,
    get_patterns_by_category(library, "error-handling"),
  )
  |> append_if(has_auth, get_patterns_by_category(library, "authentication"))
  |> append_if(has_workflow, get_patterns_by_category(library, "workflow"))
}

/// Helper to conditionally append patterns
fn append_if(
  patterns: List(Pattern),
  condition: Bool,
  to_append: List(Pattern),
) -> List(Pattern) {
  case condition {
    True -> list.append(patterns, to_append)
    False -> patterns
  }
}
