/// Multi-Spec Aggregation Analysis
///
/// Combines analysis from multiple specs to find:
/// - Common patterns across specs
/// - Duplicate behaviors
/// - Conflicting approaches
/// - Cross-spec recommendations
import gleam/dict.{type Dict}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import intent/types.{type Behavior, type Feature, type Rule, type Spec}

// =============================================================================
// Types
// =============================================================================

/// Aggregated analysis result
pub type AggregateReport {
  AggregateReport(
    specs_analyzed: Int,
    total_features: Int,
    total_behaviors: Int,
    common_patterns: List(CommonPattern),
    duplicates: List(Duplicate),
    conflicts: List(Conflict),
    recommendations: List(String),
  )
}

/// Common pattern found across specs
pub type CommonPattern {
  CommonPattern(
    pattern_type: String,
    count: Int,
    examples: List(String),
    specs: List(String),
  )
}

/// Duplicate behavior across specs
pub type Duplicate {
  Duplicate(behavior_name: String, specs: List(String), suggestion: String)
}

/// Conflict between specs
pub type Conflict {
  Conflict(
    conflict_type: String,
    description: String,
    specs: List(String),
    resolution: String,
  )
}

/// Spec with metadata for analysis
pub type SpecWithPath {
  SpecWithPath(path: String, spec: Spec)
}

// =============================================================================
// Public API
// =============================================================================

/// Aggregate analysis from multiple specs
pub fn aggregate_specs(specs: List(SpecWithPath)) -> AggregateReport {
  let specs_count = list.length(specs)

  let total_features =
    specs
    |> list.flat_map(fn(s) { s.spec.features })
    |> list.length

  let total_behaviors =
    specs
    |> list.flat_map(fn(s) {
      s.spec.features
      |> list.flat_map(fn(f) { f.behaviors })
    })
    |> list.length

  let common_patterns = find_common_patterns(specs)
  let duplicates = find_duplicates(specs)
  let conflicts = find_conflicts(specs)
  let recommendations =
    generate_recommendations(specs, common_patterns, duplicates, conflicts)

  AggregateReport(
    specs_analyzed: specs_count,
    total_features: total_features,
    total_behaviors: total_behaviors,
    common_patterns: common_patterns,
    duplicates: duplicates,
    conflicts: conflicts,
    recommendations: recommendations,
  )
}

/// Convert aggregate report to JSON
pub fn to_json(report: AggregateReport) -> json.Json {
  json.object([
    #("specs_analyzed", json.int(report.specs_analyzed)),
    #("total_features", json.int(report.total_features)),
    #("total_behaviors", json.int(report.total_behaviors)),
    #(
      "common_patterns",
      json.array(report.common_patterns, fn(p) {
        json.object([
          #("pattern_type", json.string(p.pattern_type)),
          #("count", json.int(p.count)),
          #("examples", json.array(p.examples, json.string)),
          #("specs", json.array(p.specs, json.string)),
        ])
      }),
    ),
    #(
      "duplicates",
      json.array(report.duplicates, fn(d) {
        json.object([
          #("behavior_name", json.string(d.behavior_name)),
          #("specs", json.array(d.specs, json.string)),
          #("suggestion", json.string(d.suggestion)),
        ])
      }),
    ),
    #(
      "conflicts",
      json.array(report.conflicts, fn(c) {
        json.object([
          #("conflict_type", json.string(c.conflict_type)),
          #("description", json.string(c.description)),
          #("specs", json.array(c.specs, json.string)),
          #("resolution", json.string(c.resolution)),
        ])
      }),
    ),
    #("recommendations", json.array(report.recommendations, json.string)),
  ])
}

// =============================================================================
// Pattern Detection
// =============================================================================

/// Find common patterns across specs
fn find_common_patterns(specs: List(SpecWithPath)) -> List(CommonPattern) {
  let http_methods = find_http_method_patterns(specs)
  let auth_patterns = find_auth_patterns(specs)
  let error_handling = find_error_handling_patterns(specs)
  let validation_patterns = find_validation_patterns(specs)

  list.flatten([
    http_methods,
    auth_patterns,
    error_handling,
    validation_patterns,
  ])
}

/// Find HTTP method usage patterns
fn find_http_method_patterns(specs: List(SpecWithPath)) -> List(CommonPattern) {
  let method_usage =
    specs
    |> list.flat_map(fn(s) {
      s.spec.features
      |> list.flat_map(fn(f) { f.behaviors })
      |> list.map(fn(b) { #(b.request.method, s.path) })
    })
    |> list.group(fn(pair) { pair.0 })

  method_usage
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_method, occurrences) = entry
    list.length(occurrences) >= 2
  })
  |> list.map(fn(entry) {
    let #(method, occurrences) = entry
    let spec_paths =
      occurrences
      |> list.map(fn(p) { p.1 })
      |> list.unique

    CommonPattern(
      pattern_type: "HTTP Method: " <> types.method_to_string(method),
      count: list.length(occurrences),
      examples: list.take(spec_paths, 3),
      specs: spec_paths,
    )
  })
}

/// Find authentication patterns
fn find_auth_patterns(specs: List(SpecWithPath)) -> List(CommonPattern) {
  let auth_behaviors =
    specs
    |> list.filter_map(fn(s) {
      let has_auth =
        s.spec.features
        |> list.flat_map(fn(f) { f.behaviors })
        |> list.any(fn(b) {
          string.contains(string.lowercase(b.name), "auth")
          || string.contains(string.lowercase(b.intent), "auth")
        })

      case has_auth {
        True -> Ok(s.path)
        False -> Error(Nil)
      }
    })

  case list.length(auth_behaviors) >= 2 {
    True -> [
      CommonPattern(
        pattern_type: "Authentication",
        count: list.length(auth_behaviors),
        examples: list.take(auth_behaviors, 3),
        specs: auth_behaviors,
      ),
    ]
    False -> []
  }
}

/// Find error handling patterns
fn find_error_handling_patterns(
  specs: List(SpecWithPath),
) -> List(CommonPattern) {
  let error_statuses =
    specs
    |> list.flat_map(fn(s) {
      s.spec.features
      |> list.flat_map(fn(f) { f.behaviors })
      |> list.filter(fn(b) { b.response.status >= 400 })
      |> list.map(fn(b) { #(b.response.status, s.path) })
    })
    |> list.group(fn(pair) { pair.0 })

  error_statuses
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_status, occurrences) = entry
    list.length(occurrences) >= 2
  })
  |> list.map(fn(entry) {
    let #(status, occurrences) = entry
    let spec_paths =
      occurrences
      |> list.map(fn(p) { p.1 })
      |> list.unique

    CommonPattern(
      pattern_type: "Error Status: " <> int.to_string(status),
      count: list.length(occurrences),
      examples: list.take(spec_paths, 3),
      specs: spec_paths,
    )
  })
}

/// Find validation patterns
fn find_validation_patterns(specs: List(SpecWithPath)) -> List(CommonPattern) {
  let validations =
    specs
    |> list.flat_map(fn(s) {
      s.spec.features
      |> list.flat_map(fn(f) { f.behaviors })
      |> list.filter(fn(b) {
        string.contains(string.lowercase(b.name), "invalid")
        || string.contains(string.lowercase(b.name), "valid")
        || string.contains(string.lowercase(b.intent), "validate")
      })
      |> list.map(fn(_b) { s.path })
    })

  let unique_specs = list.unique(validations)

  case list.length(unique_specs) >= 2 {
    True -> [
      CommonPattern(
        pattern_type: "Validation Testing",
        count: list.length(validations),
        examples: list.take(unique_specs, 3),
        specs: unique_specs,
      ),
    ]
    False -> []
  }
}

// =============================================================================
// Duplicate Detection
// =============================================================================

/// Find duplicate behaviors across specs
fn find_duplicates(specs: List(SpecWithPath)) -> List(Duplicate) {
  let behaviors_by_name =
    specs
    |> list.flat_map(fn(s) {
      s.spec.features
      |> list.flat_map(fn(f) { f.behaviors })
      |> list.map(fn(b) { #(string.lowercase(b.name), s.path) })
    })
    |> list.group(fn(pair) { pair.0 })

  behaviors_by_name
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_name, occurrences) = entry
    let unique_specs =
      occurrences
      |> list.map(fn(p) { p.1 })
      |> list.unique
    list.length(unique_specs) >= 2
  })
  |> list.map(fn(entry) {
    let #(name, occurrences) = entry
    let spec_paths =
      occurrences
      |> list.map(fn(p) { p.1 })
      |> list.unique

    Duplicate(
      behavior_name: name,
      specs: spec_paths,
      suggestion: "Consider consolidating duplicate behavior into shared spec",
    )
  })
}

// =============================================================================
// Conflict Detection
// =============================================================================

/// Find conflicts between specs
fn find_conflicts(specs: List(SpecWithPath)) -> List(Conflict) {
  let endpoint_conflicts = find_endpoint_conflicts(specs)
  let timeout_conflicts = find_timeout_conflicts(specs)

  list.flatten([endpoint_conflicts, timeout_conflicts])
}

/// Find endpoint conflicts (same path, different methods/status)
fn find_endpoint_conflicts(specs: List(SpecWithPath)) -> List(Conflict) {
  let endpoints =
    specs
    |> list.flat_map(fn(s) {
      s.spec.features
      |> list.flat_map(fn(f) { f.behaviors })
      |> list.map(fn(b) {
        #(b.request.path, b.request.method, b.response.status, s.path)
      })
    })
    |> list.group(fn(tuple) { tuple.0 })

  endpoints
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    let #(path, occurrences) = entry
    let unique_methods =
      occurrences
      |> list.map(fn(t) { t.1 })
      |> list.unique

    let unique_specs =
      occurrences
      |> list.map(fn(t) { t.3 })
      |> list.unique

    case list.length(unique_methods) > 1 && list.length(unique_specs) > 1 {
      True -> {
        let method_strings =
          unique_methods
          |> list.map(types.method_to_string)
          |> string.join(", ")

        Ok(Conflict(
          conflict_type: "Endpoint Method Mismatch",
          description: "Path "
            <> path
            <> " has different HTTP methods across specs: "
            <> method_strings,
          specs: unique_specs,
          resolution: "Standardize HTTP method usage for " <> path,
        ))
      }
      False -> Error(Nil)
    }
  })
}

/// Find timeout configuration conflicts
fn find_timeout_conflicts(specs: List(SpecWithPath)) -> List(Conflict) {
  let timeouts =
    specs
    |> list.map(fn(s) { #(s.spec.config.timeout_ms, s.path) })
    |> list.unique

  case list.length(timeouts) >= 2 {
    True -> {
      let timeout_values =
        timeouts
        |> list.map(fn(t) { int.to_string(t.0) <> "ms" })
        |> string.join(", ")

      let spec_paths =
        timeouts
        |> list.map(fn(t) { t.1 })

      [
        Conflict(
          conflict_type: "Timeout Configuration Mismatch",
          description: "Different timeout values across specs: "
            <> timeout_values,
          specs: spec_paths,
          resolution: "Standardize timeout configuration across all specs",
        ),
      ]
    }
    False -> []
  }
}

// =============================================================================
// Recommendations
// =============================================================================

/// Generate cross-spec recommendations
fn generate_recommendations(
  specs: List(SpecWithPath),
  patterns: List(CommonPattern),
  duplicates: List(Duplicate),
  conflicts: List(Conflict),
) -> List(String) {
  let base_recommendations = []

  // Recommendation: Consolidate if many duplicates
  let duplicate_recommendation = case list.length(duplicates) > 3 {
    True -> [
      "Consider creating a shared base spec to reduce duplication ("
      <> int.to_string(list.length(duplicates))
      <> " duplicate behaviors found)",
    ]
    False -> []
  }

  // Recommendation: Resolve conflicts
  let conflict_recommendation = case list.length(conflicts) > 0 {
    True -> [
      "Resolve "
      <> int.to_string(list.length(conflicts))
      <> " conflicts between specs to ensure consistent behavior",
    ]
    False -> []
  }

  // Recommendation: Extract common patterns
  let pattern_recommendation = case list.length(patterns) > 5 {
    True -> [
      "Extract common patterns into reusable templates or shared definitions",
    ]
    False -> []
  }

  // Recommendation: Standardize error handling
  let error_pattern_count =
    patterns
    |> list.filter(fn(p) { string.starts_with(p.pattern_type, "Error Status:") })
    |> list.length

  let error_recommendation = case error_pattern_count >= 3 {
    True -> ["Standardize error handling patterns across all specs"]
    False -> []
  }

  // Recommendation: Coverage summary
  let total_behaviors =
    specs
    |> list.flat_map(fn(s) {
      s.spec.features
      |> list.flat_map(fn(f) { f.behaviors })
    })
    |> list.length

  let coverage_recommendation = case total_behaviors > 50 {
    True -> [
      "Large test suite detected ("
      <> int.to_string(total_behaviors)
      <> " behaviors). Consider organizing into test suites by feature or service",
    ]
    False -> []
  }

  list.flatten([
    base_recommendations,
    duplicate_recommendation,
    conflict_recommendation,
    pattern_recommendation,
    error_recommendation,
    coverage_recommendation,
  ])
}
