//// MVP Analyzer - Critical Path Detection for Minimum Viable Products
////
//// Analyzes a ShapeSection to identify:
//// - Which features are on the critical path for MVP
//// - MVP clarity score (0-100)
//// - Issues and recommendations for improving MVP definition
////
//// Used in Phase 2 (Shape) of the 4-phase planning workflow.

import gleam/float
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{Some}
import gleam/string
import intent/json_output
import intent/planning_types.{type FeatureShape, type ShapeSection}

// =============================================================================
// PUBLIC TYPES
// =============================================================================

/// Report from MVP analysis
pub type MVPReport {
  MVPReport(
    critical_path_features: List(CriticalPathFeature),
    total_features: Int,
    critical_count: Int,
    critical_ratio: Float,
    mvp_clarity_score: Int,
    issues: List(String),
    recommendations: List(String),
  )
}

/// A feature identified as being on the critical path
pub type CriticalPathFeature {
  CriticalPathFeature(
    name: String,
    description: String,
    reason: String,
    in_mvp_slice: Bool,
  )
}

// =============================================================================
// PUBLIC API
// =============================================================================

/// Analyze a ShapeSection to identify critical path features and MVP clarity
pub fn analyze_mvp(shape: ShapeSection) -> MVPReport {
  let total_features = list.length(shape.features)

  // Identify features on the critical path
  let critical_path_features =
    shape.features
    |> list.filter(fn(f) { list.contains(shape.critical_path, f.name) })
    |> list.map(fn(f) { feature_to_critical(f, shape) })

  let critical_count = list.length(critical_path_features)

  // Calculate critical ratio
  let critical_ratio = case total_features {
    0 -> 0.0
    _ -> int.to_float(critical_count) /. int.to_float(total_features) *. 100.0
  }

  // Calculate MVP clarity score
  let clarity_score = calculate_clarity_score(shape)

  // Identify issues
  let issues = identify_issues(shape)

  // Generate recommendations
  let recommendations =
    generate_recommendations(shape, critical_count, clarity_score)

  MVPReport(
    critical_path_features: critical_path_features,
    total_features: total_features,
    critical_count: critical_count,
    critical_ratio: critical_ratio,
    mvp_clarity_score: clarity_score,
    issues: issues,
    recommendations: recommendations,
  )
}

// =============================================================================
// CRITICAL PATH ANALYSIS
// =============================================================================

/// Convert a FeatureShape to a CriticalPathFeature with context
fn feature_to_critical(
  feature: FeatureShape,
  shape: ShapeSection,
) -> CriticalPathFeature {
  let in_mvp = list.contains(shape.mvp_slice.features, feature.name)

  let reason = case in_mvp {
    True -> "Required for MVP validation: " <> shape.validation_moment
    False -> "On critical path but not in immediate MVP slice"
  }

  CriticalPathFeature(
    name: feature.name,
    description: feature.description,
    reason: reason,
    in_mvp_slice: in_mvp,
  )
}

// =============================================================================
// CLARITY SCORING
// =============================================================================

/// Calculate MVP clarity score (0-100)
/// Dimensions:
/// - Has critical path defined (0-25)
/// - Has validation moment (0-25)
/// - Has shortcuts documented (0-20)
/// - Has post-MVP deferred (0-15)
/// - MVP slice is focused (0-15)
fn calculate_clarity_score(shape: ShapeSection) -> Int {
  let critical_path_score = case shape.critical_path {
    [] -> 0
    [_] -> 15
    _ -> 25
  }

  let validation_score = case string.length(shape.validation_moment) {
    0 -> 0
    len if len < 10 -> 10
    _ -> 25
  }

  let shortcuts_score = case shape.mvp_slice.shortcuts {
    [] -> 0
    [_] -> 10
    _ -> 20
  }

  let post_mvp_score = case shape.post_mvp {
    [] -> 0
    [_] -> 8
    _ -> 15
  }

  let focus_score = calculate_focus_score(shape)

  critical_path_score
  + validation_score
  + shortcuts_score
  + post_mvp_score
  + focus_score
}

/// Calculate focus score based on MVP slice size relative to total
fn calculate_focus_score(shape: ShapeSection) -> Int {
  let total = list.length(shape.features)
  let mvp_size = list.length(shape.mvp_slice.features)

  case total, mvp_size {
    0, _ -> 0
    _, 0 -> 0
    t, m if m <= t / 3 -> 15
    // MVP is focused (1/3 or less of total)
    t, m if m <= t / 2 -> 10
    // MVP is moderate (half or less)
    _, _ -> 5
    // MVP includes most features
  }
}

// =============================================================================
// ISSUE DETECTION
// =============================================================================

/// Identify issues with the MVP definition
fn identify_issues(shape: ShapeSection) -> List(String) {
  let issues = []

  // Check for missing critical path
  let issues = case shape.critical_path {
    [] -> [
      "No critical path defined - unclear which features are essential",
      ..issues
    ]
    _ -> issues
  }

  // Check for missing validation moment
  let issues = case string.length(shape.validation_moment) {
    0 -> [
      "Missing validation moment - no clear success criteria for MVP",
      ..issues
    ]
    len if len < 10 -> [
      "Validation moment too brief - be more specific",
      ..issues
    ]
    _ -> issues
  }

  // Check for missing shortcuts
  let issues = case shape.mvp_slice.shortcuts {
    [] -> [
      "No shortcuts documented - unclear what corners can be cut",
      ..issues
    ]
    _ -> issues
  }

  // Check for features on critical path but not in features list
  let defined_names = list.map(shape.features, fn(f) { f.name })
  let undefined_critical =
    shape.critical_path
    |> list.filter(fn(name) { !list.contains(defined_names, name) })

  let issues = case undefined_critical {
    [] -> issues
    names -> [
      "Critical path references undefined features: "
        <> string.join(names, ", "),
      ..issues
    ]
  }

  list.reverse(issues)
}

// =============================================================================
// RECOMMENDATIONS
// =============================================================================

/// Generate recommendations for improving MVP definition
fn generate_recommendations(
  shape: ShapeSection,
  _critical_count: Int,
  clarity_score: Int,
) -> List(String) {
  let recommendations = []

  // Recommend defining critical path
  let recommendations = case shape.critical_path {
    [] -> [
      "Define a critical path to clarify essential features",
      ..recommendations
    ]
    _ -> recommendations
  }

  // Recommend validation moment
  let recommendations = case string.length(shape.validation_moment) {
    0 -> [
      "Add a validation moment describing when MVP is proven",
      ..recommendations
    ]
    _ -> recommendations
  }

  // Recommend documenting shortcuts
  let recommendations = case shape.mvp_slice.shortcuts {
    [] -> [
      "Document shortcuts - what are you NOT building in the MVP?",
      ..recommendations
    ]
    _ -> recommendations
  }

  // Recommend deferring work
  let recommendations = case shape.post_mvp {
    [] -> ["Identify post-MVP features to defer scope", ..recommendations]
    _ -> recommendations
  }

  // Warn about large MVP scope
  let total = list.length(shape.features)
  let mvp_size = list.length(shape.mvp_slice.features)
  let recommendations = case total, mvp_size {
    t, m if t > 0 && m > 5 -> [
      "Consider reducing MVP scope - "
        <> int.to_string(m)
        <> " features may be too large",
      ..recommendations
    ]
    t, m if t > 0 && m == t && t > 3 -> [
      "MVP includes all features - consider deferring some to reduce scope",
      ..recommendations
    ]
    _, _ -> recommendations
  }

  // Warn about low clarity
  let recommendations = case clarity_score {
    s if s < 30 -> [
      "MVP definition needs significant improvement (clarity: "
        <> int.to_string(s)
        <> "%)",
      ..recommendations
    ]
    s if s < 50 -> [
      "MVP definition could be clearer (clarity: " <> int.to_string(s) <> "%)",
      ..recommendations
    ]
    _ -> recommendations
  }

  list.reverse(recommendations)
}

// =============================================================================
// FORMATTING
// =============================================================================

/// Format MVP report as human-readable text
pub fn format_report(report: MVPReport) -> String {
  let header = "=== MVP Critical Path Analysis ===\n\n"

  let summary =
    "Total features: "
    <> int.to_string(report.total_features)
    <> "\n"
    <> "Critical path features: "
    <> int.to_string(report.critical_count)
    <> "\n"
    <> "Critical ratio: "
    <> float_to_string_1dp(report.critical_ratio)
    <> "%\n"
    <> "MVP clarity score: "
    <> int.to_string(report.mvp_clarity_score)
    <> "/100\n\n"

  let critical_section = case report.critical_path_features {
    [] -> "No critical path features identified.\n\n"
    features ->
      "--- Critical Path Features ---\n\n"
      <> {
        features
        |> list.map(format_critical_feature)
        |> string.join("\n")
      }
      <> "\n\n"
  }

  let issues_section = case report.issues {
    [] -> ""
    issues ->
      "--- Issues ---\n\n"
      <> {
        issues
        |> list.map(fn(i) { "  * " <> i })
        |> string.join("\n")
      }
      <> "\n\n"
  }

  let recommendations_section = case report.recommendations {
    [] -> ""
    recs ->
      "--- Recommendations ---\n\n"
      <> {
        recs
        |> list.map(fn(r) { "  > " <> r })
        |> string.join("\n")
      }
      <> "\n\n"
  }

  header
  <> summary
  <> critical_section
  <> issues_section
  <> recommendations_section
}

/// Format a single critical path feature
fn format_critical_feature(feature: CriticalPathFeature) -> String {
  let mvp_marker = case feature.in_mvp_slice {
    True -> " [MVP]"
    False -> ""
  }

  "  "
  <> feature.name
  <> mvp_marker
  <> "\n"
  <> "    "
  <> feature.description
  <> "\n"
  <> "    Reason: "
  <> feature.reason
}

/// Format float to 1 decimal place
fn float_to_string_1dp(f: Float) -> String {
  let int_part = float.truncate(f)
  let decimal_part = float.truncate({ f -. int.to_float(int_part) } *. 10.0)
  int.to_string(int_part) <> "." <> int.to_string(decimal_part)
}

// =============================================================================
// JSON OUTPUT
// =============================================================================

/// Convert MVPReport to action-based JSON for AI consumption
pub fn mvp_report_to_action_json(report: MVPReport, plan_name: String) -> Json {
  let data = mvp_report_to_json(report)

  json_output.create_response("mvp_analysis", "mvp", data, Some(plan_name), 0)
  |> json_output.to_json
}

/// Convert MVPReport to JSON
fn mvp_report_to_json(report: MVPReport) -> Json {
  json.object([
    #(
      "critical_path_features",
      json.array(report.critical_path_features, critical_feature_to_json),
    ),
    #("total_features", json.int(report.total_features)),
    #("critical_count", json.int(report.critical_count)),
    #("critical_ratio", json.float(report.critical_ratio)),
    #("mvp_clarity_score", json.int(report.mvp_clarity_score)),
    #("issues", json.array(report.issues, json.string)),
    #("recommendations", json.array(report.recommendations, json.string)),
  ])
}

/// Convert CriticalPathFeature to JSON
fn critical_feature_to_json(feature: CriticalPathFeature) -> Json {
  json.object([
    #("name", json.string(feature.name)),
    #("description", json.string(feature.description)),
    #("reason", json.string(feature.reason)),
    #("in_mvp_slice", json.bool(feature.in_mvp_slice)),
  ])
}
