/// Partial Spec Validation
///
/// Handles incomplete/partial specs from interview sessions without hard failures.
/// Validates what exists and reports what's missing for incremental analysis.
import gleam/dict.{type Dict}
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import intent/types.{type Spec}

/// Validation result with partial data
pub type PartialSpec {
  PartialSpec(
    name: Option(String),
    description: Option(String),
    audience: Option(String),
    version: Option(String),
    success_criteria: List(String),
    config: Option(types.Config),
    features: List(types.Feature),
    rules: List(types.Rule),
    anti_patterns: List(types.AntiPattern),
    ai_hints: Option(types.AIHints),
    missing_fields: List(String),
    validation_errors: List(String),
  )
}

/// Validation report
pub type ValidationReport {
  ValidationReport(
    is_complete: Bool,
    missing_required: List(String),
    present_fields: List(String),
    has_features: Bool,
    has_behaviors: Bool,
    total_behaviors: Int,
    quality: PartialQuality,
  )
}

pub type PartialQuality {
  PartialQuality(
    can_analyze_coverage: Bool,
    can_analyze_gaps: Bool,
    can_analyze_quality: Bool,
    analysis_warnings: List(String),
  )
}

/// Try to convert PartialSpec to full Spec
pub fn to_spec(partial: PartialSpec) -> Result(Spec, List(String)) {
  case partial.missing_fields {
    [] ->
      case
        partial.name,
        partial.description,
        partial.audience,
        partial.version,
        partial.config,
        partial.ai_hints
      {
        Some(n), Some(d), Some(a), Some(v), Some(c), Some(h) ->
          Ok(types.Spec(
            name: n,
            description: d,
            audience: a,
            version: v,
            success_criteria: partial.success_criteria,
            config: c,
            features: partial.features,
            rules: partial.rules,
            anti_patterns: partial.anti_patterns,
            ai_hints: h,
          ))
        _, _, _, _, _, _ -> Error(["Failed to extract required fields"])
      }
    missing -> Error(missing)
  }
}

/// Validate what exists in a partial spec
pub fn validate(partial: PartialSpec) -> ValidationReport {
  let missing_required = partial.missing_fields
  let is_complete = list.is_empty(missing_required)

  let present_fields = build_present_fields_list(partial)
  let has_features = !list.is_empty(partial.features)
  let total_behaviors =
    partial.features
    |> list.flat_map(fn(f) { f.behaviors })
    |> list.length()
  let has_behaviors = total_behaviors > 0

  let quality = assess_analysis_capability(partial)

  ValidationReport(
    is_complete: is_complete,
    missing_required: missing_required,
    present_fields: present_fields,
    has_features: has_features,
    has_behaviors: has_behaviors,
    total_behaviors: total_behaviors,
    quality: quality,
  )
}

fn build_present_fields_list(partial: PartialSpec) -> List(String) {
  []
  |> add_if_present(option.is_some(partial.name), "name")
  |> add_if_present(option.is_some(partial.description), "description")
  |> add_if_present(option.is_some(partial.audience), "audience")
  |> add_if_present(option.is_some(partial.version), "version")
  |> add_if_present(
    !list.is_empty(partial.success_criteria),
    "success_criteria",
  )
  |> add_if_present(option.is_some(partial.config), "config")
  |> add_if_present(!list.is_empty(partial.features), "features")
  |> add_if_present(!list.is_empty(partial.rules), "rules")
  |> add_if_present(!list.is_empty(partial.anti_patterns), "anti_patterns")
  |> add_if_present(option.is_some(partial.ai_hints), "ai_hints")
}

fn add_if_present(
  fields: List(String),
  condition: Bool,
  field: String,
) -> List(String) {
  case condition {
    True -> [field, ..fields]
    False -> fields
  }
}

fn assess_analysis_capability(partial: PartialSpec) -> PartialQuality {
  let behaviors =
    partial.features
    |> list.flat_map(fn(f) { f.behaviors })

  let has_behaviors = !list.is_empty(behaviors)
  let can_analyze_coverage = has_behaviors
  let can_analyze_gaps = has_behaviors
  let can_analyze_quality = has_behaviors

  let mut_warnings = []

  let mut_warnings = case has_behaviors {
    False -> [
      "No behaviors defined - all analysis will be limited",
      ..mut_warnings
    ]
    True -> mut_warnings
  }

  let mut_warnings = case option.is_some(partial.ai_hints) {
    False -> [
      "No AI hints - AI readiness scoring will be limited",
      ..mut_warnings
    ]
    True -> mut_warnings
  }

  let mut_warnings = case list.is_empty(partial.rules) {
    True -> [
      "No global rules defined - quality analysis will be limited",
      ..mut_warnings
    ]
    False -> mut_warnings
  }

  let mut_warnings = case list.is_empty(partial.anti_patterns) {
    True -> [
      "No anti-patterns defined - inversion gap analysis will be limited",
      ..mut_warnings
    ]
    False -> mut_warnings
  }

  PartialQuality(
    can_analyze_coverage: can_analyze_coverage,
    can_analyze_gaps: can_analyze_gaps,
    can_analyze_quality: can_analyze_quality,
    analysis_warnings: mut_warnings,
  )
}

/// Create an empty partial spec
pub fn empty() -> PartialSpec {
  PartialSpec(
    name: None,
    description: None,
    audience: None,
    version: None,
    success_criteria: [],
    config: None,
    features: [],
    rules: [],
    anti_patterns: [],
    ai_hints: None,
    missing_fields: [
      "name", "description", "audience", "version", "config", "features",
      "ai_hints",
    ],
    validation_errors: [],
  )
}

/// Format validation report for display
pub fn format_validation_report(report: ValidationReport) -> String {
  let status = case report.is_complete {
    True -> "✅ Complete specification"
    False -> "⚠️  Partial specification"
  }

  let missing_section = case list.is_empty(report.missing_required) {
    True -> ""
    False ->
      "\n\nMissing Required Fields:\n"
      <> {
        report.missing_required
        |> list.map(fn(f) { "  • " <> f })
        |> string.join("\n")
      }
  }

  let present_section =
    "\n\nPresent Fields ("
    <> string.inspect(list.length(report.present_fields))
    <> "):\n"
    <> {
      report.present_fields
      |> list.map(fn(f) { "  ✓ " <> f })
      |> string.join("\n")
    }

  let behavior_section =
    "\n\nBehaviors: "
    <> string.inspect(report.total_behaviors)
    <> case report.has_behaviors {
      True -> " (sufficient for analysis)"
      False -> " (insufficient - need at least 1)"
    }

  let analysis_section = case list.is_empty(report.quality.analysis_warnings) {
    True -> "\n\n✅ All analysis types supported"
    False ->
      "\n\nAnalysis Warnings:\n"
      <> {
        report.quality.analysis_warnings
        |> list.map(fn(w) { "  ⚠️  " <> w })
        |> string.join("\n")
      }
  }

  status
  <> missing_section
  <> present_section
  <> behavior_section
  <> analysis_section
}

/// Format validation report as JSON
pub fn format_validation_json(report: ValidationReport) -> json.Json {
  json.object([
    #("is_complete", json.bool(report.is_complete)),
    #("missing_required", json.array(report.missing_required, json.string)),
    #("present_fields", json.array(report.present_fields, json.string)),
    #("has_features", json.bool(report.has_features)),
    #("has_behaviors", json.bool(report.has_behaviors)),
    #("total_behaviors", json.int(report.total_behaviors)),
    #("can_analyze_coverage", json.bool(report.quality.can_analyze_coverage)),
    #("can_analyze_gaps", json.bool(report.quality.can_analyze_gaps)),
    #("can_analyze_quality", json.bool(report.quality.can_analyze_quality)),
    #(
      "analysis_warnings",
      json.array(report.quality.analysis_warnings, json.string),
    ),
  ])
}
