//// READY Framework Analyzer
////
//// Evaluates specification readiness across 5 dimensions (R-E-A-D-Y):
//// - Replacement: Is the VORP (Value Over Replacement Product) still valid?
//// - Empathy: Is user friction simulated?
//// - Actionable: Do errors guide users?
//// - Discoverable: Are features findable?
//// - Yet-complete: Can the north star be achieved?
////
//// Each dimension scores 0-100, with overall readiness calculated as weighted average.
//// Generates blockers and recommendations based on scores.

import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import intent/planning_types.{
  type Blocker, type BlockerSeverity, type DimensionScore, type ReadyReport,
  type Recommendation, Blocker, Critical, DimensionScore, High, Low, Medium,
  ReadyReport, Recommendation,
}
import intent/types.{
  type AIHints, type Behavior, type Spec, AIHints, ImplementationHints,
  SecurityHints,
}

/// Get AI hints with default (empty hints if not provided)
fn get_ai_hints(spec: Spec) -> AIHints {
  option.unwrap(
    spec.ai_hints,
    AIHints(
      implementation: ImplementationHints(suggested_stack: []),
      entities: dict.new(),
      security: SecurityHints(
        password_hashing: "",
        jwt_algorithm: "",
        jwt_expiry: "",
        rate_limiting: "",
      ),
      pitfalls: [],
    ),
  )
}

// =============================================================================
// PUBLIC API
// =============================================================================

/// Analyze a spec's readiness across all 5 READY dimensions
pub fn analyze_ready(spec: Spec) -> ReadyReport {
  let replacement = analyze_replacement(spec)
  let empathy = analyze_empathy(spec)
  let actionable = analyze_actionable(spec)
  let discoverable = analyze_discoverable(spec)
  let yet_complete = analyze_yet_complete(spec)

  let overall_readiness =
    calculate_overall_readiness(
      replacement.score,
      empathy.score,
      actionable.score,
      discoverable.score,
      yet_complete.score,
    )

  let base_report =
    ReadyReport(
      replacement: replacement,
      empathy: empathy,
      actionable: actionable,
      discoverable: discoverable,
      yet_complete: yet_complete,
      overall_readiness: overall_readiness,
      blockers: [],
      recommendations: [],
    )

  let blockers = generate_blockers(base_report)
  let recommendations = generate_recommendations(base_report)

  ReadyReport(
    ..base_report,
    blockers: blockers,
    recommendations: recommendations,
  )
}

/// Format a ReadyReport as human-readable text
pub fn format_report(report: ReadyReport) -> String {
  let header =
    "╔══════════════════════════════════════╗\n"
    <> "║   READY Framework Assessment         ║\n"
    <> "╚══════════════════════════════════════╝\n\n"

  let readiness_label = case report.overall_readiness {
    r if r >= 90 -> "Launch Ready"
    r if r >= 80 -> "Production with caution"
    r if r >= 70 -> "Beta/Experimental"
    _ -> "Needs rework"
  }

  let overall =
    "📊 Overall Readiness: "
    <> int.to_string(report.overall_readiness)
    <> "% ("
    <> readiness_label
    <> ")\n\n"

  let replacement_section =
    format_dimension(
      "🔄 Replacement (VORP)",
      report.replacement.score,
      report.replacement.reasoning,
      report.replacement.issues,
    )

  let empathy_section =
    format_dimension(
      "❤️  Empathy (Friction)",
      report.empathy.score,
      report.empathy.reasoning,
      report.empathy.issues,
    )

  let actionable_section =
    format_dimension(
      "🎯 Actionable (Guidance)",
      report.actionable.score,
      report.actionable.reasoning,
      report.actionable.issues,
    )

  let discoverable_section =
    format_dimension(
      "🔍 Discoverable (Findability)",
      report.discoverable.score,
      report.discoverable.reasoning,
      report.discoverable.issues,
    )

  let yet_complete_section =
    format_dimension(
      "✅ Yet-complete (North Star)",
      report.yet_complete.score,
      report.yet_complete.reasoning,
      report.yet_complete.issues,
    )

  let blockers_section = format_blockers(report.blockers)
  let recommendations_section = format_recommendations(report.recommendations)

  header
  <> overall
  <> replacement_section
  <> empathy_section
  <> actionable_section
  <> discoverable_section
  <> yet_complete_section
  <> blockers_section
  <> recommendations_section
}

// =============================================================================
// DIMENSION ANALYZERS
// =============================================================================

fn analyze_replacement(spec: Spec) -> DimensionScore {
  let mut_score = 0

  // Base score: 20 points for having any audience
  let score = case spec.audience != "" {
    True -> mut_score + 20
    False -> mut_score
  }

  // +20 if success_criteria has 3+ items
  let score = case list.length(spec.success_criteria) >= 3 {
    True -> score + 20
    False -> score
  }

  // +20 if description is detailed (>50 chars)
  let score = case string.length(spec.description) > 50 {
    True -> score + 20
    False -> score
  }

  // +20 if ai_hints.implementation has content
  let score = case get_ai_hints(spec).implementation.suggested_stack != [] {
    True -> score + 20
    False -> score
  }

  // +20 if version looks semantic
  let score = case string.contains(spec.version, ".") {
    True -> score + 20
    False -> score
  }

  let reasoning = case score {
    s if s >= 80 ->
      "Clear value proposition with well-defined audience and success criteria"
    s if s >= 60 -> "Good foundation but could strengthen value proposition"
    s if s >= 40 -> "Audience or success criteria need more detail"
    _ -> "Value proposition unclear - needs better audience definition"
  }

  let issues = case score {
    s if s < 60 ->
      []
      |> list.append(case string.length(spec.audience) < 10 {
        True -> ["Audience too vague"]
        False -> []
      })
      |> list.append(
        case
          spec.success_criteria == [] || list.length(spec.success_criteria) < 2
        {
          True -> ["Insufficient success criteria"]
          False -> []
        },
      )
      |> list.append(case string.length(spec.description) < 30 {
        True -> ["Description lacks detail"]
        False -> []
      })
    _ -> []
  }

  DimensionScore(score: score, reasoning: reasoning, issues: issues)
}

fn analyze_empathy(spec: Spec) -> DimensionScore {
  let behaviors = get_all_behaviors(spec)

  // Count error behaviors (4xx status codes)
  let error_behaviors =
    behaviors
    |> list.filter(fn(b) { b.response.status >= 400 && b.response.status < 500 })
    |> list.length()

  // Base score from error handling (up to 75 points)
  let base_score = case error_behaviors {
    0 -> 0
    1 -> 25
    2 -> 50
    _ -> 75
  }

  // +15 for anti_patterns (shows awareness)
  let score = case list.length(spec.anti_patterns) >= 2 {
    True -> base_score + 15
    False -> base_score
  }

  // +10 for validation keywords in behavior text
  let has_validation =
    behaviors
    |> list.any(fn(b) {
      let text = string.lowercase(b.intent <> " " <> b.notes)
      string.contains(text, "validat")
      || string.contains(text, "check")
      || string.contains(text, "verif")
    })

  let score = case has_validation {
    True -> score + 10
    False -> score
  }

  let reasoning = case score {
    s if s >= 75 ->
      "Strong error handling with anti-pattern awareness and validation"
    s if s >= 50 -> "Good error coverage but could expand anti-patterns"
    s if s >= 25 -> "Basic error handling present, needs more scenarios"
    _ -> "Missing error handling and validation behaviors"
  }

  let issues = case score {
    s if s < 50 ->
      []
      |> list.append(case error_behaviors == 0 {
        True -> ["No error handling behaviors"]
        False -> []
      })
      |> list.append(case spec.anti_patterns == [] {
        True -> ["No anti-patterns documented"]
        False -> []
      })
    _ -> []
  }

  DimensionScore(score: score, reasoning: reasoning, issues: issues)
}

fn analyze_actionable(spec: Spec) -> DimensionScore {
  let behaviors = get_all_behaviors(spec)

  // Count behaviors with response checks
  let behaviors_with_checks =
    behaviors
    |> list.filter(fn(b) { dict.size(b.response.checks) > 0 })
    |> list.length()

  // Base score (up to 90 points)
  let base_score = case behaviors_with_checks {
    0 -> 0
    1 -> 30
    2 -> 60
    _ -> 90
  }

  // +5 for quality checks (have "why" field populated)
  let has_quality_checks =
    behaviors
    |> list.any(fn(b) {
      dict.values(b.response.checks)
      |> list.any(fn(check) { string.length(check.why) > 10 })
    })

  let score = case has_quality_checks {
    True -> base_score + 5
    False -> base_score
  }

  // +5 if intent fields are descriptive
  let has_clear_intents =
    behaviors
    |> list.any(fn(b) { string.length(b.intent) > 20 })

  let score = case has_clear_intents {
    True -> score + 5
    False -> score
  }

  let reasoning = case score {
    s if s >= 80 -> "Excellent response checks with clear guidance"
    s if s >= 60 -> "Good checks but could improve reasoning"
    s if s >= 30 -> "Basic checks present, needs more detail"
    _ -> "Missing response checks - errors won't guide users"
  }

  let issues = case score {
    s if s < 60 ->
      []
      |> list.append(case behaviors_with_checks == 0 {
        True -> ["No response checks defined"]
        False -> []
      })
      |> list.append(case has_quality_checks {
        False -> ["Checks lack detailed reasoning"]
        True -> []
      })
    _ -> []
  }

  DimensionScore(score: score, reasoning: reasoning, issues: issues)
}

fn analyze_discoverable(spec: Spec) -> DimensionScore {
  let features = spec.features
  let behaviors = get_all_behaviors(spec)

  // Score based on clear feature names (up to 60 points)
  let feature_score = case list.length(features) {
    0 -> 0
    1 -> 20
    2 -> 40
    _ -> 60
  }

  // +10 per behavior with tags (up to 30 points)
  let behaviors_with_tags =
    behaviors
    |> list.filter(fn(b) { b.tags != [] })
    |> list.length()

  let tag_score = case behaviors_with_tags {
    0 -> 0
    1 -> 10
    2 -> 20
    _ -> 30
  }

  // +10 if paths look RESTful
  let has_restful_paths =
    behaviors
    |> list.any(fn(b) {
      string.contains(b.request.path, "/")
      && !string.starts_with(b.request.path, "//")
    })

  let restful_bonus = case has_restful_paths {
    True -> 10
    False -> 0
  }

  let score = feature_score + tag_score + restful_bonus

  let reasoning = case score {
    s if s >= 80 -> "Excellent naming and organization with tags"
    s if s >= 60 -> "Good structure but could improve tagging"
    s if s >= 40 -> "Basic organization, needs better naming"
    _ -> "Poor discoverability - improve naming and add tags"
  }

  let issues = case score {
    s if s < 60 ->
      []
      |> list.append(case features == [] || list.length(features) < 2 {
        True -> ["Limited feature organization"]
        False -> []
      })
      |> list.append(case behaviors_with_tags == 0 {
        True -> ["No behavior tags for organization"]
        False -> []
      })
    _ -> []
  }

  DimensionScore(score: score, reasoning: reasoning, issues: issues)
}

fn analyze_yet_complete(spec: Spec) -> DimensionScore {
  let mut_score = 0

  // +25 if all features have behaviors
  let features_complete =
    spec.features
    |> list.all(fn(f) { f.behaviors != [] })

  let score = case features_complete && spec.features != [] {
    True -> mut_score + 25
    False -> mut_score
  }

  // +25 if rules defined
  let score = case spec.rules != [] {
    True -> score + 25
    False -> score
  }

  // +25 if ai_hints complete (check multiple fields)
  let ai_hints_complete =
    get_ai_hints(spec).implementation.suggested_stack != []
    || dict.size(get_ai_hints(spec).entities) > 0
    || get_ai_hints(spec).pitfalls != []

  let score = case ai_hints_complete {
    True -> score + 25
    False -> score
  }

  // +25 if config looks complete
  let config_complete = string.length(spec.config.base_url) > 0

  let score = case config_complete {
    True -> score + 25
    False -> score
  }

  let reasoning = case score {
    100 -> "Fully complete and ready to implement"
    s if s >= 75 -> "Nearly complete, minor gaps remain"
    s if s >= 50 -> "Core elements present but missing details"
    _ -> "Incomplete specification - multiple sections need work"
  }

  let issues = case score {
    s if s < 75 ->
      []
      |> list.append(case features_complete {
        False -> ["Features without behaviors"]
        True -> []
      })
      |> list.append(case spec.rules == [] {
        True -> ["No rules defined"]
        False -> []
      })
      |> list.append(case ai_hints_complete {
        False -> ["AI hints incomplete"]
        True -> []
      })
    _ -> []
  }

  DimensionScore(score: score, reasoning: reasoning, issues: issues)
}

// =============================================================================
// OVERALL CALCULATION
// =============================================================================

fn calculate_overall_readiness(
  replacement: Int,
  empathy: Int,
  actionable: Int,
  discoverable: Int,
  yet_complete: Int,
) -> Int {
  // Weighted average: R=25%, E=20%, A=20%, D=15%, Y=20%
  let weighted =
    int.to_float(replacement)
    *. 0.25
    +. int.to_float(empathy)
    *. 0.2
    +. int.to_float(actionable)
    *. 0.2
    +. int.to_float(discoverable)
    *. 0.15
    +. int.to_float(yet_complete)
    *. 0.2

  float.round(weighted)
}

// =============================================================================
// BLOCKER GENERATION
// =============================================================================

fn generate_blockers(report: ReadyReport) -> List(Blocker) {
  let mut_blockers = []

  // Check overall readiness
  let blockers = case report.overall_readiness {
    r if r < 70 -> [
      Blocker(
        severity: Critical,
        description: "Overall readiness below 70% - not ready for production",
        affected_areas: ["all_dimensions"],
      ),
      ..mut_blockers
    ]
    r if r < 80 -> [
      Blocker(
        severity: High,
        description: "Overall readiness below 80% - use caution",
        affected_areas: ["all_dimensions"],
      ),
      ..mut_blockers
    ]
    _ -> mut_blockers
  }

  // Check each dimension
  let blockers =
    blockers
    |> add_dimension_blocker(report.replacement, "Replacement", [
      "vision",
      "audience",
    ])
    |> add_dimension_blocker(report.empathy, "Empathy", [
      "error_handling",
      "ux",
    ])
    |> add_dimension_blocker(report.actionable, "Actionable", [
      "response_checks",
      "error_messages",
    ])
    |> add_dimension_blocker(report.discoverable, "Discoverable", [
      "naming",
      "organization",
    ])
    |> add_dimension_blocker(report.yet_complete, "Yet-complete", [
      "completeness",
      "implementation",
    ])

  blockers
}

fn add_dimension_blocker(
  blockers: List(Blocker),
  dimension: DimensionScore,
  dimension_name: String,
  affected_areas: List(String),
) -> List(Blocker) {
  case dimension.score {
    s if s < 40 -> [
      Blocker(
        severity: Critical,
        description: dimension_name
          <> " score critically low: "
          <> dimension.reasoning,
        affected_areas: affected_areas,
      ),
      ..blockers
    ]
    s if s < 60 -> [
      Blocker(
        severity: High,
        description: dimension_name
          <> " needs improvement: "
          <> dimension.reasoning,
        affected_areas: affected_areas,
      ),
      ..blockers
    ]
    s if s < 75 -> [
      Blocker(
        severity: Medium,
        description: dimension_name
          <> " could be stronger: "
          <> dimension.reasoning,
        affected_areas: affected_areas,
      ),
      ..blockers
    ]
    _ -> blockers
  }
}

// =============================================================================
// RECOMMENDATION GENERATION
// =============================================================================

fn generate_recommendations(report: ReadyReport) -> List(Recommendation) {
  let mut_recs = []

  // Generate recommendations based on dimension scores
  let recs =
    mut_recs
    |> add_dimension_recommendations(report.replacement, "Replacement", [
      "Clarify value proposition and target audience",
      "Add specific, measurable success criteria",
      "Detail the unique advantages over alternatives",
    ])
    |> add_dimension_recommendations(report.empathy, "Empathy", [
      "Add error handling behaviors for common failure scenarios",
      "Document anti-patterns to avoid",
      "Include validation behaviors",
    ])
    |> add_dimension_recommendations(report.actionable, "Actionable", [
      "Add response checks with clear guidance",
      "Include detailed 'why' explanations in checks",
      "Make error messages actionable",
    ])
    |> add_dimension_recommendations(report.discoverable, "Discoverable", [
      "Use consistent, descriptive naming",
      "Add tags to behaviors for organization",
      "Follow RESTful path conventions",
    ])
    |> add_dimension_recommendations(report.yet_complete, "Yet-complete", [
      "Ensure all features have behaviors",
      "Define validation rules",
      "Complete AI hints section",
    ])

  // Sort by priority
  list.sort(recs, fn(a, b) {
    case a.priority < b.priority {
      True -> order.Lt
      False ->
        case a.priority > b.priority {
          True -> order.Gt
          False -> order.Eq
        }
    }
  })
}

fn add_dimension_recommendations(
  recommendations: List(Recommendation),
  dimension: DimensionScore,
  dimension_name: String,
  suggestions: List(String),
) -> List(Recommendation) {
  case dimension.score {
    s if s < 40 -> {
      // Priority 1-2 for critical issues
      let new_recs =
        suggestions
        |> list.index_map(fn(sugg, idx) {
          Recommendation(
            priority: 1 + idx,
            description: sugg,
            rationale: dimension_name
              <> " score critically low ("
              <> int.to_string(s)
              <> "%)",
          )
        })
      list.append(recommendations, new_recs)
    }
    s if s < 60 -> {
      // Priority 3-4 for high issues
      let new_recs =
        suggestions
        |> list.take(2)
        |> list.index_map(fn(sugg, idx) {
          Recommendation(
            priority: 3 + idx,
            description: sugg,
            rationale: dimension_name
              <> " needs improvement ("
              <> int.to_string(s)
              <> "%)",
          )
        })
      list.append(recommendations, new_recs)
    }
    s if s < 75 -> {
      // Priority 5-6 for medium issues
      let new_rec =
        Recommendation(
          priority: 5,
          description: list.first(suggestions)
            |> result.unwrap("Improve " <> dimension_name),
          rationale: dimension_name
            <> " could be stronger ("
            <> int.to_string(s)
            <> "%)",
        )
      [new_rec, ..recommendations]
    }
    _ -> recommendations
  }
}

// =============================================================================
// HELPERS
// =============================================================================

fn get_all_behaviors(spec: Spec) -> List(Behavior) {
  spec.features
  |> list.flat_map(fn(f) { f.behaviors })
}

// =============================================================================
// FORMATTING
// =============================================================================

fn format_dimension(
  label: String,
  score: Int,
  reasoning: String,
  issues: List(String),
) -> String {
  let icon = case score {
    s if s >= 80 -> "✅"
    s if s >= 60 -> "⚠️ "
    _ -> "❌"
  }

  let header = label <> ": " <> int.to_string(score) <> " " <> icon <> "\n"
  let reason_line = "  " <> reasoning <> "\n"

  let issues_text = case list.is_empty(issues) {
    True -> ""
    False -> {
      let formatted_issues =
        issues
        |> list.map(fn(i) { "  • " <> i })
        |> string.join("\n")
      formatted_issues <> "\n"
    }
  }

  header <> reason_line <> issues_text <> "\n"
}

fn format_blockers(blockers: List(Blocker)) -> String {
  case list.is_empty(blockers) {
    True -> "🚧 Blockers: None\n\n"
    False -> {
      let count_str = int.to_string(list.length(blockers))
      let header = "🚧 Blockers (" <> count_str <> "):\n"

      let formatted =
        blockers
        |> list.map(fn(b) {
          let sev = severity_label(b.severity)
          let areas = string.join(b.affected_areas, ", ")
          "  ["
          <> sev
          <> "] "
          <> b.description
          <> "\n"
          <> "      Affected: "
          <> areas
        })
        |> string.join("\n")

      header <> formatted <> "\n\n"
    }
  }
}

fn format_recommendations(recommendations: List(Recommendation)) -> String {
  case list.is_empty(recommendations) {
    True -> "💡 Recommendations: None\n"
    False -> {
      let count_str = int.to_string(list.length(recommendations))
      let header = "💡 Recommendations (" <> count_str <> "):\n"

      let formatted =
        recommendations
        |> list.take(5)
        |> list.map(fn(r) {
          let priority_str = "P" <> int.to_string(r.priority)
          "  ["
          <> priority_str
          <> "] "
          <> r.description
          <> "\n"
          <> "      "
          <> r.rationale
        })
        |> string.join("\n")

      header <> formatted <> "\n"
    }
  }
}

fn severity_label(severity: BlockerSeverity) -> String {
  case severity {
    Critical -> "CRITICAL"
    High -> "HIGH"
    Medium -> "MEDIUM"
    Low -> "LOW"
  }
}

// Required imports
import gleam/order
import gleam/result
