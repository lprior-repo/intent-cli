//// VORP (Value Over Replacement) Analyzer
////
//// Evaluates how well a specification demonstrates value over existing solutions.
//// Scores across 5 dimensions:
//// - Audience clarity (0-25): Is target audience and replacement context clear?
//// - Differentiation (0-25): Does description highlight unique advantages?
//// - Measurable criteria (0-25): Are success criteria quantified?
//// - Implementation edge (0-15): Do AI hints show technical advantages?
//// - Anti-pattern awareness (0-10): Does it document pitfalls of old solutions?

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/planning_types.{type DimensionScore, DimensionScore}
import intent/types.{type Spec}

// =============================================================================
// PUBLIC API
// =============================================================================

/// Analyze a spec's VORP (Value Over Replacement) strength
/// Returns a DimensionScore (0-100) with reasoning and issues
pub fn analyze_vorp(spec: Spec) -> DimensionScore {
  let audience_score = calculate_audience_score(spec)
  let differentiation_score = calculate_differentiation_score(spec)
  let criteria_score = calculate_criteria_score(spec)
  let implementation_score = calculate_implementation_edge(spec)
  let antipattern_score = calculate_antipattern_awareness(spec)

  let total_score =
    audience_score
    + differentiation_score
    + criteria_score
    + implementation_score
    + antipattern_score

  let reasoning = generate_reasoning(total_score)
  let issues = generate_issues(total_score, spec)

  DimensionScore(score: total_score, reasoning: reasoning, issues: issues)
}

// =============================================================================
// SCORING COMPONENTS
// =============================================================================

/// Calculate audience clarity score (0-25)
/// Checks if audience field provides clear replacement context
fn calculate_audience_score(spec: Spec) -> Int {
  let audience_len = string.length(spec.audience)

  case audience_len {
    0 -> 0
    len if len < 20 -> 10
    // Too vague
    _ -> {
      // Check for replacement context keywords
      let lower_audience = string.lowercase(spec.audience)
      let has_replacement_context =
        string.contains(lower_audience, "replace")
        || string.contains(lower_audience, "instead of")
        || string.contains(lower_audience, "better than")
        || string.contains(lower_audience, "vs")
        || string.contains(lower_audience, "alternative to")

      case has_replacement_context {
        True -> 25
        False -> 15
      }
    }
  }
}

/// Calculate differentiation score (0-25)
/// Checks if description highlights unique advantages
fn calculate_differentiation_score(spec: Spec) -> Int {
  let desc_len = string.length(spec.description)

  case desc_len {
    0 -> 0
    len if len < 30 -> 5
    len if len < 50 -> 10
    _ -> {
      // Check for differentiation keywords
      let lower_desc = string.lowercase(spec.description)
      let matches =
        [
          string.contains(lower_desc, "10x"),
          string.contains(lower_desc, "100x"),
          string.contains(lower_desc, "unique"),
          string.contains(lower_desc, "novel"),
          string.contains(lower_desc, "deterministic"),
          string.contains(lower_desc, "guarantee"),
          string.contains(lower_desc, "contract"),
        ]
        |> list.filter(fn(x) { x })

      case matches {
        [] -> 15
        [_] -> 20
        _ -> 25
      }
    }
  }
}

/// Calculate measurable criteria score (0-25)
/// Checks if success criteria include quantified metrics
fn calculate_criteria_score(spec: Spec) -> Int {
  let criteria_count = list.length(spec.success_criteria)

  case criteria_count {
    0 -> 0
    1 -> 8
    2 -> 12
    _ -> {
      // Check for metrics in criteria
      let has_metrics =
        spec.success_criteria
        |> list.any(fn(criterion) {
          let lower = string.lowercase(criterion)
          string.contains(lower, "10x")
          || string.contains(lower, "100x")
          || string.contains(lower, "%")
          || string.contains(lower, "faster")
          || string.contains(lower, "fewer")
          || string.contains(lower, "reduction")
          || has_numbers(criterion)
        })

      case has_metrics {
        True -> 25
        False -> 15
      }
    }
  }
}

/// Calculate implementation edge score (0-15)
/// Checks if AI hints show technical advantages
fn calculate_implementation_edge(spec: Spec) -> Int {
  let stack_score = case spec.ai_hints {
    Some(hints) ->
      case hints.implementation.suggested_stack {
        [] -> 0
        [_] -> 5
        _ -> 8
      }
    None -> 0
  }

  let pitfalls_score = case spec.ai_hints {
    Some(hints) ->
      case hints.pitfalls {
        [] -> 0
        [_] -> 3
        _ -> 7
      }
    None -> 0
  }

  stack_score + pitfalls_score
}

/// Calculate anti-pattern awareness score (0-10)
/// Checks if spec documents pitfalls of old solutions
fn calculate_antipattern_awareness(spec: Spec) -> Int {
  case spec.anti_patterns {
    [] -> 0
    [_] -> 5
    _ -> 10
  }
}

// =============================================================================
// REASONING AND ISSUES
// =============================================================================

/// Generate reasoning based on total score
fn generate_reasoning(score: Int) -> String {
  case score {
    s if s >= 90 ->
      "Excellent VORP - clear differentiation with quantified improvements"
    s if s >= 75 ->
      "Strong VORP - good value proposition with measurable benefits"
    s if s >= 60 ->
      "Moderate VORP - value proposition present but could be stronger"
    s if s >= 40 -> "Weak VORP - differentiation unclear or not quantified"
    _ -> "Poor VORP - lacks clear value over existing solutions"
  }
}

/// Generate list of issues based on score and spec analysis
fn generate_issues(score: Int, spec: Spec) -> List(String) {
  let mut_issues = []

  // Check audience
  let issues = case string.length(spec.audience) {
    0 -> [
      "Audience is empty - define target users and what they replace",
      ..mut_issues
    ]
    len if len < 20 -> [
      "Audience too vague - specify what users currently use",
      ..mut_issues
    ]
    _ -> {
      let lower = string.lowercase(spec.audience)
      case
        string.contains(lower, "replace")
        || string.contains(lower, "instead of")
        || string.contains(lower, "vs")
      {
        True -> mut_issues
        False -> ["Audience lacks replacement context", ..mut_issues]
      }
    }
  }

  // Check description
  let issues = case string.length(spec.description) {
    0 -> ["Description is empty", ..issues]
    len if len < 50 -> [
      "Description too brief - explain differentiation",
      ..issues
    ]
    _ -> issues
  }

  // Check success criteria
  let issues = case spec.success_criteria {
    [] -> ["No success criteria defined", ..issues]
    [_] | [_, _] -> ["Add more success criteria (< 3)", ..issues]
    criteria -> {
      let has_metrics =
        criteria
        |> list.any(fn(c) {
          let lower = string.lowercase(c)
          string.contains(lower, "10x")
          || string.contains(lower, "%")
          || has_numbers(c)
        })

      case has_metrics {
        True -> issues
        False -> ["Success criteria lack quantified metrics", ..issues]
      }
    }
  }

  // Check AI hints
  let issues = case spec.ai_hints {
    Some(hints) ->
      case hints.implementation.suggested_stack {
        [] -> ["AI hints empty - specify technical advantages", ..issues]
        _ -> issues
      }
    None -> ["AI hints empty - specify technical advantages", ..issues]
  }

  // Check anti-patterns
  let issues = case spec.anti_patterns {
    [] -> ["No anti-patterns documented", ..issues]
    _ -> issues
  }

  // Return empty list if score is high enough
  case score {
    s if s >= 75 -> []
    _ -> list.reverse(issues)
  }
}

// =============================================================================
// HELPERS
// =============================================================================

/// Check if a string contains numbers (for metric detection)
fn has_numbers(text: String) -> Bool {
  text
  |> string.to_graphemes
  |> list.any(fn(char) {
    case char {
      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
      _ -> False
    }
  })
}
