/// Vision Alignment Checker - Detects drift from Phase 1 (Vision) to Phase 4 (Spec)
///
/// This module compares the original vision (persona, north star, VORP, scope)
/// against the final spec to detect alignment issues and drift.
///
/// Part of the 4-phase planning system:
/// - Phase 1: Vision (DDD) - What & Why
/// - Phase 2: Shape (MVP) - Minimum viable scope
/// - Phase 3: Spec (KIRK) - Detailed specification
/// - Phase 4: Ready (Ship) - Readiness assessment <- Vision alignment check happens here
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import intent/planning_types.{type DimensionScore, DimensionScore}
import intent/types.{type Spec}
import intent/vision_types.{type Scenario, type VisionSection}

// =============================================================================
// Types
// =============================================================================

/// Complete alignment report comparing vision to spec
pub type AlignmentReport {
  AlignmentReport(
    persona_alignment: DimensionScore,
    north_star_alignment: DimensionScore,
    scope_integrity: DimensionScore,
    vorp_delivery: DimensionScore,
    overall_alignment: Int,
    recommendations: List(String),
  )
}

// =============================================================================
// Main Entry Point
// =============================================================================

/// Analyze alignment between vision and spec
/// Returns comprehensive report with dimension scores and recommendations
pub fn analyze_alignment(vision: VisionSection, spec: Spec) -> AlignmentReport {
  let persona = check_persona_alignment(vision, spec)
  let north_star = check_north_star_alignment(vision, spec)
  let scope = check_scope_integrity(vision, spec)
  let vorp = check_vorp_delivery(vision, spec)

  let overall =
    { persona.score + north_star.score + scope.score + vorp.score } / 4

  let recommendations =
    generate_recommendations(persona, north_star, scope, vorp)

  AlignmentReport(
    persona_alignment: persona,
    north_star_alignment: north_star,
    scope_integrity: scope,
    vorp_delivery: vorp,
    overall_alignment: overall,
    recommendations: recommendations,
  )
}

// =============================================================================
// Dimension Checkers
// =============================================================================

/// Check if spec audience aligns with vision persona
pub fn check_persona_alignment(
  vision: VisionSection,
  spec: Spec,
) -> DimensionScore {
  let vision_persona = string.lowercase(vision.persona)
  let spec_audience = string.lowercase(spec.audience)

  case vision_persona == spec_audience {
    True ->
      DimensionScore(
        score: 100,
        reasoning: "Persona and audience are identical",
        issues: [],
      )
    False -> {
      let similarity = calculate_similarity(vision_persona, spec_audience)

      case similarity {
        s if s >= 80 ->
          DimensionScore(
            score: s,
            reasoning: "High similarity in target audience keywords",
            issues: [],
          )
        s if s >= 50 ->
          DimensionScore(
            score: s,
            reasoning: "Moderate overlap between persona and audience",
            issues: [
              "Vision persona: " <> vision.persona,
              "Spec audience: " <> spec.audience,
            ],
          )
        s ->
          DimensionScore(
            score: s,
            reasoning: "Significant drift between vision persona and spec audience",
            issues: [
              "Vision persona: " <> vision.persona,
              "Spec audience: " <> spec.audience,
              "Consider revising spec audience to match vision persona",
            ],
          )
      }
    }
  }
}

/// Check if spec success criteria cover the vision north star
pub fn check_north_star_alignment(
  vision: VisionSection,
  spec: Spec,
) -> DimensionScore {
  case list.length(spec.success_criteria) {
    0 ->
      DimensionScore(
        score: 0,
        reasoning: "No success criteria defined in spec",
        issues: ["Add success criteria that align with: " <> vision.north_star],
      )
    _ -> {
      let north_star_lower = string.lowercase(vision.north_star)
      let criteria_text =
        spec.success_criteria
        |> list.map(string.lowercase)
        |> string.join(" ")

      let coverage = calculate_goal_coverage(north_star_lower, criteria_text)

      case coverage {
        c if c >= 90 ->
          DimensionScore(
            score: c,
            reasoning: "Success criteria fully cover north star goals",
            issues: [],
          )
        c if c >= 60 ->
          DimensionScore(
            score: c,
            reasoning: "Success criteria partially cover north star goals",
            issues: [
              "North star: " <> vision.north_star,
              "Some aspects may not be fully addressed in success criteria",
            ],
          )
        c ->
          DimensionScore(
            score: c,
            reasoning: "Success criteria diverge from north star goals",
            issues: [
              "North star: " <> vision.north_star,
              "Success criteria don't adequately reflect this vision",
              "Review and align success criteria with north star",
            ],
          )
      }
    }
  }
}

/// Check scope integrity - detect creep and reduction
pub fn check_scope_integrity(
  vision: VisionSection,
  spec: Spec,
) -> DimensionScore {
  let scenario_keywords = extract_scenario_keywords(vision.scenarios)
  let out_of_scope_lower =
    vision.out_of_scope
    |> list.map(string.lowercase)

  // Extract keywords from out_of_scope items to catch variations
  let oos_keywords =
    out_of_scope_lower
    |> list.flat_map(extract_keywords)
    |> list.unique

  let creep_features =
    spec.features
    |> list.filter(fn(feature) {
      let feature_name_lower = string.lowercase(feature.name)
      let feature_desc_lower = string.lowercase(feature.description)
      let feature_keywords = extract_keywords(feature_name_lower <> " " <> feature_desc_lower)

      // Check if feature contains out-of-scope keywords
      let is_out_of_scope =
        list.any(oos_keywords, fn(oos_kw) {
          list.contains(feature_keywords, oos_kw)
        })

      is_out_of_scope
    })

  let feature_names_lower =
    spec.features
    |> list.map(fn(f) { string.lowercase(f.name <> " " <> f.description) })
    |> string.join(" ")

  // Check if we have no features at all - that's a major reduction
  let has_features = list.length(spec.features) > 0

  let uncovered_scenarios =
    vision.scenarios
    |> list.filter(fn(scenario) {
      let scenario_text =
        string.lowercase(scenario.motivation <> " " <> scenario.outcome)
      let keywords = extract_keywords(scenario_text)

      !list.any(keywords, fn(keyword) {
        string.contains(feature_names_lower, keyword)
      })
    })

  let creep_count = list.length(creep_features)
  let reduction_count = list.length(uncovered_scenarios)

  // Apply heavier penalty for empty features
  let base_penalty = { creep_count + reduction_count } * 15
  let empty_penalty = case has_features {
    True -> 0
    False -> 50  // Major penalty for no features when scenarios exist
  }
  let total_penalty = base_penalty + empty_penalty
  let score = int.max(0, 100 - total_penalty)

  let issues = case creep_count, reduction_count {
    0, 0 -> case has_features {
      True -> []
      False -> ["No features defined - missing scenario coverage"]
    }
    c, 0 -> [
      int.to_string(c)
      <> " feature(s) may be out of scope or not justified by scenarios",
    ]
    0, r -> [
      int.to_string(r) <> " scenario(s) not covered by spec features",
    ]
    c, r -> [
      int.to_string(c) <> " feature(s) may be scope creep",
      int.to_string(r) <> " scenario(s) not covered",
    ]
  }

  let reasoning = case creep_count, reduction_count {
    0, 0 -> case has_features {
      True -> "Perfect scope alignment - all scenarios covered, no creep"
      False -> "No features defined - significant scope reduction"
    }
    _, 0 -> "Potential scope creep detected"
    0, _ -> "Some scenarios not covered by features"
    _, _ -> "Both scope creep and reduction detected"
  }

  DimensionScore(score: score, reasoning: reasoning, issues: issues)
}

/// Check if spec delivers on the vision VORP (Value Over Replacement)
pub fn check_vorp_delivery(vision: VisionSection, spec: Spec) -> DimensionScore {
  let vorp_keywords = extract_keywords(string.lowercase(vision.vorp))

  let spec_text =
    string.lowercase(
      spec.description
      <> " "
      <> list.map(spec.features, fn(f) { f.name <> " " <> f.description })
      |> string.join(" ")
      <> " "
      <> string.join(spec.ai_hints.implementation.suggested_stack, " "),
    )

  let found_keywords =
    list.filter(vorp_keywords, fn(keyword) {
      string.contains(spec_text, keyword)
    })

  let found_count = list.length(found_keywords)
  let total_count = list.length(vorp_keywords)

  // Be more generous with scoring - 50% match should be good
  let score = case total_count {
    0 -> 50
    _ -> {
      let raw_score = { found_count * 100 } / total_count
      // Boost score if at least half the keywords match
      case found_count >= { total_count / 2 } {
        True -> int.min(100, raw_score + 20)
        False -> raw_score
      }
    }
  }

  case score {
    s if s >= 70 ->
      DimensionScore(
        score: s,
        reasoning: "Spec delivers on VORP differentiation points",
        issues: [],
      )
    s if s >= 40 ->
      DimensionScore(
        score: s,
        reasoning: "Spec partially delivers on VORP",
        issues: ["Consider emphasizing: " <> vision.vorp],
      )
    s ->
      DimensionScore(
        score: s,
        reasoning: "Spec doesn't clearly deliver on VORP promise",
        issues: [
          "VORP promise: " <> vision.vorp,
          "Ensure spec features and implementation emphasize differentiation",
        ],
      )
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

fn calculate_similarity(s1: String, s2: String) -> Int {
  let words1 = extract_keywords(s1)
  let words2 = extract_keywords(s2)

  case list.length(words1), list.length(words2) {
    0, _ -> 0
    _, 0 -> 0
    _, _ -> {
      let common =
        list.filter(words1, fn(w) { list.contains(words2, w) })
        |> list.length

      // Use min length as base for scoring - this rewards high overlap on shorter text
      let min_length = int.min(list.length(words1), list.length(words2))
      let raw_score = { common * 100 } / min_length

      // Calculate percentage overlap
      let overlap_pct = { common * 100 } / int.max(list.length(words1), list.length(words2))

      // Boost score if we have good keyword overlap
      // If more than 50% overlap, we're in good territory
      case overlap_pct >= 50, common {
        True, c if c >= 3 -> int.min(100, raw_score + 20)
        True, c if c >= 2 -> int.min(100, raw_score + 15)
        False, c if c >= 4 -> int.min(100, raw_score + 15)
        False, c if c >= 3 -> int.min(100, raw_score + 12)
        False, c if c >= 2 -> int.min(100, raw_score + 8)
        _, _ -> raw_score
      }
    }
  }
}

fn calculate_goal_coverage(north_star: String, criteria: String) -> Int {
  let goal_keywords = extract_keywords(north_star)

  case list.length(goal_keywords) {
    0 -> 50
    _ -> {
      let covered =
        list.filter(goal_keywords, fn(keyword) {
          string.contains(criteria, keyword)
        })
        |> list.length

      { covered * 100 } / list.length(goal_keywords)
    }
  }
}

fn extract_keywords(text: String) -> List(String) {
  let stopwords = [
    "the", "and", "for", "with", "that", "this", "from", "are", "but", "has",
    "was", "will", "been", "have",
  ]

  // Split on both spaces and hyphens to catch compound words
  let words =
    string.split(text, " ")
    |> list.flat_map(fn(word) {
      string.split(word, "-")
    })

  words
  |> list.map(fn(word) {
    string.trim(word)
    |> string.replace(",", "")
    |> string.replace(".", "")
    |> string.replace("(", "")
    |> string.replace(")", "")
  })
  |> list.filter(fn(word) { string.length(word) >= 3 })
  |> list.filter(fn(word) { !list.contains(stopwords, word) })
  |> list.unique
}

fn extract_scenario_keywords(scenarios: List(Scenario)) -> List(String) {
  scenarios
  |> list.flat_map(fn(scenario) {
    let text = string.lowercase(scenario.motivation <> " " <> scenario.outcome)
    extract_keywords(text)
  })
  |> list.unique
}

fn generate_recommendations(
  persona: DimensionScore,
  north_star: DimensionScore,
  scope: DimensionScore,
  vorp: DimensionScore,
) -> List(String) {
  let threshold = 70

  let recs = []

  let recs = case persona.score < threshold {
    True ->
      list.prepend(
        recs,
        "Revise spec audience to better align with vision persona",
      )
    False -> recs
  }

  let recs = case north_star.score < threshold {
    True ->
      list.prepend(
        recs,
        "Update success criteria to better reflect north star goals",
      )
    False -> recs
  }

  let recs = case scope.score < threshold {
    True ->
      list.prepend(
        recs,
        "Review features for scope creep or missing scenario coverage",
      )
    False -> recs
  }

  let recs = case vorp.score < threshold {
    True ->
      list.prepend(
        recs,
        "Strengthen differentiation points from VORP in spec implementation",
      )
    False -> recs
  }

  list.reverse(recs)
}

// =============================================================================
// JSON Serialization
// =============================================================================

fn dimension_score_to_json(score: DimensionScore) -> json.Json {
  json.object([
    #("score", json.int(score.score)),
    #("reasoning", json.string(score.reasoning)),
    #("issues", json.array(score.issues, json.string)),
  ])
}

pub fn alignment_report_to_json(report: AlignmentReport) -> json.Json {
  json.object([
    #("persona_alignment", dimension_score_to_json(report.persona_alignment)),
    #(
      "north_star_alignment",
      dimension_score_to_json(report.north_star_alignment),
    ),
    #("scope_integrity", dimension_score_to_json(report.scope_integrity)),
    #("vorp_delivery", dimension_score_to_json(report.vorp_delivery)),
    #("overall_alignment", json.int(report.overall_alignment)),
    #("recommendations", json.array(report.recommendations, json.string)),
  ])
}
