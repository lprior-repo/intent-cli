//// KIRK → Enhanced Bead Transformers
////
//// Transforms KIRK analysis findings into enhanced beads with full traceability.
//// Each KIRK command produces findings that become actionable work units.

import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string
import intent/enhanced_bead_generator.{
  type EnhancedBead, AcceptanceCriterion, EnhancedBead, KirkSource, TestCase,
}
import intent/kirk/coverage_analyzer.{type CoverageReport}
import intent/kirk/effects_analyzer.{type EffectsReport}
import intent/kirk/gap_detector.{type GapReport}
import intent/kirk/inversion_checker.{type InversionReport}
import intent/kirk/ready
import intent/planning_types.{type ReadyReport}
import intent/quality_analyzer.{type QualityReport}
import intent/types.{type Spec}

// =============================================================================
// QUALITY → BEADS
// =============================================================================

pub fn quality_to_beads(
  report: QualityReport,
  spec_path: String,
) -> List(EnhancedBead) {
  report.issues
  |> list.index_map(fn(issue, i) {
    let issue_str = quality_analyzer.format_issue(issue)
    let severity = quality_issue_severity(issue)
    let category = quality_issue_category(issue)

    make_kirk_bead(
      index: i + 1,
      analysis_type: "quality",
      finding_id: "quality-" <> category <> "-" <> int.to_string(i + 1),
      severity: severity,
      category: category,
      original_text: issue_str,
      suggestion: quality_issue_suggestion(issue),
      spec_path: spec_path,
      title: "Fix quality issue: " <> category,
      description: issue_str,
      issue_type: "quality_fix",
      round: 2,
      labels: ["quality", category],
    )
  })
}

fn quality_issue_severity(issue: quality_analyzer.QualityIssue) -> String {
  case issue {
    quality_analyzer.MissingErrorTests -> "high"
    quality_analyzer.MissingAuthenticationTest -> "high"
    quality_analyzer.MissingEdgeCases -> "medium"
    quality_analyzer.VagueRules -> "medium"
    quality_analyzer.NoExamples -> "low"
    quality_analyzer.MissingExplanations -> "low"
    quality_analyzer.UntestedRules -> "medium"
    quality_analyzer.MissingAIHints -> "low"
  }
}

fn quality_issue_category(issue: quality_analyzer.QualityIssue) -> String {
  case issue {
    quality_analyzer.MissingErrorTests -> "missing-error-tests"
    quality_analyzer.MissingAuthenticationTest -> "missing-auth-test"
    quality_analyzer.MissingEdgeCases -> "missing-edge-cases"
    quality_analyzer.VagueRules -> "vague-rules"
    quality_analyzer.NoExamples -> "no-examples"
    quality_analyzer.MissingExplanations -> "missing-explanations"
    quality_analyzer.UntestedRules -> "untested-rules"
    quality_analyzer.MissingAIHints -> "missing-ai-hints"
  }
}

fn quality_issue_suggestion(
  issue: quality_analyzer.QualityIssue,
) -> Option(String) {
  case issue {
    quality_analyzer.MissingErrorTests ->
      Some("Add behaviors testing 4xx and 5xx status codes")
    quality_analyzer.MissingAuthenticationTest ->
      Some("Add behaviors testing authentication flows")
    quality_analyzer.MissingEdgeCases ->
      Some("Add edge case behaviors (empty lists, large payloads, etc)")
    quality_analyzer.VagueRules ->
      Some("Make check rules more specific with concrete assertions")
    quality_analyzer.NoExamples ->
      Some("Add response.example to behaviors for AI context")
    quality_analyzer.MissingExplanations ->
      Some("Add 'why' explanations to all checks")
    quality_analyzer.UntestedRules ->
      Some("Ensure all rules have corresponding behavior checks")
    quality_analyzer.MissingAIHints ->
      Some("Add ai_hints section with implementation guidance")
  }
}

// =============================================================================
// COVERAGE → BEADS
// =============================================================================

pub fn coverage_to_beads(
  report: CoverageReport,
  spec_path: String,
) -> List(EnhancedBead) {
  let owasp_beads =
    report.owasp.missing
    |> list.index_map(fn(missing_category, i) {
      make_kirk_bead(
        index: i + 1,
        analysis_type: "coverage",
        finding_id: "coverage-owasp-" <> int.to_string(i + 1),
        severity: "high",
        category: "owasp-gap",
        original_text: "Missing OWASP coverage: " <> missing_category,
        suggestion: Some(
          "Add behaviors testing " <> missing_category <> " scenarios",
        ),
        spec_path: spec_path,
        title: "Add OWASP coverage: " <> missing_category,
        description: "Missing test coverage for OWASP category: "
          <> missing_category,
        issue_type: "security_coverage",
        round: 3,
        labels: ["security", "owasp", "coverage"],
      )
    })

  let edge_beads =
    report.edge_cases.suggested
    |> list.index_map(fn(edge_case, i) {
      let offset = list.length(owasp_beads)
      make_kirk_bead(
        index: offset + i + 1,
        analysis_type: "coverage",
        finding_id: "coverage-edge-" <> int.to_string(i + 1),
        severity: "medium",
        category: "edge-case",
        original_text: "Missing edge case test: " <> edge_case,
        suggestion: Some("Add behavior testing " <> edge_case),
        spec_path: spec_path,
        title: "Add edge case test: " <> edge_case,
        description: "Missing edge case coverage for: " <> edge_case,
        issue_type: "edge_case_coverage",
        round: 3,
        labels: ["coverage", "edge-case"],
      )
    })

  list.concat([owasp_beads, edge_beads])
}

// =============================================================================
// GAPS → BEADS
// =============================================================================

pub fn gaps_to_beads(report: GapReport, spec_path: String) -> List(EnhancedBead) {
  let all_gaps =
    list.concat([
      report.inversion_gaps,
      report.second_order_gaps,
      report.checklist_gaps,
      report.coverage_gaps,
      report.security_gaps,
    ])

  all_gaps
  |> list.index_map(fn(gap, i) {
    let severity = gap_severity_to_string(gap.severity)
    let round = gap_mental_model_to_round(gap.mental_model)

    make_kirk_bead(
      index: i + 1,
      analysis_type: "gaps",
      finding_id: "gap-"
        <> gap_type_to_string(gap.gap_type)
        <> "-"
        <> int.to_string(i + 1),
      severity: severity,
      category: gap_type_to_string(gap.gap_type),
      original_text: gap.description,
      suggestion: Some(gap.suggestion),
      spec_path: spec_path,
      title: "Fill gap: " <> gap.description,
      description: gap.description
        <> " (mental model: "
        <> gap.mental_model
        <> ")",
      issue_type: "gap_fill",
      round: round,
      labels: ["gap", gap_type_to_string(gap.gap_type)],
    )
  })
}

fn gap_severity_to_string(severity: gap_detector.GapSeverity) -> String {
  case severity {
    gap_detector.Critical -> "critical"
    gap_detector.High -> "high"
    gap_detector.Medium -> "medium"
    gap_detector.Low -> "low"
  }
}

fn gap_type_to_string(gap_type: gap_detector.GapType) -> String {
  case gap_type {
    gap_detector.InversionGap -> "inversion"
    gap_detector.SecondOrderGap -> "second-order"
    gap_detector.ChecklistGap -> "checklist"
    gap_detector.CoverageGap -> "coverage"
    gap_detector.SecurityGap -> "security"
  }
}

fn gap_mental_model_to_round(model: String) -> Int {
  case string.lowercase(model) {
    "inversion" -> 3
    "second-order" | "second_order" | "effects" -> 4
    "checklist" -> 2
    "coverage" -> 2
    "security" -> 3
    _ -> 2
  }
}

// =============================================================================
// INVERSIONS → BEADS
// =============================================================================

pub fn inversions_to_beads(
  report: InversionReport,
  spec_path: String,
) -> List(EnhancedBead) {
  let all_gaps =
    list.concat([
      report.security_gaps
        |> list.map(fn(g) { #("security", g) }),
      report.usability_gaps
        |> list.map(fn(g) { #("usability", g) }),
      report.integration_gaps
        |> list.map(fn(g) { #("integration", g) }),
    ])

  all_gaps
  |> list.index_map(fn(pair, i) {
    let #(gap_category, gap) = pair
    let severity = inversion_severity_to_string(gap.severity)

    make_kirk_bead(
      index: i + 1,
      analysis_type: "invert",
      finding_id: "invert-" <> gap.category <> "-" <> int.to_string(i + 1),
      severity: severity,
      category: gap.category,
      original_text: gap.description,
      suggestion: Some(gap.what_could_fail),
      spec_path: spec_path,
      title: "Test inversion: " <> gap.description,
      description: gap.description <> ". Risk: " <> gap.what_could_fail,
      issue_type: "test_scenario",
      round: 3,
      labels: ["inversion", gap_category, gap.category],
    )
  })
}

fn inversion_severity_to_string(
  severity: inversion_checker.GapSeverity,
) -> String {
  case severity {
    inversion_checker.Critical -> "critical"
    inversion_checker.High -> "high"
    inversion_checker.Medium -> "medium"
    inversion_checker.Low -> "low"
  }
}

// =============================================================================
// EFFECTS → BEADS
// =============================================================================

pub fn effects_to_beads(
  report: EffectsReport,
  spec_path: String,
) -> List(EnhancedBead) {
  let missing_verification_beads =
    report.behavior_effects
    |> list.flat_map(fn(be) {
      be.missing_verifications
      |> list.map(fn(mv) { #(be.behavior_name, mv) })
    })
    |> list.index_map(fn(pair, i) {
      let #(behavior_name, missing) = pair
      make_kirk_bead(
        index: i + 1,
        analysis_type: "effects",
        finding_id: "effects-verify-" <> int.to_string(i + 1),
        severity: "medium",
        category: "missing-verification",
        original_text: "Missing verification for second-order effect of "
          <> behavior_name
          <> ": "
          <> missing,
        suggestion: Some("Add a verification behavior to confirm: " <> missing),
        spec_path: spec_path,
        title: "Add verification: " <> missing,
        description: "Behavior '"
          <> behavior_name
          <> "' has unverified second-order effect: "
          <> missing,
        issue_type: "verification_needed",
        round: 4,
        labels: ["effects", "verification"],
      )
    })

  let orphan_beads =
    report.orphaned_resources
    |> list.index_map(fn(orphan, i) {
      let offset = list.length(missing_verification_beads)
      make_kirk_bead(
        index: offset + i + 1,
        analysis_type: "effects",
        finding_id: "effects-orphan-" <> int.to_string(i + 1),
        severity: "high",
        category: "orphaned-resource",
        original_text: "Orphaned "
          <> orphan.resource_type
          <> " from "
          <> orphan.caused_by
          <> ": "
          <> orphan.description,
        suggestion: Some(orphan.mitigation),
        spec_path: spec_path,
        title: "Handle orphaned resource: " <> orphan.resource_type,
        description: orphan.description <> ". Mitigation: " <> orphan.mitigation,
        issue_type: "verification_needed",
        round: 4,
        labels: ["effects", "orphan", "data-integrity"],
      )
    })

  list.concat([missing_verification_beads, orphan_beads])
}

// =============================================================================
// BEHAVIORS → BEADS
// =============================================================================

pub fn behaviors_to_beads(spec: Spec, spec_path: String) -> List(EnhancedBead) {
  spec.features
  |> list.flat_map(fn(f) { f.behaviors })
  |> list.index_map(fn(behavior, i) {
    enhanced_bead_generator.behavior_to_enhanced_bead(
      behavior,
      spec_path,
      i + 1,
    )
  })
}

// =============================================================================
// READY → BEADS
// =============================================================================

pub fn ready_to_beads(
  report: ReadyReport,
  spec_path: String,
) -> List(EnhancedBead) {
  let blocker_beads =
    report.blockers
    |> list.index_map(fn(blocker, i) {
      let severity = blocker_severity_to_string(blocker.severity)
      let areas_string = string.join(blocker.affected_areas, ", ")

      make_kirk_bead(
        index: i + 1,
        analysis_type: "ready",
        finding_id: "ready-blocker-" <> int.to_string(i + 1),
        severity: severity,
        category: "blocker",
        original_text: blocker.description
          <> " (affected areas: "
          <> areas_string
          <> ")",
        suggestion: Some(blocker.description),
        spec_path: spec_path,
        title: "Resolve blocker: " <> blocker.description,
        description: blocker.description <> " (affects: " <> areas_string <> ")",
        issue_type: "blocker_resolution",
        round: 5,
        labels: ["ready", "blocker", ..blocker.affected_areas],
      )
    })

  let recommendation_beads =
    report.recommendations
    |> list.index_map(fn(rec, i) {
      let offset = list.length(blocker_beads)
      let priority =
        enhanced_bead_generator.severity_to_priority(case rec.priority {
          1 | 2 -> "critical"
          3 | 4 -> "high"
          _ -> "medium"
        })

      EnhancedBead(
        id: enhanced_bead_generator.make_bead_id(
          "ready-recommendation",
          "improvement",
          offset + i + 1,
        ),
        title: "Implement improvement: " <> rec.description,
        description: rec.description,
        source_type: "kirk",
        kirk_sources: [
          KirkSource(
            analysis_type: "ready",
            finding_id: "ready-rec-" <> int.to_string(i + 1),
            severity: "medium",
            category: "recommendation",
            original_text: rec.description,
            suggestion: Some(rec.rationale),
          ),
        ],
        spec_path: Some(spec_path),
        behavior_name: None,
        ears_patterns: [],
        contracts: enhanced_bead_generator.empty_contracts(),
        scenarios: [
          TestCase(
            name: "Apply recommendation",
            given: ["READY analysis completed"],
            when: "Improvement implemented",
            then: rec.description <> " is addressed",
            assertion: "Readiness score improves",
          ),
        ],
        acceptance_criteria: [
          AcceptanceCriterion(
            id: "AC-001",
            description: rec.description,
            verification_type: "review",
            check_expression: None,
            verified: False,
          ),
        ],
        types_needed: [],
        effort: "20min",
        priority: priority,
        status: "pending",
        dependencies: [],
        blocks: [],
        round: 5,
        profile_type: "api",
        issue_type: "improvement",
        labels: ["ready", "recommendation"],
        ai_hints: rec.rationale,
        pitfalls: [],
      )
    })

  list.concat([blocker_beads, recommendation_beads])
}

fn blocker_severity_to_string(
  severity: planning_types.BlockerSeverity,
) -> String {
  case severity {
    planning_types.Critical -> "critical"
    planning_types.High -> "high"
    planning_types.Medium -> "medium"
    planning_types.Low -> "low"
  }
}

// =============================================================================
// AGGREGATE
// =============================================================================

pub fn generate_all_beads(spec: Spec, spec_path: String) -> List(EnhancedBead) {
  // Run all 6 KIRK analyses
  let quality_report = quality_analyzer.analyze_spec(spec)
  let coverage_report = coverage_analyzer.analyze_coverage(spec)
  let gap_report = gap_detector.detect_gaps(spec)
  let inversion_report = inversion_checker.analyze_inversions(spec)
  let effects_report = effects_analyzer.analyze_effects(spec)
  let ready_report = ready.analyze_ready(spec)

  // Transform to beads
  let behavior_beads = behaviors_to_beads(spec, spec_path)
  let quality_beads = quality_to_beads(quality_report, spec_path)
  let coverage_beads = coverage_to_beads(coverage_report, spec_path)
  let gap_beads = gaps_to_beads(gap_report, spec_path)
  let inversion_beads = inversions_to_beads(inversion_report, spec_path)
  let effects_beads = effects_to_beads(effects_report, spec_path)
  let ready_beads = ready_to_beads(ready_report, spec_path)

  // Combine all beads
  let all_beads =
    list.concat([
      behavior_beads,
      quality_beads,
      coverage_beads,
      gap_beads,
      inversion_beads,
      effects_beads,
      ready_beads,
    ])

  // Deduplicate by title similarity
  let deduplicated = deduplicate_beads(all_beads)

  // Infer dependencies from behavior.requires and spec structure
  let with_dependencies = infer_dependencies(deduplicated, spec)

  // Assign parallel groups (waves)
  let with_waves = assign_parallel_groups(with_dependencies)

  // Sort by round then priority
  list.sort(with_waves, fn(a, b) {
    case a.round == b.round {
      True -> {
        case a.priority < b.priority {
          True -> order.Lt
          False -> order.Gt
        }
      }
      False -> {
        case a.round < b.round {
          True -> order.Lt
          False -> order.Gt
        }
      }
    }
  })
}

/// Filter beads by round number
pub fn filter_by_round(
  beads: List(EnhancedBead),
  round: Int,
) -> List(EnhancedBead) {
  list.filter(beads, fn(b) { b.round == round })
}

/// Filter beads by minimum severity
pub fn filter_by_min_severity(
  beads: List(EnhancedBead),
  min_severity: String,
) -> List(EnhancedBead) {
  let min_priority = enhanced_bead_generator.severity_to_priority(min_severity)
  list.filter(beads, fn(b) { b.priority <= min_priority })
}

// =============================================================================
// DEDUPLICATION
// =============================================================================

/// Deduplicate beads by title similarity
/// Removes beads with similar titles (>80% similarity) keeping the higher priority one
pub fn deduplicate_beads(beads: List(EnhancedBead)) -> List(EnhancedBead) {
  deduplicate_loop(beads, [])
}

fn deduplicate_loop(
  remaining: List(EnhancedBead),
  acc: List(EnhancedBead),
) -> List(EnhancedBead) {
  case remaining {
    [] -> list.reverse(acc)
    [head, ..tail] -> {
      let is_duplicate =
        list.any(acc, fn(b) { title_similarity(b.title, head.title) >. 0.8 })

      case is_duplicate {
        True -> {
          // Keep the higher priority bead
          let existing =
            list.find(acc, fn(b) {
              title_similarity(b.title, head.title) >. 0.8
            })
          case existing {
            Ok(existing_bead) -> {
              case existing_bead.priority > head.priority {
                True -> deduplicate_loop(tail, acc)
                False -> {
                  // Replace with higher priority one
                  let filtered =
                    list.filter(acc, fn(b) {
                      title_similarity(b.title, head.title) <=. 0.8
                    })
                  deduplicate_loop(tail, [head, ..filtered])
                }
              }
            }
            Error(_) -> deduplicate_loop(tail, [head, ..acc])
          }
        }
        False -> deduplicate_loop(tail, [head, ..acc])
      }
    }
  }
}

/// Calculate similarity between two titles using simple token overlap
fn title_similarity(title1: String, title2: String) -> Float {
  let tokens1 = tokenize(title1)
  let tokens2 = tokenize(title2)

  case list.length(tokens1) + list.length(tokens2) {
    0 -> 1.0
    total_tokens -> {
      let common_tokens =
        list.filter(tokens1, fn(t) { list.contains(tokens2, t) })
      let overlap = list.length(common_tokens)
      int.to_float(overlap * 2) /. int.to_float(total_tokens)
    }
  }
}

/// Tokenize a string into lowercase words
fn tokenize(text: String) -> List(String) {
  text
  |> string.lowercase
  |> string.replace(" ", "-")
  |> string.replace(":", "")
  |> string.replace("(", "")
  |> string.replace(")", "")
  |> string.split("-")
  |> list.filter(fn(s) { string.length(s) > 0 })
}

// =============================================================================
// DEPENDENCY INFERENCE
// =============================================================================

/// Infer dependencies from behavior.requires and spec structure
/// Creates dependency links between related beads
pub fn infer_dependencies(
  beads: List(EnhancedBead),
  spec: Spec,
) -> List(EnhancedBead) {
  let behavior_map = create_behavior_map(spec)

  list.map(beads, fn(bead) {
    case bead.behavior_name {
      Some(behavior_name) -> {
        let deps = case dict.get(behavior_map, behavior_name) {
          Ok(behavior) -> behavior.requires
          Error(_) -> []
        }
        EnhancedBead(..bead, dependencies: deps)
      }
      None -> bead
    }
  })
}

/// Create a map of behavior name to behavior for easy lookup
fn create_behavior_map(spec: Spec) -> dict.Dict(String, types.Behavior) {
  spec.features
  |> list.flat_map(fn(f) { f.behaviors })
  |> list.fold(from_dict([]), fn(acc, behavior) {
    dict.insert(acc, behavior.name, behavior)
  })
}

fn from_dict(list: List(#(a, b))) -> dict.Dict(a, b) {
  dict.from_list(list)
}

// =============================================================================
// PARALLEL GROUP ASSIGNMENT
// =============================================================================

/// Assign parallel groups (waves) to beads based on dependencies
/// Beads in same wave can be executed in parallel
pub fn assign_parallel_groups(beads: List(EnhancedBead)) -> List(EnhancedBead) {
  let sorted_by_round =
    list.sort(beads, fn(a, b) {
      case a.round == b.round {
        True -> {
          case a.priority < b.priority {
            True -> order.Lt
            False -> order.Gt
          }
        }
        False -> {
          case a.round < b.round {
            True -> order.Lt
            False -> order.Gt
          }
        }
      }
    })

  build_waves(sorted_by_round, [], [], 1)
}

fn build_waves(
  beads: List(EnhancedBead),
  completed: List(EnhancedBead),
  completed_ids: List(String),
  wave_number: Int,
) -> List(EnhancedBead) {
  case beads {
    [] -> list.reverse(completed)
    _ -> {
      // Find all beads that can be in next wave
      let #(ready_beads, remaining_beads) =
        list.partition(beads, fn(bead) {
          list.all(bead.dependencies, fn(dep_id) {
            list.contains(completed_ids, dep_id)
          })
        })

      case ready_beads {
        [] -> {
          // No beads ready - circular dependency or missing dependencies
          // Force next bead with warning
          case remaining_beads {
            [next_bead, ..rest] -> {
              build_waves(
                rest,
                [next_bead, ..completed],
                [next_bead.id, ..completed_ids],
                wave_number + 1,
              )
            }
            [] -> list.reverse(completed)
          }
        }
        _ -> {
          // Create wave with ready beads
          let wave_with_blocks =
            list.map(ready_beads, fn(bead) {
              let other_in_wave =
                list.filter(ready_beads, fn(b) { b.id != bead.id })
                |> list.map(fn(b) { b.id })
              EnhancedBead(..bead, blocks: other_in_wave)
            })

          let wave_ids = list.map(wave_with_blocks, fn(b) { b.id })

          build_waves(
            remaining_beads,
            list.append(list.reverse(wave_with_blocks), completed),
            list.append(list.reverse(wave_ids), completed_ids),
            wave_number + 1,
          )
        }
      }
    }
  }
}

// =============================================================================
// SHARED HELPERS
// =============================================================================

/// Generate a unique bead ID from components
pub fn generate_bead_id(
  issue_type: String,
  category: String,
  index: Int,
) -> String {
  let slugified_type =
    issue_type
    |> string.lowercase
    |> string.replace(" ", "-")
    |> string.replace("_", "-")

  let slugified_category =
    category
    |> string.lowercase
    |> string.replace(" ", "-")
    |> string.replace("_", "-")

  "bead-"
  <> slugified_type
  <> "-"
  <> slugified_category
  <> "-"
  <> string.pad_left(int.to_string(index), 3, "0")
}

/// Map mental model string to round number
pub fn mental_model_to_round(mental_model: String) -> Int {
  case string.lowercase(mental_model) {
    "inversion" -> 3
    "second-order" | "second_order" | "effects" -> 4
    "checklist" -> 2
    "coverage" -> 2
    "security" -> 3
    "contracts" -> 2
    "quality" -> 2
    _ -> 2
  }
}

/// Build a KirkSource record from analysis finding
pub fn make_kirk_source(
  analysis_type: String,
  finding_id: String,
  severity: String,
  category: String,
  original_text: String,
  suggestion: Option(String),
) -> enhanced_bead_generator.KirkSource {
  enhanced_bead_generator.KirkSource(
    analysis_type: analysis_type,
    finding_id: finding_id,
    severity: severity,
    category: category,
    original_text: original_text,
    suggestion: suggestion,
  )
}

/// Build an AcceptanceCriterion record
pub fn make_acceptance_criterion(
  id: String,
  description: String,
  verification_type: String,
  check_expression: Option(String),
  verified: Bool,
) -> enhanced_bead_generator.AcceptanceCriterion {
  enhanced_bead_generator.AcceptanceCriterion(
    id: id,
    description: description,
    verification_type: verification_type,
    check_expression: check_expression,
    verified: verified,
  )
}

// =============================================================================
// INTERNAL HELPERS
// =============================================================================

fn make_kirk_bead(
  index index: Int,
  analysis_type analysis_type: String,
  finding_id finding_id: String,
  severity severity: String,
  category category: String,
  original_text original_text: String,
  suggestion suggestion: Option(String),
  spec_path spec_path: String,
  title title: String,
  description description: String,
  issue_type issue_type: String,
  round round: Int,
  labels labels: List(String),
) -> EnhancedBead {
  let priority = enhanced_bead_generator.severity_to_priority(severity)
  let effort = enhanced_bead_generator.severity_to_effort(severity)

  let ac_description = case suggestion {
    Some(s) -> s
    None -> description
  }

  EnhancedBead(
    id: enhanced_bead_generator.make_bead_id(issue_type, category, index),
    title: title,
    description: description,
    source_type: "kirk",
    kirk_sources: [
      KirkSource(
        analysis_type: analysis_type,
        finding_id: finding_id,
        severity: severity,
        category: category,
        original_text: original_text,
        suggestion: suggestion,
      ),
    ],
    spec_path: Some(spec_path),
    behavior_name: None,
    ears_patterns: [],
    contracts: enhanced_bead_generator.empty_contracts(),
    scenarios: [
      TestCase(
        name: "Verify " <> title,
        given: ["Spec loaded from " <> spec_path],
        when: "KIRK " <> analysis_type <> " analysis is run",
        then: ac_description,
        assertion: "Finding is resolved",
      ),
    ],
    acceptance_criteria: [
      AcceptanceCriterion(
        id: "AC-001",
        description: ac_description,
        verification_type: "review",
        check_expression: None,
        verified: False,
      ),
    ],
    types_needed: [],
    effort: effort,
    priority: priority,
    status: "pending",
    dependencies: [],
    blocks: [],
    round: round,
    profile_type: "api",
    issue_type: issue_type,
    labels: labels,
    ai_hints: case suggestion {
      Some(s) -> s
      None -> ""
    },
    pitfalls: [],
  )
}
