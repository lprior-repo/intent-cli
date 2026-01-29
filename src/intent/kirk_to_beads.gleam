//// KIRK → Enhanced Bead Transformers
////
//// Transforms KIRK analysis findings into enhanced beads with full traceability.
//// Each KIRK command produces findings that become actionable work units.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/enhanced_bead_generator.{
  type EnhancedBead, AcceptanceCriterion, EnhancedBead, KirkSource, TestCase,
}
import intent/kirk/coverage_analyzer.{type CoverageReport}
import intent/kirk/effects_analyzer.{type EffectsReport}
import intent/kirk/gap_detector.{type GapReport}
import intent/kirk/inversion_checker.{type InversionReport}
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
// AGGREGATE
// =============================================================================

pub fn generate_all_beads(spec: Spec, spec_path: String) -> List(EnhancedBead) {
  // Run all KIRK analyses
  let quality_report = quality_analyzer.analyze_spec(spec)
  let coverage_report = coverage_analyzer.analyze_coverage(spec)
  let gap_report = gap_detector.detect_gaps(spec)
  let inversion_report = inversion_checker.analyze_inversions(spec)
  let effects_report = effects_analyzer.analyze_effects(spec)

  // Transform to beads
  let behavior_beads = behaviors_to_beads(spec, spec_path)
  let quality_beads = quality_to_beads(quality_report, spec_path)
  let coverage_beads = coverage_to_beads(coverage_report, spec_path)
  let gap_beads = gaps_to_beads(gap_report, spec_path)
  let inversion_beads = inversions_to_beads(inversion_report, spec_path)
  let effects_beads = effects_to_beads(effects_report, spec_path)

  // Combine all beads
  list.concat([
    behavior_beads,
    quality_beads,
    coverage_beads,
    gap_beads,
    inversion_beads,
    effects_beads,
  ])
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

/// Map severity string to priority int (critical→1, high→2, medium→3, low→4)
pub fn severity_to_priority(severity: String) -> Int {
  case string.lowercase(severity) {
    "critical" -> 1
    "high" -> 2
    "medium" -> 3
    "low" -> 4
    _ -> 3
  }
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
