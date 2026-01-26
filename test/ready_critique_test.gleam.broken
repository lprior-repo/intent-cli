import gleam/list
import gleeunit
import gleeunit/should
import intent/planning_types.{
  type Blocker, type DimensionScore, type ReadyReport, Blocker,
  Critical as BlockerCritical, DimensionScore, High, ReadyReport,
}
import intent/ready_critique.{
  type CritiqueIssue, Critical, CritiqueIssue, RollbackPlan, SuccessCriteria,
  VisionAlignment, critique_ready, validate_rollback_plan,
  validate_success_criteria, validate_vision_alignment,
}

pub fn main() {
  gleeunit.main()
}

fn perfect_ready() -> ReadyReport {
  ReadyReport(
    replacement: DimensionScore(
      score: 90,
      reasoning: "Clear value proposition with well-defined audience and success criteria",
      issues: [],
    ),
    empathy: DimensionScore(
      score: 85,
      reasoning: "Strong error handling with anti-pattern awareness",
      issues: [],
    ),
    actionable: DimensionScore(
      score: 85,
      reasoning: "Excellent response checks with clear guidance",
      issues: [],
    ),
    discoverable: DimensionScore(
      score: 80,
      reasoning: "Good naming and organization",
      issues: [],
    ),
    yet_complete: DimensionScore(
      score: 90,
      reasoning: "Fully complete and ready to implement",
      issues: [],
    ),
    overall_readiness: 86,
    blockers: [],
    recommendations: [],
  )
}

pub fn validate_vision_alignment_perfect_ready_test() {
  let ready = perfect_ready()
  let issues = validate_vision_alignment(ready)
  issues |> should.equal([])
}

pub fn validate_vision_alignment_low_replacement_test() {
  let ready =
    ReadyReport(
      ..perfect_ready(),
      replacement: DimensionScore(
        score: 35,
        reasoning: "Value proposition unclear",
        issues: ["Audience too vague", "Insufficient success criteria"],
      ),
    )
  let issues = validate_vision_alignment(ready)
  issues |> should.not_equal([])
  let has_critical =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(VisionAlignment, Critical, _, _) -> True
        _ -> False
      }
    })
  has_critical |> should.be_true
}

pub fn validate_vision_alignment_multiple_replacement_issues_test() {
  let ready =
    ReadyReport(
      ..perfect_ready(),
      replacement: DimensionScore(
        score: 60,
        reasoning: "Good foundation but could strengthen value proposition",
        issues: ["Audience too vague", "Description lacks detail"],
      ),
    )
  let issues = validate_vision_alignment(ready)
  issues |> should.not_equal([])
}

pub fn validate_success_criteria_perfect_ready_test() {
  let ready = perfect_ready()
  let issues = validate_success_criteria(ready)
  issues |> should.equal([])
}

pub fn validate_success_criteria_low_yet_complete_test() {
  let ready =
    ReadyReport(
      ..perfect_ready(),
      yet_complete: DimensionScore(
        score: 40,
        reasoning: "Incomplete specification - multiple sections need work",
        issues: ["Features without behaviors", "No rules defined"],
      ),
    )
  let issues = validate_success_criteria(ready)
  issues |> should.not_equal([])
}

pub fn validate_success_criteria_critical_blockers_test() {
  let ready =
    ReadyReport(..perfect_ready(), blockers: [
      Blocker(
        severity: BlockerCritical,
        description: "Overall readiness below 70%",
        affected_areas: ["all_dimensions"],
      ),
      Blocker(
        severity: BlockerCritical,
        description: "Replacement score critically low",
        affected_areas: ["vision", "audience"],
      ),
    ])
  let issues = validate_success_criteria(ready)
  issues |> should.not_equal([])
  let has_critical =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(SuccessCriteria, Critical, _, _) -> True
        _ -> False
      }
    })
  has_critical |> should.be_true
}

pub fn validate_success_criteria_low_empathy_actionable_test() {
  let ready =
    ReadyReport(
      ..perfect_ready(),
      empathy: DimensionScore(
        score: 35,
        reasoning: "Missing error handling",
        issues: ["No error handling behaviors"],
      ),
      actionable: DimensionScore(
        score: 30,
        reasoning: "Missing response checks",
        issues: ["No response checks defined"],
      ),
    )
  let issues = validate_success_criteria(ready)
  issues |> should.not_equal([])
}

pub fn validate_rollback_plan_perfect_ready_test() {
  let ready = perfect_ready()
  let issues = validate_rollback_plan(ready)
  issues |> should.equal([])
}

pub fn validate_rollback_plan_low_overall_readiness_test() {
  let ready = ReadyReport(..perfect_ready(), overall_readiness: 65)
  let issues = validate_rollback_plan(ready)
  issues |> should.not_equal([])
  let has_critical =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(RollbackPlan, Critical, _, _) -> True
        _ -> False
      }
    })
  has_critical |> should.be_true
}

pub fn validate_rollback_plan_high_severity_blockers_test() {
  let ready =
    ReadyReport(..perfect_ready(), blockers: [
      Blocker(
        severity: High,
        description: "Replacement needs improvement",
        affected_areas: ["vision"],
      ),
      Blocker(
        severity: High,
        description: "Empathy needs improvement",
        affected_areas: ["error_handling"],
      ),
      Blocker(
        severity: High,
        description: "Actionable needs improvement",
        affected_areas: ["response_checks"],
      ),
    ])
  let issues = validate_rollback_plan(ready)
  issues |> should.not_equal([])
}

pub fn validate_rollback_plan_low_discoverable_test() {
  let ready =
    ReadyReport(
      ..perfect_ready(),
      discoverable: DimensionScore(
        score: 35,
        reasoning: "Poor discoverability",
        issues: ["Limited feature organization", "No behavior tags"],
      ),
    )
  let issues = validate_rollback_plan(ready)
  issues |> should.not_equal([])
}

pub fn critique_ready_perfect_passes_test() {
  let ready = perfect_ready()
  let result = critique_ready(ready)
  result.passed |> should.be_true
  result.score |> should.equal(100)
  result.issues |> should.equal([])
}

pub fn critique_ready_warnings_reduce_score_test() {
  let ready =
    ReadyReport(
      ..perfect_ready(),
      replacement: DimensionScore(
        score: 65,
        reasoning: "Good foundation but could strengthen value proposition",
        issues: ["Audience could be more specific"],
      ),
    )
  let result = critique_ready(ready)
  // Should pass but with reduced score
  result.passed |> should.be_true
  { result.score < 100 && result.score > 70 } |> should.be_true
}

pub fn critique_ready_multiple_criticals_fail_hard_test() {
  let ready =
    ReadyReport(
      replacement: DimensionScore(
        score: 30,
        reasoning: "Value proposition unclear",
        issues: ["Audience too vague", "Insufficient success criteria"],
      ),
      empathy: DimensionScore(
        score: 25,
        reasoning: "Missing error handling",
        issues: ["No error handling behaviors", "No anti-patterns"],
      ),
      actionable: DimensionScore(
        score: 20,
        reasoning: "Missing response checks",
        issues: ["No response checks defined"],
      ),
      discoverable: DimensionScore(
        score: 30,
        reasoning: "Poor discoverability",
        issues: ["Limited feature organization"],
      ),
      yet_complete: DimensionScore(
        score: 35,
        reasoning: "Incomplete specification",
        issues: ["Features without behaviors", "No rules defined"],
      ),
      overall_readiness: 28,
      blockers: [
        Blocker(
          severity: BlockerCritical,
          description: "Overall readiness below 70%",
          affected_areas: ["all_dimensions"],
        ),
      ],
      recommendations: [],
    )
  let result = critique_ready(ready)
  result.passed |> should.be_false
  { result.score < 30 } |> should.be_true
}

pub fn critique_ready_moderate_readiness_warning_test() {
  let ready = ReadyReport(..perfect_ready(), overall_readiness: 75)
  let result = critique_ready(ready)
  result.passed |> should.be_true
  { result.score >= 70 && result.score < 100 } |> should.be_true
}
