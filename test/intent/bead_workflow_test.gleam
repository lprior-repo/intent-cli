import gleam/list
import gleam/option
import gleam/string
import gleeunit/should
import intent/bead_feedback
import intent/bead_types
import intent/bead_verify
import intent/bead_workflow

pub fn empty_evidence_test() {
  let evidence = bead_workflow.empty_evidence("feature")

  evidence.issue_type
  |> should.equal("feature")

  evidence.feedback
  |> should.equal([])

  evidence.custom_evidence
  |> should.equal([])
}

pub fn add_feedback_test() {
  let evidence = bead_workflow.empty_evidence("bug")

  let feedback =
    bead_feedback.create_success_feedback(
      "TEST-001",
      "Test passed",
      "2026-01-01T00:00:00Z",
      100,
    )

  let updated = bead_workflow.add_feedback(evidence, feedback)

  updated.feedback
  |> list.length
  |> should.equal(1)

  updated.issue_type
  |> should.equal("bug")
}

pub fn add_custom_evidence_test() {
  let evidence = bead_workflow.empty_evidence("feature")

  let updated =
    bead_workflow.add_custom_evidence(evidence, ["Evidence 1", "Evidence 2"])

  updated.custom_evidence
  |> list.length
  |> should.equal(2)
}

pub fn build_evidence_list_test() {
  let evidence = bead_workflow.empty_evidence("api_endpoint")

  let evidence =
    evidence
    |> bead_workflow.add_custom_evidence(["Custom evidence"])

  let feedback =
    bead_feedback.create_success_feedback(
      "API-001",
      "All tests passed",
      "2026-01-01T00:00:00Z",
      500,
    )

  let evidence = bead_workflow.add_feedback(evidence, feedback)

  let list = bead_workflow.build_evidence_list(evidence)

  list
  |> list.length
  |> should.equal(2)
}

pub fn verify_bead_for_close_in_progress_test() {
  let evidence =
    bead_workflow.empty_evidence("feature")
    |> bead_workflow.add_custom_evidence([
      "Acceptance criteria met",
      "Tests written",
      "Documentation updated",
    ])

  let result =
    bead_workflow.verify_bead_for_close(
      "TEST-001",
      bead_types.InProgress,
      evidence,
    )

  case result {
    bead_workflow.BeadClosed(_, report) -> {
      report.required_passed
      |> should.be_true()
    }
    _ -> {
      should.fail()
    }
  }
}

pub fn verify_bead_for_close_open_test() {
  let evidence = bead_workflow.empty_evidence("feature")

  let result =
    bead_workflow.verify_bead_for_close("TEST-001", bead_types.Open, evidence)

  case result {
    bead_workflow.InvalidState(bead_id, reason) -> {
      bead_id |> should.equal("TEST-001")
      reason |> string.contains("InProgress") |> should.be_true()
    }
    _ -> {
      should.fail()
    }
  }
}

pub fn verify_bead_for_close_closed_test() {
  let evidence = bead_workflow.empty_evidence("feature")

  let result =
    bead_workflow.verify_bead_for_close("TEST-001", bead_types.Closed, evidence)

  case result {
    bead_workflow.InvalidState(bead_id, reason) -> {
      bead_id |> should.equal("TEST-001")
      reason |> string.contains("already closed") |> should.be_true()
    }
    _ -> {
      should.fail()
    }
  }
}

pub fn verify_with_feedback_test() {
  let feedback =
    bead_feedback.create_success_feedback(
      "FEAT-001",
      "All acceptance criteria met with tests written",
      "2026-01-01T00:00:00Z",
      1000,
    )

  let result =
    bead_workflow.verify_with_feedback(
      "FEAT-001",
      bead_types.InProgress,
      [feedback],
      "feature",
    )

  case result {
    bead_workflow.BeadClosed(_, report) -> {
      report.required_passed |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn format_close_result_test() {
  let evidence =
    bead_workflow.empty_evidence("feature")
    |> bead_workflow.add_custom_evidence([
      "Acceptance criteria met",
      "Tests written",
      "Documentation updated",
    ])

  let result =
    bead_workflow.verify_bead_for_close(
      "TEST-001",
      bead_types.InProgress,
      evidence,
    )

  let formatted = bead_workflow.format_close_result(result)

  formatted |> string.contains("TEST-001") |> should.be_true()
  formatted |> string.contains("closed successfully") |> should.be_true()
}

pub fn close_result_to_json_test() {
  let evidence = bead_workflow.empty_evidence("feature")
  let result =
    bead_workflow.verify_bead_for_close(
      "TEST-001",
      bead_types.InProgress,
      evidence,
    )

  let json = bead_workflow.close_result_to_json(result)

  option.is_some(json) |> should.be_true()
}

pub fn hooks_for_issue_type_test() {
  let hooks = bead_verify.hooks_for_issue_type("feature")

  hooks
  |> list.length
  |> should.equal(1)

  case list.first(hooks) {
    Ok(first) -> first.id |> should.equal("feature-complete")
    Error(_) -> should.fail()
  }
}

pub fn default_hooks_test() {
  let hooks = bead_verify.default_hooks()

  hooks
  |> list.length
  |> should.equal(3)
}

pub fn verify_criterion_test() {
  let result =
    bead_verify.verify_criterion("Code compiles without errors", [
      "Code compiles",
      "Tests pass",
    ])

  result |> should.be_true()
}

pub fn verify_criterion_no_match_test() {
  let result =
    bead_verify.verify_criterion("Unknown criterion", ["Something else"])

  result |> should.be_false()
}
