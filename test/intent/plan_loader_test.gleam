//// Tests for the plan_loader module
//// Validates loading Plan schemas from CUE files
//// Following the same FC/IS pattern as loader.gleam

import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import intent/plan_loader.{
  CueExportFailed, CueValidationFailed, JsonDecodeFailed, PlanParseFailed,
  SecurityError,
}

// ============================================================================
// parse_cue_validation_result Tests (PURE FUNCTION)
// ============================================================================

pub fn parse_plan_cue_validation_result_success_test() {
  // Successful validation returns Ok(Nil)
  plan_loader.parse_cue_validation_result("plan.cue", Ok(""))
  |> should.be_ok
}

pub fn parse_plan_cue_validation_result_failure_test() {
  // Failed validation returns CueValidationFailed error
  let result =
    plan_loader.parse_cue_validation_result("plan.cue", Error(#(1, "syntax error")))

  case result {
    Error(CueValidationFailed(path, exit_code, stderr)) -> {
      path |> should.equal("plan.cue")
      exit_code |> should.equal(1)
      stderr |> should.equal("syntax error")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// parse_cue_export_result Tests (PURE FUNCTION)
// ============================================================================

pub fn parse_plan_cue_export_result_success_test() {
  // Successful export returns the JSON string
  plan_loader.parse_cue_export_result("plan.cue", Ok("{\"id\": \"test-plan\"}"))
  |> should.equal(Ok("{\"id\": \"test-plan\"}"))
}

pub fn parse_plan_cue_export_result_failure_test() {
  // Failed export returns CueExportFailed error
  let result =
    plan_loader.parse_cue_export_result("plan.cue", Error(#(1, "export failed")))

  case result {
    Error(CueExportFailed(path, exit_code, stderr)) -> {
      path |> should.equal("plan.cue")
      exit_code |> should.equal(1)
      stderr |> should.equal("export failed")
    }
    _ -> should.fail()
  }
}

// ============================================================================
// parse_json_to_plan Tests (PURE FUNCTION)
// ============================================================================

pub fn parse_json_to_plan_minimal_test() {
  // Minimal valid Plan with only required fields
  let json = "{
    \"id\": \"test-plan-001\",
    \"created_at\": \"2026-01-25T00:00:00Z\",
    \"updated_at\": \"2026-01-25T00:00:00Z\",
    \"vision\": {
      \"press_release\": \"Revolutionary planning tool\",
      \"persona\": \"AI-first developers\",
      \"non_personas\": [\"Manual testers\"],
      \"north_star\": \"Zero-ambiguity specifications\",
      \"scenarios\": [],
      \"vorp\": \"10x faster planning\",
      \"out_of_scope\": [\"HTTP testing\"]
    },
    \"shape\": {
      \"features\": [],
      \"critical_path\": [],
      \"mvp_slice\": {
        \"description\": \"Core planning workflow\",
        \"features\": [\"vision\", \"shape\"],
        \"shortcuts\": [\"Manual spec editing\"]
      },
      \"post_mvp\": [],
      \"validation_moment\": \"First complete plan generated\"
    }
  }"

  let result = plan_loader.parse_json_to_plan(json)

  case result {
    Ok(plan) -> {
      plan.id |> should.equal("test-plan-001")
      plan.vision.press_release |> should.equal("Revolutionary planning tool")
      plan.shape.mvp_slice.description |> should.equal("Core planning workflow")
      plan.spec |> should.equal(None)
      plan.ready |> should.equal(None)
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_json_to_plan_with_spec_test() {
  // Plan with optional Spec section
  let json = "{
    \"id\": \"test-plan-002\",
    \"created_at\": \"2026-01-25T00:00:00Z\",
    \"updated_at\": \"2026-01-25T00:00:00Z\",
    \"vision\": {
      \"press_release\": \"Test\",
      \"persona\": \"Developers\",
      \"non_personas\": [],
      \"north_star\": \"Success\",
      \"scenarios\": [],
      \"vorp\": \"Better\",
      \"out_of_scope\": []
    },
    \"shape\": {
      \"features\": [],
      \"critical_path\": [],
      \"mvp_slice\": {
        \"description\": \"MVP\",
        \"features\": [],
        \"shortcuts\": []
      },
      \"post_mvp\": [],
      \"validation_moment\": \"Done\"
    },
    \"spec\": {
      \"name\": \"Test Spec\",
      \"description\": \"Detailed specification\",
      \"rounds_complete\": 5,
      \"kirk_health\": {
        \"coverage_score\": 0.95,
        \"quality_score\": 0.90,
        \"gaps\": [],
        \"inversions\": [],
        \"effects\": []
      }
    }
  }"

  let result = plan_loader.parse_json_to_plan(json)

  case result {
    Ok(plan) -> {
      case plan.spec {
        Some(spec) -> {
          spec.name |> should.equal("Test Spec")
          spec.rounds_complete |> should.equal(5)
        }
        None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn parse_json_to_plan_invalid_json_test() {
  // Invalid JSON should return JsonDecodeFailed
  let result = plan_loader.parse_json_to_plan("{invalid json")

  case result {
    Error(JsonDecodeFailed(_)) -> Nil
    _ -> should.fail()
  }
}

pub fn parse_json_to_plan_missing_required_field_test() {
  // Missing required field (vision) should return PlanParseFailed
  let json = "{
    \"id\": \"test-plan-003\",
    \"created_at\": \"2026-01-25T00:00:00Z\",
    \"updated_at\": \"2026-01-25T00:00:00Z\",
    \"shape\": {
      \"features\": [],
      \"critical_path\": [],
      \"mvp_slice\": {
        \"description\": \"MVP\",
        \"features\": [],
        \"shortcuts\": []
      },
      \"post_mvp\": [],
      \"validation_moment\": \"Done\"
    }
  }"

  let result = plan_loader.parse_json_to_plan(json)

  case result {
    Error(PlanParseFailed(_)) -> Nil
    _ -> should.fail()
  }
}

// ============================================================================
// format_error Tests
// ============================================================================

pub fn format_plan_cue_validation_error_test() {
  let error = CueValidationFailed("plan.cue", 1, "syntax error on line 10")
  let formatted = plan_loader.format_error(error)

  string.contains(formatted, "CUE validation failed") |> should.be_true()
  string.contains(formatted, "plan.cue") |> should.be_true()
  string.contains(formatted, "syntax error on line 10") |> should.be_true()
}

pub fn format_plan_cue_export_error_test() {
  let error = CueExportFailed("plan.cue", 1, "export failed")
  let formatted = plan_loader.format_error(error)

  string.contains(formatted, "CUE export failed") |> should.be_true()
  string.contains(formatted, "plan.cue") |> should.be_true()
}

pub fn format_plan_parse_error_test() {
  let error = PlanParseFailed([])
  let formatted = plan_loader.format_error(error)

  string.contains(formatted, "Plan parse error") |> should.be_true()
}

pub fn format_plan_security_error_test() {
  let error = SecurityError("Path traversal detected")
  let formatted = plan_loader.format_error(error)

  formatted |> should.equal("Path traversal detected")
}
