//// Tests for example plan files
//// Verifies that example plan JSON files exist and are valid

import gleam/json
import gleeunit/should
import simplifile

pub fn simple_api_plan_exists_test() {
  // Verify the simple API plan example exists
  let result = simplifile.read("examples/plan-simple-api.json")
  should.be_ok(result)
}

pub fn simple_api_plan_is_valid_json_test() {
  // Verify the simple API plan is valid JSON
  let assert Ok(content) = simplifile.read("examples/plan-simple-api.json")
  let result = json.decode(content, fn(_) { Ok(Nil) })
  should.be_ok(result)
}

pub fn complex_planning_workflow_plan_exists_test() {
  // Verify the complex planning workflow example exists
  let result = simplifile.read("examples/plan-complete-workflow.json")
  should.be_ok(result)
}

pub fn complex_planning_workflow_plan_is_valid_json_test() {
  // Verify the complex workflow plan is valid JSON
  let assert Ok(content) =
    simplifile.read("examples/plan-complete-workflow.json")
  let result = json.decode(content, fn(_) { Ok(Nil) })
  should.be_ok(result)
}
