import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import intent/enhanced_bead_generator
import intent/types

pub fn main() {
  gleeunit.main()
}

// =============================================================================
// Test Helpers
// =============================================================================

fn make_simple_behavior() -> types.Behavior {
  types.Behavior(
    name: "create_user",
    intent: "Create a new user account",
    notes: "",
    requires: [],
    tags: [],
    request: types.Request(
      method: types.Post,
      path: "/users",
      headers: dict.new(),
      query: dict.new(),
      body: json.object([#("email", json.string("test@example.com"))]),
    ),
    response: types.Response(
      status: 201,
      example: json.object([#("id", json.string("123"))]),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

fn make_behavior_with_checks() -> types.Behavior {
  let checks =
    dict.new()
    |> dict.insert(
      "email_valid",
      types.Check(
        rule: "response.email matches /^[^@]+@[^@]+$/",
        why: "Email must be properly formatted",
      ),
    )
    |> dict.insert(
      "id_present",
      types.Check(
        rule: "response.id != null",
        why: "Created resource must have an ID",
      ),
    )
    |> dict.insert(
      "status_correct",
      types.Check(
        rule: "status == 201",
        why: "Creation should return 201 Created",
      ),
    )

  types.Behavior(
    name: "create_user_validated",
    intent: "Create a new user with validation",
    notes: "",
    requires: ["database_ready", "email_service_available"],
    tags: ["critical"],
    request: types.Request(
      method: types.Post,
      path: "/users",
      headers: dict.new(),
      query: dict.new(),
      body: json.object([
        #("email", json.string("test@example.com")),
        #("name", json.string("Test User")),
      ]),
    ),
    response: types.Response(
      status: 201,
      example: json.object([
        #("id", json.string("123")),
        #("email", json.string("test@example.com")),
      ]),
      checks: checks,
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

fn make_behavior_with_ears_event_driven() -> types.Behavior {
  types.Behavior(
    name: "submit_form",
    intent: "WHEN user clicks submit THE SYSTEM SHALL validate form data",
    notes: "",
    requires: [],
    tags: [],
    request: types.Request(
      method: types.Post,
      path: "/forms/submit",
      headers: dict.new(),
      query: dict.new(),
      body: json.object([#("data", json.string("test"))]),
    ),
    response: types.Response(
      status: 200,
      example: json.object([#("valid", json.bool(True))]),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

fn make_behavior_with_ears_state_driven() -> types.Behavior {
  types.Behavior(
    name: "show_dashboard",
    intent: "WHILE user is authenticated THE SYSTEM SHALL display dashboard",
    notes: "",
    requires: [],
    tags: [],
    request: types.Request(
      method: types.Get,
      path: "/dashboard",
      headers: dict.new(),
      query: dict.new(),
      body: json.null(),
    ),
    response: types.Response(
      status: 200,
      example: json.object([#("dashboard", json.string("data"))]),
      checks: dict.new(),
      headers: dict.new(),
    ),
    captures: dict.new(),
  )
}

// =============================================================================
// Contract Extraction Tests
// =============================================================================

pub fn test_extract_contracts_with_multiple_checks_test() {
  let behavior = make_behavior_with_checks()
  let contracts =
    enhanced_bead_generator.extract_contracts_from_behavior(behavior)

  // Should have 2 preconditions from requires list
  contracts.preconditions
  |> should.equal(["database_ready", "email_service_available"])

  // Should have 3 postconditions from checks
  contracts.postconditions
  |> list.length
  |> should.equal(3)

  // Check that postconditions contain the contract checks
  let postcondition_names =
    contracts.postconditions
    |> list.map(fn(check) { check.check_name })

  postcondition_names
  |> list.contains("email_valid")
  |> should.be_true()

  postcondition_names
  |> list.contains("id_present")
  |> should.be_true()
}

pub fn test_extract_contracts_with_empty_checks_test() {
  let behavior = make_simple_behavior()
  let contracts =
    enhanced_bead_generator.extract_contracts_from_behavior(behavior)

  // Should have empty postconditions when no checks
  contracts.postconditions
  |> should.equal([])

  // Should have empty preconditions when no requires
  contracts.preconditions
  |> should.equal([])

  // Invariants should be empty
  contracts.invariants
  |> should.equal([])
}

pub fn test_contract_check_preserves_rule_and_why_test() {
  let behavior = make_behavior_with_checks()
  let contracts =
    enhanced_bead_generator.extract_contracts_from_behavior(behavior)

  // Find the email_valid check
  let email_check =
    contracts.postconditions
    |> list.find(fn(check) { check.check_name == "email_valid" })

  case email_check {
    Ok(check) -> {
      check.rule
      |> should.equal("response.email matches /^[^@]+@[^@]+$/")

      check.why
      |> should.equal("Email must be properly formatted")
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// EARS Pattern Extraction Tests
// =============================================================================

pub fn test_extract_ears_event_driven_test() {
  let behavior = make_behavior_with_ears_event_driven()
  let patterns = enhanced_bead_generator.extract_ears_patterns(behavior.intent)

  patterns
  |> list.length
  |> should.equal(1)

  let assert Ok(pattern) = list.first(patterns)

  pattern.pattern_type
  |> should.equal("EventDriven")

  case pattern.trigger {
    Some(trigger) ->
      trigger
      |> should.equal("user clicks submit")
    None -> should.fail()
  }

  pattern.behavior
  |> should.equal("validate form data")
}

pub fn test_extract_ears_state_driven_test() {
  let behavior = make_behavior_with_ears_state_driven()
  let patterns = enhanced_bead_generator.extract_ears_patterns(behavior.intent)

  patterns
  |> list.length
  |> should.equal(1)

  let assert Ok(pattern) = list.first(patterns)

  pattern.pattern_type
  |> should.equal("StateDriven")

  case pattern.state {
    Some(state) ->
      state
      |> should.equal("user is authenticated")
    None -> should.fail()
  }

  pattern.behavior
  |> should.equal("display dashboard")
}

pub fn test_extract_ears_ubiquitous_fallback_test() {
  let behavior = make_simple_behavior()
  let patterns = enhanced_bead_generator.extract_ears_patterns(behavior.intent)

  patterns
  |> list.length
  |> should.equal(1)

  let assert Ok(pattern) = list.first(patterns)

  pattern.pattern_type
  |> should.equal("Ubiquitous")

  // Should capture full intent as behavior
  pattern.behavior
  |> should.equal("Create a new user account")

  // Trigger/state/condition should be None for ubiquitous
  pattern.trigger
  |> should.equal(None)

  pattern.state
  |> should.equal(None)
}

// =============================================================================
// Type Definition Generation Tests
// =============================================================================

pub fn test_generate_type_definitions_post_request_test() {
  let behavior = make_behavior_with_checks()
  let type_defs = enhanced_bead_generator.generate_type_definitions(behavior)

  // Should generate at least one type definition
  type_defs
  |> list.is_empty
  |> should.be_false()

  // Should include handler function type
  let has_handler =
    type_defs
    |> list.any(fn(typedef) {
      string.contains(typedef.signature, "handle_")
      && string.contains(typedef.signature, "Request")
      && string.contains(typedef.signature, "Response")
    })

  has_handler
  |> should.be_true()
}

pub fn test_generate_type_definitions_includes_request_body_test() {
  let behavior = make_behavior_with_checks()
  let type_defs = enhanced_bead_generator.generate_type_definitions(behavior)

  // Should include request body type when request has body
  let has_request_type =
    type_defs
    |> list.any(fn(typedef) { string.contains(typedef.name, "Request") })

  has_request_type
  |> should.be_true()
}

// =============================================================================
// Test Case Generation Tests
// =============================================================================

pub fn test_generate_test_cases_from_checks_test() {
  let behavior = make_behavior_with_checks()
  let test_cases = enhanced_bead_generator.generate_test_cases(behavior)

  // Should generate test case for each check (3 checks)
  test_cases
  |> list.length
  |> should.equal(3)

  // Each test case should have populated fields
  test_cases
  |> list.all(fn(tc) {
    !string.is_empty(tc.name)
    && !string.is_empty(tc.when)
    && !string.is_empty(tc.then)
    && !string.is_empty(tc.assertion)
  })
  |> should.be_true()
}

pub fn test_generate_test_cases_includes_preconditions_test() {
  let behavior = make_behavior_with_checks()
  let test_cases = enhanced_bead_generator.generate_test_cases(behavior)

  // Each test case should include preconditions from requires
  let assert Ok(first_test) = list.first(test_cases)

  first_test.given
  |> should.equal(["database_ready", "email_service_available"])
}

pub fn test_generate_test_cases_maps_check_why_to_then_test() {
  let behavior = make_behavior_with_checks()
  let test_cases = enhanced_bead_generator.generate_test_cases(behavior)

  // Find test case for email_valid check
  let email_test =
    test_cases
    |> list.find(fn(tc) { string.contains(tc.name, "email_valid") })

  case email_test {
    Ok(tc) -> {
      tc.then
      |> should.equal("Email must be properly formatted")

      tc.assertion
      |> should.equal("response.email matches /^[^@]+@[^@]+$/")
    }
    Error(_) -> should.fail()
  }
}

// =============================================================================
// Enhanced Bead Generation Integration Tests
// =============================================================================

pub fn test_generate_enhanced_bead_integration_test() {
  let behavior = make_behavior_with_checks()
  let enhanced = enhanced_bead_generator.generate_enhanced_bead(behavior, "api")

  // Should have base bead populated
  enhanced.base.title
  |> string.is_empty
  |> should.be_false()

  // Should have EARS patterns
  enhanced.ears_patterns
  |> list.is_empty
  |> should.be_false()

  // Should have contracts
  enhanced.contracts.postconditions
  |> list.length
  |> should.equal(3)

  // Should have type definitions
  enhanced.types
  |> list.is_empty
  |> should.be_false()

  // Should have test cases
  enhanced.tests
  |> list.length
  |> should.equal(3)
}

pub fn test_generate_enhanced_bead_with_simple_behavior_test() {
  let behavior = make_simple_behavior()
  let enhanced = enhanced_bead_generator.generate_enhanced_bead(behavior, "api")

  // Should handle behavior with no checks gracefully
  enhanced.contracts.postconditions
  |> should.equal([])

  // Should still generate EARS pattern (Ubiquitous fallback)
  enhanced.ears_patterns
  |> list.length
  |> should.equal(1)

  // Should still generate type definitions
  enhanced.types
  |> list.is_empty
  |> should.be_false()
}
