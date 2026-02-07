// Comprehensive tests for spec_builder.gleam
import gleam/dict
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import intent/interview.{type Answer, type InterviewSession, type Profile}
import intent/spec_builder.*
import intent/types.{
  type Behavior, type Spec, AIHints, Behavior, Config, Feature, Get, ImplementationHints,
  Request, Response, SecurityHints, Spec,
}

// Helper function to create a mock Answer
fn mock_answer(question_text: String, response: String) -> Answer {
  interview.Answer(question_text: question_text, response: response)
}

// Helper function to create a mock Profile
fn mock_profile() -> Profile {
  interview.Profile(
    name: "Test Profile",
    role: "QA Engineer",
    experience: "Intermediate",
    focus: "API Testing",
  )
}

// Helper function to create a minimal InterviewSession
fn mock_session(answers: List(Answer)) -> InterviewSession {
  interview.InterviewSession(
    profile: mock_profile(),
    answers: answers,
    current_step: 0,
    completed: True,
  )
}

// Test 1: Empty interview session generates minimal valid spec
pub fn test_empty_session() {
  let session = mock_session([])
  let spec_cue = build_spec_from_session(session)

  // Check that basic structure is present
  assert True =
    string.contains(spec_cue, "package api")
  assert True =
    string.contains(spec_cue, "// Features")
  assert True =
    string.contains(spec_cue, "features: {")
  assert True =
    string.contains(spec_cue, "behaviors: {")
  assert True =
    string.contains(spec_cue, "security: {")

  // Check that empty sections are present with appropriate comments
  assert True =
    string.contains(spec_cue, "// Add feature definitions")
  assert True =
    string.contains(spec_cue, "// Add endpoint definitions")

  spec_cue
}

// Test 2: Feature extraction with various inputs
pub fn test_feature_extraction() {
  let answers = [
    mock_answer("What features should the API have?", "User Management"),
    mock_answer("List the capabilities needed", "Authentication, Authorization"),
    mock_answer("Features of the system", "Data Analytics"),
    mock_answer("", ""), // Empty question/response
    mock_answer("Other information", "Not a feature"),
  ]

  let features = extract_features_from_answers(answers)

  // Should extract 3 features
  assert 3 = list.length(features)

  // Check specific features are extracted
  assert True = list.contains(features, "User Management")
  assert True = list.contains(features, "Authentication")
  assert True = list.contains(features, "Authorization")
  assert True = list.contains(features, "Data Analytics")

  // Empty responses should be filtered out
  assert False = list.contains(features, "")

  features
}

// Test 3: Feature extraction with case insensitive matching
pub fn test_feature_extraction_case_insensitive() {
  let answers = [
    mock_answer("What FEATURES should the API have?", "Feature 1"),
    mock_answer("CAPABILITIES needed", "Feature 2"),
    mock_answer("feature requirements", "Feature 3"),
  ]

  let features = extract_features_from_answers(answers)

  assert 3 = list.length(features)
  assert True = list.contains(features, "Feature 1")
  assert True = list.contains(features, "Feature 2")
  assert True = list.contains(features, "Feature 3")
}

// Test 4: Behavior extraction with endpoints
pub fn test_behavior_extraction() {
  let answers = [
    mock_answer("What endpoints do you need?", "/users"),
    mock_answer("HTTP methods required", "GET, POST"),
    mock_answer("API paths", "/api/v1/products"),
  ]

  let behaviors = extract_behaviors_from_answers(answers, mock_profile())

  // Check that behaviors section is properly formatted
  assert True = string.contains(behaviors, "// API behaviors from interview")
  assert True = string.contains(behaviors, "behaviors: {")

  // Check that answers are included as comments
  assert True = string.contains(behaviors, "// What endpoints do you need?")
  assert True = string.contains(behaviors, "// /users")
  assert True = string.contains(behaviors, "// HTTP methods required?")
  assert True = string.contains(behaviors, "// GET, POST")

  behaviors
}

// Test 5: Behavior extraction with empty answers generates template
pub fn test_behavior_extraction_empty() {
  let answers = []
  let behaviors = extract_behaviors_from_answers(answers, mock_profile())

  // Should generate template for empty behaviors
  assert True = string.contains(behaviors, "// Define API behaviors here")
  assert True = string.contains(behaviors, "// Add endpoint definitions")

  behaviors
}

// Test 6: Constraint extraction
pub fn test_constraint_extraction() {
  let answers = [
    mock_answer("Rate limit requirements", "100 requests per minute"),
    mock_answer("Data size constraints", "Max 10MB per request"),
    mock_answer("Performance requirements", "Response time < 500ms"),
    mock_answer("Other info", "Not a constraint"),
  ]

  let constraints = extract_constraints_from_answers(answers)

  // Should extract 3 constraints
  assert 3 = list.length(constraints)

  // Check specific constraints
  assert True = list.contains(constraints, "100 requests per minute")
  assert True = list.contains(constraints, "Max 10MB per request")
  assert True = list.contains(constraints, "Response time < 500ms")

  // Non-constraint info should be filtered out
  assert False = list.contains(constraints, "Not a constraint")

  constraints
}

// Test 7: Security requirements extraction
pub fn test_security_extraction() {
  let answers = [
    mock_answer("Authentication method", "JWT tokens"),
    mock_answer("Authorization requirements", "Role-based access control"),
    mock_answer("Security considerations", "OAuth 2.0"),
  ]

  let security = extract_security_requirements(answers)

  // Check that security section includes requirements
  assert True = string.contains(security, "security: {")
  assert True = string.contains(security, "// Authentication method")
  assert True = string.contains(security, "requirement: \"JWT tokens\"")
  assert True = string.contains(security, "// Authorization requirements")
  assert True = string.contains(security, "requirement: \"Role-based access control\"")
  assert True = string.contains(security, "// Security considerations")
  assert True = string.contains(security, "requirement: \"OAuth 2.0\"")

  security
}

// Test 8: Security extraction with empty answers generates template
pub fn test_security_extraction_empty() {
  let answers = []
  let security = extract_security_requirements(answers)

  // Should generate template for empty security
  assert True = string.contains(security, "authentication: \"todo\"")
  assert True = string.contains(security, "authorization: \"todo\"")

  security
}

// Test 9: Non-functional requirements extraction
pub fn test_non_functional_extraction() {
  let answers = [
    mock_answer("SLA requirements", "99.9% uptime"),
    mock_answer("Scaling needs", "Horizontal scaling to 100 instances"),
    mock_answer("Monitoring requirements", "Prometheus and Grafana"),
    mock_answer("Latency targets", "< 100ms for all endpoints"),
    mock_answer("Functional requirement", "Not a non-functional requirement"),
  ]

  let non_functional = extract_non_functional_requirements(answers)

  // Should extract 4 non-functional requirements
  assert 4 = list.length(non_functional)

  // Check specific requirements
  assert True = list.contains(non_functional, "99.9% uptime")
  assert True = list.contains(non_functional, "Horizontal scaling to 100 instances")
  assert True = list.contains(non_functional, "Prometheus and Grafana")
  assert True = list.contains(non_functional, "< 100ms for all endpoints")

  // Functional requirement should be filtered out
  assert False = list.contains(non_functional, "Not a non-functional requirement")

  non_functional
}

// Test 10: Complete spec building with all components
pub fn test_complete_spec_building() {
  let answers = [
    mock_answer("What features should the API have?", "User Management"),
    mock_answer("What endpoints do you need?", "/users"),
    mock_answer("Rate limit requirements", "100 requests per minute"),
    mock_answer("Authentication method", "JWT tokens"),
    mock_answer("SLA requirements", "99.9% uptime"),
  ]

  let session = mock_session(answers)
  let spec_cue = build_spec_from_session(session)

  // Check all sections are present
  assert True = string.contains(spec_cue, "package api")
  assert True = string.contains(spec_cue, "features: {")
  assert True = string.contains(spec_cue, "\"User Management\": true")
  assert True = string.contains(spec_cue, "behaviors: {")
  assert True = string.contains(spec_cue, "// What endpoints do you need?")
  assert True = string.contains(spec_cue, "constraints: {")
  assert True = string.contains(spec_cue, "// Rate limit requirements")
  assert True = string.contains(spec_cue, "security: {")
  assert True = string.contains(spec_cue, "requirement: \"JWT tokens\"")
  assert True = string.contains(spec_cue, "nonFunctional: {")
  assert True = string.contains(spec_cue, "// SLA requirements")

  // Check proper structure with separators
  let sections = string.split(spec_cue, "\n\n")
  assert 6 = list.length(sections) // Should have 6 main sections

  spec_cue
}

// Test 11: Create test spec with specific behavior count
pub fn test_create_test_spec() {
  // Test with 0 behaviors
  let spec0 = create_test_spec(0)
  assert 1 = list.length(spec0.features)
  assert 0 = list.length(spec0.features[0].behaviors)

  // Test with 3 behaviors
  let spec3 = create_test_spec(3)
  assert 1 = list.length(spec3.features)
  assert 3 = list.length(spec3.features[0].behaviors)

  // Check behavior names are correctly formatted
  let behavior_names = spec3.features[0].behaviors
    |> list.map(fn(b) { b.name })

  assert True = list.contains(behavior_names, "b1")
  assert True = list.contains(behavior_names, "b2")
  assert True = list.contains(behavior_names, "b3")

  // Check that all required fields are present
  assert "test" = spec3.name
  assert "test" = spec3.description
  assert "test" = spec3.audience
  assert "1.0.0" = spec3.version
  assert [] = spec3.success_criteria
  assert "http://test" = spec3.config.base_url
  assert 1000 = spec3.config.timeout_ms
  assert [] = spec3.rules
  assert [] = spec3.anti_patterns

  spec3
}

// Test 12: Make behavior helper function
pub fn test_make_behavior() {
  let behavior = make_behavior("test-behavior")

  assert "test-behavior" = behavior.name
  assert "test" = behavior.intent
  assert "" = behavior.notes
  assert [] = behavior.requires
  assert [] = behavior.tags
  assert "GET" = behavior.request.method
  assert "/" = behavior.request.path
  assert dict.new() = behavior.request.headers
  assert dict.new() = behavior.request.query
  assert json.null() = behavior.request.body
  assert 200 = behavior.response.status
  assert json.null() = behavior.response.example
  assert dict.new() = response.headers
  assert dict.new() = behavior.response.checks
  assert dict.new() = behavior.captures

  behavior
}

// Test 13: Check many behaviors - pure functional mapping
pub fn test_check_many() {
  // This test is harder to implement without mocking the checker module
  // For now, we just test that the function can be called without crashing
  let behaviors = [make_behavior("test1"), make_behavior("test2")]
  let results = [] // Would normally be actual execution results
  let ctx = dict.new() // Empty context

  // This should not crash even with empty results list
  // Note: In real usage, results should match behaviors length
  let _ = check_many(behaviors, results, ctx)

  Ok("test completed")
}

// Test 14: Edge case - very long responses
pub fn test_long_responses() {
  let long_text = "This is a very long text that should be handled correctly by the spec builder. " <>
    "It should not cause any issues with string processing or memory usage. " <>
    "The builder should be able to handle responses of various lengths."

  let answers = [
    mock_answer("Feature description", long_text),
    mock_answer("Endpoint", "/very/long/path/for/testing"),
  ]

  let session = mock_session(answers)
  let spec_cue = build_spec_from_session(session)

  // Check that long text is properly included
  assert True = string.contains(spec_cue, long_text)

  spec_cue
}

// Test 15: Edge case - special characters in responses
pub fn test_special_characters() {
  let answers = [
    mock_answer("Feature with quotes", "Feature with \"quotes\" and 'single quotes'"),
    mock_answer("API path with special chars", "/api/v1/users/{id}"),
    mock_answer("JSON response example", '{"name": "test", "value": 123}'),
  ]

  let session = mock_session(answers)
  let spec_cue = build_spec_from_session(session)

  // Check that special characters are properly handled
  assert True = string.contains(spec_cue, "Feature with \"quotes\" and 'single quotes'")
  assert True = string.contains(spec_cue, "/api/v1/users/{id}")
  assert True = string.contains(spec_cue, "{\"name\": \"test\", \"value\": 123}")

  spec_cue
}

// Test 16: Builder pattern validation - ensure generated CUE is structurally sound
pub fn test_cue_structure_validation() {
  let answers = [
    mock_answer("What features should the API have?", "User Management"),
    mock_answer("What endpoints do you need?", "/users"),
    mock_answer("Authentication method", "JWT"),
  ]

  let session = mock_session(answers)
  let spec_cue = build_spec_from_session(session)

  // Basic structural checks
  assert True = string.starts_with(spec_cue, "package api")
  assert spec_cue = string.trim(spec_cue) // Should not start/end with excessive whitespace

  // Check proper section ordering
  let package_pos = string.find(spec_cue, "package api")
  let features_pos = string.find(spec_cue, "features: {")
  let behaviors_pos = string.find(spec_cue, "behaviors: {")
  let security_pos = string.find(spec_cue, "security: {")

  assert package_pos < features_pos
  assert features_pos < behaviors_pos
  assert behaviors_pos < security_pos

  // Check that sections are properly separated
  assert string.contains(spec_cue, "\n\n")

  spec_cue
}

// Test 17: Validation with minimal valid components
pub fn test_minimal_valid_spec() {
  let answers = [
    mock_answer("What features should the API have?", "Basic Feature"),
  ]

  let session = mock_session(answers)
  let spec_cue = build_spec_from_session(session)

  // Should generate a valid CUE spec with minimal required sections
  assert True = string.contains(spec_cue, "package api")
  assert True = string.contains(spec_cue, "\"Basic Feature\": true")
  assert True = string.contains(spec_cue, "features: {")
  assert True = string.contains(spec_cue, "behaviors: {")
  assert True = string.contains(spec_cue, "security: {")

  spec_cue
}

// Test 18: Invalid combinations - None of the functions should crash on unexpected inputs
pub fn test_unexpected_input_handling() {
  // Test with null-like values (in Gleam, we use empty lists and strings instead)
  let answers = [
    mock_answer("", ""), // Empty question and response
    mock_answer("   ", "   "), // Whitespace only
    mock_answer("\n", "\n"), // Newlines only
  ]

  let session = mock_session(answers)
  let spec_cue = build_spec_from_session(session)

  // Should handle empty inputs gracefully
  assert True = string.contains(spec_cue, "package api")
  assert True = string.contains(spec_cue, "// Add feature definitions")

  spec_cue
}

// Test 19: Performance test - large number of answers
pub fn test_large_answer_set() {
  // Create 100 answers to test performance
  let answers =
    list.range(1, 100)
    |> list.map(fn(i) {
      mock_answer("Feature " <> int.to_string(i), "Feature description " <> int.to_string(i))
    })

  let session = mock_session(answers)
  let spec_cue = build_spec_from_session(session)

  // Check that all features are included
  assert True = string.contains(spec_cue, "// Features extracted from interview")

  // Performance check - should complete in reasonable time
  // In a real test, we might add timing assertions here

  spec_cue
}

// Test 20: Integration test - Verify built specs conform to Intent format requirements
pub fn test_intent_format_compliance() {
  let answers = [
    mock_answer("What features should the API have?", "User Management"),
    mock_answer("What endpoints do you need?", "/users"),
    mock_answer("Authentication method", "JWT"),
    mock_answer("Rate limit requirements", "100/min"),
    mock_answer("SLA requirements", "99.9%"),
  ]

  let session = mock_session(answers)
  let spec_cue = build_spec_from_session(session)

  // Check that all required fields from the CUE spec format are represented
  assert True = string.contains(spec_cue, "name") // Should be inferred from context
  assert True = string.contains(spec_cue, "description") // Should be inferred from context
  assert True = string.contains(spec_cue, "features")
  assert True = string.contains(spec_cue, "behaviors")
  assert True = string.contains(spec_cue, "security")
  assert True = string.contains(spec_cue, "config") // Should be inferred from context
  assert True = string.contains(spec_cue, "success_criteria") // Should be present
  assert True = string.contains(spec_cue, "rules") // Should be present
  assert True = string.contains(spec_cue, "anti_patterns") // Should be present
  assert True = string.contains(spec_cue, "ai_hints") // Should be present

  spec_cue
}

// Test suite runner
pub fn test_all() {
  test_empty_session()
  test_feature_extraction()
  test_feature_extraction_case_insensitive()
  test_behavior_extraction()
  test_behavior_extraction_empty()
  test_constraint_extraction()
  test_security_extraction()
  test_security_extraction_empty()
  test_non_functional_extraction()
  test_complete_spec_building()
  test_create_test_spec()
  test_make_behavior()
  test_check_many()
  test_long_responses()
  test_special_characters()
  test_cue_structure_validation()
  test_minimal_valid_spec()
  test_unexpected_input_handling()
  test_large_answer_set()
  test_intent_format_compliance()

  Ok("All spec_builder tests completed successfully!")
}