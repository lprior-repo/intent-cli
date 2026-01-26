//// Enhanced Bead Generator
////
//// Generates enriched work items (beads) with:
//// - EARS requirements patterns
//// - Contract specifications (pre/post conditions)
//// - Type definitions needed
//// - Test cases to write
////
//// This provides AI agents with comprehensive implementation context.

import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import intent/bead_templates
import intent/types

// =============================================================================
// TYPES
// =============================================================================

/// Simplified EARS pattern information for beads
pub type EarsPatternInfo {
  EarsPatternInfo(
    pattern_type: String,
    trigger: Option(String),
    state: Option(String),
    condition: Option(String),
    behavior: String,
  )
}

/// Contract check with metadata
pub type ContractCheck {
  ContractCheck(rule: String, why: String, check_name: String)
}

/// Contract specifications for a bead
pub type BeadContracts {
  BeadContracts(
    preconditions: List(String),
    postconditions: List(ContractCheck),
    invariants: List(String),
  )
}

/// BDD-style test scenario
pub type TestCase {
  TestCase(
    name: String,
    given: List(String),
    when: String,
    then: String,
    assertion: String,
  )
}

/// Type signature needed for implementation
pub type TypeDefinition {
  TypeDefinition(name: String, signature: String, purpose: String)
}

/// Enhanced bead with all AI implementation context
pub type EnhancedBeadRecord {
  EnhancedBeadRecord(
    base: bead_templates.BeadRecord,
    ears_patterns: List(EarsPatternInfo),
    contracts: BeadContracts,
    types: List(TypeDefinition),
    tests: List(TestCase),
  )
}

// =============================================================================
// EXTRACTION FUNCTIONS
// =============================================================================

/// Extract contract specifications from behavior
///
/// Preconditions come from behavior.requires (dependencies that must be satisfied first)
/// Postconditions come from response.checks (validations that must pass after execution)
/// Invariants are currently empty (could be enhanced with global rules)
pub fn extract_contracts_from_behavior(
  behavior: types.Behavior,
) -> BeadContracts {
  // Extract preconditions from requires list
  let preconditions = behavior.requires

  // Extract postconditions from response.checks
  let postconditions =
    behavior.response.checks
    |> dict.to_list
    |> list.map(fn(pair) {
      let #(check_name, check) = pair
      ContractCheck(rule: check.rule, why: check.why, check_name: check_name)
    })

  // Invariants empty for now (could add global rules later)
  let invariants = []

  BeadContracts(
    preconditions: preconditions,
    postconditions: postconditions,
    invariants: invariants,
  )
}

/// Extract EARS patterns from intent text
///
/// Detects EARS keywords and extracts structured pattern information:
/// - WHEN [trigger] THE SYSTEM SHALL [behavior] -> EventDriven
/// - WHILE [state] THE SYSTEM SHALL [behavior] -> StateDriven
/// - WHERE [condition] THE SYSTEM SHALL [behavior] -> Optional
/// - IF [condition] THEN THE SYSTEM SHALL NOT [behavior] -> Unwanted
/// - Default to Ubiquitous if no keywords found
pub fn extract_ears_patterns(intent: String) -> List(EarsPatternInfo) {
  let intent_upper = string.uppercase(intent)

  // Try each pattern in order of precedence
  case try_event_driven_pattern(intent_upper, intent) {
    Ok(pattern) -> [pattern]
    Error(_) ->
      case try_state_driven_pattern(intent_upper, intent) {
        Ok(pattern) -> [pattern]
        Error(_) ->
          case try_optional_pattern(intent_upper, intent) {
            Ok(pattern) -> [pattern]
            Error(_) ->
              case try_unwanted_pattern(intent_upper, intent) {
                Ok(pattern) -> [pattern]
                Error(_) -> [make_ubiquitous_pattern(intent)]
              }
          }
      }
  }
}

/// Try to parse EventDriven pattern: WHEN [trigger] THE SYSTEM SHALL [behavior]
fn try_event_driven_pattern(
  intent_upper: String,
  _original: String,
) -> Result(EarsPatternInfo, Nil) {
  case
    string.contains(intent_upper, "WHEN")
    && string.contains(intent_upper, "THE SYSTEM SHALL")
  {
    True ->
      parse_ears_pattern(
        intent_upper,
        "WHEN",
        "THE SYSTEM SHALL",
        fn(trigger, behavior) {
          EarsPatternInfo(
            pattern_type: "EventDriven",
            trigger: Some(trigger),
            state: None,
            condition: None,
            behavior: behavior,
          )
        },
      )
    False -> Error(Nil)
  }
}

/// Try to parse StateDriven pattern: WHILE [state] THE SYSTEM SHALL [behavior]
fn try_state_driven_pattern(
  intent_upper: String,
  _original: String,
) -> Result(EarsPatternInfo, Nil) {
  case
    string.contains(intent_upper, "WHILE")
    && string.contains(intent_upper, "THE SYSTEM SHALL")
  {
    True ->
      parse_ears_pattern(
        intent_upper,
        "WHILE",
        "THE SYSTEM SHALL",
        fn(state, behavior) {
          EarsPatternInfo(
            pattern_type: "StateDriven",
            trigger: None,
            state: Some(state),
            condition: None,
            behavior: behavior,
          )
        },
      )
    False -> Error(Nil)
  }
}

/// Try to parse Optional pattern: WHERE [condition] THE SYSTEM SHALL [behavior]
fn try_optional_pattern(
  intent_upper: String,
  _original: String,
) -> Result(EarsPatternInfo, Nil) {
  case
    string.contains(intent_upper, "WHERE")
    && string.contains(intent_upper, "THE SYSTEM SHALL")
  {
    True ->
      parse_ears_pattern(
        intent_upper,
        "WHERE",
        "THE SYSTEM SHALL",
        fn(condition, behavior) {
          EarsPatternInfo(
            pattern_type: "Optional",
            trigger: None,
            state: None,
            condition: Some(condition),
            behavior: behavior,
          )
        },
      )
    False -> Error(Nil)
  }
}

/// Try to parse Unwanted pattern: IF [condition] THEN THE SYSTEM SHALL NOT [behavior]
fn try_unwanted_pattern(
  intent_upper: String,
  _original: String,
) -> Result(EarsPatternInfo, Nil) {
  case
    string.contains(intent_upper, "SHALL NOT")
    || string.contains(intent_upper, "THEN THE SYSTEM SHALL NOT")
  {
    True -> {
      let parts = string.split(intent_upper, "SHALL NOT")
      case parts {
        [before, after] -> {
          let condition =
            before
            |> string.replace("IF", "")
            |> string.replace("THEN THE SYSTEM", "")
            |> string.trim
            |> string.lowercase

          let behavior = string.trim(after) |> string.lowercase

          Ok(EarsPatternInfo(
            pattern_type: "Unwanted",
            trigger: None,
            state: None,
            condition: Some(condition),
            behavior: behavior,
          ))
        }
        _ -> Error(Nil)
      }
    }
    False -> Error(Nil)
  }
}

/// Generic EARS pattern parser for SHALL patterns
fn parse_ears_pattern(
  text: String,
  keyword: String,
  separator: String,
  builder: fn(String, String) -> EarsPatternInfo,
) -> Result(EarsPatternInfo, Nil) {
  let parts = string.split(text, separator)
  case parts {
    [before, after] -> {
      let prefix =
        before
        |> string.replace(keyword, "")
        |> string.trim
        |> string.lowercase

      let behavior = string.trim(after) |> string.lowercase

      Ok(builder(prefix, behavior))
    }
    _ -> Error(Nil)
  }
}

/// Helper to create ubiquitous pattern (default/fallback)
fn make_ubiquitous_pattern(intent: String) -> EarsPatternInfo {
  EarsPatternInfo(
    pattern_type: "Ubiquitous",
    trigger: None,
    state: None,
    condition: None,
    behavior: intent,
  )
}

/// Generate type definitions from behavior
///
/// Infers type signatures needed for implementation:
/// - Handler function based on method and path
/// - Request body type (if present)
/// - Response body type (if present)
pub fn generate_type_definitions(
  behavior: types.Behavior,
) -> List(TypeDefinition) {
  // Generate handler function signature
  let method_str = types.method_to_string(behavior.request.method)
  let path_clean =
    behavior.request.path
    |> string.replace("/", "_")
    |> string.replace("{", "")
    |> string.replace("}", "")

  let handler_name = "handle" <> path_clean
  let handler_sig =
    "fn " <> handler_name <> "(request: Request) -> Response<ResponseBody>"

  let handler_def =
    TypeDefinition(
      name: handler_name,
      signature: handler_sig,
      purpose: method_str
        <> " "
        <> behavior.request.path
        <> " - "
        <> behavior.intent,
    )

  // Build list of type definitions
  let request_body_def = case json.to_string(behavior.request.body) {
    "null" -> None
    _ ->
      Some(TypeDefinition(
        name: "RequestBody",
        signature: "type RequestBody",
        purpose: "Request payload for " <> behavior.request.path,
      ))
  }

  let response_body_def = case json.to_string(behavior.response.example) {
    "null" -> None
    _ ->
      Some(TypeDefinition(
        name: "ResponseBody",
        signature: "type ResponseBody",
        purpose: "Response payload for " <> behavior.request.path,
      ))
  }

  // Combine all type definitions
  [handler_def]
  |> prepend_option(response_body_def)
  |> prepend_option(request_body_def)
}

/// Helper to prepend optional value to list
fn prepend_option(lst: List(a), opt: Option(a)) -> List(a) {
  case opt {
    Some(value) -> [value, ..lst]
    None -> lst
  }
}

/// Generate test cases from behavior checks
///
/// Creates BDD-style test scenarios (given/when/then/assertion) for each check
pub fn generate_test_cases(behavior: types.Behavior) -> List(TestCase) {
  let method_str = types.method_to_string(behavior.request.method)
  let when_text =
    behavior.intent <> " with " <> method_str <> " " <> behavior.request.path

  behavior.response.checks
  |> dict.to_list
  |> list.map(fn(pair) {
    let #(check_name, check) = pair
    TestCase(
      name: check_name <> " validation",
      given: behavior.requires,
      when: when_text,
      then: check.why,
      assertion: check.rule,
    )
  })
}

/// Generate enhanced bead from behavior
///
/// Orchestrates all extraction functions to produce a comprehensive bead record
pub fn generate_enhanced_bead(
  behavior: types.Behavior,
  profile_type: String,
) -> EnhancedBeadRecord {
  // Create base bead record
  let base =
    bead_templates.BeadRecord(
      title: "Implement " <> behavior.name,
      description: behavior.intent,
      profile_type: profile_type,
      priority: 3,
      issue_type: "behavior_implementation",
      labels: behavior.tags,
      ai_hints: "Implement behavior: " <> behavior.intent,
      acceptance_criteria: [
        "Behavior implementation complete",
        "All checks pass",
        "Tests written and passing",
      ],
      dependencies: behavior.requires,
    )

  // Extract all enhanced information
  let contracts = extract_contracts_from_behavior(behavior)
  let ears_patterns = extract_ears_patterns(behavior.intent)
  let type_defs = generate_type_definitions(behavior)
  let test_cases = generate_test_cases(behavior)

  EnhancedBeadRecord(
    base: base,
    ears_patterns: ears_patterns,
    contracts: contracts,
    types: type_defs,
    tests: test_cases,
  )
}
