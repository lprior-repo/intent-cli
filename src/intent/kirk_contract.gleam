//// KIRK Contract Generator
////
//// Transforms EARS requirements into Design-by-Contract specifications
//// with preconditions, postconditions, and second-order effects.
////
//// KIRK (Knowledge-Informed Requirements & Kontract) extends behavior
//// specifications with formal contract elements from Bertrand Meyer's
//// Design by Contract methodology.
////
//// ## Contract Elements
////
//// 1. **Preconditions**: What must be true before execution
////    - Authentication requirements
////    - Required input fields
////    - Field-level constraints
////
//// 2. **Postconditions**: What must be true after execution
////    - State changes that occurred
////    - Response guarantees
////
//// 3. **Second-Order Effects**: What happens after this action
////    - Cascading impacts
////    - Triggered events
////    - Side effects
////
//// This module provides Railway-Oriented error handling and produces
//// CUE-validated contract structures.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import intent/kirk/ears_parser

/// Preconditions define what must be true before behavior executes
pub type Preconditions {
  Preconditions(
    auth_required: Bool,
    required_fields: List(String),
    field_constraints: List(FieldConstraint),
  )
}

/// Field-level constraint (e.g., "email must be valid format")
pub type FieldConstraint {
  FieldConstraint(field: String, constraint: String)
}

/// Postconditions define what must be true after behavior executes
pub type Postconditions {
  Postconditions(
    state_changes: List(String),
    response_guarantees: List(ResponseGuarantee),
  )
}

/// Response guarantee (e.g., "status must be 200", "body must contain id")
pub type ResponseGuarantee {
  ResponseGuarantee(aspect: String, guarantee: String)
}

/// Complete KIRK contract for a behavior
pub type KirkContract {
  KirkContract(
    requirement: ears_parser.EarsRequirement,
    preconditions: Preconditions,
    postconditions: Postconditions,
    second_order_effects: List(String),
    confidence: Float,
  )
}

/// Error types for contract generation
pub type ContractError {
  InvalidRequirement(reason: String)
  GenerationFailed(reason: String)
  NoContractGenerated
}

/// Generate KIRK contract from EARS requirement
pub fn generate_contract(
  requirement: ears_parser.EarsRequirement,
) -> Result(KirkContract, ContractError) {
  // Extract preconditions based on requirement pattern
  use preconditions <- result.try(extract_preconditions(requirement))

  // Extract postconditions from system_shall behavior
  use postconditions <- result.try(extract_postconditions(requirement))

  // Infer second-order effects
  let second_order_effects = infer_second_order_effects(requirement)

  // Calculate confidence based on explicit vs inferred information
  let confidence = calculate_contract_confidence(requirement)

  Ok(KirkContract(
    requirement: requirement,
    preconditions: preconditions,
    postconditions: postconditions,
    second_order_effects: second_order_effects,
    confidence: confidence,
  ))
}

/// Generate contracts for multiple requirements
pub fn generate_contracts(
  requirements: List(ears_parser.EarsRequirement),
) -> Result(List(KirkContract), ContractError) {
  requirements
  |> list.map(generate_contract)
  |> result.all
}

/// Extract preconditions from EARS requirement
fn extract_preconditions(
  requirement: ears_parser.EarsRequirement,
) -> Result(Preconditions, ContractError) {
  let behavior_lower = string.lowercase(requirement.system_shall)

  // Detect authentication requirements
  let auth_required =
    string.contains(behavior_lower, "authenticate")
    || string.contains(behavior_lower, "authorized")
    || string.contains(behavior_lower, "logged in")
    || string.contains(behavior_lower, "authenticated")

  // Extract required fields from trigger/state/condition clauses
  let required_fields = case requirement.pattern {
    ears_parser.EventDriven -> {
      case requirement.trigger {
        Some(trigger_text) -> extract_fields_from_text(trigger_text)
        None -> []
      }
    }
    ears_parser.StateDriven -> {
      case requirement.state {
        Some(state_text) -> extract_fields_from_text(state_text)
        None -> []
      }
    }
    ears_parser.Optional -> {
      case requirement.condition {
        Some(condition_text) -> extract_fields_from_text(condition_text)
        None -> []
      }
    }
    ears_parser.Unwanted -> {
      case requirement.condition {
        Some(condition_text) -> extract_fields_from_text(condition_text)
        None -> []
      }
    }
    _ -> []
  }

  // Infer field constraints from behavior description
  let field_constraints = infer_field_constraints(requirement)

  Ok(Preconditions(
    auth_required: auth_required,
    required_fields: required_fields,
    field_constraints: field_constraints,
  ))
}

/// Extract postconditions from system_shall behavior
fn extract_postconditions(
  requirement: ears_parser.EarsRequirement,
) -> Result(Postconditions, ContractError) {
  let behavior_lower = string.lowercase(requirement.system_shall)

  // Detect state changes
  let state_changes =
    []
    |> add_if(string.contains(behavior_lower, "create"), "Resource created")
    |> add_if(string.contains(behavior_lower, "update"), "Resource updated")
    |> add_if(string.contains(behavior_lower, "delete"), "Resource deleted")
    |> add_if(string.contains(behavior_lower, "send"), "Message sent")
    |> add_if(
      string.contains(behavior_lower, "store")
        || string.contains(behavior_lower, "save"),
      "Data persisted",
    )
    |> add_if(
      string.contains(behavior_lower, "authenticate"),
      "Session established",
    )
    |> add_if(
      string.contains(behavior_lower, "authorize"),
      "Permissions granted",
    )

  // Extract response guarantees
  let response_guarantees = extract_response_guarantees(requirement)

  Ok(Postconditions(
    state_changes: state_changes,
    response_guarantees: response_guarantees,
  ))
}

/// Infer second-order effects from requirement
fn infer_second_order_effects(
  requirement: ears_parser.EarsRequirement,
) -> List(String) {
  let behavior_lower = string.lowercase(requirement.system_shall)
  let effects = []

  // Database operations trigger cache invalidation
  let effects = case
    string.contains(behavior_lower, "create")
    || string.contains(behavior_lower, "update")
    || string.contains(behavior_lower, "delete")
  {
    True -> [
      "Cache may need invalidation",
      "Database indexes updated",
      ..effects
    ]
    False -> effects
  }

  // Authentication triggers session management
  let effects = case string.contains(behavior_lower, "authenticate") {
    True -> [
      "Session created in session store",
      "Authentication audit log entry created",
      ..effects
    ]
    False -> effects
  }

  // Authorization checks trigger permission lookups
  let effects = case string.contains(behavior_lower, "authorize") {
    True -> [
      "Permission cache accessed",
      "Authorization decision logged",
      ..effects
    ]
    False -> effects
  }

  // Send/notify operations trigger external systems
  let effects = case
    string.contains(behavior_lower, "send")
    || string.contains(behavior_lower, "notify")
  {
    True -> [
      "External service called",
      "Message queue may be involved",
      "Delivery tracking initiated",
      ..effects
    ]
    False -> effects
  }

  // Error cases trigger error handling
  let effects = case
    string.contains(behavior_lower, "error")
    || string.contains(behavior_lower, "fail")
  {
    True -> [
      "Error logged for monitoring",
      "Error metrics incremented",
      ..effects
    ]
    False -> effects
  }

  // Rate limiting effects
  let effects = case requirement.pattern {
    ears_parser.Unwanted -> [
      "Rate limiting counter incremented",
      "Security monitoring alerted",
      ..effects
    ]
    _ -> effects
  }

  list.reverse(effects)
}

/// Extract field names from conditional text (when/while/where/if clauses)
fn extract_fields_from_text(text: String) -> List(String) {
  let lower = string.lowercase(text)
  let fields = []

  // Common field patterns
  let fields = case string.contains(lower, "user") {
    True -> ["user", ..fields]
    False -> fields
  }

  let fields = case string.contains(lower, "credential") {
    True -> ["credentials", ..fields]
    False -> fields
  }

  let fields = case string.contains(lower, "token") {
    True -> ["token", ..fields]
    False -> fields
  }

  let fields = case string.contains(lower, "id") {
    True -> ["id", ..fields]
    False -> fields
  }

  let fields = case string.contains(lower, "email") {
    True -> ["email", ..fields]
    False -> fields
  }

  let fields = case string.contains(lower, "password") {
    True -> ["password", ..fields]
    False -> fields
  }

  list.reverse(fields)
}

/// Infer field constraints from behavior
fn infer_field_constraints(
  requirement: ears_parser.EarsRequirement,
) -> List(FieldConstraint) {
  let behavior_lower = string.lowercase(requirement.system_shall)
  let constraints = []

  // Email validation
  let constraints = case string.contains(behavior_lower, "email") {
    True -> [
      FieldConstraint("email", "Must be valid email format"),
      ..constraints
    ]
    False -> constraints
  }

  // Password constraints
  let constraints = case string.contains(behavior_lower, "password") {
    True -> [
      FieldConstraint("password", "Must meet security requirements"),
      ..constraints
    ]
    False -> constraints
  }

  // ID constraints
  let constraints = case string.contains(behavior_lower, "id") {
    True -> [FieldConstraint("id", "Must be valid identifier"), ..constraints]
    False -> constraints
  }

  // Non-empty constraints for creates
  let constraints = case string.contains(behavior_lower, "create") {
    True -> [
      FieldConstraint("required_fields", "Must not be empty"),
      ..constraints
    ]
    False -> constraints
  }

  list.reverse(constraints)
}

/// Extract response guarantees from behavior
fn extract_response_guarantees(
  requirement: ears_parser.EarsRequirement,
) -> List(ResponseGuarantee) {
  let behavior_lower = string.lowercase(requirement.system_shall)
  let guarantees = []

  // Success responses
  let guarantees = case string.contains(behavior_lower, "return") {
    True -> {
      case string.contains(behavior_lower, "200") {
        True -> [ResponseGuarantee("status", "Must be 200 OK"), ..guarantees]
        False -> guarantees
      }
    }
    False -> guarantees
  }

  // Created responses
  let guarantees = case
    string.contains(behavior_lower, "create")
    && string.contains(behavior_lower, "201")
  {
    True -> [ResponseGuarantee("status", "Must be 201 Created"), ..guarantees]
    False -> guarantees
  }

  // Error responses
  let guarantees = case string.contains(behavior_lower, "401") {
    True -> [
      ResponseGuarantee("status", "Must be 401 Unauthorized"),
      ..guarantees
    ]
    False -> guarantees
  }

  let guarantees = case string.contains(behavior_lower, "403") {
    True -> [ResponseGuarantee("status", "Must be 403 Forbidden"), ..guarantees]
    False -> guarantees
  }

  let guarantees = case string.contains(behavior_lower, "404") {
    True -> [ResponseGuarantee("status", "Must be 404 Not Found"), ..guarantees]
    False -> guarantees
  }

  let guarantees = case string.contains(behavior_lower, "400") {
    True -> [
      ResponseGuarantee("status", "Must be 400 Bad Request"),
      ..guarantees
    ]
    False -> guarantees
  }

  // Content guarantees
  let guarantees = case
    string.contains(behavior_lower, "contain")
    || string.contains(behavior_lower, "include")
  {
    True -> [
      ResponseGuarantee("body", "Must contain expected fields"),
      ..guarantees
    ]
    False -> guarantees
  }

  list.reverse(guarantees)
}

/// Helper to conditionally add item to list
fn add_if(list: List(a), condition: Bool, item: a) -> List(a) {
  case condition {
    True -> [item, ..list]
    False -> list
  }
}

/// Calculate confidence score for generated contract
fn calculate_contract_confidence(
  requirement: ears_parser.EarsRequirement,
) -> Float {
  let behavior_lower = string.lowercase(requirement.system_shall)

  // Start with base confidence
  let confidence = 0.5

  // Higher confidence for explicit patterns
  let confidence = case requirement.pattern {
    ears_parser.Unwanted -> confidence +. 0.2
    // Unwanted has explicit constraints
    ears_parser.EventDriven -> confidence +. 0.15
    // Event-driven has clear triggers
    ears_parser.StateDriven -> confidence +. 0.15
    // State-driven has clear conditions
    _ -> confidence +. 0.1
  }

  // Higher confidence if behavior mentions specific HTTP codes
  let confidence = case
    string.contains(behavior_lower, "200")
    || string.contains(behavior_lower, "201")
    || string.contains(behavior_lower, "400")
    || string.contains(behavior_lower, "401")
    || string.contains(behavior_lower, "403")
    || string.contains(behavior_lower, "404")
  {
    True -> confidence +. 0.15
    False -> confidence
  }

  // Higher confidence if behavior mentions authentication
  let confidence = case
    string.contains(behavior_lower, "auth")
    || string.contains(behavior_lower, "login")
  {
    True -> confidence +. 0.1
    False -> confidence
  }

  // Cap at 1.0
  case confidence >. 1.0 {
    True -> 1.0
    False -> confidence
  }
}

/// Format contract as human-readable text
pub fn format_contract(contract: KirkContract) -> String {
  let lines = [
    "KIRK Contract",
    "═════════════",
    "",
    "Requirement: " <> contract.requirement.raw_text,
    "",
    "Preconditions:",
  ]

  let lines = case contract.preconditions.auth_required {
    True -> list.append(lines, ["  • Authentication required"])
    False -> list.append(lines, ["  • No authentication required"])
  }

  let lines = case contract.preconditions.required_fields {
    [] -> lines
    fields -> {
      let field_list = string.join(fields, ", ")
      list.append(lines, ["  • Required fields: " <> field_list])
    }
  }

  let lines =
    list.fold(contract.preconditions.field_constraints, lines, fn(acc, c) {
      list.append(acc, ["  • " <> c.field <> ": " <> c.constraint])
    })

  let lines = list.append(lines, ["", "Postconditions:"])

  let lines = case contract.postconditions.state_changes {
    [] -> list.append(lines, ["  • No state changes"])
    changes ->
      list.fold(changes, lines, fn(acc, change) {
        list.append(acc, ["  • " <> change])
      })
  }

  let lines =
    list.fold(contract.postconditions.response_guarantees, lines, fn(acc, g) {
      list.append(acc, ["  • " <> g.aspect <> ": " <> g.guarantee])
    })

  let lines = case contract.second_order_effects {
    [] -> list.append(lines, ["", "Second-Order Effects: None identified"])
    effects -> {
      let with_header = list.append(lines, ["", "Second-Order Effects:"])
      list.fold(effects, with_header, fn(acc, effect) {
        list.append(acc, ["  • " <> effect])
      })
    }
  }

  string.join(lines, "\n")
}
