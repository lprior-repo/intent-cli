//// Implementation Prompt Generator with Codebase Context
////
//// Generates AI-ready implementation prompts from bead work items with
//// full codebase context and style guidelines.

import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option
import gleam/string

// ============================================================================
// Type Definitions
// ============================================================================

/// AI prompt generation profile
pub type PromptProfile {
  AiProfile
  HumanProfile
}

/// Implementation guideline with code patterns
pub type ImplementationGuide {
  ImplementationGuide(
    architecture: String,
    error_handling: String,
    code_patterns: String,
    testing: String,
  )
}

/// Exported security hints for AI profile
pub type SecurityHintsExport {
  SecurityHintsExport(
    password_hashing: String,
    jwt_algorithm: String,
    jwt_expiry: String,
    rate_limiting: String,
  )
}

/// AI profile information for context-aware prompts
pub type AIProfileInfo {
  AIProfileInfo(
    persona: String,
    constraints: List(String),
    guardrails: List(String),
    implementation_stack: List(String),
    security_hints: SecurityHintsExport,
    entity_fields: List(String),
    pitfalls: List(String),
  )
}

// ============================================================================
// FUNCTIONAL CORE - Pure Function Logic
// ============================================================================

/// Build implementation architecture guidelines
fn build_architecture_guide() -> String {
  "FUNCTIONAL CORE / IMPERATIVE SHELL (FC/IS) ARCHITECTURE\n"
  <> "========================================================\n\n"
  <> "FUNCTIONAL CORE (Pure functions - no I/O):\n"
  <> "- Business logic isolated from side effects\n"
  <> "- Deterministic, testable functions\n"
  <> "- No file I/O, network, or state mutations\n"
  <> "- Returns Result types for error handling\n\n"
  <> "IMPERATIVE SHELL (I/O wrappers with dependency injection):\n"
  <> "- File I/O, network operations, state management\n"
  <> "- Injects dependencies into pure functions\n"
  <> "- Minimal business logic, mostly orchestration\n"
  <> "- Uses Result types to propagate errors to caller\n"
}

/// Build error handling guidelines
fn build_error_handling_guide() -> String {
  "ERROR HANDLING\n"
  <> "==============\n\n"
  <> "1. Use Result types for recoverable errors\n"
  <> "2. Use let-try for error propagation:\n"
  <> "   use value <- result.try(operation())\n"
  <> "3. Map errors to user-friendly messages\n"
  <> "4. Never unwrap - always handle both cases\n"
}

/// Build code pattern guidelines
fn build_pattern_guide() -> String {
  "COMMON CODE PATTERNS\n"
  <> "====================\n\n"
  <> "List processing:\n"
  <> "  list |> list.map(transform) |> list.filter(predicate)\n\n"
  <> "String handling:\n"
  <> "  string.split(s, \",\") |> list.map(string.trim)\n\n"
  <> "Dictionary operations:\n"
  <> "  dict.get(d, key) |> result.ok_or(\"not found\")\n\n"
  <> "Pattern matching:\n"
  <> "  case value { Ok(v) -> do_success(v); Error(e) -> do_error(e) }\n"
}

/// Build style guidelines section
fn build_style_section() -> String {
  "CODEBASE STYLE GUIDE\n"
  <> "====================\n"
  <> "- Result types required for error handling\n"
  <> "- Exhaustive matching for all case statements\n"
  <> "- Use pipelines (|>) instead of nested calls\n"
  <> "- No panics/unwraps/defaults\n"
  <> "- Small focused functions\n"
  <> "- All fields explicit in specs\n"
}

/// Build guardrails section with implementation constraints
fn build_guardrails_section() -> String {
  "GUARDRAILS\n"
  <> "==========\n\n"
  <> "ERROR HANDLING:\n"
  <> "  - Use Result<T, E> for all fallible operations\n"
  <> "  - Never use unwrap(), expect(), or panic!\n"
  <> "  - Handle both Ok and Error branches in case expressions\n"
  <> "  - Use let-try for error propagation: use x <- result.try(op())\n\n"
  <> "CODE PATTERNS:\n"
  <> "  - Compose functions with pipelines: value |> list.map(f) |> list.filter(g)\n"
  <> "  - Use small, single-purpose functions (< 20 lines ideal)\n"
  <> "  - All pattern matches must be exhaustive (no fallthrough)\n"
  <> "  - Explicitly name all function parameters (no implicit args)\n\n"
  <> "ARCHITECTURE:\n"
  <> "  - Separate Functional Core (pure logic) from Imperative Shell (I/O)\n"
  <> "  - Keep side effects at module boundaries\n"
  <> "  - Inject dependencies; don't hardcode values\n"
  <> "  - Return Result types from module boundaries\n\n"
  <> "TESTING & BUILD:\n"
  <> "  - Run: gleam build (must pass with zero warnings)\n"
  <> "  - Run: gleam test (all tests must pass)\n"
  <> "  - Test both success and error paths\n"
  <> "  - Add doc comments to public functions\n\n"
  <> "DOCUMENTATION:\n"
  <> "  - Add module-level doc comments (////) explaining purpose\n"
  <> "  - Document all public functions with examples when helpful\n"
  <> "  - Document error cases in Result types\n"
  <> "  - Keep comments current when code changes\n\n"
  <> "SPEC CONSTRAINTS:\n"
  <> "  - All fields in records must be explicit (no defaults)\n"
  <> "  - Use exhaustive matching for all union types\n"
  <> "  - No partial application or implicit currying\n"
}

/// Build AI profile information with constraints and guardrails
pub fn build_ai_profile(info: AIProfileInfo) -> String {
  let persona_section =
    "AI PERSONA\n" <> "==========\n" <> info.persona <> "\n\n"

  let constraints_text =
    info.constraints |> list.map(fn(c) { "- " <> c }) |> string.join("\n")

  let constraints_section =
    "CONSTRAINTS\n" <> "===========\n" <> constraints_text <> "\n\n"

  let guardrails_text =
    info.guardrails |> list.map(fn(g) { "- " <> g }) |> string.join("\n")

  let guardrails_section =
    "GUARDRAILS FOR THIS SPEC\n"
    <> "========================\n"
    <> guardrails_text
    <> "\n\n"

  let stack_text =
    info.implementation_stack
    |> list.map(fn(s) { "- " <> s })
    |> string.join("\n")

  let stack_section =
    "RECOMMENDED STACK\n" <> "=================\n" <> stack_text <> "\n\n"

  let security_section =
    "SECURITY REQUIREMENTS\n"
    <> "====================\n"
    <> "- Password Hashing: "
    <> info.security_hints.password_hashing
    <> "\n"
    <> "- JWT Algorithm: "
    <> info.security_hints.jwt_algorithm
    <> "\n"
    <> "- JWT Expiry: "
    <> info.security_hints.jwt_expiry
    <> "\n"
    <> "- Rate Limiting: "
    <> info.security_hints.rate_limiting
    <> "\n\n"

  let entities_section = case list.is_empty(info.entity_fields) {
    False -> {
      let entity_text =
        info.entity_fields |> list.map(fn(f) { "- " <> f }) |> string.join("\n")
      "ENTITY FIELDS\n" <> "=============\n" <> entity_text <> "\n\n"
    }
    True -> ""
  }

  let pitfalls_section = case list.is_empty(info.pitfalls) {
    False -> {
      let pitfall_text =
        info.pitfalls |> list.map(fn(p) { "- " <> p }) |> string.join("\n")
      "COMMON PITFALLS TO AVOID\n"
      <> "========================\n"
      <> pitfall_text
      <> "\n\n"
    }
    True -> ""
  }

  persona_section
  <> constraints_section
  <> guardrails_section
  <> stack_section
  <> security_section
  <> entities_section
  <> pitfalls_section
}

/// Create AIProfileInfo from spec hints with built-in persona and guardrails
pub fn create_ai_profile_info(
  stack: List(String),
  password_hashing: String,
  jwt_algorithm: String,
  jwt_expiry: String,
  rate_limiting: String,
  entity_fields: List(String),
  pitfalls: List(String),
) -> AIProfileInfo {
  AIProfileInfo(
    persona: "You are an expert Gleam developer implementing contract-driven API specifications. "
      <> "Your role is to write production-grade code that follows functional programming principles, "
      <> "maintains exhaustive type safety, and handles errors gracefully with Result types. "
      <> "You understand the Functional Core/Imperative Shell architecture pattern.",
    constraints: [
      "Never use unwrap(), expect(), panic() or functions that can crash",
      "Never write code that doesn't compile with 'gleam build'",
      "Never skip exhaustive pattern matching in case expressions",
      "Never hide errors - always propagate with Result types",
      "Never mix business logic with I/O operations",
      "Never make assumptions about input - validate and return Errors",
      "Never expose implementation details in public APIs",
    ],
    guardrails: [
      "All functions must return Result<T, E> for fallible operations",
      "All case expressions must be exhaustive (cover all variants)",
      "All code must compose with pipelines (|>) for readability",
      "All modules must separate pure logic from side effects",
      "All public functions must be documented with doc comments",
      "All field accesses must be explicit (no defaults or implicit values)",
      "All dependencies must be injected, never hardcoded",
    ],
    implementation_stack: stack,
    security_hints: SecurityHintsExport(
      password_hashing: password_hashing,
      jwt_algorithm: jwt_algorithm,
      jwt_expiry: jwt_expiry,
      rate_limiting: rate_limiting,
    ),
    entity_fields: entity_fields,
    pitfalls: pitfalls,
  )
}

/// Build a complete implementation guide
pub fn build_implementation_guide() -> ImplementationGuide {
  ImplementationGuide(
    architecture: build_architecture_guide(),
    error_handling: build_error_handling_guide(),
    code_patterns: build_pattern_guide(),
    testing: "Run: gleam build && gleam test\n",
  )
}

// ============================================================================
// Prompt Building
// ============================================================================

/// Build implementation prompt with full codebase context
pub fn build_implementation_prompt(
  bead_id: String,
  bead_title: String,
  bead_description: String,
  bead_type: String,
  bead_priority: Int,
  guide: ImplementationGuide,
  profile: PromptProfile,
) -> String {
  build_implementation_prompt_with_profile(
    bead_id,
    bead_title,
    bead_description,
    bead_type,
    bead_priority,
    guide,
    profile,
    option.None,
  )
}

/// Build implementation prompt with optional AI profile info
pub fn build_implementation_prompt_with_profile(
  bead_id: String,
  bead_title: String,
  bead_description: String,
  bead_type: String,
  bead_priority: Int,
  guide: ImplementationGuide,
  profile: PromptProfile,
  ai_profile_info: option.Option(AIProfileInfo),
) -> String {
  let header = "=== IMPLEMENTATION PROMPT ===\n\n"

  let bead_info =
    "BEAD ID: "
    <> bead_id
    <> "\n"
    <> "TITLE: "
    <> bead_title
    <> "\n"
    <> "TYPE: "
    <> bead_type
    <> "\n"
    <> "PRIORITY: "
    <> int.to_string(bead_priority)
    <> "\n\n"

  let description_section =
    "DESCRIPTION\n" <> "===========\n" <> bead_description <> "\n\n"

  let style_section = build_style_section() <> "\n"

  // AI Profile section - only included for AiProfile when info is provided
  let ai_profile_section = case profile {
    AiProfile ->
      case ai_profile_info {
        option.Some(info) -> build_ai_profile(info) <> "\n"
        option.None -> ""
      }
    HumanProfile -> ""
  }

  let guardrails_section = build_guardrails_section() <> "\n"

  let architecture_section = case profile {
    AiProfile ->
      guide.architecture
      <> "\n"
      <> guide.error_handling
      <> "\n"
      <> guide.code_patterns
      <> "\n"
    HumanProfile ->
      "For implementation details, see CLAUDE.md style guide.\n"
      <> guide.architecture
      <> "\n"
  }

  let checklist =
    "IMPLEMENTATION CHECKLIST\n"
    <> "========================\n"
    <> "- [ ] Build succeeds: gleam build\n"
    <> "- [ ] Tests pass: gleam test\n"
    <> "- [ ] No compiler warnings\n"
    <> "- [ ] Follows guardrails above\n"
    <> "- [ ] Exhaustive pattern matching\n"
    <> "- [ ] Uses pipelines (|>)\n"
    <> "- [ ] All criteria met\n"
    <> "- [ ] Error cases handled\n"

  let next_steps =
    "\nNEXT STEPS\n"
    <> "==========\n"
    <> "1. Review this prompt and codebase\n"
    <> "2. Study guardrails above\n"
    <> "3. bd update "
    <> bead_id
    <> " --status in_progress\n"
    <> "4. Implement the work\n"
    <> "5. Verify: gleam build && gleam test\n"
    <> "6. bd close "
    <> bead_id
    <> " --reason 'Completed: ...'\n"

  header
  <> bead_info
  <> description_section
  <> style_section
  <> ai_profile_section
  <> guardrails_section
  <> architecture_section
  <> checklist
  <> next_steps
}

// ============================================================================
// JSON Prompt Building
// ============================================================================

/// Convert AIProfileInfo to JSON
pub fn ai_profile_info_to_json(profile: AIProfileInfo) -> Json {
  let security_obj =
    json.object([
      #(
        "password_hashing",
        json.string(profile.security_hints.password_hashing),
      ),
      #("jwt_algorithm", json.string(profile.security_hints.jwt_algorithm)),
      #("jwt_expiry", json.string(profile.security_hints.jwt_expiry)),
      #("rate_limiting", json.string(profile.security_hints.rate_limiting)),
    ])

  json.object([
    #("persona", json.string(profile.persona)),
    #("constraints", json.array(profile.constraints, json.string)),
    #("guardrails", json.array(profile.guardrails, json.string)),
    #(
      "implementation_stack",
      json.array(profile.implementation_stack, json.string),
    ),
    #("security_hints", security_obj),
    #("entity_fields", json.array(profile.entity_fields, json.string)),
    #("pitfalls", json.array(profile.pitfalls, json.string)),
  ])
}

/// Build JSON representation of implementation prompt
pub fn build_json_implementation_prompt(
  bead_id: String,
  bead_title: String,
  bead_description: String,
  bead_type: String,
  bead_priority: Int,
  guide: ImplementationGuide,
) -> Json {
  build_json_implementation_prompt_with_profile(
    bead_id,
    bead_title,
    bead_description,
    bead_type,
    bead_priority,
    guide,
    option.None,
  )
}

/// Build JSON representation with optional AI profile info
pub fn build_json_implementation_prompt_with_profile(
  bead_id: String,
  bead_title: String,
  bead_description: String,
  bead_type: String,
  bead_priority: Int,
  guide: ImplementationGuide,
  ai_profile_info: option.Option(AIProfileInfo),
) -> Json {
  let bead_obj =
    json.object([
      #("id", json.string(bead_id)),
      #("title", json.string(bead_title)),
      #("description", json.string(bead_description)),
      #("type", json.string(bead_type)),
      #("priority", json.int(bead_priority)),
    ])

  let guidelines_obj =
    json.object([
      #("architecture", json.string(guide.architecture)),
      #("error_handling", json.string(guide.error_handling)),
      #("code_patterns", json.string(guide.code_patterns)),
      #("testing", json.string(guide.testing)),
    ])

  let guardrails = build_guardrails_section()

  let base_fields = [
    #("bead", bead_obj),
    #("guidelines", guidelines_obj),
    #("guardrails", json.string(guardrails)),
  ]

  let all_fields = case ai_profile_info {
    option.Some(info) ->
      list.append(base_fields, [#("profile", ai_profile_info_to_json(info))])
    option.None -> base_fields
  }

  json.object(all_fields)
}

// ============================================================================
// Stub Functions for Integration
// ============================================================================

/// Stub: Would load bead from storage in full implementation
pub fn generate_implementation_prompt(
  _bead_id: String,
  profile: PromptProfile,
) -> Result(String, String) {
  let guide = build_implementation_guide()
  Ok(build_implementation_prompt(
    "bead-id",
    "Example Bead",
    "This is an example prompt",
    "feature",
    1,
    guide,
    profile,
  ))
}

/// Stub: Would load bead from storage in full implementation
pub fn generate_json_implementation_prompt(
  _bead_id: String,
) -> Result(Json, String) {
  let guide = build_implementation_guide()
  Ok(build_json_implementation_prompt(
    "bead-id",
    "Example Bead",
    "This is an example prompt",
    "feature",
    1,
    guide,
  ))
}
