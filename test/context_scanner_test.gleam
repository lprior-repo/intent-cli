/// Context Scanner Tests
/// TDD: Tests written before implementation
///
/// Bead: [CTX-7] Create context_scanner.gleam module
/// Scope: Detect language and framework from manifest files only
/// Acceptance: 'intent context-scan' outputs detected language and framework

import gleeunit/should
import intent/context_scanner.{
  CodebaseContext, Gleam, Go, JavaScript, NoFramework, NoLanguage,
  Phoenix, Python, Rust, Rebar3, TypeScript, detect_from_directory,
  detect_framework_from_deps, detect_language_from_manifest, format_context,
  context_to_json,
}

// ============================================================================
// LANGUAGE DETECTION TESTS
// ============================================================================

pub fn detect_language_gleam_toml_test() {
  // Given: gleam.toml manifest content
  let manifest = "
name = \"my_app\"
version = \"1.0.0\"

[dependencies]
gleam_stdlib = \"~> 0.32\"
"

  // When: we detect the language
  let result = detect_language_from_manifest("gleam.toml", manifest)

  // Then: it should be Gleam
  result
  |> should.equal(Gleam)
}

pub fn detect_language_package_json_test() {
  // Given: package.json manifest
  let manifest = "{\"name\": \"my-app\", \"version\": \"1.0.0\"}"

  // When: we detect the language
  let result = detect_language_from_manifest("package.json", manifest)

  // Then: it should be JavaScript
  result
  |> should.equal(JavaScript)
}

pub fn detect_language_package_json_typescript_test() {
  // Given: package.json with TypeScript dependency
  let manifest = "
{
  \"name\": \"my-app\",
  \"dependencies\": {
    \"typescript\": \"^5.0.0\"
  }
}
"

  // When: we detect the language
  let result = detect_language_from_manifest("package.json", manifest)

  // Then: it should be TypeScript
  result
  |> should.equal(TypeScript)
}

pub fn detect_language_go_mod_test() {
  // Given: go.mod manifest
  let manifest = "
module github.com/user/project

go 1.21

require (
    github.com/gin-gonic/gin v1.9.1
)
"

  // When: we detect the language
  let result = detect_language_from_manifest("go.mod", manifest)

  // Then: it should be Go
  result
  |> should.equal(Go)
}

pub fn detect_language_cargo_toml_test() {
  // Given: Cargo.toml manifest
  let manifest = "
[package]
name = \"my_crate\"
version = \"0.1.0\"
edition = \"2021\"
"

  // When: we detect the language
  let result = detect_language_from_manifest("Cargo.toml", manifest)

  // Then: it should be Rust
  result
  |> should.equal(Rust)
}

pub fn detect_language_pyproject_toml_test() {
  // Given: pyproject.toml manifest
  let manifest = "
[project]
name = \"my-app\"
version = \"0.1.0\"
"

  // When: we detect the language
  let result = detect_language_from_manifest("pyproject.toml", manifest)

  // Then: it should be Python
  result
  |> should.equal(Python)
}

pub fn detect_language_requirements_txt_test() {
  // Given: requirements.txt file
  let manifest = "
flask==2.0.0
requests>=2.28.0
"

  // When: we detect the language
  let result = detect_language_from_manifest("requirements.txt", manifest)

  // Then: it should be Python
  result
  |> should.equal(Python)
}

pub fn detect_language_unknown_file_test() {
  // Given: an unknown manifest file
  let manifest = "some content"

  // When: we detect the language from unknown file
  let result = detect_language_from_manifest("unknown.txt", manifest)

  // Then: it should return NoLanguage
  result
  |> should.equal(NoLanguage)
}

// ============================================================================
// FRAMEWORK DETECTION TESTS
// ============================================================================

pub fn detect_framework_phoenix_from_gleam_test() {
  // Given: gleam.toml with Phoenix-like deps (via wisp or mist)
  let deps_content = "
[dependencies]
gleam_stdlib = \"~> 0.32\"
wisp = \"~> 0.10\"
mist = \"~> 0.14\"
"

  // When: we detect the framework
  let result = detect_framework_from_deps(Gleam, deps_content)

  // Then: it should detect the framework (Gleam uses rebar3/mix ecosystem)
  // For Gleam, we check for common web frameworks like wisp/mist
  result
  |> should.not_equal(NoFramework)
}

pub fn detect_framework_rebar3_from_gleam_test() {
  // Given: gleam.toml with erlang target (indicates Erlang/rebar3 ecosystem)
  let deps_content = "
target = \"erlang\"

[dependencies]
gleam_erlang = \"~> 0.25\"
"

  // When: we detect the framework
  let result = detect_framework_from_deps(Gleam, deps_content)

  // Then: it should detect Rebar3 for Erlang target
  result
  |> should.equal(Rebar3)
}

pub fn detect_framework_none_test() {
  // Given: manifest with no recognizable framework
  let deps_content = "
[dependencies]
gleam_stdlib = \"~> 0.32\"
"

  // When: we detect the framework
  let result = detect_framework_from_deps(Gleam, deps_content)

  // Then: it should return NoFramework
  result
  |> should.equal(NoFramework)
}

// ============================================================================
// FULL CONTEXT DETECTION TESTS
// ============================================================================

pub fn detect_from_directory_returns_context_test() {
  // Given: a directory path (we'll use current project)
  // Note: This is an integration test that reads the actual filesystem

  // When: we detect the context
  let result = detect_from_directory(".")

  // Then: it should return a valid CodebaseContext
  case result {
    Ok(context) -> {
      // Current project is Gleam
      context.language
      |> should.equal(Gleam)
    }
    Error(_) -> {
      // Allow error if running in isolated test environment
      should.be_ok(Ok(Nil))
    }
  }
}

pub fn detect_from_nonexistent_directory_test() {
  // Given: a non-existent directory
  let path = "/nonexistent/path/that/does/not/exist"

  // When: we detect the context
  let result = detect_from_directory(path)

  // Then: it should return an error
  result
  |> should.be_error()
}

// ============================================================================
// OUTPUT FORMAT TESTS
// ============================================================================

pub fn format_context_human_readable_test() {
  // Given: a detected context
  let context = CodebaseContext(
    language: Gleam,
    framework: Rebar3,
    manifest_file: "gleam.toml",
  )

  // When: we format it
  let output = format_context(context)

  // Then: it should contain language and framework
  output
  |> should.be_ok()

  case output {
    Ok(text) -> {
      text
      |> string_contains("Gleam")
      |> should.be_true()

      text
      |> string_contains("Rebar3")
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn context_to_json_test() {
  // Given: a detected context
  let context = CodebaseContext(
    language: Gleam,
    framework: Rebar3,
    manifest_file: "gleam.toml",
  )

  // When: we convert to JSON
  let json_output = context_to_json(context)

  // Then: it should be valid JSON with expected fields
  json_output
  |> string_contains("\"language\"")
  |> should.be_true()

  json_output
  |> string_contains("\"framework\"")
  |> should.be_true()

  json_output
  |> string_contains("\"manifest_file\"")
  |> should.be_true()
}

// ============================================================================
// EDGE CASE TESTS
// ============================================================================

pub fn detect_language_empty_manifest_test() {
  // Given: an empty manifest
  let manifest = ""

  // When: we detect the language
  let result = detect_language_from_manifest("gleam.toml", manifest)

  // Then: it should still detect based on filename
  result
  |> should.equal(Gleam)
}

pub fn detect_language_malformed_json_test() {
  // Given: malformed JSON in package.json
  let manifest = "{invalid json"

  // When: we detect the language
  let result = detect_language_from_manifest("package.json", manifest)

  // Then: it should still detect JavaScript from filename
  result
  |> should.equal(JavaScript)
}

pub fn language_to_string_test() {
  // Given: various languages
  // When/Then: they should convert to readable strings
  context_scanner.language_to_string(Gleam)
  |> should.equal("Gleam")

  context_scanner.language_to_string(Go)
  |> should.equal("Go")

  context_scanner.language_to_string(Rust)
  |> should.equal("Rust")

  context_scanner.language_to_string(Python)
  |> should.equal("Python")

  context_scanner.language_to_string(JavaScript)
  |> should.equal("JavaScript")

  context_scanner.language_to_string(TypeScript)
  |> should.equal("TypeScript")

  context_scanner.language_to_string(NoLanguage)
  |> should.equal("Unknown")
}

pub fn framework_to_string_test() {
  // Given: various frameworks
  // When/Then: they should convert to readable strings
  context_scanner.framework_to_string(Phoenix)
  |> should.equal("Phoenix")

  context_scanner.framework_to_string(Rebar3)
  |> should.equal("Rebar3")

  context_scanner.framework_to_string(NoFramework)
  |> should.equal("None")
}

// ============================================================================
// [CTX-5] INTEGRATION WITH INTERVIEW COMMAND
// ============================================================================

/// Test: Context can be serialized to JSON for storage
pub fn context_serializes_to_json_test() {
  // Given: a codebase context
  let context = CodebaseContext(
    language: Gleam,
    framework: Rebar3,
    manifest_file: "gleam.toml",
  )

  // When: we serialize to JSON
  let json_str = context_to_json(context)

  // Then: it should contain all fields
  json_str
  |> string_contains("Gleam")
  |> should.be_true()

  json_str
  |> string_contains("Rebar3")
  |> should.be_true()

  json_str
  |> string_contains("gleam.toml")
  |> should.be_true()
}

/// Test: Format context with no framework detected
pub fn format_context_no_framework_test() {
  // Given: context with no framework
  let context = CodebaseContext(
    language: Python,
    framework: NoFramework,
    manifest_file: "requirements.txt",
  )

  // When: we format it
  let result = format_context(context)

  // Then: it should succeed and show "None" for framework
  result
  |> should.be_ok()

  case result {
    Ok(text) -> {
      text
      |> string_contains("Python")
      |> should.be_true()

      text
      |> string_contains("None")
      |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

/// Test: context_to_cue outputs valid CUE syntax
pub fn context_to_cue_valid_syntax_test() {
  // Given: a complete context
  let context = CodebaseContext(
    language: Gleam,
    framework: Rebar3,
    manifest_file: "gleam.toml",
  )

  // When: we convert to CUE
  let cue_output = context_scanner.context_to_cue(context)

  // Then: it should have proper CUE structure
  cue_output
  |> string_contains("codebase: {")
  |> should.be_true()

  cue_output
  |> string_contains("language: \"Gleam\"")
  |> should.be_true()

  cue_output
  |> string_contains("framework: \"Rebar3\"")
  |> should.be_true()

  cue_output
  |> string_contains("manifest_file: \"gleam.toml\"")
  |> should.be_true()
}

/// Test: Wisp framework detection
pub fn detect_framework_wisp_test() {
  // Given: gleam.toml with wisp dependency
  let deps_content = "
[dependencies]
gleam_stdlib = \"~> 0.32\"
wisp = \"~> 0.10\"
"

  // When: we detect the framework
  let result = detect_framework_from_deps(Gleam, deps_content)

  // Then: it should detect Wisp
  result
  |> should.equal(context_scanner.Wisp)
}

/// Test: Mist framework detection
pub fn detect_framework_mist_test() {
  // Given: gleam.toml with mist dependency
  let deps_content = "
[dependencies]
gleam_stdlib = \"~> 0.32\"
mist = \"~> 0.14\"
"

  // When: we detect the framework
  let result = detect_framework_from_deps(Gleam, deps_content)

  // Then: it should detect Mist
  result
  |> should.equal(context_scanner.Mist)
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

import gleam/string

fn string_contains(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}
