/// Context Scanner Module
/// Detects language and framework from project manifest files
///
/// Bead: [CTX-7] Create context_scanner.gleam module
/// Scope: SIMPLIFIED - Only detect language and framework from manifest files
/// Do NOT attempt to infer patterns or entry points from code structure
///
/// Supported manifest files:
/// - gleam.toml (Gleam)
/// - package.json (JavaScript/TypeScript)
/// - go.mod (Go)
/// - Cargo.toml (Rust)
/// - pyproject.toml, requirements.txt (Python)

import gleam/json
import gleam/list
import gleam/string
import simplifile

/// Detected programming language
pub type DetectedLanguage {
  Gleam
  JavaScript
  TypeScript
  Go
  Rust
  Python
  NoLanguage
}

/// Detected framework/build system
pub type DetectedFramework {
  /// Gleam/Erlang build tool
  Rebar3
  /// Elixir web framework (if using BEAM ecosystem)
  Phoenix
  /// Gleam web framework
  Wisp
  /// HTTP server for Gleam
  Mist
  /// No specific framework detected
  NoFramework
}

/// Codebase context with detected language and framework
pub type CodebaseContext {
  CodebaseContext(
    language: DetectedLanguage,
    framework: DetectedFramework,
    manifest_file: String,
  )
}

/// Manifest file search order (priority from most to least specific)
const manifest_files = [
  "gleam.toml", "Cargo.toml", "go.mod", "package.json", "pyproject.toml",
  "requirements.txt",
]

// ============================================================================
// LANGUAGE DETECTION
// ============================================================================

/// Detect language from manifest filename and content
pub fn detect_language_from_manifest(
  filename: String,
  content: String,
) -> DetectedLanguage {
  case string.lowercase(filename) {
    "gleam.toml" -> Gleam
    "cargo.toml" -> Rust
    "go.mod" -> Go
    "pyproject.toml" -> Python
    "requirements.txt" -> Python
    "package.json" -> detect_js_or_ts(content)
    _ -> NoLanguage
  }
}

/// Detect JavaScript vs TypeScript from package.json content
fn detect_js_or_ts(content: String) -> DetectedLanguage {
  // Check for TypeScript indicators in the content
  let lower_content = string.lowercase(content)

  case
    string.contains(lower_content, "\"typescript\"")
    || string.contains(lower_content, "\"ts-node\"")
    || string.contains(lower_content, "\"@types/")
  {
    True -> TypeScript
    False -> JavaScript
  }
}

// ============================================================================
// FRAMEWORK DETECTION
// ============================================================================

/// Detect framework from dependencies content
pub fn detect_framework_from_deps(
  language: DetectedLanguage,
  deps_content: String,
) -> DetectedFramework {
  case language {
    Gleam -> detect_gleam_framework(deps_content)
    _ -> NoFramework
  }
}

/// Detect Gleam-specific frameworks/build targets
fn detect_gleam_framework(content: String) -> DetectedFramework {
  let lower_content = string.lowercase(content)

  // Check for web frameworks first (higher priority)
  case string.contains(lower_content, "wisp") {
    True -> Wisp
    False ->
      case string.contains(lower_content, "mist") {
        True -> Mist
        False ->
          // Check for Erlang target (indicates rebar3 ecosystem)
          case
            string.contains(lower_content, "target = \"erlang\"")
            || string.contains(lower_content, "gleam_erlang")
          {
            True -> Rebar3
            False -> NoFramework
          }
      }
  }
}

// ============================================================================
// DIRECTORY SCANNING
// ============================================================================

/// Detect codebase context from a directory
/// Scans for known manifest files and extracts language/framework
pub fn detect_from_directory(path: String) -> Result(CodebaseContext, String) {
  // Verify directory exists
  case simplifile.verify_is_directory(path) {
    Error(_) -> Error("Directory does not exist: " <> path)
    Ok(_) -> scan_for_manifests(path)
  }
}

/// Scan directory for manifest files
fn scan_for_manifests(dir: String) -> Result(CodebaseContext, String) {
  // Try each manifest file in priority order
  let found =
    manifest_files
    |> list.filter_map(fn(filename) {
      let filepath = case string.ends_with(dir, "/") {
        True -> dir <> filename
        False -> dir <> "/" <> filename
      }

      case simplifile.read(filepath) {
        Ok(content) -> Ok(#(filename, content))
        Error(_) -> Error(Nil)
      }
    })
    |> list.first()

  case found {
    Ok(#(filename, content)) -> {
      let language = detect_language_from_manifest(filename, content)
      let framework = detect_framework_from_deps(language, content)
      Ok(CodebaseContext(
        language: language,
        framework: framework,
        manifest_file: filename,
      ))
    }
    Error(_) -> Error("No manifest file found in: " <> dir)
  }
}

// ============================================================================
// OUTPUT FORMATTING
// ============================================================================

/// Convert language to string representation
pub fn language_to_string(lang: DetectedLanguage) -> String {
  case lang {
    Gleam -> "Gleam"
    JavaScript -> "JavaScript"
    TypeScript -> "TypeScript"
    Go -> "Go"
    Rust -> "Rust"
    Python -> "Python"
    NoLanguage -> "Unknown"
  }
}

/// Convert framework to string representation
pub fn framework_to_string(framework: DetectedFramework) -> String {
  case framework {
    Rebar3 -> "Rebar3"
    Phoenix -> "Phoenix"
    Wisp -> "Wisp"
    Mist -> "Mist"
    NoFramework -> "None"
  }
}

/// Format context as human-readable text
pub fn format_context(context: CodebaseContext) -> Result(String, String) {
  let lang_str = language_to_string(context.language)
  let framework_str = framework_to_string(context.framework)

  let output =
    "Codebase Context\n"
    <> "═══════════════════════════════════════════════════════════════════\n"
    <> "Language:      " <> lang_str <> "\n"
    <> "Framework:     " <> framework_str <> "\n"
    <> "Manifest:      " <> context.manifest_file <> "\n"

  Ok(output)
}

/// Convert context to JSON string
pub fn context_to_json(context: CodebaseContext) -> String {
  let json_obj =
    json.object([
      #("language", json.string(language_to_string(context.language))),
      #("framework", json.string(framework_to_string(context.framework))),
      #("manifest_file", json.string(context.manifest_file)),
    ])

  json.to_string(json_obj)
}

// ============================================================================
// CUE OUTPUT (for schema compatibility)
// ============================================================================

/// Convert context to CUE format matching #CodebaseContext schema
pub fn context_to_cue(context: CodebaseContext) -> String {
  let lang_str = language_to_string(context.language)
  let framework_str = framework_to_string(context.framework)

  "// Auto-generated codebase context\n"
  <> "// Generated by: intent context-scan\n\n"
  <> "codebase: {\n"
  <> "\tlanguage: \"" <> lang_str <> "\"\n"
  <> "\tframework: \"" <> framework_str <> "\"\n"
  <> "\tmanifest_file: \"" <> context.manifest_file <> "\"\n"
  <> "}\n"
}
