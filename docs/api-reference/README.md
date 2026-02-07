# API Reference

This document provides comprehensive documentation of all public functions, types, and modules in the Intent CLI.

## Table of Contents

- [Core Modules](#core-modules)
  - [checker](#checker)
  - [kirk](#kirk)
  - [interview](#interview)
  - [loader](#loader)
  - [parser](#parser)
  - [runner](#runner)
  - [validator](#validator)
- [Utility Modules](#utility-modules)
  - [cli_ui](#cli_ui)
  - [errors](#errors)
  - [output](#output)
  - [question_loader](#question_loader)
  - [answer_loader](#answer_loader)
  - [bead_feedback](#bead_feedback)
  - [bead_templates](#bead_templates)
  - [anti_patterns](#anti_patterns)
  - [interpolate](#interpolate)
  - [array_indexing](#array_indexing)
  - [case_insensitive](#case_insensitive)
  - [improver](#improver)
  - [quality_analyzer](#quality_analyzer)
  - [spec_builder](#spec_builder)
  - [spec_linter](#spec_linter)
  - [security](#security)
  - [http_client](#http_client)
  - [resolver](#resolver)
  - [rule](#rule)
  - [rules_engine](#rules_engine)
  - [stdin](#stdin)
  - [formats](#formats)
  - [interview_contract](#interview_contract)
  - [interview_questions](#interview_questions)
  - [interview_storage](#interview_storage)
  - [plan_mode](#plan_mode)
  - [types](#types)

## Core Modules

### checker

Main response validation engine that checks API responses against CUE specifications.

#### Types

```gleam
pub type Rule {
  Equals {
    field: String,
    expected: Dynamic,
  }
  Contains {
    field: String,
    expected: String,
  }
  Regex {
    field: String,
    pattern: String,
  }
  JsonPath {
    path: String,
    expected: Dynamic,
  }
  Header {
    name: String,
    expected: String,
  }
  ArrayIndex {
    path: String,
    index: Int,
    expected: Dynamic,
  }
  StatusCode {
    expected: Int,
  }
}
```

#### Functions

```gleam
/// Check a response against a rule
pub fn check(
  response: types.Response,
  rule: Rule,
) -> Result(types.CheckResult, types.CheckError)

/// Validate response against multiple rules
pub fn validate(
  response: types.Response,
  rules: List(Rule),
) -> List(types.CheckResult)

/// Check if a value matches an expected value
pub fn matches(
  actual: Dynamic,
  expected: Dynamic,
) -> Result(Bool, types.CheckError)
```

#### Usage Examples

```gleam
import intent.checker

let response = types.Response(
  status_code: 200,
  headers: [#("Content-Type", "application/json")],
  body: "{ \"user\": { \"id\": 1, \"name\": \"Alice\" } }",
)

let rule = checker.Equals(
  field: "user.id",
  expected: dynamic.int(1),
)

case checker.check(response, rule) {
  Ok(types.CheckResult(passed: True, errors: [])) -> {
    "Rule passed!"
  }
  Ok(types.CheckResult(passed: False, errors: [e])) -> {
    "Rule failed: " <> e.message
  }
  Error(e) -> {
    "Validation error: " <> e.message
  }
}
```

### kirk

KIRK (Design by Contract) analysis modules for quality assessment and inversion detection.

#### Types

```gleam
pub type QualityDimension {
  Completeness
  Consistency
  Testability
  Correctness
  Usability
}

pub type InversionType {
  Security
  Usability
  Integration
}

pub type QualityScore {
  Completeness(Int)
  Consistency(Int)
  Testability(Int)
  Correctness(Int)
  Usability(Int)
}
```

#### Functions

```gleam
/// Analyze spec quality across all dimensions
pub fn analyze(
  spec: types.Spec,
) -> Result(types.Analysis, types.AnalysisError)

/// Detect missing failure modes (inversions)
pub fn detect_inversions(
  spec: types.Spec,
) -> List(types.Inversion)

/// Calculate coverage metrics
pub fn calculate_coverage(
  spec: types.Spec,
) -> types.CoverageMetrics

/// Generate quality report
pub fn generate_report(
  analysis: types.Analysis,
) -> String
```

#### Usage Examples

```gleam
import intent.kirk

let spec = types.Spec(
  name: "User API",
  behaviors: [
    types.Behavior(
      name: "get-user",
      method: "GET",
      path: "/users/1",
      expected_status: 200,
    ),
  ],
)

case kirk.analyze(spec) {
  Ok(analysis) -> {
    // Access scores
    analysis.scores.completeness
    analysis.scores.consistency
    analysis.scores.testability
  }
  Error(e) -> {
    "Analysis failed: " <> e.message
  }
}
```

### interview

Main interview orchestration engine for interactive question-answer loops.

#### Types

```gleam
pub type InterviewState {
  InterviewState(
    session_id: String,
    profile: types.Profile,
    questions: types.QuestionSet,
    index: Int,
    answers: types.Answers,
  )
}

pub type InterviewEvent {
  AskQuestion(types.Question)
  ShowProgress(types.Progress)
  Complete(types.Bead)
  Error(types.InterviewError)
}
```

#### Functions

```gleam
/// Start a new interview
pub fn start(
  profile: types.Profile,
  session_id: Option(String),
) -> Result(InterviewState, types.InterviewError)

/// Submit answer to current question
pub fn answer(
  state: InterviewState,
  answer: String,
) -> Result(InterviewState, types.InterviewError)

/// Get current question
pub fn current_question(
  state: InterviewState,
) -> types.Question

/// Check if interview is complete
pub fn is_complete(
  state: InterviewState,
) -> Bool

/// Generate beads from completed interview
pub fn generate_beads(
  state: InterviewState,
) -> Result(types.Bead, types.InterviewError)
```

#### Usage Examples

```gleam
import intent.interview

// Start interview
case interview.start(types.Profile.api, None) {
  Ok(state) -> {
    // Get first question
    let question = interview.current_question(state)

    // Submit answer
    let assert Ok(new_state) = interview.answer(state, "Return user data")

    // Continue until complete
    loop {
      case interview.is_complete(new_state) {
        True -> {
          // Generate beads
          case interview.generate_beads(new_state) {
            Ok(beads) -> "Generated " <> string.from_int(list.length(beads)) <> " beads"
            Error(e) -> "Failed to generate beads"
          }
        }
        False -> {
          // Get next question
          let question = interview.current_question(new_state)
          // Submit answer, continue loop
        }
      }
    }
  }
  Error(e) -> "Failed to start interview"
}
```

### loader

CUE specification file loading and validation.

#### Types

```gleam
pub type LoaderError {
  FileNotFound(path: String)
  InvalidCue(path: String, error: String)
  ParseError(message: String)
}
```

#### Functions

```gleam
/// Load and validate a CUE spec file
pub fn load_spec(
  path: String,
) -> Result(types.Spec, LoaderError)

/// Validate CUE syntax without loading
pub fn validate_cue(
  path: String,
) -> Result(Nil, LoaderError)

/// Get spec metadata
pub fn get_spec_info(
  path: String,
) -> Result(types.SpecInfo, LoaderError)
```

#### Usage Examples

```gleam
import intent.loader

case loader.load_spec("examples/user-api.cue") {
  Ok(spec) -> {
    "Loaded spec: " <> spec.name
    "Behaviors: " <> string.from_int(list.length(spec.behaviors))
  }
  Error(e) -> {
    "Error: " <> error_to_string(e)
  }
}
```

### parser

JSON parsing and type conversion utilities.

#### Types

```gleam
pub type ParseError {
  InvalidJson(message: String)
  TypeMismatch(expected: String, actual: String)
  FieldNotFound(field: String)
}
```

#### Functions

```gleam
/// Parse JSON string to dynamic value
pub fn parse_json(
  json: String,
) -> Result(Dynamic, ParseError)

/// Convert dynamic value to JSON string
pub fn to_json(
  value: Dynamic,
) -> Result(String, ParseError)

/// Navigate JSON path (e.g., "user.id")
pub fn get_path(
  value: Dynamic,
  path: String,
) -> Result(Dynamic, ParseError)

/// Convert JSON to Gleam type
pub fn to_type(
  value: Dynamic,
  type_info: types.TypeInfo,
) -> Result(Dynamic, ParseError)
```

#### Usage Examples

```gleam
import intent.parser

case parser.parse_json("{ \"user\": { \"id\": 1, \"name\": \"Alice\" } }") {
  Ok(dynamic) -> {
    case parser.get_path(dynamic, "user.id") {
      Ok(dynamic.int(1)) -> "Found user ID: 1"
      Ok(other) -> "Unexpected type"
      Error(e) -> "Error: " <> e.message
    }
  }
  Error(e) -> "Parse error: " <> e.message
}
```

### runner

Test execution engine that runs CUE specs against target APIs.

#### Types

```gleam
pub type RunnerConfig {
  RunnerConfig(
    timeout_ms: Int,
    retry_count: Int,
    follow_redirects: Bool,
  )
}

pub type ExecutionResult {
  ExecutionResult(
    success: Bool,
    passed: Int,
    failed: Int,
    total: Int,
    results: List(types.ExecutionResult),
  )
}
```

#### Functions

```gleam
/// Run a spec against a target URL
pub fn run(
  spec: types.Spec,
  target: String,
  config: RunnerConfig,
) -> Result(ExecutionResult, types.RunnerError)

/// Execute single behavior
pub fn execute_behavior(
  behavior: types.Behavior,
  target: String,
  config: RunnerConfig,
) -> Result(types.ExecutionResult, types.RunnerError)

/// Run with retries on failure
pub fn run_with_retries(
  spec: types.Spec,
  target: String,
  config: RunnerConfig,
) -> Result(ExecutionResult, types.RunnerError)
```

#### Usage Examples

```gleam
import intent.runner

let spec = types.Spec(
  name: "Test API",
  behaviors: [
    types.Behavior(
      name: "test-get",
      method: "GET",
      path: "/api/health",
      expected_status: 200,
    ),
  ],
)

let config = types.RunnerConfig(
  timeout_ms: 5000,
  retry_count: 3,
  follow_redirects: True,
)

case runner.run(spec, "http://localhost:8080", config) {
  Ok(result) -> {
    "Results: " <> string.from_int(result.passed) <> "/" <> string.from_int(result.total)
  }
  Error(e) -> "Runner error: " <> e.message
}
```

### validator

CUE spec validation utilities.

#### Types

```gleam
pub type ValidationError {
  EmptySpec
  MissingRequiredField(field: String)
  InvalidType(field: String, expected: String, actual: String)
  DuplicateBehavior(name: String)
}
```

#### Functions

```gleam
/// Validate spec structure
pub fn validate(
  spec: types.Spec,
) -> Result(Nil, ValidationError)

/// Check for required fields
pub fn has_required_fields(
  spec: types.Spec,
) -> Result(Nil, ValidationError)

/// Check behavior uniqueness
pub fn behaviors_unique(
  spec: types.Spec,
) -> Result(Nil, ValidationError)
```

#### Usage Examples

```gleam
import intent.validator

case validator.validate(spec) {
  Ok(Nil) -> "Spec is valid"
  Error(e) -> "Validation error: " <> error_to_string(e)
}
```

## Utility Modules

### cli_ui

Command-line user interface utilities.

#### Functions

```gleam
/// Print formatted message
pub fn print_message(message: String, level: MessageLevel)

/// Show progress bar
pub fn show_progress(
  current: Int,
  total: Int,
  message: String,
)

/// Display error message
pub fn print_error(message: String)

/// Display success message
pub fn print_success(message: String)

/// Display warning message
pub fn print_warning(message: String)

/// Ask user for confirmation
pub fn confirm(message: String) -> Result(Bool, Nil)

/// Display table
pub fn display_table(headers: List(String), rows: List(List(String)))
```

### errors

Error type definitions and utilities.

#### Types

```gleam
pub type Error {
  NotFound(message: String)
  ValidationError(message: String)
  RuntimeError(message: String)
  NetworkError(message: String)
}
```

#### Functions

```gleam
/// Convert error to string
pub fn to_string(error: Error) -> String

/// Convert error to JSON
pub fn to_json(error: Error) -> String

/// Check if error is retryable
pub fn is_retryable(error: Error) -> Bool
```

### output

Result formatting and display utilities.

#### Functions

```gleam
/// Format execution results
pub fn format_results(
  results: List(types.ExecutionResult),
) -> String

/// Format check results
pub fn format_checks(
  checks: List(types.CheckResult),
) -> String

/// Format spec info
pub fn format_spec_info(
  spec: types.Spec,
) -> String

/// Output to stdout
pub fn output_stdout(content: String)

/// Output to stderr
pub fn output_stderr(content: String)
```

### question_loader

Question database loader for interview system.

#### Types

```gleam
pub type QuestionDatabase {
  QuestionDatabase(
    profiles: types.QuestionSet,
    categories: List(String),
  )
}
```

#### Functions

```gleam
/// Load question database from CUE
pub fn load_db(path: String) -> Result(QuestionDatabase, types.Error)

/// Get questions for profile
pub fn get_questions(
  db: QuestionDatabase,
  profile: types.Profile,
) -> types.QuestionSet

/// Search questions by category
pub fn search_questions(
  db: QuestionDatabase,
  category: String,
) -> List(types.Question)
```

### answer_loader

Answer storage and retrieval for interview sessions.

#### Types

```gleam
pub type AnswerStore {
  AnswerStore(
    session_id: String,
    answers: types.Answers,
    timestamps: List(types.Timestamp),
  )
}
```

#### Functions

```gleam
/// Save answer
pub fn save_answer(
  store: AnswerStore,
  question_id: String,
  answer: String,
) -> Result(AnswerStore, types.Error)

/// Get answer
pub fn get_answer(
  store: AnswerStore,
  question_id: String,
) -> Result(String, types.Error)

/// Get all answers
pub fn get_answers(
  store: AnswerStore,
) -> types.Answers

/// Delete answer
pub fn delete_answer(
  store: AnswerStore,
  question_id: String,
) -> Result(AnswerStore, types.Error)
```

### bead_feedback

Bead feedback and scoring system.

#### Types

```gleam
pub type FeedbackType {
  Completeness
  Clarity
  Testability
  Feasibility
}

pub type Feedback {
  Feedback(
    bead_id: String,
    type: FeedbackType,
    message: String,
    score: Int,
  )
}
```

#### Functions

```gleam
/// Evaluate bead quality
pub fn evaluate(
  bead: types.Bead,
) -> List(Feedback)

/// Calculate bead score
pub fn score(
  bead: types.Bead,
) -> Int

/// Generate feedback report
pub fn generate_report(
  bead: types.Bead,
) -> String

/// Get feedback for type
pub fn get_feedback(
  bead: types.Bead,
  type: FeedbackType,
) -> List(Feedback)
```

### bead_templates

Bead generation templates.

#### Types

```gleam
pub type Template {
  Template(
    name: String,
    template: String,
  )
}
```

#### Functions

```gleam
/// Get template by name
pub fn get_template(
  name: String,
) -> Result(Template, types.Error)

/// Generate bead from template
pub fn generate(
  template: Template,
  data: types.Bead,
) -> Result(String, types.Error)

/// List all templates
pub fn list_templates() -> List(String)

/// Save custom template
pub fn save_template(
  name: String,
  template: String,
) -> Result(Nil, types.Error)
```

### anti_patterns

Anti-pattern detection for CUE specifications.

#### Types

```gleam
pub type AntiPattern {
  AntiPattern(
    name: String,
    description: String,
    severity: String,
    behavior: types.Behavior,
  )
}

pub type PatternType {
  Security
  Usability
  Performance
  Maintainability
}
```

#### Functions

```gleam
/// Detect anti-patterns in spec
pub fn detect(
  spec: types.Spec,
) -> List(AntiPattern)

/// Find security anti-patterns
pub fn find_security(
  spec: types.Spec,
) -> List(AntiPattern)

/// Find performance anti-patterns
pub fn find_performance(
  spec: types.Spec,
) -> List(AntiPattern)

/// Generate anti-pattern report
pub fn generate_report(
  patterns: List(AntiPattern),
) -> String
```

### interpolate

Variable interpolation for dynamic content.

#### Types

```gleam
pub type InterpolationError {
  MissingVariable(name: String)
  InvalidSyntax(message: String)
}
```

#### Functions

```gleam
/// Interpolate variables in string
pub fn interpolate(
  template: String,
  variables: types.Variables,
) -> Result(String, InterpolationError)

/// Extract variables from string
pub fn extract_variables(
  template: String,
) -> List(String)

/// Replace variables in JSON
pub fn replace_vars_in_json(
  json: String,
  variables: types.Variables,
) -> Result(String, InterpolationError)

/// Validate variable names
pub fn validate_variable(
  name: String,
) -> Bool
```

#### Usage Examples

```gleam
import intent.interpolate

let template = "Hello {{name}}, you have {{count}} items"
let variables = types.Variables(
  name: "Alice",
  count: "5",
)

case interpolate.interpolate(template, variables) {
  Ok(result) -> result  // "Hello Alice, you have 5 items"
  Error(e) -> "Error: " <> e.message
}
```

### array_indexing

Array indexing utilities for JSON path navigation.

#### Types

```gleam
pub type Indexer {
  Indexer(
    path: String,
    index: Int,
  )
}

pub type IndexError {
  InvalidPath(path: String)
  IndexOutOfBounds(index: Int, max: Int)
  TypeMismatch(expected: String)
}
```

#### Functions

```gleam
/// Navigate to array element
pub fn navigate(
  value: Dynamic,
  path: String,
  index: Int,
) -> Result(Dynamic, IndexError)

/// Get first element
pub fn first(
  list: Dynamic,
) -> Result(Dynamic, IndexError)

/// Get last element
pub fn last(
  list: Dynamic,
) -> Result(Dynamic, IndexError)

/// Get element at index
pub fn get(
  list: Dynamic,
  index: Int,
) -> Result(Dynamic, IndexError)

/// Get random element
pub fn random(
  list: Dynamic,
) -> Result(Dynamic, IndexError)
```

### case_insensitive

Case-insensitive string comparison utilities.

#### Types

```gleam
pub type CaseInsensitiveString {
  CaseInsensitiveString(String)
}
```

#### Functions

```gleam
/// Create case-insensitive string
pub fn from_string(str: String) -> CaseInsensitiveString

/// Compare strings case-insensitively
pub fn compare(
  a: CaseInsensitiveString,
  b: CaseInsensitiveString,
) -> Int

/// Check if equal (case-insensitive)
pub fn equal(
  a: CaseInsensitiveString,
  b: CaseInsensitiveString,
) -> Bool

/// Convert to lower case
pub fn to_lower(
  str: CaseInsensitiveString,
) -> String

/// Convert to upper case
pub fn to_upper(
  str: CaseInsensitiveString,
) -> String

/// Check if starts with (case-insensitive)
pub fn starts_with(
  str: CaseInsensitiveString,
  prefix: String,
) -> Bool

/// Check if ends with (case-insensitive)
pub fn ends_with(
  str: CaseInsensitiveString,
  suffix: String,
) -> Bool
```

#### Usage Examples

```gleam
import intent.case_insensitive

let a = from_string("Hello")
let b = from_string("hELLO")

case compare(a, b) {
  0 -> "Equal (case-insensitive)"
  _ -> "Not equal"
}

case starts_with(from_string("Hello World"), "hel") {
  True -> "Starts with 'hel' (case-insensitive)"
  False -> "Does not start with 'hel'"
}
```

### improver

Code improvement suggestions.

#### Types

```gleam
pub type Improvement {
  Improvement(
    category: String,
    description: String,
    suggestion: String,
    severity: String,
  )
}
```

#### Functions

```gleam
/// Analyze code for improvements
pub fn analyze(
  code: String,
) -> List(Improvement)

/// Suggest improvements for behavior
pub fn suggest(
  behavior: types.Behavior,
) -> List(Improvement)

/// Generate improvement report
pub fn generate_report(
  improvements: List(Improvement),
) -> String

/// Filter by severity
pub fn filter_by_severity(
  improvements: List(Improvement),
  severity: String,
) -> List(Improvement)
```

### quality_analyzer

Spec quality analysis utilities.

#### Types

```gleam
pub type QualityMetric {
  Completeness(Int)
  Consistency(Int)
  Testability(Int)
  Correctness(Int)
  Usability(Int)
}

pub type QualityReport {
  QualityReport(
    score: Float,
    metrics: QualityMetric,
    issues: List(String),
    suggestions: List(String),
  )
}
```

#### Functions

```gleam
/// Analyze spec quality
pub fn analyze(
  spec: types.Spec,
) -> Result(QualityReport, types.Error)

/// Get completeness score
pub fn completeness_score(
  spec: types.Spec,
) -> Int

/// Get consistency score
pub fn consistency_score(
  spec: types.Spec,
) -> Int

/// Get testability score
pub fn testability_score(
  spec: types.Spec,
) -> Int

/// Generate quality report
pub fn report(
  spec: types.Spec,
) -> String
```

### spec_builder

CUE spec building utilities.

#### Types

```gleam
pub type Builder {
  Builder(
    name: String,
    description: Option(String),
    behaviors: List(types.Behavior),
    preconditions: List(String),
    postconditions: List(String),
  )
}
```

#### Functions

```gleam
/// Create new builder
pub fn new(name: String) -> Builder

/// Add description
pub fn with_description(
  builder: Builder,
  description: String,
) -> Builder

/// Add behavior
pub fn add_behavior(
  builder: Builder,
  behavior: types.Behavior,
) -> Builder

/// Add precondition
pub fn add_precondition(
  builder: Builder,
  precondition: String,
) -> Builder

/// Add postcondition
pub fn add_postcondition(
  builder: Builder,
  postcondition: String,
) -> Builder

/// Build spec
pub fn build(builder: Builder) -> Result(types.Spec, types.Error)

/// Build to CUE string
pub fn build_to_cue(builder: Builder) -> Result(String, types.Error)
```

#### Usage Examples

```gleam
import intent.spec_builder

let builder = spec_builder.new("User API")

let assert Ok(spec) = builder
  |> spec_builder.with_description("REST API for user management")
  |> spec_builder.add_behavior(types.Behavior(
    name: "get-user",
    method: "GET",
    path: "/users/1",
    expected_status: 200,
  ))
  |> spec_builder.build()
```

### spec_linter

Specification linting utilities.

#### Types

```gleam
pub type Lint {
  Lint(
    level: String,
    location: String,
    message: String,
    suggestion: Option(String),
  )
}
```

#### Functions

```gleam
/// Lint spec
pub fn lint(
  spec: types.Spec,
) -> List(Lint)

/// Check behavior naming conventions
pub fn check_naming(
  spec: types.Spec,
) -> List(Lint)

/// Check URL patterns
pub fn check_urls(
  spec: types.Spec,
) -> List(Lint)

/// Check HTTP method usage
pub fn check_methods(
  spec: types.Spec,
) -> List(Lint)

/// Generate lint report
pub fn generate_report(
  lints: List(Lint),
) -> String
```

### security

Security analysis utilities.

#### Types

```gleam
pub type SecurityIssue {
  SecurityIssue(
    type: String,
    severity: String,
    description: String,
    location: String,
  )
}
```

#### Functions

```gleam
/// Scan spec for security issues
pub fn scan(
  spec: types.Spec,
) -> List(SecurityIssue)

/// Check for SQL injection
pub fn check_sql_injection(
  spec: types.Spec,
) -> List(SecurityIssue)

/// Check for XSS vulnerabilities
pub fn check_xss(
  spec: types.Spec,
) -> List(SecurityIssue)

/// Check for authentication bypass
pub fn check_auth_bypass(
  spec: types.Spec,
) -> List(SecurityIssue)

/// Generate security report
pub fn generate_report(
  issues: List(SecurityIssue),
) -> String
```

### http_client

HTTP client utilities for making requests.

#### Types

```gleam
pub type HttpClient {
  HttpClient(
    timeout_ms: Int,
    user_agent: String,
    retry_count: Int,
  )
}

pub type Response {
  Response(
    status_code: Int,
    headers: List(#(String, String)),
    body: String,
    elapsed_ms: Int,
  )
}
```

#### Functions

```gleam
/// Create HTTP client
pub fn new_client(
  timeout_ms: Int,
) -> HttpClient

/// Make GET request
pub fn get(
  client: HttpClient,
  url: String,
) -> Result(Response, types.Error)

/// Make POST request
pub fn post(
  client: HttpClient,
  url: String,
  body: String,
) -> Result(Response, types.Error)

/// Make PUT request
pub fn put(
  client: HttpClient,
  url: String,
  body: String,
) -> Result(Response, types.Error)

/// Make DELETE request
pub fn delete(
  client: HttpClient,
  url: String,
) -> Result(Response, types.Error)

/// Make request with retries
pub fn request_with_retries(
  client: HttpClient,
  method: String,
  url: String,
  body: Option(String),
) -> Result(Response, types.Error)
```

#### Usage Examples

```gleam
import intent.http_client

let client = new_client(5000)

case client.get(client, "http://localhost:8080/api/health") {
  Ok(response) -> {
    "Status: " <> string.from_int(response.status_code)
    "Body: " <> response.body
  }
  Error(e) -> "Request failed: " <> e.message
}
```

### resolver

Behavior dependency resolution.

#### Types

```gleam
pub type Resolution {
  Resolution(
    behavior: types.Behavior,
    dependencies: List(types.Behavior),
  )
}

pub type ResolutionError {
  CircularDependency
  MissingDependency(name: String)
}
```

#### Functions

```gleam
/// Resolve behavior dependencies
pub fn resolve(
  spec: types.Spec,
) -> Result(List(Resolution), ResolutionError)

/// Check for circular dependencies
pub fn has_circular_deps(
  spec: types.Spec,
) -> Bool

/// Get missing dependencies
pub fn get_missing(
  spec: types.Spec,
) -> List(types.Behavior)

/// Sort by dependency order
pub fn sort_by_deps(
  behaviors: List(types.Behavior),
) -> List(types.Behavior)
```

### rule

Rule expression parsing and evaluation.

#### Types

```gleam
pub type Rule {
  Equals {
    field: String,
    expected: Dynamic,
  }
  Contains {
    field: String,
    expected: String,
  }
  Regex {
    field: String,
    pattern: String,
  }
}
```

#### Functions

```gleam
/// Parse rule from string
pub fn parse(rule_str: String) -> Result(Rule, types.Error)

/// Evaluate rule
pub fn evaluate(
  rule: Rule,
  value: Dynamic,
) -> Result(Bool, types.Error)

/// Compile rule to executable
pub fn compile(rule: Rule) -> Result(fn(Dynamic) -> Result(Bool, types.Error), types.Error)
```

### rules_engine

Global rules evaluation engine.

#### Types

```gleam
pub type RuleEngine {
  RuleEngine(
    rules: List(Rule),
    variables: types.Variables,
  )
}

pub type RuleResult {
  RuleResult(
    passed: Bool,
    rule: Rule,
    error: Option(types.Error),
  )
}
```

#### Functions

```gleam
/// Create rule engine
pub fn new_engine(
  rules: List(Rule),
) -> RuleEngine

/// Add rule to engine
pub fn add_rule(
  engine: RuleEngine,
  rule: Rule,
) -> RuleEngine

/// Evaluate all rules
pub fn evaluate(
  engine: RuleEngine,
  value: Dynamic,
) -> List(RuleResult)

/// Evaluate single rule
pub fn evaluate_rule(
  engine: RuleEngine,
  rule: Rule,
  value: Dynamic,
) -> Result(Bool, types.Error)

/// Clear all rules
pub fn clear_rules(
  engine: RuleEngine,
) -> RuleEngine
```

### stdin

Standard input handling utilities.

#### Types

```gleam
pub type StdinConfig {
  StdinConfig(
    timeout_ms: Int,
    prompt: Option(String),
  )
}
```

#### Functions

```gleam
/// Read from stdin
pub fn read(
  config: StdinConfig,
) -> Result(String, types.Error)

/// Read line from stdin
pub fn read_line(
  config: StdinConfig,
) -> Result(String, types.Error)

/// Read with timeout
pub fn read_timeout(
  config: StdinConfig,
) -> Result(String, types.Error)

/// Check if stdin is available
pub fn is_available() -> Bool

/// Read all input
pub fn read_all(
  config: StdinConfig,
) -> Result(String, types.Error)
```

### formats

Data format utilities.

#### Types

```gleam
pub type Format {
  Json
  CUE
  Yaml
  TOML
}

pub type FormatError {
  UnsupportedFormat(format: String)
  ParseError(message: String)
}
```

#### Functions

```gleam
/// Detect format from content
pub fn detect(content: String) -> Result(Format, FormatError)

/// Convert to format
pub fn to_format(
  data: Dynamic,
  format: Format,
) -> Result(String, FormatError)

/// Parse format
pub fn parse(
  content: String,
  format: Format,
) -> Result(Dynamic, FormatError)

/// Validate format
pub fn validate(
  content: String,
  format: Format,
) -> Result(Nil, FormatError)
```

### interview_contract

Interview contract utilities.

#### Types

```gleam
pub type Contract {
  Contract(
    name: String,
    preconditions: List(String),
    postconditions: List(String),
    invariants: List(String),
  )
}
```

#### Functions

```gleam
/// Create new contract
pub fn new(name: String) -> Contract

/// Add precondition
pub fn add_precondition(
  contract: Contract,
  condition: String,
) -> Contract

/// Add postcondition
pub fn add_postcondition(
  contract: Contract,
  condition: String,
) -> Contract

/// Add invariant
pub fn add_invariant(
  contract: Contract,
  invariant: String,
) -> Contract

/// Validate contract
pub fn validate(
  contract: Contract,
  state: types.State,
) -> Result(Nil, types.Error)

/// Check precondition
pub fn check_precondition(
  contract: Contract,
  state: types.State,
) -> Result(Bool, types.Error)

/// Check postcondition
pub fn check_postcondition(
  contract: Contract,
  state: types.State,
) -> Result(Bool, types.Error)
```

### interview_questions

Question database utilities.

#### Types

```gleam
pub type Question {
  Question(
    id: String,
    category: String,
    text: String,
    pattern: String,
    required: Bool,
  )
}
```

#### Functions

```gleam
/// Get question by ID
pub fn get_question(
  id: String,
) -> Result(Question, types.Error)

/// Get questions by category
pub fn get_by_category(
  category: String,
) -> List(Question)

/// Search questions
pub fn search(
  query: String,
) -> List(Question)

/// Get all questions
pub fn all() -> List(Question)

/// Add question
pub fn add_question(
  question: Question,
) -> Result(Nil, types.Error)

/// Update question
pub fn update_question(
  question: Question,
) -> Result(Nil, types.Error)

/// Delete question
pub fn delete_question(
  id: String,
) -> Result(Nil, types.Error)
```

### interview_storage

Interview session storage.

#### Types

```gleam
pub type Storage {
  Storage(
    session_id: String,
    state: types.State,
    events: List(types.Event),
    metadata: types.Metadata,
  )
}

pub type StorageError {
  StorageNotFound(session_id: String)
  StorageError(message: String)
}
```

#### Functions

```gleam
/// Save session
pub fn save_session(
  storage: Storage,
) -> Result(Nil, StorageError)

/// Load session
pub fn load_session(
  session_id: String,
) -> Result(Storage, StorageError)

/// Delete session
pub fn delete_session(
  session_id: String,
) -> Result(Nil, StorageError)

/// List sessions
pub fn list_sessions() -> Result(List(String), StorageError)

/// Get session count
pub fn session_count() -> Result(Int, StorageError)

/// Clear all sessions
pub fn clear_all() -> Result(Nil, StorageError)
```

### plan_mode

Planning mode utilities.

#### Types

```gleam
pub type Plan {
  Plan(
    phases: List(types.Phase),
    total_steps: Int,
    estimated_time: String,
  )
}

pub type Phase {
  Phase(
    name: String,
    steps: List(types.Step),
    duration: String,
  )
}
```

#### Functions

```gleam
/// Generate plan from requirements
pub fn generate_plan(
  requirements: String,
) -> Result(Plan, types.Error)

/// Generate plan from spec
pub fn generate_plan_from_spec(
  spec: types.Spec,
) -> Result(Plan, types.Error)

/// Add phase to plan
pub fn add_phase(
  plan: Plan,
  phase: types.Phase,
) -> Plan

/// Get phase by name
pub fn get_phase(
  plan: Plan,
  name: String,
) -> Result(types.Phase, types.Error)

/// Calculate total steps
pub fn total_steps(plan: Plan) -> Int

/// Estimate completion time
pub fn estimate_completion(plan: Plan) -> String

/// Export plan to CUE
pub fn export_to_cue(plan: Plan) -> Result(String, types.Error)
```

### types

Core type definitions.

#### Types

```gleam
pub type Spec {
  Spec(
    name: String,
    description: Option(String),
    behaviors: List(Behavior),
    preconditions: List(String),
    postconditions: List(String),
  )
}

pub type Behavior {
  Behavior(
    name: String,
    method: Method,
    path: String,
    expected_status: Int,
    intent: String,
    preconditions: List(String),
    postconditions: List(String),
  )
}

pub type Method {
  Get
  Post
  Put
  Delete
  Patch
}

pub type Response {
  Response(
    status_code: Int,
    headers: List(#(String, String)),
    body: String,
  )
}

pub type Profile {
  Profile(
    name: String,
    questions: String,
  )
}
```

#### Usage Examples

```gleam
import intent.types

let spec = types.Spec(
  name: "User API",
  description: Some("REST API for user management"),
  behaviors: [
    types.Behavior(
      name: "get-user",
      method: types.Method.Get,
      path: "/users/1",
      expected_status: 200,
      intent: "Return user data by ID",
      preconditions: ["User exists"],
      postconditions: ["Response contains user data"],
    ),
  ],
  preconditions: [],
  postconditions: [],
)
```
