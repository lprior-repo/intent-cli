/// Parser for Intent specs from JSON (exported from CUE)
import gleam/dict.{type Dict}
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import intent/types.{
  type AIHints, type AntiPattern, type Behavior, type Boundary, type Check,
  type CodebaseContext, type CodebasePatterns, type CodebaseStack, type Config,
  type EntityHint, type EntryPoint, type Feature, type ImplementationHints,
  type LightBehavior, type LightRequest, type LightResponse, type LightSpec,
  type Method, type Request, type Response, type Rule, type RuleCheck,
  type SecurityHints, type Spec, type When, AIHints, AntiPattern, Behavior,
  Boundary, Check, CodebaseContext, CodebasePatterns, CodebaseStack, Config,
  Delete, EntityHint, EntryPoint, Feature, Get, Head, ImplementationHints,
  LightBehavior, LightRequest, LightResponse, LightSpec, Options, Patch, Post,
  Put, Request, Response, Rule, RuleCheck, SecurityHints, Spec, When,
}

/// Parse a spec from a JSON value
/// All fields are required - no backwards compatibility defaults
pub fn parse_spec(data: Dynamic) -> Result(Spec, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use audience <- result.try(dynamic.field("audience", dynamic.string)(data))
  use version <- result.try(dynamic.field("version", dynamic.string)(data))
  use success_criteria <- result.try(dynamic.field(
    "success_criteria",
    dynamic.list(dynamic.string),
  )(data))
  use config <- result.try(dynamic.field("config", parse_config)(data))
  use features <- result.try(dynamic.field(
    "features",
    dynamic.list(parse_feature),
  )(data))
  use rules <- result.try(dynamic.field("rules", dynamic.list(parse_rule))(data))
  use anti_patterns <- result.try(dynamic.field(
    "anti_patterns",
    dynamic.list(parse_anti_pattern),
  )(data))
  use ai_hints <- result.try(dynamic.field("ai_hints", parse_ai_hints)(data))

  Ok(Spec(
    name: name,
    description: description,
    audience: audience,
    version: version,
    success_criteria: success_criteria,
    config: config,
    features: features,
    rules: rules,
    anti_patterns: anti_patterns,
    ai_hints: ai_hints,
  ))
}

fn parse_config(data: Dynamic) -> Result(Config, List(DecodeError)) {
  use base_url <- result.try(dynamic.field("base_url", dynamic.string)(data))
  use timeout_ms <- result.try(dynamic.field("timeout_ms", dynamic.int)(data))
  use headers <- result.try(dynamic.field("headers", parse_string_dict)(data))
  Ok(Config(base_url, timeout_ms, headers))
}

fn parse_string_dict(
  data: Dynamic,
) -> Result(Dict(String, String), List(DecodeError)) {
  dynamic.dict(dynamic.string, dynamic.string)(data)
}

fn parse_feature(data: Dynamic) -> Result(Feature, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use behaviors <- result.try(dynamic.field(
    "behaviors",
    dynamic.list(parse_behavior),
  )(data))
  Ok(Feature(name, description, behaviors))
}

fn parse_behavior(data: Dynamic) -> Result(Behavior, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use intent <- result.try(dynamic.field("intent", dynamic.string)(data))
  use notes <- result.try(dynamic.field("notes", dynamic.string)(data))
  use requires <- result.try(dynamic.field(
    "requires",
    dynamic.list(dynamic.string),
  )(data))
  use tags <- result.try(dynamic.field("tags", dynamic.list(dynamic.string))(
    data,
  ))
  use request <- result.try(dynamic.field("request", parse_request)(data))
  use response <- result.try(dynamic.field("response", parse_response)(data))
  use captures <- result.try(dynamic.field("captures", parse_string_dict)(data))
  Ok(Behavior(
    name: name,
    intent: intent,
    notes: notes,
    requires: requires,
    tags: tags,
    request: request,
    response: response,
    captures: captures,
  ))
}

fn parse_method(data: Dynamic) -> Result(Method, List(DecodeError)) {
  data
  |> dynamic.string
  |> result.then(fn(s) {
    case s {
      "GET" -> Ok(Get)
      "POST" -> Ok(Post)
      "PUT" -> Ok(Put)
      "PATCH" -> Ok(Patch)
      "DELETE" -> Ok(Delete)
      "HEAD" -> Ok(Head)
      "OPTIONS" -> Ok(Options)
      _ ->
        Error([
          dynamic.DecodeError(expected: "HTTP method", found: s, path: []),
        ])
    }
  })
}

fn parse_request(data: Dynamic) -> Result(Request, List(DecodeError)) {
  use method <- result.try(dynamic.field("method", parse_method)(data))
  use path <- result.try(dynamic.field("path", dynamic.string)(data))
  use headers <- result.try(dynamic.field("headers", parse_string_dict)(data))
  use query <- result.try(dynamic.field("query", parse_json_dict)(data))
  use body <- result.try(dynamic.field("body", parse_json_value)(data))
  Ok(Request(method, path, headers, query, body))
}

fn parse_json_dict(
  data: Dynamic,
) -> Result(Dict(String, Json), List(DecodeError)) {
  data
  |> dynamic.dict(dynamic.string, dynamic.dynamic)
  |> result.map(fn(d) { dict.map_values(d, fn(_, v) { dynamic_to_json(v) }) })
}

fn parse_json_value(data: Dynamic) -> Result(Json, List(DecodeError)) {
  Ok(dynamic_to_json(data))
}

/// Convert a Dynamic value to Json
pub fn dynamic_to_json(data: Dynamic) -> Json {
  case dynamic.classify(data) {
    "Nil" -> json.null()
    "Bool" ->
      case dynamic.bool(data) {
        Ok(b) -> json.bool(b)
        Error(_) -> json.null()
      }
    "Int" ->
      case dynamic.int(data) {
        Ok(i) -> json.int(i)
        Error(_) -> json.null()
      }
    "Float" ->
      case dynamic.float(data) {
        Ok(f) -> json.float(f)
        Error(_) -> json.null()
      }
    "String" | "BitArray" ->
      case dynamic.string(data) {
        Ok(s) -> json.string(s)
        Error(_) -> json.null()
      }
    "List" | "Tuple" ->
      case dynamic.list(dynamic.dynamic)(data) {
        Ok(items) -> json.array(items, dynamic_to_json)
        Error(_) -> json.null()
      }
    "Dict" | "Map" ->
      case dynamic.dict(dynamic.string, dynamic.dynamic)(data) {
        Ok(d) ->
          d
          |> dict.to_list
          |> list.map(fn(pair) { #(pair.0, dynamic_to_json(pair.1)) })
          |> json.object
        Error(_) -> json.null()
      }
    _ -> json.null()
  }
}

fn parse_response(data: Dynamic) -> Result(Response, List(DecodeError)) {
  use status <- result.try(dynamic.field("status", dynamic.int)(data))
  use example <- result.try(dynamic.field("example", parse_json_value)(data))
  use checks <- result.try(dynamic.field("checks", parse_checks)(data))
  // Headers are optional - use empty dict if not present
  let headers =
    dynamic.field("headers", parse_string_dict)(data)
    |> result.unwrap(dict.new())
  Ok(Response(status, example, checks, headers))
}

fn parse_checks(data: Dynamic) -> Result(Dict(String, Check), List(DecodeError)) {
  data
  |> dynamic.dict(dynamic.string, parse_check)
}

fn parse_check(data: Dynamic) -> Result(Check, List(DecodeError)) {
  use rule <- result.try(dynamic.field("rule", dynamic.string)(data))
  use why <- result.try(dynamic.field("why", dynamic.string)(data))
  Ok(Check(rule, why))
}

fn parse_rule(data: Dynamic) -> Result(Rule, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use when <- result.try(dynamic.field("when", parse_when)(data))
  use check <- result.try(dynamic.field("check", parse_rule_check)(data))
  use example <- result.try(dynamic.field("example", parse_json_value)(data))
  Ok(Rule(name, description, when, check, example))
}

/// Parse when clause with optional fields (intent-cli-5zd)
/// Missing fields are treated as wildcards (None = match everything)
fn parse_when(data: Dynamic) -> Result(When, List(DecodeError)) {
  // Use optional_field instead of field - returns Result(Option(T), ...)
  let status =
    dynamic.optional_field("status", dynamic.string)(data)
    |> result.unwrap(None)

  let method =
    dynamic.optional_field("method", parse_method)(data)
    |> result.unwrap(None)

  let path =
    dynamic.optional_field("path", dynamic.string)(data)
    |> result.unwrap(None)

  Ok(When(status, method, path))
}

/// Expose parse_when for testing (intent-cli-5zd)
pub fn parse_when_for_test(data: Dynamic) -> Result(When, List(DecodeError)) {
  parse_when(data)
}

fn parse_rule_check(data: Dynamic) -> Result(RuleCheck, List(DecodeError)) {
  use body_must_not_contain <- result.try(dynamic.field(
    "body_must_not_contain",
    dynamic.list(dynamic.string),
  )(data))
  use body_must_contain <- result.try(dynamic.field(
    "body_must_contain",
    dynamic.list(dynamic.string),
  )(data))
  use fields_must_exist <- result.try(dynamic.field(
    "fields_must_exist",
    dynamic.list(dynamic.string),
  )(data))
  use fields_must_not_exist <- result.try(dynamic.field(
    "fields_must_not_exist",
    dynamic.list(dynamic.string),
  )(data))
  use header_must_exist <- result.try(dynamic.field(
    "header_must_exist",
    dynamic.string,
  )(data))
  use header_must_not_exist <- result.try(dynamic.field(
    "header_must_not_exist",
    dynamic.string,
  )(data))
  Ok(RuleCheck(
    body_must_not_contain,
    body_must_contain,
    fields_must_exist,
    fields_must_not_exist,
    header_must_exist,
    header_must_not_exist,
  ))
}

fn parse_anti_pattern(data: Dynamic) -> Result(AntiPattern, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use bad_example <- result.try(dynamic.field("bad_example", parse_json_value)(
    data,
  ))
  use good_example <- result.try(dynamic.field("good_example", parse_json_value)(
    data,
  ))
  use why <- result.try(dynamic.field("why", dynamic.string)(data))

  Ok(AntiPattern(
    name: name,
    description: description,
    bad_example: bad_example,
    good_example: good_example,
    why: why,
  ))
}

fn parse_ai_hints(data: Dynamic) -> Result(AIHints, List(DecodeError)) {
  use implementation <- result.try(dynamic.field(
    "implementation",
    parse_implementation_hints,
  )(data))
  use entities <- result.try(dynamic.field("entities", parse_entities)(data))
  use security <- result.try(dynamic.field("security", parse_security_hints)(
    data,
  ))
  use pitfalls <- result.try(dynamic.field(
    "pitfalls",
    dynamic.list(dynamic.string),
  )(data))

  // Codebase context is optional
  let codebase = case dynamic.field("codebase", parse_codebase_context)(data) {
    Ok(ctx) -> Some(ctx)
    Error(_) -> None
  }

  Ok(AIHints(implementation, entities, security, pitfalls, codebase))
}

fn parse_implementation_hints(
  data: Dynamic,
) -> Result(ImplementationHints, List(DecodeError)) {
  use suggested_stack <- result.try(dynamic.field(
    "suggested_stack",
    dynamic.list(dynamic.string),
  )(data))
  Ok(ImplementationHints(suggested_stack))
}

fn parse_entities(
  data: Dynamic,
) -> Result(Dict(String, EntityHint), List(DecodeError)) {
  dynamic.dict(dynamic.string, parse_entity_hint)(data)
}

fn parse_entity_hint(data: Dynamic) -> Result(EntityHint, List(DecodeError)) {
  use fields <- result.try(dynamic.field("fields", parse_string_dict)(data))
  Ok(EntityHint(fields))
}

fn parse_security_hints(
  data: Dynamic,
) -> Result(SecurityHints, List(DecodeError)) {
  use password_hashing <- result.try(dynamic.field(
    "password_hashing",
    dynamic.string,
  )(data))
  use jwt_algorithm <- result.try(dynamic.field("jwt_algorithm", dynamic.string)(
    data,
  ))
  use jwt_expiry <- result.try(dynamic.field("jwt_expiry", dynamic.string)(data))
  use rate_limiting <- result.try(dynamic.field("rate_limiting", dynamic.string)(
    data,
  ))
  Ok(SecurityHints(password_hashing, jwt_algorithm, jwt_expiry, rate_limiting))
}

// =============================================================================
// Codebase Context Parsing
// =============================================================================

fn parse_codebase_context(
  data: Dynamic,
) -> Result(CodebaseContext, List(DecodeError)) {
  // All fields in codebase context are optional
  let patterns = case dynamic.field("patterns", parse_codebase_patterns)(data) {
    Ok(p) -> Some(p)
    Error(_) -> None
  }

  let stack = case dynamic.field("stack", parse_codebase_stack)(data) {
    Ok(s) -> Some(s)
    Error(_) -> None
  }

  let entry_points =
    dynamic.field("entry_points", dynamic.list(parse_entry_point))(data)
    |> result.unwrap([])

  let boundaries =
    dynamic.field("boundaries", dynamic.list(parse_boundary))(data)
    |> result.unwrap([])

  Ok(CodebaseContext(
    patterns: patterns,
    stack: stack,
    entry_points: entry_points,
    boundaries: boundaries,
  ))
}

fn parse_codebase_patterns(
  data: Dynamic,
) -> Result(CodebasePatterns, List(DecodeError)) {
  // All pattern fields default to empty string if not present
  let error_handling =
    dynamic.field("error_handling", dynamic.string)(data)
    |> result.unwrap("")
  let auth_middleware =
    dynamic.field("auth_middleware", dynamic.string)(data)
    |> result.unwrap("")
  let validation =
    dynamic.field("validation", dynamic.string)(data)
    |> result.unwrap("")
  let testing =
    dynamic.field("testing", dynamic.string)(data)
    |> result.unwrap("")

  Ok(CodebasePatterns(
    error_handling: error_handling,
    auth_middleware: auth_middleware,
    validation: validation,
    testing: testing,
  ))
}

fn parse_codebase_stack(
  data: Dynamic,
) -> Result(CodebaseStack, List(DecodeError)) {
  // All stack fields default to empty string if not present
  let language =
    dynamic.field("language", dynamic.string)(data)
    |> result.unwrap("")
  let framework =
    dynamic.field("framework", dynamic.string)(data)
    |> result.unwrap("")
  let database =
    dynamic.field("database", dynamic.string)(data)
    |> result.unwrap("")
  let orm =
    dynamic.field("orm", dynamic.string)(data)
    |> result.unwrap("")
  let testing =
    dynamic.field("testing", dynamic.string)(data)
    |> result.unwrap("")

  Ok(CodebaseStack(
    language: language,
    framework: framework,
    database: database,
    orm: orm,
    testing: testing,
  ))
}

fn parse_entry_point(data: Dynamic) -> Result(EntryPoint, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use path <- result.try(dynamic.field("path", dynamic.string)(data))
  let description =
    dynamic.field("description", dynamic.string)(data)
    |> result.unwrap("")

  Ok(EntryPoint(name: name, path: path, description: description))
}

fn parse_boundary(data: Dynamic) -> Result(Boundary, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  let description =
    dynamic.field("description", dynamic.string)(data)
    |> result.unwrap("")
  let modules =
    dynamic.field("modules", dynamic.list(dynamic.string))(data)
    |> result.unwrap([])

  Ok(Boundary(name: name, description: description, modules: modules))
}

// =============================================================================
// Light Spec Parsing - Minimal spec for simple tasks
// =============================================================================

/// Parse a light spec from a JSON value
/// Detect light spec by absence of config/rules/features blocks
pub fn parse_light_spec(data: Dynamic) -> Result(LightSpec, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use description <- result.try(dynamic.field("description", dynamic.string)(
    data,
  ))
  use behaviors <- result.try(dynamic.field(
    "behaviors",
    dynamic.list(parse_light_behavior),
  )(data))

  // Optional fields - use empty list / None if not present
  let anti_patterns =
    dynamic.field("anti_patterns", dynamic.list(parse_anti_pattern))(data)
    |> result.unwrap([])

  let ai_hints = case dynamic.field("ai_hints", parse_ai_hints)(data) {
    Ok(hints) -> Some(hints)
    Error(_) -> None
  }

  Ok(LightSpec(
    name: name,
    description: description,
    behaviors: behaviors,
    anti_patterns: anti_patterns,
    ai_hints: ai_hints,
  ))
}

/// Parse a light behavior from JSON
fn parse_light_behavior(
  data: Dynamic,
) -> Result(LightBehavior, List(DecodeError)) {
  use name <- result.try(dynamic.field("name", dynamic.string)(data))
  use intent <- result.try(dynamic.field("intent", dynamic.string)(data))
  use request <- result.try(dynamic.field("request", parse_light_request)(data))
  use response <- result.try(dynamic.field("response", parse_light_response)(
    data,
  ))

  Ok(LightBehavior(
    name: name,
    intent: intent,
    request: request,
    response: response,
  ))
}

/// Parse a light request from JSON
fn parse_light_request(data: Dynamic) -> Result(LightRequest, List(DecodeError)) {
  use method <- result.try(dynamic.field("method", parse_method)(data))
  use path <- result.try(dynamic.field("path", dynamic.string)(data))

  // Body is optional for light requests
  let body =
    dynamic.field("body", parse_json_value)(data)
    |> result.unwrap(json.null())

  Ok(LightRequest(method: method, path: path, body: body))
}

/// Parse a light response from JSON
fn parse_light_response(
  data: Dynamic,
) -> Result(LightResponse, List(DecodeError)) {
  use status <- result.try(dynamic.field("status", dynamic.int)(data))

  // Checks are optional for light responses
  let checks =
    dynamic.field("checks", parse_checks)(data)
    |> result.unwrap(dict.new())

  Ok(LightResponse(status: status, checks: checks))
}

/// Detect whether JSON data represents a light spec or full spec
/// Light specs have behaviors directly at top level, no config/features/rules
pub fn is_light_spec(data: Dynamic) -> Bool {
  // Light spec: has "behaviors" at top level, no "config" or "features"
  let has_behaviors =
    dynamic.field("behaviors", dynamic.list(dynamic.dynamic))(data)
    |> result.is_ok

  let has_config =
    dynamic.field("config", dynamic.dynamic)(data)
    |> result.is_ok

  let has_features =
    dynamic.field("features", dynamic.list(dynamic.dynamic))(data)
    |> result.is_ok

  has_behaviors && !has_config && !has_features
}
