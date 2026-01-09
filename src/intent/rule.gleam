/// Rule expression parser and types
/// Parses human-friendly rule strings like "equals foo" or "integer >= 5"
import gleam/dict
import gleam/dynamic
import gleam/float
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/string
import intent/formats.{validate_email, validate_uuid}
import intent/parser

/// Parsed rule expression
pub type RuleExpr {
  // Equality
  Equals(String)
  EqualsVariable(String)
  EqualsInt(Int)
  EqualsFloat(Float)
  EqualsBool(Bool)

  // Types
  IsString
  IsInteger
  IsNumber
  IsBoolean
  IsArray
  IsObject
  IsNull

  // String patterns
  StringMatching(String)
  StringStartingWith(String)
  StringEndingWith(String)
  StringContaining(String)
  NonEmptyString
  IsEmail
  IsUuid
  IsUri
  IsJwt
  IsIso8601

  // Numbers
  IntegerGte(Int)
  IntegerGt(Int)
  IntegerLte(Int)
  IntegerLt(Int)
  IntegerBetween(Int, Int)
  NumberBetween(Float, Float)

  // Presence
  Present
  Absent
  NotNull

  // Arrays
  NonEmptyArray
  ArrayOfLength(Int)
  ArrayWithMinItems(Int)
  ArrayWithMaxItems(Int)
  ArrayWhereEach(RuleExpr)

  // Compound
  ValidJwt
  ValidIso8601
  OneOf(List(String))

  // Contains reference
  ContainsVariable(String)

  // Unknown/raw for rules we can't parse yet
  Raw(String)
}

/// Parse a rule string into a RuleExpr
pub fn parse(rule: String) -> RuleExpr {
  let rule = string.trim(rule)

  // Try each parser in order
  case try_parse_equals(rule) {
    Some(expr) -> expr
    None ->
      case try_parse_type(rule) {
        Some(expr) -> expr
        None ->
          case try_parse_string_pattern(rule) {
            Some(expr) -> expr
            None ->
              case try_parse_number(rule) {
                Some(expr) -> expr
                None ->
                  case try_parse_presence(rule) {
                    Some(expr) -> expr
                    None ->
                      case try_parse_array(rule) {
                        Some(expr) -> expr
                        None ->
                          case try_parse_compound(rule) {
                            Some(expr) -> expr
                            None -> Raw(rule)
                          }
                      }
                  }
              }
          }
      }
  }
}

fn try_parse_equals(rule: String) -> Option(RuleExpr) {
  case string.starts_with(rule, "equals ") {
    True -> {
      let value = string.drop_left(rule, 7)
      case string.starts_with(value, "${") && string.ends_with(value, "}") {
        True -> {
          let var_name =
            value
            |> string.drop_left(2)
            |> string.drop_right(1)
          Some(EqualsVariable(var_name))
        }
        False ->
          case value {
            "true" -> Some(EqualsBool(True))
            "false" -> Some(EqualsBool(False))
            _ ->
              case int.parse(value) {
                Ok(n) -> Some(EqualsInt(n))
                Error(_) ->
                  case float.parse(value) {
                    Ok(f) -> Some(EqualsFloat(f))
                    Error(_) -> Some(Equals(value))
                  }
              }
          }
      }
    }
    False -> None
  }
}

fn try_parse_type(rule: String) -> Option(RuleExpr) {
  case rule {
    "string" -> Some(IsString)
    "integer" -> Some(IsInteger)
    "number" -> Some(IsNumber)
    "boolean" -> Some(IsBoolean)
    "array" -> Some(IsArray)
    "object" -> Some(IsObject)
    "null" -> Some(IsNull)
    _ -> None
  }
}

fn try_parse_string_pattern(rule: String) -> Option(RuleExpr) {
  case rule {
    "non-empty string" -> Some(NonEmptyString)
    "email" -> Some(IsEmail)
    "uuid" -> Some(IsUuid)
    "uri" -> Some(IsUri)
    "jwt" -> Some(IsJwt)
    "iso8601 datetime" -> Some(IsIso8601)
    _ -> {
      case string.starts_with(rule, "string matching ") {
        True -> Some(StringMatching(string.drop_left(rule, 16)))
        False ->
          case string.starts_with(rule, "string starting with ") {
            True -> Some(StringStartingWith(string.drop_left(rule, 21)))
            False ->
              case string.starts_with(rule, "string ending with ") {
                True -> Some(StringEndingWith(string.drop_left(rule, 19)))
                False ->
                  case string.starts_with(rule, "string containing ") {
                    True -> Some(StringContaining(string.drop_left(rule, 18)))
                    False ->
                      case string.starts_with(rule, "contains ${") {
                        True -> {
                          let var =
                            rule
                            |> string.drop_left(11)
                            |> string.drop_right(1)
                          Some(ContainsVariable(var))
                        }
                        False -> None
                      }
                  }
              }
          }
      }
    }
  }
}

fn try_parse_number(rule: String) -> Option(RuleExpr) {
  // integer >= N
  case string.starts_with(rule, "integer >= ") {
    True -> {
      let num_str = string.drop_left(rule, 11)
      case int.parse(num_str) {
        Ok(n) -> Some(IntegerGte(n))
        Error(_) -> None
      }
    }
    False ->
      case string.starts_with(rule, "integer > ") {
        True -> {
          let rest = string.drop_left(rule, 10)
          // Check for "integer > X and < Y"
          case parse_range(rest, " and < ") {
            Some(#(low, high)) -> Some(IntegerBetween(low + 1, high - 1))
            None ->
              case int.parse(rest) {
                Ok(n) -> Some(IntegerGt(n))
                Error(_) -> None
              }
          }
        }
        False ->
          case string.starts_with(rule, "integer <= ") {
            True -> {
              let num_str = string.drop_left(rule, 11)
              case int.parse(num_str) {
                Ok(n) -> Some(IntegerLte(n))
                Error(_) -> None
              }
            }
            False ->
              case string.starts_with(rule, "integer < ") {
                True -> {
                  let num_str = string.drop_left(rule, 10)
                  case int.parse(num_str) {
                    Ok(n) -> Some(IntegerLt(n))
                    Error(_) -> None
                  }
                }
                False ->
                  case string.starts_with(rule, "number between ") {
                    True -> {
                      let rest = string.drop_left(rule, 15)
                      case parse_float_range(rest) {
                        Some(#(low, high)) -> Some(NumberBetween(low, high))
                        None -> None
                      }
                    }
                    False -> None
                  }
              }
          }
      }
  }
}

fn parse_range(s: String, sep: String) -> Option(#(Int, Int)) {
  case string.split(s, sep) {
    [low_str, high_str] ->
      case int.parse(low_str), int.parse(high_str) {
        Ok(low), Ok(high) -> Some(#(low, high))
        _, _ -> None
      }
    _ -> None
  }
}

fn parse_float_range(s: String) -> Option(#(Float, Float)) {
  case string.split(s, " and ") {
    [low_str, high_str] ->
      case float.parse(low_str), float.parse(high_str) {
        Ok(low), Ok(high) -> Some(#(low, high))
        _, _ -> None
      }
    _ -> None
  }
}

fn try_parse_presence(rule: String) -> Option(RuleExpr) {
  case rule {
    "present" -> Some(Present)
    "absent" -> Some(Absent)
    "not null" -> Some(NotNull)
    _ -> None
  }
}

fn try_parse_array(rule: String) -> Option(RuleExpr) {
  case rule {
    "non-empty array" -> Some(NonEmptyArray)
    _ ->
      case string.starts_with(rule, "array of length ") {
        True -> {
          let num_str = string.drop_left(rule, 16)
          case int.parse(num_str) {
            Ok(n) -> Some(ArrayOfLength(n))
            Error(_) -> None
          }
        }
        False ->
          case string.starts_with(rule, "array with min ") {
            True -> {
              let rest = string.drop_left(rule, 15)
              case string.split(rest, " item") {
                [num_str, ..] ->
                  case int.parse(num_str) {
                    Ok(n) -> Some(ArrayWithMinItems(n))
                    Error(_) -> None
                  }
                _ -> None
              }
            }
            False ->
              case string.starts_with(rule, "array with max ") {
                True -> {
                  let rest = string.drop_left(rule, 15)
                  case string.split(rest, " item") {
                    [num_str, ..] ->
                      case int.parse(num_str) {
                        Ok(n) -> Some(ArrayWithMaxItems(n))
                        Error(_) -> None
                      }
                    _ -> None
                  }
                }
                False ->
                  case string.starts_with(rule, "array where each ") {
                    True -> {
                      let inner = string.drop_left(rule, 17)
                      // Handle "is X" or "matches X"
                      let inner_rule = case string.starts_with(inner, "is ") {
                        True -> string.drop_left(inner, 3)
                        False ->
                          case string.starts_with(inner, "matches ") {
                            True ->
                              "string matching " <> string.drop_left(inner, 8)
                            False -> inner
                          }
                      }
                      Some(ArrayWhereEach(parse(inner_rule)))
                    }
                    False -> None
                  }
              }
          }
      }
  }
}

fn try_parse_compound(rule: String) -> Option(RuleExpr) {
  case rule {
    "valid JWT" -> Some(ValidJwt)
    "valid ISO8601 datetime" -> Some(ValidIso8601)
    _ ->
      case string.starts_with(rule, "one of ") {
        True -> {
          let list_str = string.drop_left(rule, 7)
          case parse_string_list(list_str) {
            Ok(items) -> Some(OneOf(items))
            Error(_) -> None
          }
        }
        False -> None
      }
  }
}

/// Parse a list like ["a", "b", "c"]
fn parse_string_list(s: String) -> Result(List(String), Nil) {
  let s = string.trim(s)
  case string.starts_with(s, "[") && string.ends_with(s, "]") {
    True -> {
      let inner =
        s
        |> string.drop_left(1)
        |> string.drop_right(1)
        |> string.trim

      inner
      |> string.split(",")
      |> list.map(fn(item) {
        let item = string.trim(item)
        // Remove quotes
        case string.starts_with(item, "\"") && string.ends_with(item, "\"") {
          True ->
            item
            |> string.drop_left(1)
            |> string.drop_right(1)
          False -> item
        }
      })
      |> Ok
    }
    False -> Error(Nil)
  }
}

/// Format a rule expression back to a human-readable string
pub fn to_string(expr: RuleExpr) -> String {
  case expr {
    Equals(s) -> "equals " <> s
    EqualsVariable(v) -> "equals ${" <> v <> "}"
    EqualsInt(n) -> "equals " <> int.to_string(n)
    EqualsFloat(f) -> "equals " <> float.to_string(f)
    EqualsBool(True) -> "equals true"
    EqualsBool(False) -> "equals false"
    IsString -> "string"
    IsInteger -> "integer"
    IsNumber -> "number"
    IsBoolean -> "boolean"
    IsArray -> "array"
    IsObject -> "object"
    IsNull -> "null"
    StringMatching(p) -> "string matching " <> p
    StringStartingWith(p) -> "string starting with " <> p
    StringEndingWith(p) -> "string ending with " <> p
    StringContaining(p) -> "string containing " <> p
    NonEmptyString -> "non-empty string"
    IsEmail -> "email"
    IsUuid -> "uuid"
    IsUri -> "uri"
    IsJwt -> "jwt"
    IsIso8601 -> "iso8601 datetime"
    IntegerGte(n) -> "integer >= " <> int.to_string(n)
    IntegerGt(n) -> "integer > " <> int.to_string(n)
    IntegerLte(n) -> "integer <= " <> int.to_string(n)
    IntegerLt(n) -> "integer < " <> int.to_string(n)
    IntegerBetween(low, high) ->
      "integer > "
      <> int.to_string(low - 1)
      <> " and < "
      <> int.to_string(high + 1)
    NumberBetween(low, high) ->
      "number between "
      <> float.to_string(low)
      <> " and "
      <> float.to_string(high)
    Present -> "present"
    Absent -> "absent"
    NotNull -> "not null"
    NonEmptyArray -> "non-empty array"
    ArrayOfLength(n) -> "array of length " <> int.to_string(n)
    ArrayWithMinItems(n) -> "array with min " <> int.to_string(n) <> " items"
    ArrayWithMaxItems(n) -> "array with max " <> int.to_string(n) <> " items"
    ArrayWhereEach(inner) -> "array where each " <> to_string(inner)
    ValidJwt -> "valid JWT"
    ValidIso8601 -> "valid ISO8601 datetime"
    OneOf(items) ->
      "one of ["
      <> string.join(list.map(items, fn(i) { "\"" <> i <> "\"" }), ", ")
      <> "]"
    ContainsVariable(v) -> "contains ${" <> v <> "}"
    Raw(s) -> s
  }
}

pub type Parser =
  fn(String) -> Option(RuleExpr)

fn get_standard_parsers() -> List(Parser) {
  [
    try_parse_equals,
    try_parse_type,
    try_parse_string_pattern,
    try_parse_number,
    try_parse_presence,
    try_parse_array,
    try_parse_compound,
  ]
}

pub fn parse_with_custom(rule: String, custom_parsers: List(Parser)) -> RuleExpr {
  let parsers = list.append(custom_parsers, get_standard_parsers())
  case find_first_parser(parsers, rule) {
    Some(expr) -> expr
    None -> Raw(rule)
  }
}

fn find_first_parser(parsers: List(Parser), rule: String) -> Option(RuleExpr) {
  case parsers {
    [] -> None
    [parser, ..rest] ->
      case parser(rule) {
        Some(expr) -> Some(expr)
        None -> find_first_parser(rest, rule)
      }
  }
}

pub type Validator =
  fn(json.Json) -> Result(Nil, String)

pub fn make_rule_validator(expr: RuleExpr) -> Validator {
  fn(value: json.Json) -> Result(Nil, String) {
    evaluate_rule_for_validator(expr, value)
  }
}

fn evaluate_rule_for_validator(
  expr: RuleExpr,
  value: json.Json,
) -> Result(Nil, String) {
  case expr {
    Equals(expected) -> check_equals_string(value, expected)
    EqualsInt(expected) -> check_equals_int(value, expected)
    EqualsFloat(expected) -> check_equals_float(value, expected)
    EqualsBool(expected) -> check_equals_bool(value, expected)
    IsString -> check_is_string(value)
    IsInteger -> check_is_integer(value)
    IsNumber -> check_is_number(value)
    IsBoolean -> check_is_boolean(value)
    IsArray -> check_is_array(value)
    IsObject -> check_is_object(value)
    IsNull -> check_is_null(value)
    NonEmptyString -> check_non_empty_string(value)
    StringMatching(pattern) -> check_string_matching(value, pattern)
    StringStartingWith(prefix) -> check_string_starting_with(value, prefix)
    StringEndingWith(suffix) -> check_string_ending_with(value, suffix)
    StringContaining(substring) -> check_string_containing(value, substring)
    IsEmail -> check_is_email(value)
    IsUuid -> check_is_uuid(value)
    IsUri -> check_is_uri(value)
    IsJwt -> check_is_jwt(value)
    IsIso8601 -> check_is_iso8601(value)
    ValidJwt -> check_is_jwt(value)
    ValidIso8601 -> check_is_iso8601(value)
    IntegerGte(n) -> check_integer_gte(value, n)
    IntegerGt(n) -> check_integer_gt(value, n)
    IntegerLte(n) -> check_integer_lte(value, n)
    IntegerLt(n) -> check_integer_lt(value, n)
    IntegerBetween(low, high) -> check_integer_between(value, low, high)
    NumberBetween(low, high) -> check_number_between(value, low, high)
    NotNull -> check_not_null(value)
    NonEmptyArray -> check_non_empty_array(value)
    ArrayOfLength(n) -> check_array_of_length(value, n)
    ArrayWithMinItems(n) -> check_array_min_items(value, n)
    ArrayWithMaxItems(n) -> check_array_max_items(value, n)
    OneOf(options) -> check_one_of(value, options)
    Raw(raw) -> {
      case string.starts_with(raw, "equals ") {
        True -> {
          let expected = string.drop_left(raw, 7)
          check_equals_string(value, expected)
        }
        False -> Error("Unknown rule: " <> raw)
      }
    }
    _ -> Error("Rule not implemented: " <> to_string(expr))
  }
}

fn check_equals_string(
  value: json.Json,
  expected: String,
) -> Result(Nil, String) {
  let actual = json_to_raw_string(value)
  case actual == expected {
    True -> Ok(Nil)
    False -> Error("Expected '" <> expected <> "' but got '" <> actual <> "'")
  }
}

fn check_equals_int(value: json.Json, expected: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual == expected {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected "
            <> int.to_string(expected)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_equals_float(value: json.Json, expected: Float) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.float) {
    Ok(actual) ->
      case actual == expected {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected "
            <> float.to_string(expected)
            <> " but got "
            <> float.to_string(actual),
          )
      }
    Error(_) -> Error("Expected float but got " <> json.to_string(value))
  }
}

fn check_equals_bool(value: json.Json, expected: Bool) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.bool) {
    Ok(actual) ->
      case actual == expected {
        True -> Ok(Nil)
        False -> {
          let expected_str = case expected {
            True -> "true"
            False -> "false"
          }
          let actual_str = case actual {
            True -> "true"
            False -> "false"
          }
          Error("Expected " <> expected_str <> " but got " <> actual_str)
        }
      }
    Error(_) -> Error("Expected boolean but got " <> json.to_string(value))
  }
}

fn check_is_string(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_is_integer(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_is_number(value: json.Json) -> Result(Nil, String) {
  let json_str = json.to_string(value)
  case json.decode(json_str, dynamic.int) {
    Ok(_) -> Ok(Nil)
    Error(_) ->
      case json.decode(json_str, dynamic.float) {
        Ok(_) -> Ok(Nil)
        Error(_) -> Error("Expected number but got " <> json_str)
      }
  }
}

fn check_is_boolean(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.bool) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Expected boolean but got " <> json.to_string(value))
  }
}

fn check_is_array(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

fn check_is_object(value: json.Json) -> Result(Nil, String) {
  case
    json.decode(
      json.to_string(value),
      dynamic.dict(dynamic.string, dynamic.dynamic),
    )
  {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Expected object but got " <> json.to_string(value))
  }
}

fn check_is_null(value: json.Json) -> Result(Nil, String) {
  case json.to_string(value) {
    "null" -> Ok(Nil)
    other -> Error("Expected null but got " <> other)
  }
}

fn check_non_empty_string(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) ->
      case string.is_empty(s) {
        True -> Error("Expected non-empty string but got empty string")
        False -> Ok(Nil)
      }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_string_matching(
  value: json.Json,
  pattern: String,
) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> {
      case regexp.compile(pattern, regexp.Options(False, False)) {
        Ok(re) ->
          case regexp.check(re, s) {
            True -> Ok(Nil)
            False ->
              Error(
                "String '"
                <> s
                <> "' does not match pattern /"
                <> pattern
                <> "/",
              )
          }
        Error(_) -> Error("Invalid regex pattern: " <> pattern)
      }
    }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_string_starting_with(
  value: json.Json,
  prefix: String,
) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) ->
      case string.starts_with(s, prefix) {
        True -> Ok(Nil)
        False ->
          Error("String '" <> s <> "' does not start with '" <> prefix <> "'")
      }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_string_ending_with(
  value: json.Json,
  suffix: String,
) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) ->
      case string.ends_with(s, suffix) {
        True -> Ok(Nil)
        False ->
          Error("String '" <> s <> "' does not end with '" <> suffix <> "'")
      }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_string_containing(
  value: json.Json,
  substring: String,
) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) ->
      case string.contains(s, substring) {
        True -> Ok(Nil)
        False ->
          Error("String '" <> s <> "' does not contain '" <> substring <> "'")
      }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_is_email(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> validate_email(s)
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_is_uuid(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> validate_uuid(s)
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_is_uri(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> validate_uri(s)
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_is_jwt(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> {
      let parts = string.split(s, ".")
      case parts {
        [header, payload, _signature] -> {
          case validate_jwt_part(header, "header") {
            Ok(_) ->
              case validate_jwt_header_has_alg(header) {
                Ok(_) -> validate_jwt_part(payload, "payload")
                Error(e) -> Error(e)
              }
            Error(e) -> Error(e)
          }
        }
        _ ->
          Error(
            "'" <> s <> "' is not a valid JWT (expected 3 dot-separated parts)",
          )
      }
    }
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn validate_jwt_part(part: String, name: String) -> Result(Nil, String) {
  case base64_url_decode(part) {
    Ok(decoded) ->
      case json.decode(decoded, dynamic.dynamic) {
        Ok(_) -> Ok(Nil)
        Error(_) ->
          Error("JWT " <> name <> " is not valid JSON after Base64 decoding")
      }
    Error(_) -> Error("JWT " <> name <> " is not valid Base64URL encoding")
  }
}

fn validate_jwt_header_has_alg(header: String) -> Result(Nil, String) {
  case base64_url_decode(header) {
    Ok(decoded) ->
      case json.decode(decoded, dynamic.dict(dynamic.string, dynamic.dynamic)) {
        Ok(obj) ->
          case dict.has_key(obj, "alg") {
            True -> Ok(Nil)
            False -> Error("JWT header missing required 'alg' field")
          }
        Error(_) -> Error("JWT header is not a valid JSON object")
      }
    Error(_) -> Error("JWT header is not valid Base64URL encoding")
  }
}

@external(erlang, "intent_ffi", "base64_url_decode")
fn base64_url_decode(input: String) -> Result(String, String)

fn check_is_iso8601(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.string) {
    Ok(s) -> validate_iso8601(s)
    Error(_) -> Error("Expected string but got " <> json.to_string(value))
  }
}

fn check_integer_gte(value: json.Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual >= n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected integer >= "
            <> int.to_string(n)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_integer_gt(value: json.Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual > n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected integer > "
            <> int.to_string(n)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_integer_lte(value: json.Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual <= n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected integer <= "
            <> int.to_string(n)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_integer_lt(value: json.Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual < n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected integer < "
            <> int.to_string(n)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_integer_between(
  value: json.Json,
  low: Int,
  high: Int,
) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.int) {
    Ok(actual) ->
      case actual >= low && actual <= high {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected integer between "
            <> int.to_string(low)
            <> " and "
            <> int.to_string(high)
            <> " but got "
            <> int.to_string(actual),
          )
      }
    Error(_) -> Error("Expected integer but got " <> json.to_string(value))
  }
}

fn check_number_between(
  value: json.Json,
  low: Float,
  high: Float,
) -> Result(Nil, String) {
  let json_str = json.to_string(value)
  case json.decode(json_str, dynamic.float) {
    Ok(actual) ->
      case actual >=. low && actual <=. high {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected number between "
            <> float.to_string(low)
            <> " and "
            <> float.to_string(high)
            <> " but got "
            <> float.to_string(actual),
          )
      }
    Error(_) ->
      case json.decode(json_str, dynamic.int) {
        Ok(i) -> {
          let actual = int.to_float(i)
          case actual >=. low && actual <=. high {
            True -> Ok(Nil)
            False ->
              Error(
                "Expected number between "
                <> float.to_string(low)
                <> " and "
                <> float.to_string(high)
                <> " but got "
                <> float.to_string(actual),
              )
          }
        }
        Error(_) -> Error("Expected number but got " <> json_str)
      }
  }
}

fn check_not_null(value: json.Json) -> Result(Nil, String) {
  case json.to_string(value) {
    "null" -> Error("Expected non-null value but got null")
    _ -> Ok(Nil)
  }
}

fn check_non_empty_array(value: json.Json) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(arr) ->
      case list.is_empty(arr) {
        True -> Error("Expected non-empty array but got empty array")
        False -> Ok(Nil)
      }
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

fn check_array_of_length(value: json.Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(arr) -> {
      let actual_len = list.length(arr)
      case actual_len == n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected array of length "
            <> int.to_string(n)
            <> " but got length "
            <> int.to_string(actual_len),
          )
      }
    }
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

fn check_array_min_items(value: json.Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(arr) -> {
      let actual_len = list.length(arr)
      case actual_len >= n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected array with at least "
            <> int.to_string(n)
            <> " items but got "
            <> int.to_string(actual_len),
          )
      }
    }
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

fn check_array_max_items(value: json.Json, n: Int) -> Result(Nil, String) {
  case json.decode(json.to_string(value), dynamic.list(dynamic.dynamic)) {
    Ok(arr) -> {
      let actual_len = list.length(arr)
      case actual_len <= n {
        True -> Ok(Nil)
        False ->
          Error(
            "Expected array with at most "
            <> int.to_string(n)
            <> " items but got "
            <> int.to_string(actual_len),
          )
      }
    }
    Error(_) -> Error("Expected array but got " <> json.to_string(value))
  }
}

fn check_one_of(value: json.Json, options: List(String)) -> Result(Nil, String) {
  let actual = json_to_raw_string(value)
  case list.contains(options, actual) {
    True -> Ok(Nil)
    False ->
      Error(
        "Expected one of ["
        <> string.join(options, ", ")
        <> "] but got '"
        <> actual
        <> "'",
      )
  }
}

fn json_to_raw_string(value: json.Json) -> String {
  let encoded = json.to_string(value)
  case string.starts_with(encoded, "\"") && string.ends_with(encoded, "\"") {
    True ->
      encoded
      |> string.drop_left(1)
      |> string.drop_right(1)
    False -> encoded
  }
}

fn validate_uri(s: String) -> Result(Nil, String) {
  case string.starts_with(s, "http://") || string.starts_with(s, "https://") {
    True -> Ok(Nil)
    False -> Error("Invalid URI format: " <> s)
  }
}

fn validate_iso8601(s: String) -> Result(Nil, String) {
  case
    regexp.compile(
      "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|[+-]\\d{2}:\\d{2})?$",
      regexp.Options(False, False),
    )
  {
    Ok(iso8601_regex) ->
      case regexp.check(iso8601_regex, s) {
        True -> Ok(Nil)
        False -> Error("Invalid ISO8601 format: " <> s)
      }
    Error(_) -> Error("Invalid ISO8601 pattern")
  }
}

pub fn validate_field(
  body: json.Json,
  field: String,
  validators: List(Validator),
) -> Result(json.Json, String) {
  case get_field_value(body, field) {
    None -> Error("Field '" <> field <> "' not found in body")
    Some(value) ->
      case run_validators(value, validators) {
        Ok(Nil) -> Ok(value)
        Error(msg) -> Error(msg)
      }
  }
}

fn get_field_value(body: json.Json, field: String) -> Option(json.Json) {
  navigate_json_path(body, string.split(field, "."))
}

fn navigate_json_path(value: json.Json, path: List(String)) -> Option(json.Json) {
  case path {
    [] -> Some(value)
    [key, ..rest] -> {
      let json_str = json.to_string(value)
      case
        json.decode(json_str, dynamic.dict(dynamic.string, dynamic.dynamic))
      {
        Ok(obj) ->
          case dict.get(obj, key) {
            Ok(next) -> {
              let next_json = parser.dynamic_to_json(next)
              navigate_json_path(next_json, rest)
            }
            Error(_) -> None
          }
        Error(_) -> None
      }
    }
  }
}

fn run_validators(
  value: json.Json,
  validators: List(Validator),
) -> Result(Nil, String) {
  case validators {
    [] -> Ok(Nil)
    [validator, ..rest] ->
      case validator(value) {
        Ok(Nil) -> run_validators(value, rest)
        Error(msg) -> Error(msg)
      }
  }
}
