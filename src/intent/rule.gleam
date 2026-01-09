/// Rule expression parser and types
/// Parses human-friendly rule strings like "equals foo" or "integer >= 5"

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

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
                            True -> "string matching " <> string.drop_left(inner, 8)
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
