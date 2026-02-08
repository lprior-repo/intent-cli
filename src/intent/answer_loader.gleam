// Answer loader module - loads pre-filled answers from files
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/int
import gleam/json
import gleam/list
import gleam/result
import gleam/string
// import shellout
// import simplifile

/// Decode error with structured context for type conversion failures
pub type DecodeError {
  DecodeError(
    path: String,
    expected: String,
    actual: String,
    message: String,
  )
}

pub type AnswerLoaderError {
  FileNotFound(path: String)
  PermissionDenied(path: String)
  ParseErrorWithDetails(path: String, decode_error: DecodeError)
  SchemaError(message: String)
  IoError(message: String)
  // Legacy ParseError kept for backward compatibility
  ParseError(path: String, message: String)
}

/// Load answers from a file (JSON format)
pub fn load_from_file(
  path: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  // TODO: Re-enable when simplifile is added back to dependencies
  // case simplifile.read(path) {
  //   Error(_) -> Error(FileNotFound(path))
  //   Ok(contents) -> parse_answers(path, contents)
  // }
  Error(FileNotFound(path))
}

// UNUSED: Kept for potential future use when simplifile is re-enabled
// fn parse_answers(
//   path: String,
//   contents: String,
// ) -> Result(Dict(String, String), AnswerLoaderError) {
//   // TODO: Re-enable CUE export handling when shellout is added back
//   // case path_is_cue(path) {
//   //   True -> {
//   //     case shellout.command("cue", ["export", path, "-e", "answers"], ".", []) {
//   //       Ok(json_str) -> parse_answers_json(path, json_str)
//   //       Error(#(_, stderr)) -> {
//   //         case parse_answers_json(path, contents) {
//   //           Ok(parsed) -> Ok(parsed)
//   //           Error(_) -> Error(ParseError(path, stderr))
//   //         }
//   //       }
//   //     }
//   //   }
//   //   False -> parse_answers_json(path, contents)
//   // }
//   parse_answers_json(path, contents)
// }

fn parse_answers_json(
  path: String,
  json_str: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  case json.decode(json_str, dynamic.dynamic) {
    Error(_) -> {
      // Capture decode error details
      Error(ParseErrorWithDetails(path, DecodeError(
        path: "<root>",
        expected: "JSON",
        actual: "invalid",
        message: "Failed to decode answers JSON",
      )))
    }
    Ok(data) -> {
      case dynamic.dict(dynamic.string, dynamic.dynamic)(data) {
        Error(_) -> {
          // Capture type mismatch details
          Error(ParseErrorWithDetails(path, DecodeError(
            path: "<root>",
            expected: "Object",
            actual: dynamic.classify(data),
            message: "Top-level value must be an object/map"
          )))
        }
        Ok(entries) -> Ok(flatten_answers(entries))
      }
    }
  }
}

fn flatten_answers(
  entries: Dict(String, dynamic.Dynamic),
) -> Dict(String, String) {
  entries
  |> dict.to_list
  |> list.fold(dict.new(), fn(acc, entry) {
    let #(key, value) = entry
    flatten_dynamic(key, value, acc)
  })
}

fn flatten_dynamic(
  key_path: String,
  value: dynamic.Dynamic,
  acc: Dict(String, String),
) -> Dict(String, String) {
  case dynamic.dict(dynamic.string, dynamic.dynamic)(value) {
    Ok(nested) -> {
      // First, add the parent object as a JSON string
      let with_parent = case dynamic_to_json(value) {
        Ok(json_val) -> dict.insert(acc, key_path, json.to_string(json_val))
        Error(_) -> acc
      }

      // Then recursively flatten nested entries
      nested
      |> dict.to_list
      |> list.fold(with_parent, fn(inner_acc, entry) {
        let #(nested_key, nested_value) = entry
        flatten_dynamic(key_path <> "." <> nested_key, nested_value, inner_acc)
      })
    }
    Error(_) -> {
      case dynamic_value_to_string(value) {
        Ok(value_as_text) ->
          insert_answer_key_variants(acc, key_path, value_as_text)
        Error(err) -> {
          let fallback = "<" <> err.expected <> " decode error>"
          insert_answer_key_variants(acc, key_path, fallback)
        }
      }
    }
  }
}

fn insert_answer_key_variants(
  acc: Dict(String, String),
  key_path: String,
  value: String,
) -> Dict(String, String) {
  let with_path = dict.insert(acc, key_path, value)

  // Only add short key variant for non-nested paths (no dots)
  case string.contains(key_path, ".") {
    True -> with_path  // Don't add short key for nested paths
    False -> {
      case last_key_segment(key_path) {
        Ok("") -> with_path
        Ok(short_key) -> {
          case dict.get(with_path, short_key) {
            Ok(_) -> with_path
            Error(_) -> dict.insert(with_path, short_key, value)
          }
        }
        Error(_) -> with_path
      }
    }
  }
}

fn last_key_segment(key_path: String) -> Result(String, Nil) {
  key_path
  |> string.split(".")
  |> list.reverse
  |> list.first
}

fn dynamic_value_to_string(value: dynamic.Dynamic) -> Result(String, DecodeError) {
  case dynamic.classify(value) {
    "String" | "BitArray" ->
      dynamic.string(value)
      |> result.map_error(fn(_) {
        DecodeError(
          path: "<value>",
          expected: "String",
          actual: dynamic.classify(value),
          message: "Value classified as String/BitArray but failed to decode",
        )
      })

    "Int" ->
      dynamic.int(value)
      |> result.map(int.to_string)
      |> result.map_error(fn(_) {
        DecodeError(
          path: "<value>",
          expected: "Int",
          actual: dynamic.classify(value),
          message: "Value classified as Int but failed to decode",
        )
      })

    "Bool" ->
      dynamic.bool(value)
      |> result.map(bool.to_string)
      |> result.map_error(fn(_) {
        DecodeError(
          path: "<value>",
          expected: "Bool",
          actual: dynamic.classify(value),
          message: "Value classified as Bool but failed to decode",
        )
      })

    "Float" | "List" | "Tuple" | "Dict" | "Map" | "Nil" | _ ->
      case dynamic_to_json(value) {
        Ok(json_val) -> Ok(json.to_string(json_val))
        Error(err) -> Error(err)
      }
  }
}

fn dynamic_to_json(value: dynamic.Dynamic) -> Result(json.Json, DecodeError) {
  case dynamic.classify(value) {
    "Nil" -> Ok(json.null())

    "Bool" ->
      dynamic.bool(value)
      |> result.map(json.bool)
      |> result.map_error(fn(_) {
        DecodeError(
          path: "<value>",
          expected: "Bool",
          actual: dynamic.classify(value),
          message: "Failed to decode as Bool",
        )
      })

    "Int" ->
      dynamic.int(value)
      |> result.map(json.int)
      |> result.map_error(fn(_) {
        DecodeError(
          path: "<value>",
          expected: "Int",
          actual: dynamic.classify(value),
          message: "Failed to decode as Int",
        )
      })

    "Float" ->
      dynamic.float(value)
      |> result.map(json.float)
      |> result.map_error(fn(_) {
        DecodeError(
          path: "<value>",
          expected: "Float",
          actual: dynamic.classify(value),
          message: "Failed to decode as Float",
        )
      })

    "String" | "BitArray" ->
      dynamic.string(value)
      |> result.map(json.string)
      |> result.map_error(fn(_) {
        DecodeError(
          path: "<value>",
          expected: "String",
          actual: dynamic.classify(value),
          message: "Failed to decode as String",
        )
      })

    "List" | "Tuple" -> {
      case dynamic.list(dynamic.dynamic)(value) {
        Ok(items) ->
          Ok(json.array(items, fn(item) {
            case dynamic_to_json(item) {
              Ok(json_val) -> json_val
              Error(_) -> json.null()
            }
          }))
        Error(_) -> {
          Error(DecodeError(
            path: "<value>",
            expected: "List",
            actual: dynamic.classify(value),
            message: "Failed to decode as List",
          ))
        }
      }
    }

    "Dict" | "Map" -> {
      case dynamic.dict(dynamic.string, dynamic.dynamic)(value) {
        Ok(entries) -> {
          entries
          |> dict.to_list
          |> list.map(fn(entry) {
            let #(key, item) = entry
            case dynamic_to_json(item) {
              Ok(json_val) -> #(key, json_val)
              Error(_) -> #(key, json.null())
            }
          })
          |> json.object
          |> Ok
        }
        Error(_) -> {
          Error(DecodeError(
            path: "<value>",
            expected: "Dict",
            actual: dynamic.classify(value),
            message: "Failed to decode as Dict",
          ))
        }
      }
    }

    _ -> {
      Error(DecodeError(
        path: "<value>",
        expected: "known type",
        actual: dynamic.classify(value),
        message: "Unknown dynamic type classification",
      ))
    }
  }
}

// UNUSED: Kept for potential future use when CUE export is re-enabled
// fn path_is_cue(path: String) -> Bool {
//   string.ends_with(path, ".cue")
// }

/// Format decode error for display
pub fn format_decode_error_for_test(err: DecodeError) -> String {
  "At '" <> err.path <> "':\n"
  <> "  Expected: " <> err.expected <> "\n"
  <> "  Actual: " <> err.actual <> "\n"
  <> "  Details: " <> err.message
}

/// Test helper: expose parse_answers_json for testing
pub fn parse_answers_json_for_test(
  path: String,
  json_str: String,
) -> Result(Dict(String, String), AnswerLoaderError) {
  parse_answers_json(path, json_str)
}

/// Get debug representation of dynamic value for error messages
// UNUSED: Kept for debugging purposes
// fn dynamic_debug(value: dynamic.Dynamic) -> String {
//   case dynamic.string(value) {
//     Ok(s) -> "\"" <> s <> "\""
//     Error(_) -> {
//       case dynamic.int(value) {
//         Ok(i) -> int.to_string(i)
//         Error(_) -> "<" <> dynamic.classify(value) <> ">"
//       }
//     }
//   }
// }
