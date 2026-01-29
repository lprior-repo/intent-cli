import gleam/dynamic
import gleam/list
import gleam/option
import gleam/result
import gleam/string

/// Parse a string into an enum variant using case-insensitive matching
/// Returns Ok(variant) if found, Error(original_string) if not
pub fn parse_enum(s: String, variants: List(#(String, t))) -> Result(t, String) {
  let lower_s = string.lowercase(s)

  variants
  |> list.find(fn(pair) {
    let #(variant_str, _) = pair
    string.lowercase(variant_str) == lower_s
  })
  |> result.map(fn(pair) { pair.1 })
  |> result.map_error(fn(_) { s })
}

/// Parse a string into an enum variant with default value
/// Returns the default variant if string doesn't match any variant
pub fn parse_enum_with_default(
  s: String,
  variants: List(#(String, t)),
  default: t,
) -> t {
  case parse_enum(s, variants) {
    Ok(variant) -> variant
    Error(_) -> default
  }
}

/// Parse an enum field from dynamic data with custom error message
pub fn parse_dynamic_enum(
  data: dynamic.Dynamic,
  enum_name: String,
  variants: List(#(String, t)),
) -> Result(t, List(dynamic.DecodeError)) {
  case dynamic.string(data) {
    Ok(s) -> {
      case parse_enum(s, variants) {
        Ok(variant) -> Ok(variant)
        Error(_) ->
          Error([
            dynamic.DecodeError(
              expected: enum_name
                <> " ("
                <> string.join(list.map(variants, fn(p) { p.0 }), "|")
                <> ")",
              found: s,
              path: [],
            ),
          ])
      }
    }
    Error(e) -> Error(e)
  }
}

/// Parse an optional enum field from dynamic data
pub fn parse_optional_dynamic_enum(
  data: dynamic.Dynamic,
  enum_name: String,
  variants: List(#(String, t)),
) -> Result(option.Option(t), List(dynamic.DecodeError)) {
  dynamic.optional_field(enum_name, fn(d) {
    parse_dynamic_enum(d, enum_name, variants)
  })(data)
}
