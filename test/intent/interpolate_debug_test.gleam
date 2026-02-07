import gleam/json
import gleam/option
import gleam/string
import gleeunit/should
import intent/interpolate

pub fn debug_array_issue_test() {
  let ctx = interpolate.new_context()
  let items = json.array([json.int(1), json.int(2), json.int(3)], fn(x) { x })
  let ctx = interpolate.set_variable(ctx, "items", items)

  // Test what happens when we try to access the variable directly
  let result1 = interpolate.get_variable(ctx, "items")
  case result1 {
    option.Some(_) -> {
      // We can get the array
      let result2 = interpolate.interpolate_string(ctx, "Items: ${items}")
      case result2 {
        Ok(str) -> {
          // This should work - the whole array as JSON
          string.contains(str, "[") |> should.be_true()
        }
        Error(_) -> should.fail()
      }
    }
    option.None -> should.fail()
  }
}

pub fn debug_array_index_test() {
  let ctx = interpolate.new_context()
  let items = json.array([json.int(1), json.int(2), json.int(3)], fn(x) { x })
  let ctx = interpolate.set_variable(ctx, "items", items)

  // The issue is that "items[0]" is not being parsed as an array index
  // Let's test the path resolution directly
  let result = interpolate.interpolate_string(ctx, "First: ${items[0]}")

  case result {
    Error(msg) -> {
      // This currently fails with "Variable not found: items[0]"
      // But it should work by parsing "items[0]" as a path with array indexing
      // Test that we can access items[0] through extract_capture if we fix the code
      // For now, this test shows the current broken behavior
      msg |> should.equal("Variable not found: items[0]")
    }
    Ok(_) -> should.fail()
  }
}