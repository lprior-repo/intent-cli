/// AI Schema Introspection
/// Provides access to CUE schemas for AI-native interface
import gleam/list
import gleam/result
import gleam/string
import simplifile

/// Schema error types
pub type SchemaError {
  SchemaDirectoryNotFound
  SchemaNotFound(String)
  InvalidSchemaType(String)
  FileReadError(String)
}

/// Schema information
pub type SchemaInfo {
  SchemaInfo(command: String, schema_type: String, content: String)
}

/// Get schema for a specific command and type
pub fn get_schema(
  command command: String,
  schema_type schema_type: String,
) -> Result(String, SchemaError) {
  // Validate schema type
  case schema_type {
    "input" | "output" -> {
      // Build schema path
      let schema_path = "schema/ai/" <> schema_type <> "/" <> command <> ".cue"

      // Read schema file
      case simplifile.read(schema_path) {
        Ok(content) -> Ok(content)
        Error(simplifile.Enoent) -> Error(SchemaNotFound(command))
        Error(_) -> Error(FileReadError(schema_path))
      }
    }
    _ -> Error(InvalidSchemaType(schema_type))
  }
}

/// Get all schemas
pub fn get_all_schemas() -> Result(List(SchemaInfo), SchemaError) {
  // Read input and output directories
  let input_schemas = read_schemas_from_directory("schema/ai/input", "input")
  let output_schemas = read_schemas_from_directory("schema/ai/output", "output")

  // Combine results
  case input_schemas, output_schemas {
    Ok(inputs), Ok(outputs) -> Ok(list.append(inputs, outputs))
    Ok(inputs), Error(_) -> Ok(inputs)
    Error(_), Ok(outputs) -> Ok(outputs)
    Error(_), Error(_) -> Error(SchemaDirectoryNotFound)
  }
}

/// List all available command names
pub fn list_commands() -> Result(List(String), SchemaError) {
  case get_all_schemas() {
    Ok(schemas) -> {
      let commands =
        schemas
        |> list.map(fn(schema) { schema.command })
        |> list.unique
      Ok(commands)
    }
    Error(e) -> Error(e)
  }
}

/// Read schemas from a directory
fn read_schemas_from_directory(
  directory: String,
  schema_type: String,
) -> Result(List(SchemaInfo), SchemaError) {
  case simplifile.read_directory(directory) {
    Ok(files) -> {
      files
      |> list.filter(fn(file) { string.ends_with(file, ".cue") })
      |> list.map(fn(file) {
        let command = string.replace(file, ".cue", "")
        let path = directory <> "/" <> file
        case simplifile.read(path) {
          Ok(content) -> Ok(SchemaInfo(command, schema_type, content))
          Error(_) -> Error(FileReadError(path))
        }
      })
      |> result.all
    }
    Error(_) -> Ok([])
  }
}
