/// Stream processor for JSONL input/output
/// Processes JSONL lines from stdin and outputs JSONL responses to stdout
import gleam/list
import gleam/string
import intent/command_router

// =============================================================================
// Public API
// =============================================================================

/// Process a stream of JSONL input and return JSONL output
/// Each non-empty line is processed and a response line is generated
pub fn process_stream(input: String) -> String {
  input
  |> string.split("\n")
  |> list.map(process_line)
  |> list.filter(fn(line) { line != "" })
  |> string.join("\n")
}

/// Process a single JSONL line
/// Empty or whitespace-only lines return empty string
pub fn process_line(line: String) -> String {
  case string.trim(line) {
    "" -> ""
    trimmed_line -> command_router.process_single_request(trimmed_line)
  }
}
