/// AI Context Aggregation
///
/// Combines multiple JSONL responses into a single aggregated context
/// with token limit control for AI consumption.
import gleam/json
import gleam/list
import gleam/option.{type Option, Some, None}
import gleam/string

// =============================================================================
// Types
// =============================================================================

/// Response data structure
pub type ResponseData {
  ResponseData(fields: List(#(String, String)))
}

/// A single response from JSONL
pub type Response {
  Response(id: String, success: Bool, command: String, data: ResponseData)
}

/// Aggregated result combining multiple responses
pub type AggregatedResult {
  AggregatedResult(
    response_count: Int,
    combined_data: List(#(String, ResponseData)),
    truncated: Bool,
    token_count: Int,
  )
}

/// Error types for aggregation
pub type AggregateError {
  TokenLimitExceeded
  NoValidResponses
}

// =============================================================================
// Public API
// =============================================================================

/// Aggregate multiple responses with optional token limit
pub fn aggregate_responses(
  responses: List(Response),
  max_tokens: Option(Int),
) -> Result(AggregatedResult, AggregateError) {
  // Filter to successful responses only
  let successful =
    responses
    |> list.filter(fn(r) { r.success })

  case successful {
    [] -> Ok(AggregatedResult(
      response_count: 0,
      combined_data: [],
      truncated: False,
      token_count: 0,
    ))
    _ -> {
      // Combine data from all successful responses
      let combined =
        successful
        |> list.map(fn(r) { #(r.command, r.data) })

      // Estimate token count (rough approximation: 1 token per 4 characters)
      let content_str =
        combined
        |> list.fold("", fn(acc, item) {
          let #(cmd, data) = item
          acc <> cmd <> format_response_data(data)
        })
      let estimated_tokens = string.length(content_str) / 4

      // Check if we exceeded token limit
      let truncated = case max_tokens {
        Some(limit) -> estimated_tokens > limit
        None -> False
      }

      // If truncated, limit the combined data
      let final_data = case truncated {
        True -> {
          case max_tokens {
            Some(limit) -> take_up_to_tokens(combined, limit)
            None -> combined
          }
        }
        False -> combined
      }

      Ok(AggregatedResult(
        response_count: list.length(successful),
        combined_data: final_data,
        truncated: truncated,
        token_count: estimated_tokens,
      ))
    }
  }
}

/// Convert aggregated result to JSON string
pub fn to_json(result: AggregatedResult) -> String {
  json.object([
    #("response_count", json.int(result.response_count)),
    #(
      "combined_data",
      json.object(
        result.combined_data
        |> list.map(fn(item) {
          let #(cmd, data) = item
          #(cmd, response_data_to_json(data))
        }),
      ),
    ),
    #("truncated", json.bool(result.truncated)),
    #("token_count", json.int(result.token_count)),
  ])
  |> json.to_string
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Format ResponseData as string for token estimation
fn format_response_data(data: ResponseData) -> String {
  data.fields
  |> list.fold("", fn(acc, field) {
    let #(key, val) = field
    acc <> key <> val
  })
}

/// Convert ResponseData to JSON
fn response_data_to_json(data: ResponseData) -> json.Json {
  json.object(
    data.fields
    |> list.map(fn(field) {
      let #(key, val) = field
      #(key, json.string(val))
    }),
  )
}

/// Take items from list until token limit reached
fn take_up_to_tokens(
  items: List(#(String, ResponseData)),
  max_tokens: Int,
) -> List(#(String, ResponseData)) {
  let #(taken, _remaining_tokens) =
    items
    |> list.fold(#([], max_tokens), fn(acc, item) {
      let #(collected, tokens_left) = acc
      let #(cmd, data) = item
      let item_size = { string.length(cmd) + string.length(format_response_data(data)) } / 4

      case tokens_left >= item_size {
        True -> #(list.append(collected, [item]), tokens_left - item_size)
        False -> acc
      }
    })

  taken
}
