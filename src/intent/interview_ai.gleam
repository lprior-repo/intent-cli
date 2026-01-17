/// AI-Friendly Interview Helpers
/// Simplified wrappers around existing interview functionality
import gleam/json

// Removed unused import
import intent/interview.{type Profile}

/// Simplified response type for AI
pub type AiResponse {
  Success(message: String)
}

/// Encode basic response to JSON
pub fn encode_response(response: AiResponse) -> json.Json {
  case response {
    Success(msg) ->
      json.object([
        #("action", json.string("success")),
        #("message", json.string(msg)),
      ])
  }
}

/// Resume latest session for a profile
pub fn resume_latest(_profile: Profile) -> Result(AiResponse, String) {
  // For now, this is a placeholder that would integrate with existing interview system
  Ok(Success(
    "AI interview mode: Use 'intent interview --cue --profile=<profile>' for now",
  ))
}
