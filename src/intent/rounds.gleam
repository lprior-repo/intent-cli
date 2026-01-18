//// 5-Round Mental Model System
////
//// This module defines the mental model rounds used in the planning phase.
//// Each round represents a different lens for analyzing and building specs.
////
//// Rounds:
//// - Round 1: EARS (100% RCS required)
//// - Round 2: Contracts (100% RCS required)
//// - Round 3: Inversion (100% RCS required)
//// - Round 4: Effects (100% RCS required)
//// - Round 5: Pre-mortem (80% RCS required)

import gleam/list

/// Mental model type for each round
pub type MentalModel {
  EARS
  Contracts
  Inversion
  Effects
  PreMortem
}

/// A single round in the 5-round system
pub type Round {
  Round(
    number: Int,
    model: MentalModel,
    output: String,
    gate: String,
    target_rcs: Int,
  )
}

/// Returns all 5 rounds in order
pub fn all_rounds() -> List(Round) {
  [
    Round(
      number: 1,
      model: EARS,
      output: "spec skeleton + ubiquitous/event/state/unwanted patterns",
      gate: "RCS₁=100%",
      target_rcs: 100,
    ),
    Round(
      number: 2,
      model: Contracts,
      output: "response.checks with rule+why",
      gate: "RCS₂=100%",
      target_rcs: 100,
    ),
    Round(
      number: 3,
      model: Inversion,
      output: "anti_patterns + error behaviors",
      gate: "RCS₃=100%",
      target_rcs: 100,
    ),
    Round(
      number: 4,
      model: Effects,
      output: "requires[] + verification behaviors",
      gate: "RCS₄=100%",
      target_rcs: 100,
    ),
    Round(
      number: 5,
      model: PreMortem,
      output: "ai_hints.pitfalls",
      gate: "RCS₅≥80%",
      target_rcs: 80,
    ),
  ]
}

/// Get a specific round by number (1-5)
pub fn get_round(number: Int) -> Result(Round, Nil) {
  all_rounds()
  |> list.find(fn(round) { round.number == number })
}

/// Get the mental model for a specific round number
pub fn get_model(number: Int) -> Result(MentalModel, Nil) {
  case get_round(number) {
    Ok(round) -> Ok(round.model)
    Error(Nil) -> Error(Nil)
  }
}

/// Check if a round number is valid (1-5)
pub fn is_valid_round_number(number: Int) -> Bool {
  number >= 1 && number <= 5
}

/// Get a human-readable name for a round
pub fn round_name(round: Round) -> String {
  "Round " <> int_to_string(round.number) <> ": " <> model_to_string(round.model)
}

/// Convert a mental model to a string
pub fn model_to_string(model: MentalModel) -> String {
  case model {
    EARS -> "EARS"
    Contracts -> "Contracts"
    Inversion -> "Inversion"
    Effects -> "Effects"
    PreMortem -> "Pre-mortem"
  }
}

// Helper function for int to string conversion
fn int_to_string(n: Int) -> String {
  case n {
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    _ -> "unknown"
  }
}
