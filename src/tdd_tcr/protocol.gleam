/// TDD-TCR Silent Protocol: Type-safe contracts between actors
/// Prevents context amnesia and enforces AI discipline through types

import gleam/option.{type Option}

// ============================================================================
// EVIDENCE PACKET: Silent Protocol (Map ↔ Territory)
// ============================================================================

/// The ONE source of truth passed to Blue Team (Implementer)
/// Contains everything needed to implement a fix, no more, no less
pub type EvidencePacket {
  EvidencePacket(
    error_log: String,
    // ^ Compiler error OR test output from Judge
    // ^ The failure we must fix (The Map)
    test_context: String,
    // ^ The full content of the failing test
    // ^ What Blue Team must make pass (The Territory)
    src_context: String,
    // ^ Current source code of target file
    // ^ What Blue Team will modify
    constitution: String,
    // ^ ARCHITECTURE.md rules and constraints
    // ^ What Blue Team must respect (no unwrap, etc)
  )
}

// ============================================================================
// STATE MACHINE: Compile-time phase enforcement
// ============================================================================

/// TDD cycle state: Red → Blue → Green → Refactor → Red
pub type TddState {
  RedPhase(iteration: Int, requirements: String)
  // ^ Auditor is writing a failing test
  BluePhase(iteration: Int, test_output: String, attempt: Int)
  // ^ Implementer is fixing the test (attempt 1-3)
  RefactorPhase(iteration: Int, successful_impls: Int)
  // ^ Architect is improving code quality
  JudgePhase(iteration: Int)
  // ^ Judge is running hard gates
}

/// Valid phase transitions (enforced by type system)
pub type Transition {
  RedToBlue(test_output: String)
  // ^ Test written and passes mudroom check
  BlueToCommit(changes: String)
  // ^ TCR success: compile + test both pass
  BlueToRevert(reason: String)
  // ^ TCR failure: revert implementation, try again
  BlueToRed(reason: String)
  // ^ Implementation failed after max attempts, auditor writes new test
  GreenToRefactor(iteration: Int)
  // ^ After N successful implementations, refactor
}

// ============================================================================
// ACTOR ROLES: Four independent workers
// ============================================================================

pub type ActorRole {
  Auditor
  Implementer
  Judge
  Architect
}

pub fn role_to_string(role: ActorRole) -> String {
  case role {
    Auditor -> "Auditor"
    Implementer -> "Implementer"
    Judge -> "Judge"
    Architect -> "Architect"
  }
}

// ============================================================================
// INTER-ACTOR MESSAGES: Type-safe communication
// ============================================================================

/// Messages passed between actors via OTP
pub type Message {
  // Input: Supervisor initiates the loop
  RequirementsInput(requirements: String, iteration: Int)

  // Red Team → Supervisor: Test written and validated
  TestWritten(test_code: String, auditor_reasoning: String, iteration: Int)

  // Red Team → Supervisor: Auditor encountered error
  AuditorFailure(reason: String, iteration: Int)

  // Supervisor → Blue Team: Here's what to implement
  ImplementationRequest(packet: EvidencePacket, attempt: Int, iteration: Int)

  // Blue Team → Judge: Please evaluate my implementation
  ImplementationReady(changes: String, iteration: Int, attempt: Int)

  // Judge → Supervisor: TCR result
  TCRComplete(success: Bool, reason: String, metrics: Metrics, iteration: Int)

  // Supervisor → Architect: Time to refactor
  RefactorRequest(iteration: Int, successful_impls: Int)

  // Architect → Supervisor: Refactoring complete
  RefactorComplete(reason: String, metrics: Metrics)

  // Generic phase completion
  PhaseComplete(phase: String, reason: String)

  // Generic phase failure
  PhaseFailure(phase: String, reason: String, iteration: Int)

  // Metrics update for telemetry
  MetricsUpdate(metrics: Metrics)

  // Shutdown signal
  Shutdown
}

// ============================================================================
// METRICS: Telemetry tracking
// ============================================================================

pub type Metrics {
  Metrics(
    tcr_commits: Int,
    // ^ Successful implementations (tests passed)
    tcr_reverts: Int,
    // ^ Failed implementations (had to revert)
    refactor_count: Int,
    // ^ Number of refactoring passes
    consecutive_failures: Int,
    // ^ Escalation trigger (3+ failures → use Opus)
    successful_impls: Int,
    // ^ Count before refactor trigger
    iteration: Int,
    // ^ Current loop iteration
  )
}

pub fn metrics_to_string(m: Metrics) -> String {
  "Commits: "
  <> m.tcr_commits |> int.to_string
  <> " | Reverts: "
  <> m.tcr_reverts |> int.to_string
  <> " | Refactors: "
  <> m.refactor_count |> int.to_string
  <> " | Iter: "
  <> m.iteration |> int.to_string
}

pub fn initial_metrics(iteration: Int) -> Metrics {
  Metrics(
    tcr_commits: 0,
    tcr_reverts: 0,
    refactor_count: 0,
    consecutive_failures: 0,
    successful_impls: 0,
    iteration: iteration,
  )
}

// ============================================================================
// LOOP RESULTS: Final outcome of TDD cycle
// ============================================================================

pub type LoopResult {
  Complete(
    reason: String,
    // ^ Why we're done (requirements met, etc)
    metrics: Metrics,
    // ^ Final statistics
  )
  PartialComplete(
    reason: String,
    // ^ Why we stopped early
    commits: Int,
    // ^ How many things we actually implemented
  )
  Failed(reason: String, error: String)
  // ^ Unrecoverable failure
}

// ============================================================================
// VIOLATIONS: Constitution check results
// ============================================================================

pub type Violation {
  Violation(line_num: Int, pattern: String, suggestion: String)
}

// ============================================================================
// IMPORTS FOR OTHER MODULES
// ============================================================================

// Re-export commonly needed types
pub type OptionString =
  Option(String)

// These are imported locally but available for convenience
// (Actual gleam/option and gleam/int must be imported where used)
