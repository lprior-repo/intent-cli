# TDD-TCR Bash → Gleam OTP Port: Implementation Strategy

**Epic ID**: `intent-cli-rbo`
**Status**: Starting Phase 1
**Tracked in Beads**: 13 phases, bv-optimized execution order

## Overview

Porting the 2248-line Bash TDD-TCR-REFACTOR script to Gleam with OTP actors. The system enforces AI discipline through **four independent actors**:

- **Red Team (Auditor)**: Writes failing tests
- **Blue Team (Implementer)**: Implements code
- **Judge**: TCR enforcement (compile + test + commit/revert)
- **Architect**: Refactoring every N iterations

**Key Innovation**: Silent Protocol via **EvidencePacket**—typed contracts between actors prevent "context amnesia."

---

## Existing Gleam Code We're Using

✅ **shellout** (already a dependency) - for running `gleam build`, `gleam test`, `git`
✅ **simplifile** - for file I/O
✅ **gleam_erlang** - for FFI (timing, UUID, base64)
✅ **intent_ffi.erl** - FFI module with `now_ms()`, `halt()`, `generate_uuid()`, `current_timestamp()`
✅ **existing intent modules** - can reuse patterns from runner.gleam, loader.gleam

**NOT building from scratch** — leveraging existing ecosystem and code patterns.

---

## Phase 1: Core Types & Dependencies

**Beads**: `intent-cli-rbo` (epic)

### 1.1 Add gleam_otp

**File**: `gleam.toml`

```toml
[dependencies]
gleam_otp = ">= 0.1.0"  # Add this
# Keep existing: gleam_stdlib, shellout, simplifile, gleam_erlang
```

**Why**: OTP provides Process, Subject, Selector for actor model, supervisor management.

### 1.2 Implement protocol.gleam

**File**: `src/tdd_tcr/protocol.gleam`

This is the **Silent Protocol** — type-safe contracts between actors.

```gleam
// Evidence Packet: What Blue Team needs to know
pub type EvidencePacket {
  EvidencePacket(
    error_log: String,        // Compiler/test output from Judge
    test_context: String,     // Failing test code (the Map)
    src_context: String,      // Current source code (the Territory)
    constitution: String,     // ARCHITECTURE.md rules
  )
}

// State machine: Strict phase enforcement
pub type TddState {
  RedPhase(iteration: Int, requirements: String)
  BluePhase(iteration: Int, test_output: String, attempt: Int)
  RefactorPhase(iteration: Int, successful_impls: Int)
  JudgePhase(iteration: Int)
}

// Valid transitions
pub type Transition {
  RedToBlue(test_output: String)      // Test written, ready for implementation
  BlueToCommit(changes: String)       // TCR success, commit
  BlueToRevert(reason: String)        // TCR failure, revert
  BlueToRed(reason: String)          // Test failed, try again
  GreenToRefactor(iteration: Int)     // After N successes, refactor
}

// Actor roles
pub type ActorRole {
  Auditor
  Implementer
  Judge
  Architect
}

// Inter-actor messages
pub type Message {
  RequirementsInput(String)
  TestWritten(test_code: String, auditor_reasoning: String)
  ImplementationRequest(EvidencePacket)
  TCRResult(success: Bool, reason: String)
  RefactorRequest(iteration: Int, successful_impls: Int)
  MetricsUpdate(Metrics)
  PhaseComplete(String)
  PhaseFailure(reason: String)
}

// Metrics for telemetry
pub type Metrics {
  Metrics(
    tcr_commits: Int,
    tcr_reverts: Int,
    refactor_count: Int,
    consecutive_failures: Int,
    successful_impls: Int,
    iteration: Int,
  )
}

// Loop result wrapper
pub type LoopResult {
  Complete(reason: String, metrics: Metrics)
  PartialComplete(reason: String, commits: Int)
  Failed(reason: String, error: String)
}
```

### 1.3 Implement config.gleam

**File**: `src/tdd_tcr/config.gleam`

```gleam
pub type Config {
  Config(
    project_lang: String,             // "gleam"
    test_cmd: String,                 // "gleam test"
    build_cmd: String,                // "gleam build"
    requirements: String,             // Feature to implement
    bead_id: option.Option(String),   // Optional linked bead
    max_iterations: Int,              // Default: 20
    refactor_interval: Int,           // Default: 5 (refactor after N successes)
    max_impl_attempts: Int,          // Default: 3
    escalation_threshold: Int,        // Default: 3 (failures before Opus)
    model_default: String,            // "sonnet"
    model_escalated: String,          // "opus"
  )
}

pub fn default_config() -> Config

pub fn from_args(requirements: String) -> Config
```

---

## Phase 2: Git & Lock Management

**Beads**: Child tasks of `intent-cli-rbo`

### 2.1 Implement git.gleam

**File**: `src/tdd_tcr/git.gleam`

Uses shellout to run git commands. Keeps state via git stash for TCR.

```gleam
pub fn git_command(args: List(String)) -> Result(String, String)

pub fn commit(message: String, allow_empty: Bool) -> Result(Nil, String)

pub fn stash() -> Result(String, String)  // Returns stash ID

pub fn reset_hard_to_stash(stash_id: String) -> Result(Nil, String)

pub fn current_branch() -> Result(String, String)

pub fn status() -> Result(String, String)

pub fn add_all() -> Result(Nil, String)
```

### 2.2 Implement file_lock.gleam

**File**: `src/tdd_tcr/file_lock.gleam`

Sentinel files in `.factory/` prevent concurrent writes.

```gleam
pub fn lock_src() -> Result(Nil, String)

pub fn unlock_src() -> Result(Nil, String)

pub fn lock_tests() -> Result(Nil, String)

pub fn unlock_tests() -> Result(Nil, String)

pub fn unlock_all() -> Result(Nil, String)

pub fn is_locked(lock_file: String) -> Bool
```

### 2.3 Implement shell.gleam

**File**: `src/tdd_tcr/shell.gleam`

Wrapper around shellout for running commands.

```gleam
pub fn run_command(cmd: String, args: List(String)) -> Result(String, String)

pub fn run_command_silent(cmd: String, args: List(String)) -> Result(Nil, String)
```

---

## Phase 3: LLM Module with Circuit Breaker

**File**: `src/tdd_tcr/llm.gleam`

Calls claude CLI with retry logic and exponential backoff.

```gleam
pub type ModelType {
  Sonnet
  Opus
  Haiku
}

pub fn call_llm(
  prompt: String,
  model: ModelType,
  max_retries: Int,
  role: ActorRole,
) -> Result(String, String)
  // Retry logic: 1s, 2s, 4s exponential backoff
  // After max_retries, return Error(CircuitOpen)
```

---

## Phase 4: Telemetry & History

### 4.1 telemetry.gleam

**File**: `src/tdd_tcr/telemetry.gleam`

Append metrics to `swarm_stats.csv`.

### 4.2 history.gleam

**File**: `src/tdd_tcr/history.gleam`

Append session reasoning to `.factory/history.md`.

---

## Phase 5: Constitution Check

**File**: `src/tdd_tcr/constitution.gleam`

Pre-flight validation to prevent testing broken code.

```gleam
pub fn check_src_file(filepath: String) -> Result(List(Violation), String)
  // Scan for: unwrap(, todo(, TODO, FIXME, pass, imperative loops
  // Return violations with line numbers
```

---

## Phase 6: State Machine

**File**: `src/tdd_tcr/state_machine.gleam`

Type-safe phase transitions.

```gleam
pub fn validate_transition(
  current: TddState,
  transition: Transition,
) -> Result(TddState, String)
  // Enforce valid transitions at compile time
```

---

## Phase 7: Four Actors (Parallelizable)

### 7a. Judge Actor

**File**: `src/tdd_tcr/actors/judge.gleam`

**Responsibilities**:
- Run `gleam build` → check for errors + warnings
- Run `gleam test` → check all tests pass
- On success: `git add -A; git commit`
- On failure: `git reset --hard` to stashed state
- Increment TCR_COMMITS / TCR_REVERTS

### 7b. Red Team Actor (Auditor)

**File**: `src/tdd_tcr/actors/red_team.gleam`

**Responsibilities**:
- Build CHALLENGER prompt (CUPID principles, 30-line rule)
- Call LLM (Sonnet, with circuit breaker)
- Extract test code from output
- Mudroom check: validate test file compiles
- Unlock test/, write to test/factory_test.gleam
- Lock test/

### 7c. Blue Team Actor (Implementer)

**File**: `src/tdd_tcr/actors/blue_team.gleam`

**Responsibilities**:
- Receive EvidencePacket (error_log, test_context, src_context, constitution)
- Build DEFENDER prompt (30-line max, HICKEY simplicity, CUPID check)
- Call LLM (Sonnet, escalate to Opus on 3+ failures)
- Extract implementation from output
- Unlock src/, write to src/ files
- Lock src/
- Send to Judge for hard gates

### 7d. Architect Actor

**File**: `src/tdd_tcr/actors/architect.gleam`

**Responsibilities**:
- After N successful implementations
- Build ARBITER prompt (metrics-first, CUPID audit, DRY police)
- Call LLM
- Apply refactoring edits
- Run hard gates on refactored code
- Create tech debt beads for findings

---

## Phase 8: Supervisor & Main Orchestration

### 8.1 supervisor.gleam

**File**: `src/tdd_tcr/supervisor.gleam`

OTP supervisor with one_for_one strategy:
- Manage 4 child actors
- Restart actor on crash
- Main loop: Red → Blue → Judge → Refactor (every N)

### 8.2 tdd_tcr.gleam

**File**: `src/tdd_tcr.gleam`

Main entry point:
- Parse CLI args
- Initialize supervisor
- Run orchestration loop
- Handle completion, metrics, history

---

## Phase 9: Integration & Testing

Unit tests for each module. Integration tests for actor communication. End-to-end test on factory_test.gleam.

---

## Phase 10: Bead Integration

**File**: `src/tdd_tcr/bead.gleam`

Link TDD-TCR loop to bd issue tracking.

---

## Dependency Graph (bv --robot-plan)

```
Phase 1 (Core Types) → [Phase 2, 3, 4, 5, 6]
Phase 2 (Git/Lock)  → [Phase 7a (Judge), Phase 7b, 7c]
Phase 3 (LLM)       → [Phase 7b (Red), 7c (Blue)]
Phase 4 (Telemetry) → [Phase 7b, 7c, 7d (Architect)]
Phase 5 (Constitution) → [Phase 7d]
Phase 6 (State Machine) → [Phase 7a, 7b, 7c]

Phase 7a, 7b, 7c, 7d (Actors) → Phase 8 (Supervisor)
Phase 8 (Supervisor) → Phase 9 (Testing)
Phase 4 (Telemetry) → Phase 10 (Beads)
```

**Parallelizable**: Phases 7a, 7b, 7c, 7d can be built concurrently once 1-6 are done.

---

## Critical Success Factors

✅ **EvidencePacket Protocol**: Typed, prevents context drift
✅ **TCR Enforcement**: Hard gates (compile + test), automatic revert
✅ **Circuit Breaker**: Retry 3x, then fail-fast
✅ **State Machine**: Compile-time phase guarantees
✅ **Existing Gleam Code**: Use shellout, patterns from runner.gleam

---

## Next Steps

1. **Claim Phase 1 bead** via `bd update intent-cli-{phase1} --status in_progress`
2. **Implement protocol.gleam** and config.gleam
3. **Move to Phase 2** once Phase 1 beads close
4. **Parallel Phase 7a-7d** once Phases 1-6 complete
5. **Use bv --robot-plan** to re-optimize after each phase completion

---

## Commands Reference

```bash
# Check current status
bv --robot-triage

# View execution plan
bv --robot-plan

# Claim a bead
bd update <bead-id> --status in_progress

# Close completed bead
bd close <bead-id> --reason "Completed"

# Create new discovered bead
bd create "Found issue" -p 1 --deps discovered-from:<parent-id>

# Run the TDD-TCR loop (once implemented)
gleam run -- tdd-tcr "Feature description"
```
