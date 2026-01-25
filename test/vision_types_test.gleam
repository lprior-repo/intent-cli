//// Comprehensive tests for intent/vision_types.gleam
//// Tests cover Vision document structure types
////
//// Design by Contract:
//// - Preconditions: Valid type construction with all required fields
//// - Postconditions: Types are immutable and correctly structured
//// - Invariants: All fields are accessible and type-safe

import gleam/list
import gleeunit
import gleeunit/should
import intent/vision_types

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Scenario Tests
// ============================================================================

pub fn scenario_creation_test() {
  let scenario =
    vision_types.Scenario(
      character: "Agent Smith",
      persona: "mid-tier LLM with 128k token limit",
      motivation: "Run a quick quality check on a spec file",
      simulation: "Smith sends a JSON request. If response is 50kb of debug logs, context window floods",
      outcome: "Must implement select fields to allow requesting only quality score",
    )

  scenario.character
  |> should.equal("Agent Smith")

  scenario.persona
  |> should.equal("mid-tier LLM with 128k token limit")

  scenario.motivation
  |> should.equal("Run a quick quality check on a spec file")

  scenario.simulation
  |> should.equal(
    "Smith sends a JSON request. If response is 50kb of debug logs, context window floods",
  )

  scenario.outcome
  |> should.equal(
    "Must implement select fields to allow requesting only quality score",
  )
}

pub fn scenario_immutability_test() {
  let scenario1 =
    vision_types.Scenario(
      character: "Agent A",
      persona: "LLM",
      motivation: "Test",
      simulation: "Sim",
      outcome: "Out",
    )

  let scenario2 =
    vision_types.Scenario(
      character: "Agent A",
      persona: "LLM",
      motivation: "Test",
      simulation: "Sim",
      outcome: "Out",
    )

  // Should be equal by structure
  scenario1.character
  |> should.equal(scenario2.character)
}

// ============================================================================
// Stakeholder Tests
// ============================================================================

pub fn stakeholder_creation_test() {
  let stakeholder =
    vision_types.Stakeholder(
      name: "Autonomous AI Agents",
      role: "Primary User",
      needs: [
        "Deterministic responses",
        "Token-optimized output",
        "Stateless operations",
      ],
      pain_points: ["Visual noise", "Stateful friction", "Ambiguous output"],
    )

  stakeholder.name
  |> should.equal("Autonomous AI Agents")

  stakeholder.role
  |> should.equal("Primary User")

  stakeholder.needs
  |> should.equal([
    "Deterministic responses",
    "Token-optimized output",
    "Stateless operations",
  ])

  stakeholder.pain_points
  |> should.equal(["Visual noise", "Stateful friction", "Ambiguous output"])
}

pub fn stakeholder_empty_lists_test() {
  let stakeholder =
    vision_types.Stakeholder(
      name: "Test Stakeholder",
      role: "Tester",
      needs: [],
      pain_points: [],
    )

  stakeholder.needs
  |> should.equal([])

  stakeholder.pain_points
  |> should.equal([])
}

// ============================================================================
// VisionSection Tests
// ============================================================================

pub fn vision_section_creation_test() {
  let section =
    vision_types.VisionSection(
      title: "The Headless AI Kernel",
      description: "Intent CLI is pivoting from human-centric to AI-native architecture",
      scenarios: [
        vision_types.Scenario(
          character: "Agent Smith",
          persona: "mid-tier LLM",
          motivation: "Quality check",
          simulation: "Sends JSON request",
          outcome: "Token-optimized response",
        ),
      ],
      stakeholders: [
        vision_types.Stakeholder(
          name: "AI Agents",
          role: "Primary User",
          needs: ["Deterministic output"],
          pain_points: ["Token waste"],
        ),
      ],
      principles: [
        "Token-Optimized: Zero chitchat",
        "Stateless: Pure functions",
        "Schema-First: CUE/JSON contracts",
      ],
    )

  section.title
  |> should.equal("The Headless AI Kernel")

  section.description
  |> should.equal(
    "Intent CLI is pivoting from human-centric to AI-native architecture",
  )

  section.scenarios
  |> list.length()
  |> should.equal(1)

  section.stakeholders
  |> list.length()
  |> should.equal(1)

  section.principles
  |> list.length()
  |> should.equal(3)
}

pub fn vision_section_empty_collections_test() {
  let section =
    vision_types.VisionSection(
      title: "Minimal Section",
      description: "A minimal vision section",
      scenarios: [],
      stakeholders: [],
      principles: [],
    )

  section.scenarios
  |> should.equal([])

  section.stakeholders
  |> should.equal([])

  section.principles
  |> should.equal([])
}

pub fn vision_section_multiple_scenarios_test() {
  let scenario1 =
    vision_types.Scenario(
      character: "Agent A",
      persona: "Fast LLM",
      motivation: "Quick checks",
      simulation: "Rapid fire queries",
      outcome: "Low latency",
    )

  let scenario2 =
    vision_types.Scenario(
      character: "Agent B",
      persona: "Deep reasoning LLM",
      motivation: "Comprehensive analysis",
      simulation: "Complex multi-step workflow",
      outcome: "Complete coverage",
    )

  let section =
    vision_types.VisionSection(
      title: "Multi-Scenario Test",
      description: "Testing multiple scenarios",
      scenarios: [scenario1, scenario2],
      stakeholders: [],
      principles: [],
    )

  section.scenarios
  |> list.length()
  |> should.equal(2)

  // Access first scenario
  let first_scenario = case section.scenarios {
    [first, ..] -> first
    [] -> panic as "Expected non-empty list"
  }

  first_scenario.character
  |> should.equal("Agent A")
}
