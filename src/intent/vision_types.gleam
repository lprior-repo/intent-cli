/// Core types for Vision documents
/// These types represent Phase 1: VISION (DDD) from INTENT_4_PLAN.md
/// The 4-phase planning system: Vision → Shape → Spec → Ready
import gleam/option.{type Option}

/// A scenario consisting of character, motivation, and simulation
/// Used to validate technical decisions against concrete use cases
pub type Scenario {
  Scenario(
    character: String,
    persona: String,
    motivation: String,
    simulation: String,
    outcome: String,
  )
}

/// A stakeholder with role, needs, and pain points
/// Represents users or actors in the system
/// DEPRECATED: Use VisionSection fields (persona, non_personas) instead
pub type Stakeholder {
  Stakeholder(
    name: String,
    role: String,
    needs: List(String),
    pain_points: List(String),
  )
}

/// Vision section from INTENT_4_PLAN.md Phase 1: VISION (DDD)
/// Defines what we're building and why, following DDD principles
pub type VisionSection {
  VisionSection(
    press_release: String,
    persona: String,
    non_personas: List(String),
    north_star: String,
    scenarios: List(Scenario),
    replaces: Option(String),
    vorp: String,
    out_of_scope: List(String),
  )
}
