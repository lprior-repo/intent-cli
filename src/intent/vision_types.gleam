/// Core types for Vision documents
/// These types represent the structure of vision and architecture documents
/// following the Product-Minded Engineering framework from VISION_3_HEADLESS.md
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
pub type Stakeholder {
  Stakeholder(
    name: String,
    role: String,
    needs: List(String),
    pain_points: List(String),
  )
}

/// A vision section containing title, description, scenarios, stakeholders, and principles
/// Represents a major section of a vision document
pub type VisionSection {
  VisionSection(
    title: String,
    description: String,
    scenarios: List(Scenario),
    stakeholders: List(Stakeholder),
    principles: List(String),
  )
}
