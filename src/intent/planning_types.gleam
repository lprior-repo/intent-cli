/// Core types for the 4-phase planning system
///
/// This module defines types for Intent CLI's planning workflow:
/// - Phase 1: Vision (DDD) - Problem definition and stakeholder alignment
/// - Phase 2: Shape (MVP) - Minimum viable product scoping
/// - Phase 3: Spec (KIRK) - Detailed specification with 5-round mental model
/// - Phase 4: Ready (Ship) - Readiness assessment and launch criteria
///
/// These types are used to structure the planning process from vision to
/// ready-to-build specifications, ensuring alignment and completeness.
import gleam/option.{type Option}
import intent/vision_types.{type VisionSection}

// =============================================================================
// PHASE 1: VISION (DDD)
// =============================================================================

// VisionSection and Scenario are re-exported from vision_types to maintain
// a single source of truth for the 4-phase planning system.
// Note: Stakeholder is deprecated in favor of persona/non_personas fields

// =============================================================================
// PHASE 2: SHAPE (MVP)
// =============================================================================

/// A simplified feature description for MVP shaping
/// Contains just name and description, without full behavior specifications
pub type FeatureShape {
  FeatureShape(name: String, description: String)
}

/// MVP slice definition with shortcuts and deferred work
/// Represents the absolute minimum needed to validate the concept
pub type MVPSlice {
  MVPSlice(description: String, features: List(String), shortcuts: List(String))
}

/// Shape section defining the MVP scope and validation criteria
/// Focuses on what's essential vs what can be deferred
pub type ShapeSection {
  ShapeSection(
    features: List(FeatureShape),
    critical_path: List(String),
    mvp_slice: MVPSlice,
    post_mvp: List(String),
    validation_moment: String,
  )
}

// =============================================================================
// PHASE 3: SPEC (KIRK)
// =============================================================================

/// Severity level for quality issues
pub type Severity {
  QualityInfo
  QualityWarning
  QualityError
  QualityCritical
}

/// Quality issue with field, description, and severity
pub type QualityIssue {
  QualityIssue(field: String, issue: String, severity: Severity)
}

/// Quality score across multiple dimensions
/// All scores are 0-100, issues list is optional
pub type QualityScore {
  QualityScore(
    completeness: Float,
    consistency: Float,
    testability: Float,
    clarity: Float,
    security: Float,
    overall: Float,
    issues: List(QualityIssue),
  )
}

/// Likelihood level for pre-mortem causes
pub type Likelihood {
  LikelihoodLow
  LikelihoodMedium
  LikelihoodHigh
}

/// Likely cause in pre-mortem analysis
pub type LikelyCause {
  LikelyCause(cause: String, probability: Likelihood, mitigation: String)
}

/// Pre-mortem analysis - "The project failed. What happened?"
pub type PreMortem {
  PreMortem(assumed_failure: String, likely_causes: List(LikelyCause))
}

/// Inversion analysis - "What would make this fail?"
/// All failure lists are optional
pub type Inversions {
  Inversions(
    security_failures: List(String),
    usability_failures: List(String),
    integration_failures: List(String),
  )
}

/// KIRK health metrics from 5-round mental model analysis
/// Tracks quality, coverage, and identified issues across all rounds
pub type KIRKHealth {
  KIRKHealth(
    coverage_score: Float,
    quality_score: Float,
    gaps: List(String),
    inversions: List(String),
    effects: List(String),
  )
}

/// Spec section representing Phase 3 of the planning workflow
/// Contains the detailed specification after KIRK analysis
pub type SpecSection {
  SpecSection(
    name: String,
    description: String,
    rounds_complete: Int,
    kirk_health: KIRKHealth,
  )
}

// =============================================================================
// PHASE 4: READY (Ship)
// =============================================================================

/// Severity level for blockers
/// Used to prioritize blocker resolution
pub type BlockerSeverity {
  Critical
  High
  Medium
  Low
}

/// Score for a single READY dimension
/// Each dimension (Replacement, Empathy, Actionable, Discoverable, Yet-complete)
/// is scored 0-100 with reasoning and identified issues
pub type DimensionScore {
  DimensionScore(score: Int, reasoning: String, issues: List(String))
}

/// A blocker preventing launch readiness
/// Includes severity, description, and affected areas
pub type Blocker {
  Blocker(
    severity: BlockerSeverity,
    description: String,
    affected_areas: List(String),
  )
}

/// A recommendation for improvement
/// Prioritized suggestions to increase readiness score
pub type Recommendation {
  Recommendation(priority: Int, description: String, rationale: String)
}

/// Complete READY framework assessment
/// Evaluates readiness across 5 dimensions (R-E-A-D-Y):
/// - Replacement: Is the VORP still valid?
/// - Empathy: Is user friction simulated?
/// - Actionable: Do errors guide users?
/// - Discoverable: Are features findable?
/// - Yet-complete: Can the north star be achieved?
pub type ReadyReport {
  ReadyReport(
    replacement: DimensionScore,
    empathy: DimensionScore,
    actionable: DimensionScore,
    discoverable: DimensionScore,
    yet_complete: DimensionScore,
    overall_readiness: Int,
    blockers: List(Blocker),
    recommendations: List(Recommendation),
  )
}

// =============================================================================
// UNIFIED PLAN CONTAINER
// =============================================================================

/// Complete planning document spanning all 4 phases
/// Vision and Shape are required, Spec and Ready are optional
/// allowing incremental building through the planning workflow
pub type Plan {
  Plan(
    id: String,
    created_at: String,
    updated_at: String,
    vision: VisionSection,
    shape: ShapeSection,
    spec: Option(SpecSection),
    ready: Option(ReadyReport),
  )
}
