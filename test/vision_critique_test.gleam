import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit
import gleeunit/should
import intent/vision_critique.{
  Critical, CritiqueIssue, ProblemReality, Warning, critique_vision,
  validate_persona, validate_problem_reality, validate_vorp_strength,
}
import intent/vision_types.{type VisionSection, Scenario, VisionSection}

pub fn main() {
  gleeunit.main()
}

fn perfect_vision() -> VisionSection {
  VisionSection(
    press_release: "AI-guided planning eliminates ambiguity and missed requirements by systematically decomposing high-level goals. Engineers waste 40% of integration time debugging unclear or incomplete specifications.",
    persona: "Backend engineers building microservices who integrate with 5+ REST APIs",
    non_personas: ["Frontend developers", "DevOps engineers", "QA testers"],
    north_star: "Run 'intent check api.cue' and catch every breaking change in 30 seconds, with zero false positives",
    scenarios: [
      Scenario(
        character: "Sarah",
        persona: "Backend engineer at fintech startup",
        motivation: "Prevent payment API integration failures in production",
        simulation: "Runs intent check before deploying payment service upgrade",
        outcome: "Catches breaking auth header change, fixes before prod, zero downtime",
      ),
      Scenario(
        character: "Marcus",
        persona: "Platform engineer at SaaS company",
        motivation: "Validate 12 microservice APIs in CI pipeline",
        simulation: "Adds intent check to GitHub Actions for all API contracts",
        outcome: "Detects 3 breaking changes per week before merge, saves 8 hours debugging",
      ),
    ],
    replaces: Some("Manual Postman testing + undocumented API assumptions"),
    vorp: "10x faster feedback (30 seconds vs 4 hours manual testing), 100x fewer production incidents (contract violations caught at build time vs runtime discovery)",
    out_of_scope: [
      "UI testing",
      "Performance benchmarking",
      "Database migrations",
    ],
  )
}

pub fn validate_problem_reality_perfect_vision_test() {
  let vision = perfect_vision()
  let issues = validate_problem_reality(vision)
  issues |> should.equal([])
}

pub fn validate_problem_reality_empty_press_release_test() {
  let vision = VisionSection(..perfect_vision(), press_release: "")
  let issues = validate_problem_reality(vision)
  issues |> should.not_equal([])
  let has_critical =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(ProblemReality, Critical, _, _) -> True
        _ -> False
      }
    })
  has_critical |> should.be_true
}

pub fn validate_problem_reality_buzzwords_test() {
  let vision =
    VisionSection(
      ..perfect_vision(),
      press_release: "Revolutionary game-changing innovative next-gen platform",
    )
  let issues = validate_problem_reality(vision)
  issues |> should.not_equal([])
}

pub fn validate_problem_reality_no_boundaries_test() {
  let vision = VisionSection(..perfect_vision(), out_of_scope: [])
  let issues = validate_problem_reality(vision)
  let has_boundary_warning =
    issues
    |> list.any(fn(issue) {
      case issue {
        CritiqueIssue(ProblemReality, Warning, msg, _) ->
          string.contains(msg, "out_of_scope")
          || string.contains(msg, "boundaries")
        _ -> False
      }
    })
  has_boundary_warning |> should.be_true
}

pub fn validate_persona_perfect_vision_test() {
  let vision = perfect_vision()
  let issues = validate_persona(vision)
  issues |> should.equal([])
}

pub fn validate_persona_vague_persona_test() {
  let vision = VisionSection(..perfect_vision(), persona: "developers")
  let issues = validate_persona(vision)
  issues |> should.not_equal([])
}

pub fn validate_persona_no_non_personas_test() {
  let vision = VisionSection(..perfect_vision(), non_personas: [])
  let issues = validate_persona(vision)
  issues |> should.not_equal([])
}

pub fn validate_persona_insufficient_scenarios_test() {
  let vision = VisionSection(..perfect_vision(), scenarios: [])
  let issues = validate_persona(vision)
  issues |> should.not_equal([])
}

pub fn validate_vorp_strength_perfect_vision_test() {
  let vision = perfect_vision()
  let issues = validate_vorp_strength(vision)
  issues |> should.equal([])
}

pub fn validate_vorp_strength_no_replaces_test() {
  let vision = VisionSection(..perfect_vision(), replaces: None)
  let issues = validate_vorp_strength(vision)
  issues |> should.not_equal([])
}

pub fn validate_vorp_strength_weak_language_test() {
  let vision =
    VisionSection(
      ..perfect_vision(),
      vorp: "Better and improved testing with faster results",
    )
  let issues = validate_vorp_strength(vision)
  issues |> should.not_equal([])
}

pub fn critique_vision_perfect_passes_test() {
  let vision = perfect_vision()
  let result = critique_vision(vision)
  result.passed |> should.be_true
  result.score |> should.equal(100)
  result.issues |> should.equal([])
}

pub fn critique_vision_warnings_reduce_score_test() {
  let vision = VisionSection(..perfect_vision(), out_of_scope: [])
  let result = critique_vision(vision)
  // Should pass but with reduced score (95 = 100 - 5 for warning)
  result.passed |> should.be_true
  { result.score < 100 && result.score > 70 } |> should.be_true
}

pub fn critique_vision_multiple_criticals_fail_hard_test() {
  let vision =
    VisionSection(
      press_release: "",
      persona: "users",
      non_personas: [],
      north_star: "",
      scenarios: [],
      replaces: None,
      vorp: "",
      out_of_scope: [],
    )
  let result = critique_vision(vision)
  result.passed |> should.be_false
  { result.score < 30 } |> should.be_true
}
