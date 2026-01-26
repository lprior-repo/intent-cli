import gleeunit
import gleeunit/should
import intent/planning_types.{FeatureShape, MVPSlice, ShapeSection, type ShapeSection}
import intent/shape_critique.{
  critique_shape, validate_achievability, validate_concept_validation,
  validate_mvp_minimalism,
}

pub fn main() {
  gleeunit.main()
}

fn perfect_shape() -> ShapeSection {
  ShapeSection(
    features: [
      FeatureShape(
        name: "CUE spec validation",
        description: "Parse and validate CUE contract specifications",
      ),
      FeatureShape(
        name: "HTTP execution",
        description: "Run HTTP requests defined in behaviors",
      ),
      FeatureShape(
        name: "Response checking",
        description: "Validate responses against contract checks",
      ),
    ],
    critical_path: [
      "Parse CUE spec",
      "Execute single HTTP request",
      "Validate response status",
    ],
    mvp_slice: MVPSlice(
      description: "Run a single CUE spec with one behavior and validate the response status code",
      features: ["CUE spec validation", "HTTP execution", "Response checking"],
      shortcuts: [
        "Hardcode single spec file path",
        "Only support GET requests",
        "Skip header validation",
      ],
    ),
    post_mvp: ["GraphQL support", "WebSocket testing", "Performance benchmarks"],
    validation_moment: "Successfully validate a breaking API change is caught by running 'intent check api.cue' and seeing it fail with the exact contract violation",
  )
}

pub fn validate_mvp_minimalism_perfect_shape_test() {
  let shape = perfect_shape()
  let issues = validate_mvp_minimalism(shape)
  issues |> should.equal([])
}

pub fn validate_mvp_minimalism_empty_features_test() {
  let shape = ShapeSection(..perfect_shape(), features: [])
  let issues = validate_mvp_minimalism(shape)
  issues |> should.not_equal([])
}

pub fn validate_mvp_minimalism_too_many_features_test() {
  let shape =
    ShapeSection(..perfect_shape(), features: [
      FeatureShape("f1", "desc1"),
      FeatureShape("f2", "desc2"),
      FeatureShape("f3", "desc3"),
      FeatureShape("f4", "desc4"),
      FeatureShape("f5", "desc5"),
      FeatureShape("f6", "desc6"),
      FeatureShape("f7", "desc7"),
    ])
  let issues = validate_mvp_minimalism(shape)
  issues |> should.not_equal([])
}

pub fn validate_mvp_minimalism_no_shortcuts_test() {
  let shape =
    ShapeSection(
      ..perfect_shape(),
      mvp_slice: MVPSlice(
        description: "Full implementation",
        features: ["CUE spec validation"],
        shortcuts: [],
      ),
    )
  let issues = validate_mvp_minimalism(shape)
  issues |> should.not_equal([])
}

pub fn validate_concept_validation_perfect_shape_test() {
  let shape = perfect_shape()
  let issues = validate_concept_validation(shape)
  issues |> should.equal([])
}

pub fn validate_concept_validation_empty_validation_moment_test() {
  let shape = ShapeSection(..perfect_shape(), validation_moment: "")
  let issues = validate_concept_validation(shape)
  issues |> should.not_equal([])
}

pub fn validate_concept_validation_vague_validation_moment_test() {
  let shape = ShapeSection(..perfect_shape(), validation_moment: "It works")
  let issues = validate_concept_validation(shape)
  issues |> should.not_equal([])
}

pub fn validate_concept_validation_empty_critical_path_test() {
  let shape = ShapeSection(..perfect_shape(), critical_path: [])
  let issues = validate_concept_validation(shape)
  issues |> should.not_equal([])
}

pub fn validate_achievability_perfect_shape_test() {
  let shape = perfect_shape()
  let issues = validate_achievability(shape)
  issues |> should.equal([])
}

pub fn validate_achievability_no_post_mvp_test() {
  let shape = ShapeSection(..perfect_shape(), post_mvp: [])
  let issues = validate_achievability(shape)
  issues |> should.not_equal([])
}

pub fn validate_achievability_empty_mvp_description_test() {
  let shape =
    ShapeSection(
      ..perfect_shape(),
      mvp_slice: MVPSlice(description: "", features: ["f1"], shortcuts: ["s1"]),
    )
  let issues = validate_achievability(shape)
  issues |> should.not_equal([])
}

pub fn critique_shape_perfect_passes_test() {
  let shape = perfect_shape()
  let result = critique_shape(shape)
  result.passed |> should.be_true
  result.score |> should.equal(100)
  result.issues |> should.equal([])
}

pub fn critique_shape_warnings_reduce_score_test() {
  let shape = ShapeSection(..perfect_shape(), post_mvp: [])
  let result = critique_shape(shape)
  // Should pass but with reduced score
  result.passed |> should.be_true
  { result.score < 100 && result.score > 70 } |> should.be_true
}

pub fn critique_shape_multiple_criticals_fail_hard_test() {
  let shape =
    ShapeSection(
      features: [],
      critical_path: [],
      mvp_slice: MVPSlice(description: "", features: [], shortcuts: []),
      post_mvp: [],
      validation_moment: "",
    )
  let result = critique_shape(shape)
  result.passed |> should.be_false
  { result.score < 30 } |> should.be_true
}
