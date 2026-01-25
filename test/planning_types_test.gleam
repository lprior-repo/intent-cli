//// Comprehensive tests for intent/planning_types.gleam Shape types
//// Tests cover ShapeSection, FeatureShape, and MVPSlice structures
////
//// Design by Contract:
//// - Preconditions: Valid type construction with all required fields
//// - Postconditions: Types are immutable and correctly structured
//// - Invariants: All fields are accessible and type-safe

import gleam/list
import gleeunit
import gleeunit/should
import intent/planning_types

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// FeatureShape Tests
// ============================================================================

pub fn feature_shape_creation_test() {
  let feature =
    planning_types.FeatureShape(
      name: "User Authentication",
      description: "Allow users to sign in and manage sessions securely",
    )

  feature.name
  |> should.equal("User Authentication")

  feature.description
  |> should.equal("Allow users to sign in and manage sessions securely")
}

pub fn feature_shape_immutability_test() {
  let feature1 =
    planning_types.FeatureShape(name: "Feature A", description: "Description A")

  let feature2 =
    planning_types.FeatureShape(name: "Feature A", description: "Description A")

  // Should be equal by structure
  feature1.name
  |> should.equal(feature2.name)

  feature1.description
  |> should.equal(feature2.description)
}

pub fn feature_shape_empty_strings_test() {
  let feature = planning_types.FeatureShape(name: "", description: "")

  feature.name
  |> should.equal("")

  feature.description
  |> should.equal("")
}

// ============================================================================
// MVPSlice Tests
// ============================================================================

pub fn mvp_slice_creation_test() {
  let mvp =
    planning_types.MVPSlice(
      description: "Minimum viable authentication with email/password only",
      features: ["Sign in", "Sign out", "Session management"],
      shortcuts: [
        "Use in-memory session store",
        "Skip email verification",
        "Hardcode rate limits",
      ],
    )

  mvp.description
  |> should.equal("Minimum viable authentication with email/password only")

  mvp.features
  |> should.equal(["Sign in", "Sign out", "Session management"])

  mvp.shortcuts
  |> should.equal([
    "Use in-memory session store",
    "Skip email verification",
    "Hardcode rate limits",
  ])
}

pub fn mvp_slice_empty_lists_test() {
  let mvp =
    planning_types.MVPSlice(
      description: "Empty MVP",
      features: [],
      shortcuts: [],
    )

  mvp.features
  |> should.equal([])

  mvp.shortcuts
  |> should.equal([])
}

pub fn mvp_slice_list_length_test() {
  let mvp =
    planning_types.MVPSlice(
      description: "Test MVP",
      features: ["Feature 1", "Feature 2", "Feature 3"],
      shortcuts: ["Shortcut 1", "Shortcut 2"],
    )

  mvp.features
  |> list.length()
  |> should.equal(3)

  mvp.shortcuts
  |> list.length()
  |> should.equal(2)
}

// ============================================================================
// ShapeSection Tests
// ============================================================================

pub fn shape_section_creation_test() {
  let feature1 =
    planning_types.FeatureShape(
      name: "Authentication",
      description: "User login system",
    )

  let feature2 =
    planning_types.FeatureShape(
      name: "Dashboard",
      description: "User home page",
    )

  let mvp =
    planning_types.MVPSlice(
      description: "Basic auth + empty dashboard",
      features: ["Authentication", "Dashboard"],
      shortcuts: ["Mock database", "Hardcode UI"],
    )

  let shape =
    planning_types.ShapeSection(
      features: [feature1, feature2],
      critical_path: ["Authentication", "Dashboard"],
      mvp_slice: mvp,
      post_mvp: ["Profile editing", "Settings page", "Notifications"],
      validation_moment: "User can sign in and see their dashboard",
    )

  shape.features
  |> list.length()
  |> should.equal(2)

  shape.critical_path
  |> should.equal(["Authentication", "Dashboard"])

  shape.mvp_slice.description
  |> should.equal("Basic auth + empty dashboard")

  shape.post_mvp
  |> list.length()
  |> should.equal(3)

  shape.validation_moment
  |> should.equal("User can sign in and see their dashboard")
}

pub fn shape_section_empty_collections_test() {
  let mvp =
    planning_types.MVPSlice(
      description: "Minimal MVP",
      features: [],
      shortcuts: [],
    )

  let shape =
    planning_types.ShapeSection(
      features: [],
      critical_path: [],
      mvp_slice: mvp,
      post_mvp: [],
      validation_moment: "Test validation",
    )

  shape.features
  |> should.equal([])

  shape.critical_path
  |> should.equal([])

  shape.post_mvp
  |> should.equal([])
}

pub fn shape_section_nested_access_test() {
  let feature =
    planning_types.FeatureShape(name: "Test Feature", description: "Test desc")

  let mvp =
    planning_types.MVPSlice(
      description: "Test MVP",
      features: ["Test Feature"],
      shortcuts: ["Test shortcut"],
    )

  let shape =
    planning_types.ShapeSection(
      features: [feature],
      critical_path: ["Test Feature"],
      mvp_slice: mvp,
      post_mvp: ["Future work"],
      validation_moment: "Test passes",
    )

  // Access nested feature
  let first_feature = case shape.features {
    [first, ..] -> first
    [] -> panic as "Expected non-empty list"
  }

  first_feature.name
  |> should.equal("Test Feature")

  // Access nested MVP fields
  shape.mvp_slice.features
  |> list.length()
  |> should.equal(1)

  shape.mvp_slice.shortcuts
  |> list.length()
  |> should.equal(1)
}

pub fn shape_section_critical_path_alignment_test() {
  let feature1 =
    planning_types.FeatureShape(name: "Core Feature", description: "Critical")

  let feature2 =
    planning_types.FeatureShape(name: "Nice to Have", description: "Optional")

  let mvp =
    planning_types.MVPSlice(
      description: "MVP with only core",
      features: ["Core Feature"],
      shortcuts: [],
    )

  let shape =
    planning_types.ShapeSection(
      features: [feature1, feature2],
      critical_path: ["Core Feature"],
      mvp_slice: mvp,
      post_mvp: ["Nice to Have"],
      validation_moment: "Core works",
    )

  // Critical path should align with MVP features
  shape.critical_path
  |> should.equal(shape.mvp_slice.features)

  // Post-MVP should include non-critical features
  shape.post_mvp
  |> list.length()
  |> should.equal(1)
}
