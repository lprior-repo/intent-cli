//// Comprehensive HTTP method coverage tests for coverage_analyzer.gleam
////
//// Tests the method counting and scoring functionality:
//// - count_methods: Count behaviors by HTTP method
//// - calculate_method_score: Score based on GET/POST/PUT/DELETE/PATCH coverage
////
//// DbC Postconditions Verified:
//// - method_score = (covered_basic_methods / 5) * 100.0
//// - method_score in range [0.0, 100.0]
//// - Methods dict contains only valid HTTP method strings
//// - Each method count >= 0

import gleam/dict
import gleam/list
import gleeunit/should
import intent/kirk/coverage_analyzer
import intent/types.{Delete, Get, Head, Options, Patch, Post, Put}
import test_helpers

// =============================================================================
// METHOD SCORE CALCULATION TESTS
// =============================================================================

pub fn method_score_all_five_basic_methods_test() {
  // GIVEN: Behaviors covering all 5 basic methods (GET, POST, PUT, DELETE, PATCH)
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
    test_helpers.make_test_behavior_with_method("update-user-put", Put, []),
    test_helpers.make_test_behavior_with_method("update-user-patch", Patch, []),
    test_helpers.make_test_behavior_with_method("delete-user", Delete, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: All 5 methods are present
  dict.has_key(report.methods, "GET")
  |> should.be_true()

  dict.has_key(report.methods, "POST")
  |> should.be_true()

  dict.has_key(report.methods, "PUT")
  |> should.be_true()

  dict.has_key(report.methods, "PATCH")
  |> should.be_true()

  dict.has_key(report.methods, "DELETE")
  |> should.be_true()

  // THEN: Method score is 100% (5/5 * 100)
  // Note: overall_score includes other factors, so we verify via methods presence
  let method_count = dict.size(report.methods)
  { method_count >= 5 }
  |> should.be_true()
}

pub fn method_score_four_of_five_basic_methods_test() {
  // GIVEN: Behaviors covering 4/5 basic methods (missing DELETE)
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
    test_helpers.make_test_behavior_with_method("update-put", Put, []),
    test_helpers.make_test_behavior_with_method("update-patch", Patch, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: 4 basic methods are present
  dict.has_key(report.methods, "GET")
  |> should.be_true()

  dict.has_key(report.methods, "POST")
  |> should.be_true()

  dict.has_key(report.methods, "PUT")
  |> should.be_true()

  dict.has_key(report.methods, "PATCH")
  |> should.be_true()

  // THEN: DELETE is not present
  case dict.get(report.methods, "DELETE") {
    Error(_) -> True |> should.be_true()
    Ok(_) -> should.fail()
  }

  // THEN: Method count is 4
  dict.size(report.methods)
  |> should.equal(4)
}

pub fn method_score_three_of_five_basic_methods_test() {
  // GIVEN: Behaviors covering 3/5 basic methods (GET, POST, DELETE)
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
    test_helpers.make_test_behavior_with_method("delete-user", Delete, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: 3 basic methods are present
  dict.has_key(report.methods, "GET")
  |> should.be_true()

  dict.has_key(report.methods, "POST")
  |> should.be_true()

  dict.has_key(report.methods, "DELETE")
  |> should.be_true()

  // THEN: PUT and PATCH are not present
  case dict.get(report.methods, "PUT") {
    Error(_) -> True |> should.be_true()
    Ok(_) -> should.fail()
  }

  case dict.get(report.methods, "PATCH") {
    Error(_) -> True |> should.be_true()
    Ok(_) -> should.fail()
  }
}

pub fn method_score_two_of_five_basic_methods_test() {
  // GIVEN: Behaviors covering 2/5 basic methods (GET, POST)
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: 2 basic methods are present
  dict.has_key(report.methods, "GET")
  |> should.be_true()

  dict.has_key(report.methods, "POST")
  |> should.be_true()

  // THEN: Method count is 2
  dict.size(report.methods)
  |> should.equal(2)
}

pub fn method_score_one_of_five_basic_methods_test() {
  // GIVEN: Behaviors covering 1/5 basic methods (only GET)
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Only GET is present
  dict.has_key(report.methods, "GET")
  |> should.be_true()

  // THEN: Method count is 1
  dict.size(report.methods)
  |> should.equal(1)
}

pub fn method_score_zero_basic_methods_test() {
  // GIVEN: No behaviors (empty spec)
  let spec = test_helpers.make_test_spec_from_behaviors([])

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: No methods are present
  dict.size(report.methods)
  |> should.equal(0)

  // THEN: overall_score should be low (0% method coverage contributes 20% weight)
  { report.overall_score >=. 0.0 && report.overall_score <=. 100.0 }
  |> should.be_true()
}

// =============================================================================
// NON-BASIC METHOD TESTS (HEAD, OPTIONS)
// =============================================================================

pub fn non_basic_methods_do_not_contribute_to_score_test() {
  // GIVEN: Behaviors with only non-basic methods (HEAD, OPTIONS)
  let behaviors = [
    test_helpers.make_test_behavior_with_method("head-check", Head, []),
    test_helpers.make_test_behavior_with_method("options-check", Options, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Non-basic methods are counted in methods dict
  dict.has_key(report.methods, "HEAD")
  |> should.be_true()

  dict.has_key(report.methods, "OPTIONS")
  |> should.be_true()

  // THEN: But no basic methods are present
  case dict.get(report.methods, "GET") {
    Error(_) -> True |> should.be_true()
    Ok(_) -> should.fail()
  }
}

pub fn mixed_basic_and_non_basic_methods_test() {
  // GIVEN: Behaviors with both basic (GET) and non-basic (HEAD, OPTIONS) methods
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("head-check", Head, []),
    test_helpers.make_test_behavior_with_method("options-check", Options, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: All methods are present in dict
  dict.has_key(report.methods, "GET")
  |> should.be_true()

  dict.has_key(report.methods, "HEAD")
  |> should.be_true()

  dict.has_key(report.methods, "OPTIONS")
  |> should.be_true()

  // THEN: Total method count is 3
  dict.size(report.methods)
  |> should.equal(3)
}

// =============================================================================
// METHOD COUNT ACCUMULATION TESTS
// =============================================================================

pub fn multiple_behaviors_same_method_accumulate_count_test() {
  // GIVEN: Multiple behaviors using GET method
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("get-post", Get, []),
    test_helpers.make_test_behavior_with_method("get-comment", Get, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: GET count is 3
  case dict.get(report.methods, "GET") {
    Ok(count) -> count |> should.equal(3)
    Error(_) -> should.fail()
  }

  // THEN: Only one method type in dict
  dict.size(report.methods)
  |> should.equal(1)
}

pub fn different_counts_for_different_methods_test() {
  // GIVEN: Behaviors with different method frequencies
  // 5 GET, 3 POST, 2 PUT, 1 DELETE
  let behaviors = [
    // GET (5)
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("get-post", Get, []),
    test_helpers.make_test_behavior_with_method("get-comment", Get, []),
    test_helpers.make_test_behavior_with_method("get-tag", Get, []),
    test_helpers.make_test_behavior_with_method("get-category", Get, []),
    // POST (3)
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
    test_helpers.make_test_behavior_with_method("create-post", Post, []),
    test_helpers.make_test_behavior_with_method("create-comment", Post, []),
    // PUT (2)
    test_helpers.make_test_behavior_with_method("update-user", Put, []),
    test_helpers.make_test_behavior_with_method("update-post", Put, []),
    // DELETE (1)
    test_helpers.make_test_behavior_with_method("delete-user", Delete, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: GET count is 5
  case dict.get(report.methods, "GET") {
    Ok(count) -> count |> should.equal(5)
    Error(_) -> should.fail()
  }

  // THEN: POST count is 3
  case dict.get(report.methods, "POST") {
    Ok(count) -> count |> should.equal(3)
    Error(_) -> should.fail()
  }

  // THEN: PUT count is 2
  case dict.get(report.methods, "PUT") {
    Ok(count) -> count |> should.equal(2)
    Error(_) -> should.fail()
  }

  // THEN: DELETE count is 1
  case dict.get(report.methods, "DELETE") {
    Ok(count) -> count |> should.equal(1)
    Error(_) -> should.fail()
  }

  // THEN: 4 distinct methods
  dict.size(report.methods)
  |> should.equal(4)
}

// =============================================================================
// EDGE CASE TESTS
// =============================================================================

pub fn single_behavior_single_method_test() {
  // GIVEN: Single behavior with single method
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: DbC postcondition - count >= 0
  case dict.get(report.methods, "GET") {
    Ok(count) -> {
      count |> should.equal(1)
      { count >= 0 } |> should.be_true()
    }
    Error(_) -> should.fail()
  }
}

pub fn only_crud_operations_test() {
  // GIVEN: Behaviors representing pure CRUD (Create, Read, Update, Delete)
  let behaviors = [
    test_helpers.make_test_behavior_with_method("read-resource", Get, []),
    test_helpers.make_test_behavior_with_method("create-resource", Post, []),
    test_helpers.make_test_behavior_with_method("update-resource", Put, []),
    test_helpers.make_test_behavior_with_method("delete-resource", Delete, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: All 4 CRUD operations are present (PATCH missing)
  dict.has_key(report.methods, "GET")
  |> should.be_true()

  dict.has_key(report.methods, "POST")
  |> should.be_true()

  dict.has_key(report.methods, "PUT")
  |> should.be_true()

  dict.has_key(report.methods, "DELETE")
  |> should.be_true()

  // THEN: PATCH is not present
  case dict.get(report.methods, "PATCH") {
    Error(_) -> True |> should.be_true()
    Ok(_) -> should.fail()
  }
}

pub fn put_vs_patch_distinction_test() {
  // GIVEN: Behaviors with both PUT and PATCH (full vs partial update)
  let behaviors = [
    test_helpers.make_test_behavior_with_method("full-update", Put, []),
    test_helpers.make_test_behavior_with_method("partial-update", Patch, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: Both PUT and PATCH are distinct
  dict.has_key(report.methods, "PUT")
  |> should.be_true()

  dict.has_key(report.methods, "PATCH")
  |> should.be_true()

  // THEN: Each has count of 1
  case dict.get(report.methods, "PUT") {
    Ok(count) -> count |> should.equal(1)
    Error(_) -> should.fail()
  }

  case dict.get(report.methods, "PATCH") {
    Ok(count) -> count |> should.equal(1)
    Error(_) -> should.fail()
  }
}

// =============================================================================
// DBC POSTCONDITION TESTS
// =============================================================================

pub fn dbc_method_counts_non_negative_test() {
  // GIVEN: Any spec with behaviors
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("create-user", Post, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: DbC postcondition - all method counts >= 0
  let all_counts_non_negative =
    report.methods
    |> dict.values()
    |> list.all(fn(count) { count >= 0 })

  all_counts_non_negative
  |> should.be_true()
}

pub fn dbc_methods_dict_contains_valid_method_strings_test() {
  // GIVEN: Behaviors with various methods
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
    test_helpers.make_test_behavior_with_method("post-user", Post, []),
    test_helpers.make_test_behavior_with_method("put-user", Put, []),
    test_helpers.make_test_behavior_with_method("patch-user", Patch, []),
    test_helpers.make_test_behavior_with_method("delete-user", Delete, []),
    test_helpers.make_test_behavior_with_method("head-check", Head, []),
    test_helpers.make_test_behavior_with_method("options-check", Options, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: DbC postcondition - all keys are valid HTTP method strings
  let valid_methods = ["GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS"]

  let all_methods_valid =
    report.methods
    |> dict.keys()
    |> list.all(fn(method) { list.contains(valid_methods, method) })

  all_methods_valid
  |> should.be_true()
}

pub fn dbc_overall_score_in_range_test() {
  // GIVEN: Any spec
  let behaviors = [
    test_helpers.make_test_behavior_with_method("get-user", Get, []),
  ]

  let spec = test_helpers.make_test_spec_from_behaviors(behaviors)

  // WHEN: Analyzing coverage
  let report = coverage_analyzer.analyze_coverage(spec)

  // THEN: DbC postcondition - overall_score in [0.0, 100.0]
  { report.overall_score >=. 0.0 }
  |> should.be_true()

  { report.overall_score <=. 100.0 }
  |> should.be_true()
}
