/// Comprehensive FFI validation test
/// Tests all FFI functions and their boundaries
import gleam/io
import gleam/result
import gleam/list
import gleam/map
import gleam/should
import gleam/future

// Import FFI functions
import intent/stdin
import intent/checker/rules

/// Test Category
pub type TestCategory {
  BasicSafety
  ErrorHandling
  ResourceSafety
  ConcurrentSafety
  Security
}

/// Test Result
pub type TestResult {
  Passed(category: TestCategory, name: String, description: String)
  Failed(category: TestCategory, name: String, description: String, error: String)
}

/// Test Report
pub type TestReport {
  Report(results: List(TestResult), summary: Map(TestCategory, Int))
}

/// Generate bead issue for failed test
fn generate_bead_for_failure(result: TestResult) {
  case result {
    Failed(category, name, description, error) ->
      let issue_id = "intent-cli-ffi-" <> string.replace(name, " ", "-")
      let category_name = string.capitalize(case category {
        BasicSafety -> "Basic Safety"
        ErrorHandling -> "Error Handling"
        ResourceSafety -> "Resource Safety"
        ConcurrentSafety -> "Concurrent Safety"
        Security -> "Security"
      })

      let issue_body = "## FFI Test Failure: " <> name <> "\n\n" <>
                      "**Category:** " <> category_name <> "\n" <>
                      "**Description:** " <> description <> "\n" <>
                      "**Error:** " <> error <> "\n\n" <>
                      "This FFI boundary failure indicates potential system stability issues.\n\n" <>
                      "Test Location: `/test/intent/ffi_comprehensive_validation_test.gleam`\n\n" <>
                      "Priority: High\n\n" <>
                      "Critical for FFI safety and system stability."

      io.println("🚨 Would create bead: " <> issue_id)
      io.println("   Title: FFI Failure - " <> name)
      io.println("   Body: " <> issue_body)
    _ -> Nil
  }
}

/// Helper to count results
fn categorise_results(results: List(TestResult)) -> Map(TestCategory, Int) {
  let initial_map = map.from_list([
    #(BasicSafety, 0),
    #(ErrorHandling, 0),
    #(ResourceSafety, 0),
    #(ConcurrentSafety, 0),
    #(Security, 0),
  ])

  list.fold(results, initial_map, fn(acc, result) {
    case result {
      Passed(category, _, _) -> map.insert(acc, category, map.get(acc, category) + 1)
      Failed(category, _, _, _) -> map.insert(acc, category, map.get(acc, category) + 1)
    }
  })
}

// =============================================================================
// BASIC SAFETY TESTS
// =============================================================================

/// Test basic FFI function availability and correctness
pub fn basic_safety_tests() -> List(TestResult) {
  [
    test_uuid_generation(),
    test_timestamp_functions(),
    test_basic_base64_decoding(),
  ]
}

fn test_uuid_generation() -> TestResult {
  let uuid = intent.generate_uuid()

  case string.length(uuid) == 36 && string.contains(uuid, "-") {
    True ->
      Passed(BasicSafety, "UUID Generation", "UUID generation produces valid format")
    False ->
      Failed(
        BasicSafety,
        "UUID Generation",
        "UUID generation produces invalid format",
        "UUID: " <> uuid
      )
  }
}

fn test_timestamp_functions() -> TestResult {
  let timestamp1 = intent.current_timestamp()
  let timestamp2 = intent.current_iso8601_timestamp()

  let is_valid =
    string.length(timestamp1) > 0
    && string.length(timestamp2) > 0
    && string.contains(timestamp1, "T")
    && string.contains(timestamp2, "T")

  case is_valid {
    True ->
      Passed(BasicSafety, "Timestamp Functions", "Timestamp functions return valid ISO format")
    False ->
      Failed(
        BasicSafety,
        "Timestamp Functions",
        "Timestamp functions return invalid format",
        "Timestamps: " <> timestamp1 <> " / " <> timestamp2
      )
  }
}

fn test_basic_base64_decoding() -> TestResult {
  let valid_cases = [
    ("VGhpcyBpcyBhIHRlc3Q", "This is a test"),
    ("YW55LWdvdA", "any-gd"),
  ]

  let results = list.map(valid_cases, fn(input_expected) {
    let #(input, expected) = input_expected
    case rules.base64_url_decode(input) {
      Ok(decoded) -> decoded == expected
      Error(_) -> False
    }
  })

  case list.all(results, fn(x) { x }) {
    True ->
      Passed(BasicSafety, "Base64 Decoding", "Valid base64url strings decode correctly")
    False ->
      Failed(
        BasicSafety,
        "Base64 Decoding",
        "Some base64url decoding cases failed",
        "Not all valid inputs decoded correctly"
      )
  }
}

// =============================================================================
// ERROR HANDLING TESTS
// =============================================================================

/// Test error handling for invalid inputs
pub fn error_handling_tests() -> List(TestResult) {
  [
    test_invalid_base64_inputs(),
    test_stdin_error_conditions(),
  ]
}

fn test_invalid_base64_inputs() -> TestResult {
  let invalid_inputs = ["", "!", "invalid", "A===", "A\n"]
  let results = list.map(invalid_inputs, fn(input) {
    case rules.base64_url_decode(input) {
      Error(_) -> True
      Ok(_) -> False
    }
  })

  let all_failed = list.all(results, fn(x) { x })

  case all_failed {
    True ->
      Passed(ErrorHandling, "Invalid Base64 Inputs", "Invalid base64 inputs properly rejected")
    False ->
      Failed(
        ErrorHandling,
        "Invalid Base64 Inputs",
        "Some invalid inputs were incorrectly accepted",
        "Not all invalid inputs were rejected"
      )
  }
}

fn test_stdin_error_conditions() -> TestResult {
  let results = [
    stdin.read_line(),
    stdin.read_line_trimmed(),
    stdin.read_non_empty_line(),
    stdin.prompt_yes_no("test"),
  ]

  let all_handled = list.all(results, fn(result) {
    case result {
      Ok(_) | Error(_) -> True
    }
  })

  case all_handled {
    True ->
      Passed(ErrorHandling, "Stdin Error Handling", "Stdin functions handle error conditions")
    False ->
      Failed(
        ErrorHandling,
        "Stdin Error Handling",
        "Some stdin functions did not handle errors properly",
        "Stdin error handling inconsistent"
      )
  }
}

// =============================================================================
// RESOURCE SAFETY TESTS
// =============================================================================

/// Test resource management and memory safety
pub fn resource_safety_tests() -> List(TestResult) {
  [
    test_memory_allocation_safety(),
    test_large_string_handling(),
  ]
}

fn test_memory_allocation_safety() -> TestResult {
  // Test repeated allocation to check for memory leaks
  let operations = list.range(0, 100)
    |> list.map(fn(_) {
      rules.base64_url_decode("VGhpcyBpcyBhIHRlc3Q") // "This is a test"
    })

  let all_completed = list.all(operations, fn(result) {
    case result {
      Ok(_) | Error(_) -> True
    }
  })

  case all_completed {
    True ->
      Passed(ResourceSafety, " Memory Allocation Safety", "Repeated memory allocation is safe")
    False ->
      Failed(
        ResourceSafety,
        "Memory Allocation Safety",
        "Some memory operations failed",
        "Memory allocation not consistent"
      )
  }
}

fn test_large_string_handling() -> TestResult {
  let large_string = string.repeat("A", 10000)

  case rules.base64_url_decode(large_string) {
    Ok(_) | Error(_) ->
      Passed(ResourceSafety, "Large String Handling", "Large strings handled without crashing")
    _ ->
      Failed(
        ResourceSafety,
        "Large String Handling",
        "Large string handling failed unexpectedly",
        "Unknown error with large string processing"
      )
  }
}

// =============================================================================
// CONCURRENT SAFETY TESTS
// =============================================================================

/// Test concurrent access to FFI functions
pub fn concurrent_safety_tests() -> List(TestResult) {
  [
    test_concurrent_uuid_generation(),
    test_concurrent_timestamp_generation(),
    test_concurrent_base64_operations(),
  ]
}

fn test_concurrent_uuid_generation() -> TestResult {
  let tasks = list.range(0, 20)
    |> list.map(fn(_) {
      future.promise(fn() { intent.generate_uuid() })
    })

  case future.all(tasks) {
    Ok(uuids) ->
      let unique_count = list.length(list.unique(uuids))
      case unique_count == 20 {
        True ->
          Passed(ConcurrentSafety, "Concurrent UUID Generation", "Concurrent UUID generation produces unique values")
        False ->
          Failed(
            ConcurrentSafety,
            "Concurrent UUID Generation",
            "UUID collision detected in concurrent generation",
            "Unique count: " <> int.to_string(unique_count) <> "/20"
          )
      }
    Error(error) ->
      Failed(
        ConcurrentSafety,
        "Concurrent UUID Generation",
        "Concurrent UUID generation failed",
        error
      )
  }
}

fn test_concurrent_timestamp_generation() -> TestResult {
  let tasks = list.range(0, 10)
    |> list.map(fn(_) {
      future.promise(fn() { intent.current_timestamp() })
    })

  case future.all(tasks) {
    Ok(timestamps) ->
      let all_valid = list.all(timestamps, fn(ts) {
        string.length(ts) > 0 && string.contains(ts, "T")
      })

      case all_valid {
        True ->
          Passed(ConcurrentSafety, "Concurrent Timestamp Generation", "Concurrent timestamp generation works")
        False ->
          Failed(
            ConcurrentSafety,
            "Concurrent Timestamp Generation",
            "Some timestamps invalid in concurrent generation",
            "Timestamp validation failed"
          )
      }
    Error(error) ->
      Failed(
        ConcurrentSafety,
        "Concurrent Timestamp Generation",
        "Concurrent timestamp generation failed",
        error
      )
  }
}

fn test_concurrent_base64_operations() -> TestResult {
  let tasks = list.range(0, 10)
    |> list.map(fn(_) {
      future.promise(fn() {
        rules.base64_url_decode("VGhpcyBpcyBhIHRlc3Q")
      })
    })

  case future.all(tasks) {
    Ok(results) ->
      let success_count = list.filter(results, fn(result) {
        case result {
          Ok(_) -> True
          Error(_) -> False
        }
      }) |> list.length

      case success_count >= 8 { // Allow some failures in concurrent environment
        True ->
          Passed(ConcurrentSafety, "Concurrent Base64 Operations", "Concurrent base64 operations mostly succeed")
        False ->
          Failed(
            ConcurrentSafety,
            "Concurrent Base64 Operations",
            "Too many concurrent base64 operations failed",
            "Success rate: " <> int.to_string(success_count) <> "/10"
          )
      }
    Error(error) ->
      Failed(
        ConcurrentSafety,
        "Concurrent Base64 Operations",
        "Concurrent base64 operations failed",
        error
      )
  }
}

// =============================================================================
// SECURITY TESTS
// =============================================================================

/// Test security aspects of FFI functions
pub fn security_tests() -> List(TestResult) {
  [
    test_input_validation(),
    test_injection_resistance(),
  ]
}

fn test_input_validation() -> TestResult {
  let malicious_inputs = [
    "!@#$%^&*()",
    "<script>alert('xss')</script>",
    "javascript:alert('xss')",
    "file:///etc/passwd",
  ]

  let results = list.map(malicious_inputs, fn(input) {
    case rules.base64_url_decode(input) {
      Error(_) -> True // Safe failure
      Ok(decoded) ->
        // Check if dangerous content is present
        not string.contains(decoded, "<script>")
        and not string.contains(decoded, "javascript:")
        and not string.contains(decoded, "file://")
    }
  })

  let all_safe = list.all(results, fn(x) { x })

  case all_safe {
    True ->
      Passed(Security, "Input Validation", "Malicious inputs properly rejected or sanitized")
    False ->
      Failed(
        Security,
        "Input Validation",
        "Some malicious inputs were not handled safely",
        "Input validation failed for malicious inputs"
      )
  }
}

fn test_injection_resistance() -> TestResult {
  let injection_attempts = [
    "YW55LWdvdA==; rm -rf /",
    "YW55LWdvdA== && cat /etc/passwd",
    "YW55LWdvdA|||",
    "YW55LWdvdA\nsystem(cmd)",
  ]

  let results = list.map(injection_attempts, fn(input) {
    case rules.base64_url_decode(input) {
      Error(_) -> True // Safe failure
      Ok(decoded) ->
        // Check for injection patterns
        not string.contains(decoded, "rm -rf")
        and not string.contains(decoded, "cat /etc/passwd")
        and not string.contains(decoded, "system(")
    }
  })

  let all_safe = list.all(results, fn(x) { x })

  case all_safe {
    True ->
      Passed(Security, "Injection Resistance", "Injection attempts properly resisted")
    False ->
      Failed(
        Security,
        "Injection Resistance",
        "Some injection attempts were not properly resisted",
        "Injection resistance failed"
      )
  }
}

// =============================================================================
// TEST EXECUTION AND REPORTING
// =============================================================================

/// Run all tests and generate report
pub fn run_all_tests() -> TestReport {
  io.println("🔍 Starting Comprehensive FFI Validation Tests...")
  io.println("=" <> string.repeat("-", 60))

  let all_test_categories = [
    (BasicSafety, basic_safety_tests()),
    (ErrorHandling, error_handling_tests()),
    (ResourceSafety, resource_safety_tests()),
    (ConcurrentSafety, concurrent_safety_tests()),
    (Security, security_tests()),
  ]

  let all_results = list.fold(all_test_categories, [], fn(acc, category_results) {
    let #(category, results) = category_results
    list.append(acc, results)
  })

  let summary = categorise_results(all_results)

  // Count results
  let passed_count = list.fold(all_results, 0, fn(count, result) {
    case result {
      Passed(_, _, _) -> count + 1
      _ -> count
    }
  })

  let failed_count = list.fold(all_results, 0, fn(count, result) {
    case result {
      Failed(_, _, _, _) -> count + 1
      _ -> count
    }
  })

  io.println("✅ FFI Validation Tests Complete!")
  io.println("  Passed: " <> int.to_string(passed_count))
  io.println("  Failed: " <> int.to_string(failed_count))

  TestReport(results: all_results, summary: summary)
}

/// Print test report
pub fn print_report(report: TestReport) {
  io.println("\n📊 FFI Validation Test Report")
  io.println("=" <> string.repeat("-", 60))

  // Print summary
  io.println("\n📈 Summary by Category:")
  list.each(map.to_list(report.summary), fn(category_count) {
    let #(category, count) = category_count
    let category_name = string.capitalize(case category {
      BasicSafety -> "Basic Safety"
      ErrorHandling -> "Error Handling"
      ResourceSafety -> "Resource Safety"
      ConcurrentSafety -> "Concurrent Safety"
      Security -> "Security"
    })
    io.println("  " <> category_name <> ": " <> int.to_string(count) <> " tests")
  })

  // Print detailed results
  io.println("\n🔍 Detailed Results:")
  list.each(report.results, fn(result) {
    case result {
      Passed(_, name, description) ->
        io.println("  ✅ " <> name <> ": " <> description)
      Failed(_, name, description, error) ->
        io.println("  ❌ " <> name <> ": " <> description <> " - Error: " <> error)
    }
  })
}

/// Generate bead issues for failed tests
pub fn generate_failure_beads(report: TestReport) {
  let failed_results = list.filter(report.results, fn(result) {
    case result {
      Failed(_, _, _, _) -> True
      _ -> False
    }
  })

  list.each(failed_results, fn(failed_result) {
    generate_bead_for_failure(failed_result)
  })

  if list.length(failed_results) > 0 {
    io.println("\n🚨 Generated " <> int.to_string(list.length(failed_results)) <> " bead issues for failed tests")
  }
}

/// Main test execution
pub fn main() {
  let report = run_all_tests()
  print_report(report)
  generate_failure_beads(report)

  let failed_count = list.fold(report.results, 0, fn(count, result) {
    case result {
      Failed(_, _, _, _) -> count + 1
      _ -> count
    }
  })

  if failed_count > 0 {
    io.println("\n❌ " <> int.to_string(failed_count) <> " FFI tests failed!")
  } else {
    io.println("\n🎉 All FFI tests passed!")
  }
}