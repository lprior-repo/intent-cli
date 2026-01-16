//// EARS Parser Tests
////
//// Tests the EARS (Easy Approach to Requirements Syntax) parser that converts
//// structured English requirements into Intent behaviors using 6 patterns:
////
//// 1. **Ubiquitous**: "THE SYSTEM SHALL [behavior]"
//// 2. **Event-Driven**: "WHEN [trigger] THE SYSTEM SHALL [behavior]"
//// 3. **State-Driven**: "WHILE [state] THE SYSTEM SHALL [behavior]"
//// 4. **Optional**: "WHERE [condition] THE SYSTEM SHALL [behavior]"
//// 5. **Unwanted**: "IF [condition] THEN THE SYSTEM SHALL NOT [behavior]"
//// 6. **Complex**: "WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]"
////
//// DbC Postconditions Verified:
//// - Pattern detection is case-insensitive
//// - Optional "the" before "system" works for all patterns
//// - Optional "then" before "shall not" works for Unwanted
//// - Line numbers in errors match input line numbers
//// - Empty lines and comments (#) are ignored
//// - requirements list length + errors list length >= input non-empty/non-comment lines
//// - All requirements have non-empty system_shall (except Unwanted which uses system_shall_not)
//// - trigger/state/condition populated correctly per pattern

import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import intent/kirk/ears_parser

// =============================================================================
// PATTERN 1: UBIQUITOUS - "THE SYSTEM SHALL [behavior]"
// =============================================================================

pub fn parse_ubiquitous_with_the_test() {
  // GIVEN: A ubiquitous requirement with "the" before "system"
  let text = "THE SYSTEM SHALL authenticate users"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed
  result.requirements
  |> list.length()
  |> should.equal(1)

  // THEN: Pattern is Ubiquitous
  case result.requirements {
    [req] -> req.pattern |> should.equal(ears_parser.Ubiquitous)
    _ -> should.fail()
  }

  // THEN: system_shall is extracted
  case result.requirements {
    [req] -> req.system_shall |> should.equal("authenticate users")
    _ -> should.fail()
  }

  // THEN: DbC postcondition - no trigger/state/condition for Ubiquitous
  case result.requirements {
    [req] -> {
      req.trigger |> should.equal(None)
      req.state |> should.equal(None)
      req.condition |> should.equal(None)
    }
    _ -> should.fail()
  }
}

pub fn parse_ubiquitous_without_the_test() {
  // GIVEN: A ubiquitous requirement without "the" before "system"
  let text = "SYSTEM SHALL validate input"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed
  result.requirements
  |> list.length()
  |> should.equal(1)

  // THEN: Pattern is Ubiquitous
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.Ubiquitous)
      req.system_shall |> should.equal("validate input")
    }
    _ -> should.fail()
  }
}

pub fn parse_ubiquitous_case_insensitive_test() {
  // GIVEN: Ubiquitous requirements in different cases
  let text =
    "the system shall do X\nThe System Shall do Y\nTHE SYSTEM SHALL do Z"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - case insensitive parsing
  result.requirements
  |> list.length()
  |> should.equal(3)

  // THEN: All parsed as Ubiquitous
  result.requirements
  |> list.all(fn(req) { req.pattern == ears_parser.Ubiquitous })
  |> should.be_true()
}

// =============================================================================
// PATTERN 2: EVENT-DRIVEN - "WHEN [trigger] THE SYSTEM SHALL [behavior]"
// =============================================================================

pub fn parse_event_driven_with_the_test() {
  // GIVEN: An event-driven requirement with "the" before "system"
  let text = "WHEN user submits form THE SYSTEM SHALL validate input"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed
  result.requirements
  |> list.length()
  |> should.equal(1)

  // THEN: Pattern is EventDriven
  case result.requirements {
    [req] -> req.pattern |> should.equal(ears_parser.EventDriven)
    _ -> should.fail()
  }

  // THEN: DbC postcondition - trigger and behavior extracted
  case result.requirements {
    [req] -> {
      req.trigger |> should.equal(Some("user submits form"))
      req.system_shall |> should.equal("validate input")
    }
    _ -> should.fail()
  }

  // THEN: DbC postcondition - no state/condition for EventDriven
  case result.requirements {
    [req] -> {
      req.state |> should.equal(None)
      req.condition |> should.equal(None)
    }
    _ -> should.fail()
  }
}

pub fn parse_event_driven_without_the_test() {
  // GIVEN: An event-driven requirement without "the" before "system"
  let text = "WHEN error occurs SYSTEM SHALL log error"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed with correct trigger
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.EventDriven)
      req.trigger |> should.equal(Some("error occurs"))
      req.system_shall |> should.equal("log error")
    }
    _ -> should.fail()
  }
}

pub fn parse_event_driven_case_insensitive_test() {
  // GIVEN: Event-driven requirement in mixed case
  let text = "when user CLICKS button the system SHALL respond"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - case insensitive
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.EventDriven)
      req.trigger |> should.equal(Some("user CLICKS button"))
    }
    _ -> should.fail()
  }
}

// =============================================================================
// PATTERN 3: STATE-DRIVEN - "WHILE [state] THE SYSTEM SHALL [behavior]"
// =============================================================================

pub fn parse_state_driven_with_the_test() {
  // GIVEN: A state-driven requirement with "the" before "system"
  let text =
    "WHILE processing payment THE SYSTEM SHALL prevent duplicate submissions"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed
  result.requirements
  |> list.length()
  |> should.equal(1)

  // THEN: Pattern is StateDriven
  case result.requirements {
    [req] -> req.pattern |> should.equal(ears_parser.StateDriven)
    _ -> should.fail()
  }

  // THEN: DbC postcondition - state and behavior extracted
  case result.requirements {
    [req] -> {
      req.state |> should.equal(Some("processing payment"))
      req.system_shall |> should.equal("prevent duplicate submissions")
    }
    _ -> should.fail()
  }

  // THEN: DbC postcondition - no trigger/condition for StateDriven
  case result.requirements {
    [req] -> {
      req.trigger |> should.equal(None)
      req.condition |> should.equal(None)
    }
    _ -> should.fail()
  }
}

pub fn parse_state_driven_without_the_test() {
  // GIVEN: A state-driven requirement without "the" before "system"
  let text = "WHILE user is authenticated SYSTEM SHALL allow access"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed with correct state
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.StateDriven)
      req.state |> should.equal(Some("user is authenticated"))
      req.system_shall |> should.equal("allow access")
    }
    _ -> should.fail()
  }
}

pub fn parse_state_driven_case_insensitive_test() {
  // GIVEN: State-driven requirement in mixed case
  let text = "while SYSTEM is BUSY the system shall queue requests"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - case insensitive
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.StateDriven)
      req.state |> should.equal(Some("SYSTEM is BUSY"))
    }
    _ -> should.fail()
  }
}

// =============================================================================
// PATTERN 4: OPTIONAL - "WHERE [condition] THE SYSTEM SHALL [behavior]"
// =============================================================================

pub fn parse_optional_with_the_test() {
  // GIVEN: An optional requirement with "the" before "system"
  let text =
    "WHERE user has premium status THE SYSTEM SHALL enable advanced features"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed
  result.requirements
  |> list.length()
  |> should.equal(1)

  // THEN: Pattern is Optional
  case result.requirements {
    [req] -> req.pattern |> should.equal(ears_parser.Optional)
    _ -> should.fail()
  }

  // THEN: DbC postcondition - condition and behavior extracted
  case result.requirements {
    [req] -> {
      req.condition |> should.equal(Some("user has premium status"))
      req.system_shall |> should.equal("enable advanced features")
    }
    _ -> should.fail()
  }

  // THEN: DbC postcondition - no trigger/state for Optional
  case result.requirements {
    [req] -> {
      req.trigger |> should.equal(None)
      req.state |> should.equal(None)
    }
    _ -> should.fail()
  }
}

pub fn parse_optional_without_the_test() {
  // GIVEN: An optional requirement without "the" before "system"
  let text = "WHERE feature flag is enabled SYSTEM SHALL show new UI"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed with correct condition
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.Optional)
      req.condition |> should.equal(Some("feature flag is enabled"))
      req.system_shall |> should.equal("show new UI")
    }
    _ -> should.fail()
  }
}

pub fn parse_optional_case_insensitive_test() {
  // GIVEN: Optional requirement in mixed case
  let text = "where DEBUG mode is ON the system shall LOG details"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - case insensitive
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.Optional)
      req.condition |> should.equal(Some("DEBUG mode is ON"))
    }
    _ -> should.fail()
  }
}

// =============================================================================
// PATTERN 5: UNWANTED - "IF [condition] THEN THE SYSTEM SHALL NOT [behavior]"
// =============================================================================

pub fn parse_unwanted_with_then_and_the_test() {
  // GIVEN: An unwanted requirement with "then" and "the"
  let text =
    "IF request lacks authentication THEN THE SYSTEM SHALL NOT expose sensitive data"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed
  result.requirements
  |> list.length()
  |> should.equal(1)

  // THEN: Pattern is Unwanted
  case result.requirements {
    [req] -> req.pattern |> should.equal(ears_parser.Unwanted)
    _ -> should.fail()
  }

  // THEN: DbC postcondition - condition and SHALL NOT behavior extracted
  case result.requirements {
    [req] -> {
      req.condition |> should.equal(Some("request lacks authentication"))
      req.system_shall_not |> should.equal(Some("expose sensitive data"))
      req.system_shall |> should.equal("")
    }
    _ -> should.fail()
  }

  // THEN: DbC postcondition - no trigger/state for Unwanted
  case result.requirements {
    [req] -> {
      req.trigger |> should.equal(None)
      req.state |> should.equal(None)
    }
    _ -> should.fail()
  }
}

pub fn parse_unwanted_without_then_test() {
  // GIVEN: An unwanted requirement without "then"
  let text = "IF input is invalid THE SYSTEM SHALL NOT process request"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed with correct condition
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.Unwanted)
      req.condition |> should.equal(Some("input is invalid"))
      req.system_shall_not |> should.equal(Some("process request"))
    }
    _ -> should.fail()
  }
}

pub fn parse_unwanted_without_the_test() {
  // GIVEN: An unwanted requirement without "the"
  let text = "IF timeout occurs THEN SYSTEM SHALL NOT retry indefinitely"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.Unwanted)
      req.system_shall_not |> should.equal(Some("retry indefinitely"))
    }
    _ -> should.fail()
  }
}

pub fn parse_unwanted_case_insensitive_test() {
  // GIVEN: Unwanted requirement in mixed case
  let text = "if USER not authorized then the system shall NOT grant access"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - case insensitive
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.Unwanted)
      req.condition |> should.equal(Some("USER not authorized"))
    }
    _ -> should.fail()
  }
}

// =============================================================================
// PATTERN 6: COMPLEX - "WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]"
// =============================================================================

pub fn parse_complex_with_the_test() {
  // GIVEN: A complex requirement combining WHILE and WHEN
  let text =
    "WHILE processing transaction WHEN error occurs THE SYSTEM SHALL rollback changes"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed
  result.requirements
  |> list.length()
  |> should.equal(1)

  // THEN: Pattern is Complex
  case result.requirements {
    [req] -> req.pattern |> should.equal(ears_parser.Complex)
    _ -> should.fail()
  }

  // THEN: DbC postcondition - state, trigger, and behavior extracted
  case result.requirements {
    [req] -> {
      req.state |> should.equal(Some("processing transaction"))
      req.trigger |> should.equal(Some("error occurs"))
      req.system_shall |> should.equal("rollback changes")
    }
    _ -> should.fail()
  }

  // THEN: DbC postcondition - no condition for Complex (WHILE+WHEN)
  case result.requirements {
    [req] -> req.condition |> should.equal(None)
    _ -> should.fail()
  }
}

pub fn parse_complex_without_the_test() {
  // GIVEN: A complex requirement without "the"
  let text =
    "WHILE user is logged in WHEN session expires SYSTEM SHALL redirect to login"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: One requirement parsed with state and trigger
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.Complex)
      req.state |> should.equal(Some("user is logged in"))
      req.trigger |> should.equal(Some("session expires"))
    }
    _ -> should.fail()
  }
}

pub fn parse_complex_case_insensitive_test() {
  // GIVEN: Complex requirement in mixed case
  let text = "while SYSTEM busy when REQUEST arrives the system shall QUEUE it"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - case insensitive
  case result.requirements {
    [req] -> {
      req.pattern |> should.equal(ears_parser.Complex)
      req.state |> should.equal(Some("SYSTEM busy"))
      req.trigger |> should.equal(Some("REQUEST arrives"))
    }
    _ -> should.fail()
  }
}

// =============================================================================
// MULTI-LINE PARSING WITH IDs
// =============================================================================

pub fn parse_multiple_requirements_test() {
  // GIVEN: Multiple requirements with different patterns
  let text =
    "THE SYSTEM SHALL authenticate users\n"
    <> "WHEN user submits form THE SYSTEM SHALL validate input\n"
    <> "WHILE processing payment THE SYSTEM SHALL prevent duplicates"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: Three requirements parsed
  result.requirements
  |> list.length()
  |> should.equal(3)

  // THEN: DbC postcondition - IDs are REQ-1, REQ-2, REQ-3
  case result.requirements {
    [req1, req2, req3] -> {
      req1.id |> should.equal("REQ-1")
      req2.id |> should.equal("REQ-2")
      req3.id |> should.equal("REQ-3")
    }
    _ -> should.fail()
  }

  // THEN: Patterns are correct
  case result.requirements {
    [req1, req2, req3] -> {
      req1.pattern |> should.equal(ears_parser.Ubiquitous)
      req2.pattern |> should.equal(ears_parser.EventDriven)
      req3.pattern |> should.equal(ears_parser.StateDriven)
    }
    _ -> should.fail()
  }
}

pub fn parse_empty_lines_ignored_test() {
  // GIVEN: Requirements with empty lines
  let text =
    "\n"
    <> "THE SYSTEM SHALL do X\n"
    <> "\n"
    <> "\n"
    <> "THE SYSTEM SHALL do Y\n"
    <> "\n"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - empty lines ignored, only 2 requirements
  result.requirements
  |> list.length()
  |> should.equal(2)

  // THEN: IDs skip empty lines (REQ-2 and REQ-5 based on line numbers)
  case result.requirements {
    [req1, req2] -> {
      req1.id |> should.equal("REQ-2")
      req2.id |> should.equal("REQ-5")
    }
    _ -> should.fail()
  }
}

pub fn parse_comment_lines_ignored_test() {
  // GIVEN: Requirements with comment lines (starting with #)
  let text =
    "# This is a comment\n"
    <> "THE SYSTEM SHALL do X\n"
    <> "# Another comment\n"
    <> "THE SYSTEM SHALL do Y"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - comments ignored, only 2 requirements
  result.requirements
  |> list.length()
  |> should.equal(2)

  // THEN: IDs skip comment lines
  case result.requirements {
    [req1, req2] -> {
      req1.id |> should.equal("REQ-2")
      req2.id |> should.equal("REQ-4")
    }
    _ -> should.fail()
  }
}

// =============================================================================
// ERROR HANDLING
// =============================================================================

pub fn parse_invalid_line_error_test() {
  // GIVEN: A line that doesn't match any EARS pattern
  let text = "This is not a valid EARS requirement"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: No requirements parsed
  result.requirements
  |> list.length()
  |> should.equal(0)

  // THEN: DbC postcondition - one error reported
  result.errors
  |> list.length()
  |> should.equal(1)

  // THEN: Error has line number and message
  case result.errors {
    [error] -> {
      error.line |> should.equal(1)
      string.contains(
        string.lowercase(error.message),
        "doesn't match any ears pattern",
      )
      |> should.be_true()
    }
    _ -> should.fail()
  }
}

pub fn parse_mixed_valid_and_invalid_test() {
  // GIVEN: Mix of valid and invalid requirements
  let text =
    "THE SYSTEM SHALL do X\n" <> "This is invalid\n" <> "THE SYSTEM SHALL do Y"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - 2 requirements + 1 error
  result.requirements
  |> list.length()
  |> should.equal(2)

  result.errors
  |> list.length()
  |> should.equal(1)

  // THEN: Error has correct line number
  case result.errors {
    [error] -> error.line |> should.equal(2)
    _ -> should.fail()
  }
}

// =============================================================================
// WARNING GENERATION
// =============================================================================

pub fn parse_missing_unwanted_warning_test() {
  // GIVEN: Requirements with no unwanted patterns
  let text = "THE SYSTEM SHALL do X\nTHE SYSTEM SHALL do Y"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - warning about missing unwanted patterns
  result.warnings
  |> list.any(fn(w) {
    string.contains(string.lowercase(w), "shall not")
    || string.contains(string.lowercase(w), "unwanted")
  })
  |> should.be_true()
}

pub fn parse_missing_error_handling_warning_test() {
  // GIVEN: Requirements with no error handling mentions
  let text =
    "THE SYSTEM SHALL process request\nTHE SYSTEM SHALL return response"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - warning about missing error handling
  result.warnings
  |> list.any(fn(w) { string.contains(string.lowercase(w), "error") })
  |> should.be_true()
}

pub fn parse_no_warnings_when_complete_test() {
  // GIVEN: Requirements with unwanted and error handling
  let text =
    "THE SYSTEM SHALL process request\n"
    <> "THE SYSTEM SHALL handle error conditions\n"
    <> "IF timeout occurs THEN THE SYSTEM SHALL NOT retry indefinitely"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: No warnings (has both unwanted and error mentions)
  result.warnings
  |> list.length()
  |> should.equal(0)
}

// =============================================================================
// PATTERN TO STRING CONVERSION
// =============================================================================

pub fn pattern_to_string_ubiquitous_test() {
  ears_parser.pattern_to_string(ears_parser.Ubiquitous)
  |> should.equal("Ubiquitous")
}

pub fn pattern_to_string_event_driven_test() {
  ears_parser.pattern_to_string(ears_parser.EventDriven)
  |> should.equal("Event-Driven")
}

pub fn pattern_to_string_state_driven_test() {
  ears_parser.pattern_to_string(ears_parser.StateDriven)
  |> should.equal("State-Driven")
}

pub fn pattern_to_string_optional_test() {
  ears_parser.pattern_to_string(ears_parser.Optional)
  |> should.equal("Optional")
}

pub fn pattern_to_string_unwanted_test() {
  ears_parser.pattern_to_string(ears_parser.Unwanted)
  |> should.equal("Unwanted")
}

pub fn pattern_to_string_complex_test() {
  ears_parser.pattern_to_string(ears_parser.Complex)
  |> should.equal("Complex")
}

// =============================================================================
// RAW TEXT PRESERVATION
// =============================================================================

pub fn parse_preserves_raw_text_test() {
  // GIVEN: Various requirement patterns
  let text =
    "THE SYSTEM SHALL authenticate users\n"
    <> "WHEN user submits form THE SYSTEM SHALL validate input"

  // WHEN: Parsing
  let result = ears_parser.parse(text)

  // THEN: DbC postcondition - raw_text matches input
  case result.requirements {
    [req1, req2] -> {
      req1.raw_text |> should.equal("THE SYSTEM SHALL authenticate users")
      req2.raw_text
      |> should.equal("WHEN user submits form THE SYSTEM SHALL validate input")
    }
    _ -> should.fail()
  }
}

// =============================================================================
// FORMAT RESULT TESTS
// =============================================================================

pub fn format_result_test() {
  // GIVEN: A parse result
  let text = "THE SYSTEM SHALL authenticate users"
  let result = ears_parser.parse(text)

  // WHEN: Formatting
  let formatted = ears_parser.format_result(result)

  // THEN: Contains header
  string.contains(formatted, "EARS Parser")
  |> should.be_true()

  // THEN: Contains requirement count
  string.contains(formatted, "Parsed: 1")
  |> should.be_true()
}

pub fn format_result_with_errors_test() {
  // GIVEN: A parse result with errors
  let text = "Invalid requirement"
  let result = ears_parser.parse(text)

  // WHEN: Formatting
  let formatted = ears_parser.format_result(result)

  // THEN: Contains error section
  string.contains(formatted, "Errors:")
  |> should.be_true()

  // THEN: Shows error count
  string.contains(formatted, "Errors: 1")
  |> should.be_true()
}

pub fn format_result_with_warnings_test() {
  // GIVEN: A parse result with warnings
  let text = "THE SYSTEM SHALL do something"
  let result = ears_parser.parse(text)

  // WHEN: Formatting
  let formatted = ears_parser.format_result(result)

  // THEN: Contains warning section if warnings exist
  case list.is_empty(result.warnings) {
    False ->
      string.contains(formatted, "Warnings:")
      |> should.be_true()
    True -> True |> should.be_true()
  }
}

// =============================================================================
// TO BEHAVIORS CONVERSION TESTS
// =============================================================================

pub fn to_behaviors_ubiquitous_test() {
  // GIVEN: A parsed ubiquitous requirement
  let text = "THE SYSTEM SHALL authenticate users"
  let result = ears_parser.parse(text)

  // WHEN: Converting to behaviors
  let behaviors = ears_parser.to_behaviors(result)

  // THEN: One behavior created
  behaviors
  |> list.length()
  |> should.equal(1)

  // THEN: Behavior has intent from system_shall
  case behaviors {
    [behavior] ->
      behavior.intent
      |> should.equal("authenticate users")
    _ -> should.fail()
  }
}

pub fn to_behaviors_infers_http_method_test() {
  // GIVEN: Requirements with different verbs
  let text =
    "THE SYSTEM SHALL create a resource\n"
    <> "THE SYSTEM SHALL delete a resource\n"
    <> "THE SYSTEM SHALL update a resource\n"
    <> "THE SYSTEM SHALL return a resource"

  let result = ears_parser.parse(text)

  // WHEN: Converting to behaviors
  let behaviors = ears_parser.to_behaviors(result)

  // THEN: HTTP methods inferred from verbs
  case behaviors {
    [create, delete, update, return] -> {
      create.method |> should.equal("POST")
      delete.method |> should.equal("DELETE")
      update.method |> should.equal("PUT")
      return.method |> should.equal("GET")
    }
    _ -> should.fail()
  }
}

pub fn to_behaviors_generates_names_test() {
  // GIVEN: Different requirement patterns
  let text =
    "THE SYSTEM SHALL validate input\n"
    <> "WHEN user clicks button THE SYSTEM SHALL respond\n"
    <> "WHILE processing SYSTEM SHALL wait"

  let result = ears_parser.parse(text)

  // WHEN: Converting to behaviors
  let behaviors = ears_parser.to_behaviors(result)

  // THEN: Names generated with pattern context
  behaviors
  |> list.all(fn(b) { !string.is_empty(b.name) })
  |> should.be_true()

  // THEN: EventDriven name includes trigger context
  case behaviors {
    [_ubiq, event, _state] ->
      string.contains(event.name, "on")
      |> should.be_true()
    _ -> should.fail()
  }
}

// =============================================================================
// DETERMINISM TESTS
// =============================================================================

pub fn parse_deterministic_test() {
  // GIVEN: The same requirements text
  let text =
    "THE SYSTEM SHALL do X\n"
    <> "WHEN trigger THE SYSTEM SHALL do Y\n"
    <> "IF condition THEN THE SYSTEM SHALL NOT do Z"

  // WHEN: Parsing twice
  let result1 = ears_parser.parse(text)
  let result2 = ears_parser.parse(text)

  // THEN: DbC postcondition - same number of requirements
  list.length(result1.requirements)
  |> should.equal(list.length(result2.requirements))

  // THEN: Same number of errors
  list.length(result1.errors)
  |> should.equal(list.length(result2.errors))

  // THEN: Same number of warnings
  list.length(result1.warnings)
  |> should.equal(list.length(result2.warnings))
}
