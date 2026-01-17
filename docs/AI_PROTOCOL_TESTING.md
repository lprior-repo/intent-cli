# AI Protocol Testing Checklist

This document provides test cases and validation criteria for AI agents implementing the Intent Interview Protocol.

## Test Suite Overview

### Test Categories

1. **Happy Path Tests** - Complete interview flows
2. **Error Handling Tests** - Validation and recovery
3. **Edge Case Tests** - Boundary conditions
4. **Protocol Compliance Tests** - JSON schema validation
5. **Performance Tests** - Timeout and efficiency

---

## Happy Path Tests

### HP-001: Complete API Interview (5 Rounds)

**Description:** Execute a full interview for the `api` profile

**Steps:**

1. Start interview: `intent interview --cue --profile api`
2. Verify response `action == "ask_question"`
3. Extract `session.id` from response
4. For each question (25 total):
   - Submit answer with session ID
   - Verify response is question or completion
   - Track progress.percent_complete increases
5. Verify final response `action == "interview_complete"`
6. Verify `spec_path` file exists

**Expected Results:**

- Total questions: ~25
- Rounds completed: 5
- Final stage: "complete"
- Generated spec file at `.interview/spec-<session-id>.cue`

**Pass Criteria:**

- All responses are valid JSON
- No validation errors
- Spec file is valid CUE
- All 5 rounds completed

---

### HP-002: Complete CLI Interview

**Description:** Execute a full interview for the `cli` profile

**Steps:** Same as HP-001 but with `--profile cli`

**Expected Results:**

- Questions specific to CLI design (commands, flags, exit codes)
- Spec includes CLI-specific behaviors

---

### HP-003: Resume Interrupted Session

**Description:** Test session persistence and resume capability

**Steps:**

1. Start interview: `intent interview --cue --profile api`
2. Answer 5 questions
3. Extract `session.id`
4. Simulate disconnect
5. Resume: `intent interview --cue --session <session-id>`
6. Verify response is question #6 (not question #1)
7. Complete remaining questions

**Expected Results:**

- Session resumes at correct question
- Previous answers preserved
- No duplicate questions

---

### HP-004: EARS Pattern Compliance

**Description:** Verify EARS pattern hints are correct

**Steps:**

1. Start interview for any profile
2. For each question:
   - Extract `question.pattern`
   - Extract `question.hint`
   - Extract `question.examples`
   - Verify hint matches pattern:
     - `ubiquitous` → "THE SYSTEM SHALL"
     - `event_driven` → "WHEN ... THE SYSTEM SHALL"
     - `state_driven` → "WHILE ... THE SYSTEM SHALL"
     - etc.
3. Submit answers following the pattern

**Expected Results:**

- All hints match their patterns
- Examples follow the pattern
- Answers following patterns are accepted

---

## Error Handling Tests

### EH-001: Answer Too Short

**Description:** Submit answer below minimum length

**Steps:**

1. Start interview
2. Submit answer with 1-2 characters: `--answer "ok"`
3. Verify response `action == "validation_error"`
4. Verify `error.code == "ANSWER_TOO_SHORT"`
5. Verify `error.retry_allowed == true`
6. Resubmit with longer answer
7. Verify interview continues

**Expected Results:**

```json
{
  "action": "validation_error",
  "error": {
    "code": "ANSWER_TOO_SHORT",
    "message": "Answer too short",
    "suggestion": "Please provide a more detailed response (minimum 3 characters)",
    "retry_allowed": true,
    "context": {
      "answer_length": 2,
      "minimum_length": 3
    }
  }
}
```

---

### EH-002: Invalid Session ID

**Description:** Attempt to answer with non-existent session

**Steps:**

1. Submit answer without starting interview:
   ```bash
   intent interview --cue --session "invalid-session-id" --answer "test"
   ```
2. Verify response `action == "validation_error"`
3. Verify `error.code == "SESSION_NOT_FOUND"`
4. Verify `error.retry_allowed == false`

**Expected Results:**

```json
{
  "action": "validation_error",
  "error": {
    "code": "SESSION_NOT_FOUND",
    "message": "Session not found: invalid-session-id",
    "suggestion": "Start a new interview with 'intent interview --cue --profile <profile>'",
    "retry_allowed": false
  }
}
```

---

### EH-003: Answer Without Session

**Description:** Submit answer without session ID

**Steps:**

1. Attempt: `intent interview --cue --answer "test"`
2. Verify error response

**Expected Results:**

- Error message: "--answer requires --session flag"
- Exit code: non-zero

---

### EH-004: Invalid Profile

**Description:** Start interview with unknown profile

**Steps:**

1. Attempt: `intent interview --cue --profile invalid_profile`
2. Verify error response

**Expected Results:**

```json
{
  "action": "validation_error",
  "error": {
    "code": "PROFILE_UNKNOWN",
    "message": "Unknown profile 'invalid_profile'",
    "suggestion": "Valid profiles: api, cli, event, data, workflow, ui",
    "retry_allowed": false
  }
}
```

---

### EH-005: Submit to Completed Session

**Description:** Attempt to answer after interview completion

**Steps:**

1. Complete a full interview
2. Extract session ID
3. Attempt to submit another answer
4. Verify error response

**Expected Results:**

```json
{
  "action": "validation_error",
  "error": {
    "code": "SESSION_COMPLETE",
    "message": "Interview already complete",
    "suggestion": "Review generated spec or start a new interview",
    "retry_allowed": false
  }
}
```

---

## Edge Case Tests

### EC-001: Shell Special Characters in Answer

**Description:** Verify proper escaping of special characters

**Steps:**

1. Submit answer with quotes, backslashes, newlines:
   ```bash
   --answer "THE SYSTEM SHALL validate \"email\" and 'password' fields with regex: [a-z]{3,}"
   ```
2. Verify answer is accepted
3. Verify answer is stored correctly in session

**Expected Results:**

- Answer accepted without escaping errors
- Special characters preserved in session storage

---

### EC-002: Very Long Answer

**Description:** Submit answer with 1000+ characters

**Steps:**

1. Generate a detailed 1500-character answer
2. Submit answer
3. Verify acceptance

**Expected Results:**

- Answer accepted (no max length limit)
- Full answer stored
- No truncation

---

### EC-003: Empty Context/Example Fields

**Description:** Handle questions with empty optional fields

**Steps:**

1. Progress through interview
2. Encounter question with empty `context` or `example`
3. Verify JSON is valid (empty strings, not null)
4. Verify interview continues

**Expected Results:**

- Empty fields are empty strings `""`
- No JSON parsing errors

---

### EC-004: Concurrent Sessions

**Description:** Run multiple interviews in parallel

**Steps:**

1. Start interview A: `--profile api`
2. Start interview B: `--profile cli`
3. Submit answers to both sessions interleaved
4. Complete both interviews

**Expected Results:**

- Each session maintains separate state
- No cross-session contamination
- Both specs generated correctly

---

### EC-005: Rapid Answer Submission

**Description:** Submit answers as fast as possible

**Steps:**

1. Start interview
2. Submit 25 answers in rapid succession (< 1 second apart)
3. Verify all answers accepted
4. Verify interview completes

**Expected Results:**

- All answers processed correctly
- No race conditions
- Session state consistent

---

## Protocol Compliance Tests

### PC-001: JSON Schema Validation (Ask Question)

**Description:** Validate question response against schema

**Steps:**

1. Start interview
2. Parse JSON response
3. Validate against JSON schema (see AI_PROTOCOL.md Appendix)

**Required Fields:**

- `action` (string)
- `question.text` (string)
- `question.id` (string, pattern: `^r[1-5]-.+$`)
- `question.round` (integer, 1-5)
- `question.perspective` (enum)
- `question.category` (enum)
- `question.priority` (enum)
- `question.context` (string)
- `question.example` (string)
- `question.pattern` (enum)
- `question.hint` (string)
- `question.examples` (array of strings)
- `question.extract_into` (array)
- `question.expected_type` (string)
- `progress.current_step` (integer)
- `progress.total_steps` (integer)
- `progress.percent_complete` (integer, 0-100)
- `progress.round` (integer)
- `progress.round_name` (string)
- `progress.category` (string)
- `session.id` (string, pattern: `^interview-.+$`)
- `session.profile` (enum)
- `session.started_at` (ISO 8601 timestamp)

---

### PC-002: JSON Schema Validation (Completion)

**Description:** Validate completion response against schema

**Steps:**

1. Complete interview
2. Parse JSON response
3. Validate against completion schema

**Required Fields:**

- `action == "interview_complete"`
- `output.spec_path` (string)
- `output.behaviors_count` (integer)
- `output.anti_patterns_count` (integer)
- `output.summary` (string)
- `output.next_steps` (array)
- `session.id` (string)
- `session.profile` (string)
- `session.started_at` (ISO timestamp)
- `session.completed_at` (ISO timestamp)
- `statistics.total_questions` (integer)
- `statistics.total_answers` (integer)
- `statistics.rounds_completed` (integer, should be 5)
- `statistics.gaps_detected` (integer)
- `statistics.conflicts_detected` (integer)
- `statistics.average_confidence` (float, 0-1)

---

### PC-003: JSONL Format Validation

**Description:** Verify output is valid JSONL (one JSON per line)

**Steps:**

1. Capture stdout for each CLI invocation
2. Verify output is a single JSON object (no newlines inside)
3. Verify output ends with newline (optional but standard)
4. Verify multiple invocations produce separate lines

**Expected Results:**

- Each invocation outputs exactly one line
- Each line is valid JSON
- No JSON arrays wrapping multiple objects

---

### PC-004: Progress Monotonicity

**Description:** Verify progress always increases

**Steps:**

1. Start interview
2. For each question, track:
   - `progress.current_step`
   - `progress.percent_complete`
3. Verify values never decrease

**Expected Results:**

- `current_step` increases by 1 each time
- `percent_complete` increases monotonically
- `current_step` reaches `total_steps` at completion

---

### PC-005: Round Progression

**Description:** Verify round numbers progress correctly

**Steps:**

1. Complete full interview
2. Track `question.round` for each question
3. Verify rounds progress: 1 → 2 → 3 → 4 → 5
4. Verify no skipping or reversing

**Expected Results:**

- All rounds 1-5 appear in order
- No duplicate round numbers in sequence
- Round 5 is the last round

---

## Performance Tests

### PERF-001: Response Time

**Description:** Measure CLI response time

**Steps:**

1. Time each CLI invocation:
   ```bash
   time intent interview --cue --profile api
   ```
2. Measure:
   - Initial question response
   - Answer submission response
   - Completion response

**Expected Results:**

- Initial question: < 500ms
- Answer submission: < 300ms
- Completion: < 1s (includes spec generation)

---

### PERF-002: Session Load Time

**Description:** Measure session resume performance

**Steps:**

1. Create session with 20 answers
2. Time resume operation:
   ```bash
   time intent interview --cue --session <id>
   ```

**Expected Results:**

- Resume time: < 500ms
- No performance degradation with large session

---

### PERF-003: Large Session Storage

**Description:** Verify storage efficiency

**Steps:**

1. Complete 10 full interviews
2. Measure `.interview/sessions.jsonl` file size
3. Verify no duplicate data

**Expected Results:**

- File size: < 100KB per session (reasonable)
- One line per session (no duplicates)

---

## Integration Tests

### INT-001: Generated Spec Validation

**Description:** Verify generated spec is valid CUE

**Steps:**

1. Complete interview
2. Extract `spec_path` from completion response
3. Validate with: `intent validate <spec_path>`

**Expected Results:**

- CUE validation passes
- Spec contains behaviors matching answers
- No syntax errors

---

### INT-002: Spec to Test Conversion

**Description:** Verify spec can be used for testing

**Steps:**

1. Complete interview
2. Run checks: `intent check <spec_path> --target http://example.com`
3. Verify spec is executable

**Expected Results:**

- Spec loads successfully
- Behaviors are testable
- No runtime errors

---

### INT-003: Session History Persistence

**Description:** Verify session persists across restarts

**Steps:**

1. Start interview, answer 5 questions
2. Extract session ID
3. Terminate CLI (Ctrl+C or kill)
4. Resume session: `intent interview --cue --session <id>`
5. Verify continues at question 6

**Expected Results:**

- Session state preserved in JSONL
- No data loss
- Resume continues correctly

---

## Automation Test Script Example

### Python Test Runner

```python
#!/usr/bin/env python3
import subprocess
import json
import sys

def run_cli(args):
    """Run intent CLI and return parsed JSON response"""
    result = subprocess.run(
        ["intent", "interview", "--cue"] + args,
        capture_output=True,
        text=True
    )

    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError as e:
        print(f"Failed to parse JSON: {result.stdout}")
        raise e

def test_hp001_complete_interview():
    """Test: Complete API interview"""
    print("Running HP-001: Complete API Interview")

    # Start interview
    response = run_cli(["--profile", "api"])
    assert response["action"] == "ask_question", "First response should be question"

    session_id = response["session"]["id"]
    total_steps = response["progress"]["total_steps"]

    print(f"Session: {session_id}, Total steps: {total_steps}")

    # Answer all questions
    for step in range(total_steps):
        # Generate answer (simple for testing)
        answer = f"THE SYSTEM SHALL perform action {step + 1}"

        response = run_cli(["--session", session_id, "--answer", answer])

        if response["action"] == "interview_complete":
            print(f"Interview completed at step {step + 1}")
            assert response["statistics"]["rounds_completed"] == 5
            assert response["output"]["spec_path"].endswith(".cue")
            print("✓ HP-001 PASSED")
            return True
        elif response["action"] == "ask_question":
            current = response["progress"]["current_step"]
            print(f"  Progress: {current}/{total_steps}")
        else:
            print(f"Unexpected action: {response['action']}")
            return False

    print("✗ HP-001 FAILED: Interview did not complete")
    return False

def test_eh001_answer_too_short():
    """Test: Error handling for short answer"""
    print("Running EH-001: Answer Too Short")

    response = run_cli(["--profile", "api"])
    session_id = response["session"]["id"]

    # Submit very short answer
    response = run_cli(["--session", session_id, "--answer", "ok"])

    assert response["action"] == "validation_error", "Should return validation error"
    assert response["error"]["code"] == "ANSWER_TOO_SHORT", "Error code should be ANSWER_TOO_SHORT"
    assert response["error"]["retry_allowed"] == True, "Retry should be allowed"

    print("✓ EH-001 PASSED")
    return True

# Run all tests
if __name__ == "__main__":
    tests = [
        test_hp001_complete_interview,
        test_eh001_answer_too_short,
        # Add more tests...
    ]

    passed = 0
    failed = 0

    for test in tests:
        try:
            if test():
                passed += 1
            else:
                failed += 1
        except Exception as e:
            print(f"✗ Test {test.__name__} FAILED with exception: {e}")
            failed += 1

    print(f"\nResults: {passed} passed, {failed} failed")
    sys.exit(0 if failed == 0 else 1)
```

---

## Test Data Sets

### Sample Answers by Profile

#### API Profile Answers

```json
{
  "r1-user-api-1": "THE SYSTEM SHALL authenticate users via JWT tokens",
  "r1-user-api-2": "Mobile apps and web frontends will use this API to access protected resources",
  "r1-user-api-3": "WHEN client sends POST /auth/login THE SYSTEM SHALL validate credentials",
  "r1-dev-api-1": "User entity (id, email, password_hash), Token entity (token, user_id, expires_at)",
  "r1-security-api-1": "JWT tokens with HS256 signing, 15-minute access tokens, 7-day refresh tokens"
}
```

#### CLI Profile Answers

```json
{
  "r1-user-cli-1": "intent check --file spec.cue --target http://api.example.com",
  "r1-user-cli-2": "API test engineers and DevOps teams testing HTTP endpoints",
  "r1-dev-cli-1": "check, validate, generate, interview, beads, quality, invert, coverage"
}
```

---

## Continuous Testing

### CI/CD Integration

```yaml
# .github/workflows/protocol-tests.yml
name: AI Protocol Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Setup Gleam
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
          gleam-version: '1.0.0'

      - name: Build Intent CLI
        run: |
          gleam build
          gleam run -- version

      - name: Run Protocol Tests
        run: |
          python3 tests/protocol_tests.py

      - name: Validate Generated Specs
        run: |
          for spec in .interview/spec-*.cue; do
            gleam run -- validate $spec
          done
```

---

## Test Coverage Goals

### Minimum Coverage

- ✅ All 6 profiles tested (api, cli, event, data, workflow, ui)
- ✅ All 5 rounds completed for at least one profile
- ✅ All error codes tested (7 error types)
- ✅ EARS patterns validated (6 patterns)
- ✅ Session persistence tested (save/load/resume)
- ✅ JSON schema compliance (100% of fields)

### Extended Coverage

- ✅ All profiles × 5 rounds = 30 complete interviews
- ✅ Edge cases (special characters, long answers, empty fields)
- ✅ Concurrent sessions (parallel interviews)
- ✅ Performance benchmarks (response times < thresholds)
- ✅ Integration with downstream commands (validate, check)

---

## Troubleshooting Guide

### Common Test Failures

#### "Session not found" after 10 seconds

**Cause:** Session may have been cleaned up or not written to disk

**Fix:** Check `.interview/sessions.jsonl` exists and is writable

---

#### JSON parse error

**Cause:** CLI output contains non-JSON text (debug messages, warnings)

**Fix:** Ensure CLI is in `--cue` mode, no other flags that produce text output

---

#### Progress not increasing

**Cause:** Same question returned multiple times

**Fix:** Verify answer is being saved (check JSONL), may be a storage issue

---

#### Spec file not found after completion

**Cause:** Spec generation failed silently

**Fix:** Check CLI error output, verify disk space, check permissions

---

**End of Testing Checklist**
