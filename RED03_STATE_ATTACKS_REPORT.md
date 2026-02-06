# RED-03: State Attack Results - Bug Reports

## BUG-01: Race Condition in Session Append (interview_storage.gleam:615-646)

**EARS Report:**

**E**vent: When multiple processes call `append_session_to_jsonl()` simultaneously with the same session ID.

**A**ction: Each process reads the entire JSONL file, filters out the old session entry, appends the new session, and writes back the entire file.

**R**esult: If Process A reads the file, then Process B reads the file, then both filter and write back, the last writer wins and the other's changes are lost.

**S**olution: Implement file locking or use atomic append operations. Consider using SQLite with proper transaction isolation instead of JSONL for concurrent writes.

**Impact:** HIGH - Data loss in multi-process scenarios
**Module:** interview_storage.gleam
**Line:** 615-646
**Severity:** P1 (High)

---

## BUG-02: Silent Corruption in JSONL Parsing (interview_storage.gleam:664-671)

**EARS Report:**

**E**vent: When a JSONL file contains both valid and invalid JSON lines.

**A**ction: The `list_sessions_from_jsonl()` function uses `result.map_error(fn(_) { Nil })` to silently skip invalid lines.

**R**esult: Corrupted data is silently discarded without warning. Users don't know their data has been partially lost.

**S**olution: Log warnings when invalid JSON is encountered. Provide a `--strict` mode that fails the entire load if any line is invalid.

**Impact:** MEDIUM - Data corruption goes undetected
**Module:** interview_storage.gleam
**Line:** 664-671
**Severity:** P2 (Medium)

---

## BUG-03: No Validation of Timestamp Format (interview_storage.gleam:801-857)

**EARS Report:**

**E**vent: When a session record contains invalid timestamp strings (e.g., "not-a-date", "also-not-a-date").

**A**ction: The `session_decoder` accepts any string for `created_at`, `updated_at`, and `completed_at` fields without validating ISO 8601 format.

**R**esult: Invalid timestamps are stored and can cause issues when sorting sessions or calculating time-based metrics.

**S**olution: Add timestamp validation using a regex or ISO 8601 parser in the decoder. Reject sessions with malformed timestamps.

**Impact:** MEDIUM - Data integrity issues with time-based operations
**Module:** interview_storage.gleam
**Line:** 801-857
**Severity:** P2 (Medium)

---

## BUG-04: Case Sensitive Stage Matching (interview_storage.gleam:827-834)

**EARS Report:**

**E**vent: When a session record uses different case for stage values (e.g., "Discovery" vs "discovery").

**A**ction: The `session_decoder` uses exact string matching which is case-sensitive: `"Discovery"` vs `"discovery"`.

**R**esult: Session records with proper casing but different case are rejected, causing interoperability issues with external tools.

**S**olution: Normalize stage strings to lowercase before comparison, or use case-insensitive matching.

**Impact:** LOW - Interoperability issues, data loss risk
**Module:** interview_storage.gleam
**Line:** 827-834
**Severity:** P3 (Low)

---

## BUG-05: No Deduplication When Loading Sessions (interview_storage.gleam:649-675)

**EARS Report:**

**E**vent: When a JSONL file contains duplicate session IDs (e.g., from manual editing or partial writes).

**A**ction: The `list_sessions_from_jsonl()` function loads all sessions without checking for duplicate IDs.

**R**esult: Multiple sessions with the same ID can exist in memory, causing confusion and potential bugs in downstream code that expects unique IDs.

**S**olution: When loading sessions, detect duplicates and either keep the latest (based on `updated_at`) or reject the entire load.

**Impact:** MEDIUM - Data inconsistency, duplicate sessions in memory
**Module:** interview_storage.gleam
**Line:** 649-675
**Severity:** P2 (Medium)

---

## BUG-06: Empty ID String Not Validated (interview_storage.gleam:803)

**EARS Report:**

**E**vent: When a session record has an empty string for the `id` field.

**A**ction: The `session_decoder` accepts empty strings as valid session IDs.

**R**esult: Sessions with empty IDs can be created, making it impossible to retrieve them by ID and breaking ID-based lookups.

**S**olution: Validate that `id` field is non-empty in the decoder. Return error if ID is empty or whitespace-only.

**Impact:** MEDIUM - Data integrity, session lookup failures
**Module:** interview_storage.gleam
**Line:** 803
**Severity:** P2 (Medium)

---

## BUG-07: No Limit on Session ID Length (interview_storage.gleam:803)

**EARS Report:**

**E**vent: When a session record has an extremely long ID (e.g., 1MB or more).

**A**ction: The `session_decoder` accepts arbitrarily long ID strings without validation.

**R**esult: Malicious or malformed data can create sessions with massive IDs, potentially causing memory exhaustion or performance issues.

**S**olution: Add a reasonable length limit (e.g., 256 characters) on session IDs.

**Impact:** LOW - DoS potential, memory exhaustion
**Module:** interview_storage.gleam
**Line:** 803
**Severity:** P3 (Low)

---

## BUG-08: No Line Length Validation in JSONL (interview_storage.gleam:664)

**EARS Report:**

**E**vent: When a JSONL file contains extremely long lines (e.g., 10MB single line).

**A**ction: The code reads each line without length validation.

**R**esult: Malicious or malformed JSONL files can cause memory exhaustion by loading massive single lines.

**S**olution: Add maximum line length validation (e.g., 10MB) before parsing JSON.

**Impact:** LOW - DoS potential, memory exhaustion
**Module:** interview_storage.gleam
**Line:** 664
**Severity:** P3 (Low)

---

## BUG-09: No Maximum Sessions Limit (interview_storage.gleam:649-675)

**EARS Report:**

**E**vent: When a JSONL file contains thousands or millions of sessions.

**A**ction: The code loads all sessions into memory without limit.

**R**esult: Massive JSONL files can cause memory exhaustion and system crashes.

**S**olution: Add optional limit parameter or pagination. Warn when loading more than N sessions.

**Impact:** MEDIUM - DoS potential, memory exhaustion
**Module:** interview_storage.gleam
**Line:** 649-675
**Severity:** P2 (Medium)

---

## BUG-10: Missing File Error Not Specific (loader.gleam:25-27)

**EARS Report:**

**E**vent: When attempting to load a spec file that doesn't exist.

**A**ction: The `load_spec()` function returns `FileNotFound` for empty paths, but this is the same error as non-existent files.

**R**esult: Can't distinguish between user error (empty path) and system error (file not found).

**S**olution: Create separate error types for empty path vs. file not found to provide better user feedback.

**Impact:** LOW - Poor error messaging
**Module:** loader.gleam
**Line:** 25-27
**Severity:** P3 (Low)

---

## BUG-11: No Concurrent Read Protection (interview_storage.gleam:652-656)

**EARS Report:**

**E**vent: When reading a JSONL file while another process is writing to it.

**A**ction: The `list_sessions_from_jsonl()` function reads the file without locking.

**R**esult: Can read partial/corrupted data if a write is in progress (especially since `append_session_to_jsonl` reads, filters, and writes the entire file).

**S**olution: Implement file locking or use atomic reads that handle partial writes gracefully.

**Impact:** MEDIUM - Data corruption during concurrent access
**Module:** interview_storage.gleam
**Line:** 652-656
**Severity:** P2 (Medium)

---

## BUG-12: Missing Field Validation Incomplete (interview_storage.gleam:839-856)

**EARS Report:**

**E**vent: When parsing session JSON, some required fields have error suppression but others don't.

**A**ction: Lines 822-824 and 839-842 use `result.map_error(fn(_) { [])` to suppress errors for `completed_at` and `raw_notes`, but `id`, `profile`, `stage` are strict.

**R**esult: Inconsistent validation - some fields can be missing without error, others cause failure.

**S**olution: Document which fields are optional vs. required clearly. Make validation consistent.

**Impact:** LOW - Confusing behavior, potential bugs
**Module:** interview_storage.gleam
**Line:** 822-824, 839-842
**Severity:** P3 (Low)

---

## Summary of Findings

**Critical Bugs (P0):** 0
**High Severity (P1):** 1
**Medium Severity (P2):** 7
**Low Severity (P3):** 4

**Total Bugs Found:** 12

### Key Vulnerability Categories:
1. **Race Conditions:** BUG-01, BUG-11 (concurrent write/read)
2. **Silent Failures:** BUG-02 (invalid JSON silently skipped)
3. **Data Validation:** BUG-03, BUG-04, BUG-05, BUG-06, BUG-12
4. **DoS/Memory:** BUG-07, BUG-08, BUG-09 (no limits on sizes)
5. **Error Handling:** BUG-10 (poor error messaging)

### Most Critical Issues:
1. **BUG-01:** Race condition in session append - HIGH priority fix needed
2. **BUG-02:** Silent corruption - needs logging/warning system
3. **BUG-05:** No duplicate detection - causes data inconsistency
