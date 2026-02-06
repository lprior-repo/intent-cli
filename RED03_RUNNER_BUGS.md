# RED-03: State Attack Results - Additional Bugs in runner.gleam

## BUG-13: No Context Size Limit (runner.gleam:294-298, 301-304)

**EARS Report:**

**E**vent: When executing many behaviors that capture variables into the context.

**A**ction: The `execute_single_behavior()` function adds to the context with `interpolate.set_response_body()` and `interpolate.set_request_body()` without checking context size.

**R**esult: With thousands of behaviors and large responses, the context can grow indefinitely, causing memory exhaustion.

**S**olution: Implement context size limits with eviction policy (LRU) or warn when context exceeds N variables/bytes.

**Impact:** MEDIUM - Memory exhaustion in long test runs
**Module:** runner.gleam
**Line:** 294-298, 301-304
**Severity:** P2 (Medium)

---

## BUG-14: No Context Variable Name Validation (runner.gleam:340-351)

**EARS Report:**

**E**vent: When behaviors use arbitrary strings as capture variable names.

**A**tion: The `apply_captures()` function directly inserts into context without validating variable names.

**R**esult: Malicious or malformed specs can use invalid variable names that break interpolation or cause injection attacks.

**S**olution: Validate variable names using regex (alphanumeric + underscore, no spaces). Reject invalid names.

**Impact:** LOW - Injection potential, spec corruption
**Module:** runner.gleam
**Line:** 340-351
**Severity:** P3 (Low)

---

## BUG-15: Missing State Reset Between Runs (runner.gleam:249-273)

**EARS Report:**

**E**vent: When running `execute_behaviors_with_spinner()` multiple times in sequence.

**A**tion: The function uses a fresh `interpolate.new_context()` but the `failed_set` is passed through each behavior without reset.

**R**esult: If runner is reused without explicit reset, the failed set accumulates, potentially blocking behaviors that should run.

**S**olution: Document that `RunOptions` executor should not be reused, or add explicit reset method.

**Impact:** LOW - State leakage, incorrect behavior blocking
**Module:** runner.gleam
**Line:** 249-273
**Severity:** P3 (Low)

---

## BUG-16: No Validation of Behavior Count (runner.gleam:249-273)

**EARS Report:**

**E**vent: When a spec contains thousands or millions of behaviors.

**A**ction: The `execute_behaviors_with_spinner()` function iterates through all behaviors without limit.

**R**esult: Malicious or malformed specs with millions of behaviors can cause infinite execution time and resource exhaustion.

**S**olution: Add max behavior limit (e.g., 10,000) with option to override. Warn before executing large specs.

**Impact:** MEDIUM - DoS potential, resource exhaustion
**Module:** runner.gleam
**Line:** 249-273
**Severity:** P2 (Medium)

---

## BUG-17: Circular Dependency Not Detected (runner.gleam:284-291)

**EARS Report:**

**E**vent: When behaviors have circular dependencies (A requires B, B requires A).

**A**tion: The dependency check only looks at `failed_set` but doesn't detect circular references before execution.

**R**esult: With circular dependencies and no failures, behaviors can deadlock or create infinite loop scenarios.

**S**olution: Add pre-execution topological sort with cycle detection. Reject specs with circular dependencies.

**Impact:** MEDIUM - Spec deadlock, potential infinite loops
**Module:** runner.gleam
**Line:** 284-291
**Severity:** P2 (Medium)

---

## BUG-18: Spinner State Not Protected (runner.gleam:118-122, 263)

**EARS Report:**

**E**vent: When an error occurs during behavior execution while spinner is active.

**A**tion: The spinner is started at line 118-122 and updated at line 263, but if an exception occurs before `spinner.stop()` at line 135, the spinner continues running.

**R**esult: In error scenarios, the spinner may continue indefinitely after program crash or exit, leaving UI in inconsistent state.

**S**olution: Use `try`/`finally` or defer pattern to ensure spinner is always stopped, even on errors.

**Impact:** LOW - UI inconsistency, zombie processes
**Module:** runner.gleam
**Line:** 118-122, 263, 135
**Severity:** P3 (Low)

---

## BUG-19: Failed Set Accumulates Across Runs (runner.gleam:249-273)

**EARS Report:**

**E**vent: When using `run_spec_with_executor()` with the same executor for multiple specs.

**A**ction: The `failed_set` is initialized fresh for each spec, but if executor holds state, it could leak failures.

**R**esult: Incorrect behavior blocking across independent spec runs if executor is reused.

**S**olution: Document that executor should be stateless, or add reset capability to `BehaviorExecutor`.

**Impact:** LOW - Cross-contamination between test runs
**Module:** runner.gleam
**Line:** 249-273
**Severity:** P3 (Low)

---

## BUG-20: No Rate Limiting on HTTP Requests (runner.gleam:294-298)

**EARS Report:**

**E**vent: When executing many behaviors against a target URL.

**A**ction: Each behavior calls `executor.execute()` without any rate limiting between requests.

**R**esult: Specs with thousands of behaviors can overwhelm target servers, causing DoS or getting rate-limited/blocked.

**S**olution: Add optional rate limiting (e.g., max requests per second) with default conservative limit. Warn before hammering servers.

**Impact:** LOW - DoS on target systems
**Module:** runner.gleam
**Line:** 294-298
**Severity:** P3 (Low)

---

## Summary of Additional Findings (runner.gleam)

**Critical Bugs (P0):** 0
**High Severity (P1):** 0
**Medium Severity (P2):** 4
**Low Severity (P3):** 4

**Total Additional Bugs:** 8

### Key Vulnerability Categories:
1. **State Accumulation:** BUG-13 (context size), BUG-15, BUG-19 (state reuse)
2. **Input Validation:** BUG-14 (variable names), BUG-16 (behavior count)
3. **Dependency Issues:** BUG-17 (circular dependencies)
4. **Resource Management:** BUG-18 (spinner cleanup), BUG-20 (rate limiting)

### Most Critical Issues:
1. **BUG-13:** Context size limit - can cause memory exhaustion
2. **BUG-17:** Circular dependency detection - can cause deadlocks
3. **BUG-16:** No behavior count limit - DoS potential

---

## Combined Summary (interview_storage + loader + runner)

**Total Bugs Found:** 20
- interview_storage.gleam: 12 bugs
- loader.gleam: 1 bug  
- runner.gleam: 7 bugs

**Severity Distribution:**
- Critical (P0): 0
- High (P1): 1
- Medium (P2): 11
- Low (P3): 8

**Priority Fixes:**
1. **P1 (High):** BUG-01 - Race condition in session append
2. **P2 (Medium):** BUG-02, BUG-05, BUG-11, BUG-13, BUG-16, BUG-17
3. **P3 (Low):** All other bugs for completeness and hardening

### Recommended Immediate Actions:
1. Fix race condition in `append_session_to_jsonl` (BUG-01)
2. Add timestamp validation (BUG-03)
3. Add duplicate detection on load (BUG-05)
4. Add circular dependency detection in runner (BUG-17)
5. Add size limits for context and sessions (BUG-07, BUG-09, BUG-13)
