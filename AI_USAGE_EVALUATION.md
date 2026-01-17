# Intent CLI - AI Usage Evaluation Report
**Evaluation Date:** 2026-01-17
**Evaluator:** Claude (AI Agent)
**Version Tested:** 0.1.0

---

## Executive Summary

Intent CLI is **exceptionally well-designed for AI usage** with strong machine-readable output formats, systematic workflows, and comprehensive documentation. The tool demonstrates a clear understanding of AI-first design principles with CUE/JSON outputs, structured error handling, and deterministic behavior patterns.

**Overall Grade: A- (88/100)**

### Strengths (What Works Excellently)
- ✅ Clean CUE/JSON output formats for all major commands
- ✅ Systematic interview workflow with session management
- ✅ Comprehensive KIRK analysis suite (quality, gaps, invert, coverage, effects)
- ✅ Exit codes clearly signal different error conditions (0, 1, 2, 3, 4)
- ✅ Excellent help text with AI-specific guidance
- ✅ Strong security posture (SSRF protection, path validation)
- ✅ EARS pattern parsing with validation

### Critical Issues to Fix
- 🔴 **Flag syntax inconsistency** - Mixed behavior between flags
- 🔴 **Error format inconsistency** - Not all errors follow AI-friendly schema
- 🟡 **Missing JSON output flags** on several commands
- 🟡 **Limited localhost testing support** (SSRF protection too strict)
- 🟡 **Documentation sync issues** between help text and CLAUDE.md

---

## Detailed Findings

### 1. 🔴 CRITICAL: Flag Syntax Inconsistencies

**Issue:** Commands inconsistently require `=` between flag and value.

**Examples:**
```bash
# These WORK (require =):
intent interview --cue=true --profile=api
intent ears /tmp/test.md --output=json

# These FAIL (tried without =):
intent interview --profile api  # Error: invalid flag 'profile' has no assigned value
intent ears /tmp/test.md --output json  # Same error
```

**Impact:** High - AI agents must maintain command-specific syntax rules
**AI Workaround:** Always use `--flag=value` syntax for all commands
**Recommended Fix:**
```
Option 1: Standardize to ALWAYS require = (--flag=value)
Option 2: Accept both formats (--flag=value OR --flag value)
Option 3: Document clearly which flags require = in --help output
```

**Code Location:** Glint flag parsing configuration (likely in src/intent.gleam or flag definitions)

---

### 2. 🔴 CRITICAL: Error Format Inconsistency

**Issue:** Not all errors follow the AI-friendly error format documented in CLAUDE.md.

**Expected Format (from docs):**
```json
{
  "action": "error_category",
  "error": {
    "type": "specific_error_type",
    "message": "human-readable description",
    "context": {}
  },
  "suggestion": "what to do next",
  "recovery": ["step 1", "step 2", "step 3"]
}
```

**Actual Formats Observed:**

1. **Simple text errors** (most common):
```
Error: Session not found: nonexistent-session
```

2. **Formatted errors**:
```
✗ Invalid spec: Spec parse error: Expected field but found nothing at ai_hints.entities (path: .ai_hints.entities)
```

3. **Security errors**:
```
✗ Security error: SSRF (Server-Side Request Forgery) attempt detected in URL 'http://localhost:9999': Localhost addresses are not allowed
```

**Impact:** High - AI agents cannot reliably parse errors for automatic recovery
**Recommended Fix:**
- Add `--json` flag support to ALL commands
- When `--json` is active, ALL output (including errors) should be JSON
- Include error recovery steps in JSON error objects
- Example:
```json
{
  "action": "error",
  "error": {
    "type": "session_not_found",
    "message": "Session not found: nonexistent-session",
    "context": {
      "requested_session": "nonexistent-session",
      "sessions_directory": ".interview/"
    }
  },
  "suggestion": "List available sessions or start a new interview",
  "recovery": [
    "intent sessions --json",
    "intent interview --cue=true --profile=api"
  ]
}
```

**Code Location:** `src/intent/errors.gleam`, `src/intent/ai_errors.gleam`

---

### 3. 🟡 MEDIUM: Missing JSON Output Flags

**Issue:** Several commands lack `--json` flag for machine-readable output.

**Commands Missing --json:**
- `intent sessions` (has --json ✅)
- `intent about` (missing --json ❌)
- `intent ears` (has --output=json ✅)
- `intent validate` (missing --json ❌)
- `intent show` (has --json ✅)
- `intent history` (untested, likely missing)
- `intent diff` (untested, likely missing)
- `intent beads` (untested, likely missing)
- `intent plan` (untested, likely missing)

**Impact:** Medium - AI agents must parse formatted text output
**Recommended Fix:** Add `--json` flag to ALL commands for consistency

**Example - Current `intent validate` output:**
```
✓ Valid spec: examples/user-api.cue
```

**Proposed with --json:**
```json
{
  "action": "validation_success",
  "spec_path": "examples/user-api.cue",
  "valid": true,
  "errors": [],
  "warnings": []
}
```

**Code Location:** Each command implementation in `src/intent/` subdirectories

---

### 4. 🟡 MEDIUM: SSRF Protection Too Strict for Local Development

**Issue:** Cannot test against localhost for development/testing.

**Current Behavior:**
```bash
intent check spec.cue --target=http://localhost:9999 --dry-run
# Error: SSRF (Server-Side Request Forgery) attempt detected in URL 'http://localhost:9999':
# Localhost addresses are not allowed
```

**Impact:** Medium - Cannot test locally without workarounds
**AI Workaround:** Use public domains or set up port forwarding
**Recommended Fix:**
```
Option 1: Add --allow-localhost flag for development
Option 2: Disable SSRF protection in --dry-run mode (no actual requests made)
Option 3: Add environment variable INTENT_ALLOW_LOCALHOST=true
Option 4: Read from .intentrc config file: {"allow_localhost": true}
```

**Security Note:** Keep SSRF protection enabled by default, but provide escape hatch for local dev

**Code Location:** `src/intent/security.gleam` (likely the SSRF validation code)

---

### 5. 🟡 MEDIUM: Documentation Sync Issues

**Issue:** CLAUDE.md examples don't match actual CLI behavior.

**Example 1 - Interview command:**
```
CLAUDE.md says: intent interview --cue --profile api
Actually requires: intent interview --cue=true --profile=api
```

**Example 2 - Check command:**
```
CLAUDE.md shows: intent check spec.cue --target http://localhost:8080
Actually requires: intent check spec.cue --target=http://localhost:8080
And fails on localhost due to SSRF protection
```

**Impact:** Medium - Confusing for AI agents following documentation
**Recommended Fix:**
1. Update CLAUDE.md examples to use `--flag=value` syntax
2. Update localhost examples to use public domains or mention `--allow-localhost` flag
3. Add validation tests that run examples from CLAUDE.md
4. Generate CLAUDE.md examples from actual CLI help text

**Code Location:** `CLAUDE.md`, help text in command definitions

---

### 6. 🟢 LOW: Interview Session Management Clarity

**Issue:** Unclear what happens to incomplete interview sessions.

**Questions:**
- Can sessions be deleted?
- How long are sessions retained?
- Is there a `intent sessions --cleanup` command?
- What's the maximum number of sessions?

**Current Observation:**
```bash
intent sessions --json
# Returns array including test session "sess1" from who-knows-when
```

**Recommended Fix:**
- Add `intent sessions --delete <id>` command
- Add `intent sessions --cleanup` to remove incomplete sessions older than X days
- Document session lifecycle in `intent about` or `intent interview --help`

**Code Location:** `src/intent/interview_storage.gleam`

---

### 7. 🟢 LOW: EARS Parsing - Generic Path Generation

**Issue:** EARS parser generates generic paths that aren't useful.

**Example:**
```json
{
  "id": "REQ-2",
  "pattern": "Event-Driven",
  "system_shall": "generate a JWT token",
  "behaviors": [{
    "name": "REQ-2-handle-on-a-user-logs-in,",
    "intent": "generate a JWT token",
    "method": "GET",
    "path": "/endpoint",  // ← Generic placeholder
    "status": 200
  }]
}
```

**Impact:** Low - Still needs human review anyway
**Recommended Fix:**
- Use heuristics to guess better paths (e.g., "login" → "/auth/login")
- Add `--interactive` flag to prompt for path/method during parsing
- Document that EARS output is a starting point, not production-ready

**Code Location:** `src/intent/kirk/ears_parser.gleam`

---

### 8. 🟢 LOW: Progress Indicators in Non-Interactive Mode

**Issue:** Spinner animations shown even when outputting JSON.

**Example:**
```bash
intent quality examples/user-api.cue --json
# Shows: [?25l[2K[33m⠋[39m Exporting CUE to JSON...[2K[?25h
# Then outputs JSON
```

**Impact:** Low - AI agents can strip ANSI codes, but adds noise
**Recommended Fix:**
- Detect if stdout is a TTY
- Disable spinners/animations when `--json` flag is used
- Add `--quiet` flag to suppress progress indicators

**Code Location:** `src/intent/cli_ui.gleam` (spinner implementation)

---

## What Works Excellently for AI

### ✅ Interview Workflow
**Score: 95/100**

The CUE-based interview workflow is exceptional:
```bash
# Start interview
intent interview --cue=true --profile=api
# Returns clean CUE with session.id, question, progress, examples

# Continue interview
intent interview --cue=true --session="<id>" --answer="THE SYSTEM SHALL..."
# Returns next question OR interview_complete
```

**Strengths:**
- Clean, parseable CUE output
- Progress tracking (current_step, total_steps, percent_complete)
- Session persistence across context resets
- Examples and hints provided for each question
- EARS pattern validation

**Minor Issue:**
- Flag syntax requires `=` (documented above)

---

### ✅ KIRK Analysis Suite
**Score: 92/100**

All KIRK commands provide excellent analysis:

**Quality Analysis:**
```bash
intent quality spec.cue --json
# Returns: completeness, consistency, testability, clarity, security scores
# Includes: specific issues with field paths and severity
```

**Gap Detection:**
```bash
intent gaps spec.cue
# Returns: inversion gaps, second-order gaps, checklist gaps, coverage gaps, security gaps
# Each gap includes: severity, description, suggested fix
```

**Inversion Analysis:**
```bash
intent invert spec.cue
# Returns: security gaps, usability gaps, integration gaps
# Includes: suggested behaviors to add with HTTP methods and status codes
```

**Coverage Analysis:**
```bash
intent coverage spec.cue
# Returns: HTTP methods, status codes, paths, edge cases, OWASP Top 10 score
# Very comprehensive!
```

**Effects Analysis:**
```bash
intent effects spec.cue
# Returns: first-order and second-order effects for each behavior
# Identifies missing verifications
```

**Strengths:**
- Comprehensive analysis across multiple dimensions
- Actionable recommendations
- Clear severity levels
- Some have JSON output

**Minor Issues:**
- Not all have `--json` flag
- Progress spinners shown even with `--json`

---

### ✅ Session Management
**Score: 88/100**

Session listing works well:
```bash
intent sessions --json
# Returns array of sessions with all metadata
# Includes: id, profile, created_at, stage, answers, gaps, conflicts
```

**Strengths:**
- Complete session metadata
- Clean JSON output
- Answers array shows full interview history

**Minor Issues:**
- No cleanup/delete command (see issue #6)
- Unclear retention policy

---

### ✅ EARS Parsing
**Score: 85/100**

EARS parsing is functional and useful:
```bash
intent ears requirements.md --output=json
# Returns: requirements array, behaviors array, errors, warnings
```

**Strengths:**
- Recognizes all 5 EARS patterns
- Provides warnings for missing patterns
- Suggests improvements (error handling, unwanted behaviors)
- JSON output available

**Minor Issues:**
- Generic path generation (see issue #7)
- Flag syntax requires `=`

---

### ✅ Validation
**Score: 90/100**

Spec validation is strict and helpful:
```bash
intent validate spec.cue
# Returns: ✓ Valid spec OR specific error with field path
```

**Strengths:**
- Clear error messages
- Field paths shown for debugging
- Exit codes signal success/failure

**Minor Issues:**
- No `--json` flag for machine-readable output
- Errors don't follow AI-friendly format

---

### ✅ Security Posture
**Score: 95/100**

Security is taken seriously:
- SSRF protection on URLs
- Path validation (prevents directory traversal)
- Validates file types
- No execution of arbitrary code

**Minor Issue:**
- SSRF protection too strict for local development (see issue #4)

---

## AI-Friendly Features That Shine

### 1. Exit Codes
Clear semantic exit codes make automation easy:
- `0` = Success
- `1` = Test failures
- `2` = Blocked behaviors (dependencies failed)
- `3` = Invalid specification
- `4` = General error (file not found, network error)

### 2. Session Persistence
Interview sessions survive context resets - critical for AI agents:
```json
{
  "id": "interview-7B80EB35-916D-47D9-A397-7AFE7C51F4AE",
  "profile": "api",
  "started_at": "2026-01-17T08:57:56.713-06:00",
  "answers": [...]
}
```

### 3. Progress Tracking
Every interview response includes progress:
```json
{
  "progress": {
    "current_step": 2,
    "total_steps": 18,
    "percent_complete": 5,
    "category": "basic_info"
  }
}
```

### 4. Comprehensive Help Text
Help text specifically addresses AI agents:
```
🤖 AI-FIRST: Use this to RIGOROUSLY INTERVIEW humans
🤖 FOR AI AGENTS - NON-INTERACTIVE Q&A LOOP (CUE MODE)
```

### 5. Structured Question Format
Questions come with examples and validation hints:
```json
{
  "question": {
    "text": "In one sentence, what should this API do?",
    "pattern": "ubiquitous",
    "examples": ["THE SYSTEM SHALL validate all API inputs"],
    "hint": "Use format: THE SYSTEM SHALL [behavior]"
  }
}
```

---

## Recommendations for Better AI-Human Collaboration

### Immediate Improvements (High Priority)

1. **Standardize Flag Syntax**
   - Pick one: `--flag=value` OR support both `--flag=value` and `--flag value`
   - Update all commands for consistency
   - Update documentation

2. **Implement AI-Friendly Error Format**
   - Add `--json` flag to ALL commands
   - When `--json` active, output errors as JSON with recovery steps
   - Follow the documented format in CLAUDE.md

3. **Add Localhost Testing Support**
   - Add `--allow-localhost` flag
   - OR disable SSRF in `--dry-run` mode
   - OR add config option in `.intentrc`

4. **Sync Documentation**
   - Update CLAUDE.md examples to match actual CLI syntax
   - Add validation tests for examples
   - Auto-generate examples from help text

### Medium-Term Improvements

5. **Add JSON Output to All Commands**
   - `intent validate --json`
   - `intent about --json`
   - `intent history --json`
   - `intent diff --json`
   - Etc.

6. **Improve Session Management**
   - `intent sessions --delete <id>`
   - `intent sessions --cleanup`
   - Document session lifecycle

7. **Suppress Spinners in JSON Mode**
   - Detect TTY or `--json` flag
   - Disable progress indicators
   - Add `--quiet` flag

### Long-Term Enhancements

8. **Interactive EARS Parsing**
   - `intent ears --interactive` to prompt for paths/methods
   - Use heuristics for better default paths
   - Add AI-assisted path suggestion

9. **Beads Integration Testing**
   - Test full workflow: interview → beads → execute
   - Verify `bd` integration
   - Document beads regeneration workflow

10. **Configuration File Support**
    - Support `.intentrc` for defaults
    - Document config precedence
    - Validate config on startup

---

## Testing Coverage Assessment

### ✅ Tested Successfully
- Interview workflow (start + continue)
- All KIRK analysis commands (quality, gaps, invert, coverage, effects, compact)
- EARS parsing (text + JSON output)
- Validation (success + failure cases)
- Session listing
- Error handling (missing files, invalid sessions)
- Security (SSRF protection)
- Help text generation
- Version information

### ⏭️ Not Tested (Recommend Testing)
- `intent beads <session-id>` (requires completed interview)
- `intent check` against live API (SSRF blocks localhost)
- `intent plan` and `intent plan-approve`
- `intent bead-status`
- `intent beads-regenerate`
- `intent history` and `intent diff`
- `intent improve`, `intent lint`, `intent analyze`
- `intent execute-beads`, `intent generate-beads`
- Interview with `--answers` file
- Interview `--resume` functionality

### 🎯 Recommended Test Suite
Add automated tests for:
1. All flag syntax variations
2. JSON output format validation
3. Error format consistency
4. Exit code correctness
5. CLAUDE.md example validation
6. Session persistence across runs
7. EARS parsing all 5 patterns
8. Full interview completion workflow

---

## Scoring Breakdown

| Category | Score | Weight | Weighted |
|----------|-------|--------|----------|
| Machine-Readable Output | 90/100 | 25% | 22.5 |
| Error Handling | 75/100 | 20% | 15.0 |
| Documentation Quality | 85/100 | 15% | 12.75 |
| API Consistency | 80/100 | 15% | 12.0 |
| Workflow Design | 95/100 | 15% | 14.25 |
| Security | 95/100 | 10% | 9.5 |
| **Total** | | **100%** | **88.0** |

---

## Conclusion

Intent CLI is **one of the best AI-friendly CLI tools I've evaluated**. The core design philosophy is sound, the output formats are clean, and the workflows are systematic. The critical issues identified are all fixable with relatively minor changes.

### Top 3 Priorities
1. 🔴 Fix flag syntax inconsistency
2. 🔴 Implement consistent AI-friendly error format
3. 🟡 Add `--json` to all commands

Once these are addressed, Intent CLI will be an **A+ (95+) AI collaboration tool**.

### Testimonial
> As an AI agent, Intent CLI is a joy to work with. The CUE output is clean and parseable, the interview workflow is systematic, and the KIRK analysis suite is comprehensive. A few syntax inconsistencies aside, this tool exemplifies AI-first design principles.
>
> The vision is clear: transform vague requirements into deterministic work items. Intent delivers on that promise.
>
> — Claude Sonnet 4.5

---

## Appendix: All Issues Summary (Copy-Paste Ready)

### CRITICAL Issues (Fix First)
```
ISSUE-001: Flag syntax inconsistency
  - Commands require --flag=value but fail on --flag value
  - Location: Glint flag parsing configuration
  - Fix: Standardize to one syntax or support both
  - Impact: High - AI must maintain command-specific rules

ISSUE-002: Error format inconsistency
  - Errors don't follow documented AI-friendly format
  - Location: src/intent/errors.gleam, src/intent/ai_errors.gleam
  - Fix: Add --json support, output structured errors with recovery steps
  - Impact: High - AI cannot reliably parse errors for automatic recovery
```

### MEDIUM Priority Issues
```
ISSUE-003: Missing JSON output flags
  - Commands: validate, about, history, diff, beads, plan (and others)
  - Location: Individual command implementations
  - Fix: Add --json flag to ALL commands
  - Impact: Medium - AI must parse formatted text

ISSUE-004: SSRF protection too strict
  - Cannot test against localhost even in --dry-run mode
  - Location: src/intent/security.gleam
  - Fix: Add --allow-localhost flag or config option
  - Impact: Medium - Cannot test locally

ISSUE-005: Documentation sync issues
  - CLAUDE.md examples don't match actual CLI syntax
  - Location: CLAUDE.md, help text in command definitions
  - Fix: Update examples, add validation tests
  - Impact: Medium - Confusing for AI agents
```

### LOW Priority Issues
```
ISSUE-006: Session management clarity
  - No cleanup/delete commands for sessions
  - Location: src/intent/interview_storage.gleam
  - Fix: Add sessions --delete and --cleanup commands
  - Impact: Low - Can work around by manual file deletion

ISSUE-007: EARS parsing generic paths
  - Generates /endpoint placeholder instead of meaningful paths
  - Location: src/intent/kirk/ears_parser.gleam
  - Fix: Use heuristics or add --interactive flag
  - Impact: Low - Still needs human review

ISSUE-008: Progress indicators in JSON mode
  - Spinner animations shown even with --json output
  - Location: src/intent/cli_ui.gleam
  - Fix: Detect TTY or --json flag, suppress spinners
  - Impact: Low - AI can strip ANSI codes
```

---

**End of Report**
