# Intent CLI Help Text System - Next Steps & Action Plan

**Review Date**: 2026-01-18
**Overall Status**: ✅ Production-Ready (82/100)
**Recommended Action**: MERGE to main with follow-up improvements

---

## Phase 1: Immediate Integration (v1.0 → v1.1, Next 2 Weeks)

### 1.1 Complete Formatter Utilities Integration

**Status**: 🔴 IN PROGRESS (50% complete)
**Priority**: HIGH
**Effort**: 4 hours

#### Current State
- Only 2-3 commands currently use `formatter_utils`
- 10+ commands could benefit from standardized output formatting
- Box headers, progress bars, and score displays are ad-hoc in places

#### Action Items

**Task 1.1.1: Integrate formatter_utils into analysis commands** (2 hours)
```gleam
// Commands to update: analyze, improve, doctor, quality, lint
// Pattern to implement:
let header = formatter_utils.box_header("Analysis Results")
let scores = [
  formatter_utils.kv_pair("Coverage", formatter_utils.format_score_int(score)),
  formatter_utils.kv_pair("Clarity", formatter_utils.format_score_int(score)),
]
let output = [header, "", ..scores] |> string.join("\n")
io.println(output)
```

**Task 1.1.2: Integrate formatter_utils into report commands** (1 hour)
```gleam
// Commands to update: invert, coverage, gaps, effects
// Pattern to implement:
let section = formatter_utils.section_header(emoji.security, "Security Issues")
let items = list.map(issues, formatter_utils.bullet_item)
```

**Task 1.1.3: Add formatter configuration to config module** (1 hour)
```gleam
// Add to config.gleam:
pub type OutputPreferences {
  OutputPreferences(
    box_width: Int,        // default: 60
    indent_size: Int,      // default: 2
    emoji_enabled: Bool,   // default: True
    color_enabled: Bool,   // default: True
  )
}

pub fn load_output_preferences() -> OutputPreferences {
  OutputPreferences(
    box_width: cli_flags.get_env_int(os.get_env, "INTENT_BOX_WIDTH", 60),
    indent_size: cli_flags.get_env_int(os.get_env, "INTENT_INDENT_SIZE", 2),
    emoji_enabled: cli_flags.get_env_bool(os.get_env, "INTENT_EMOJI", True),
    color_enabled: cli_flags.get_env_bool(os.get_env, "INTENT_COLORS", True),
  )
}
```

**Completion Criteria**:
- ✅ All analysis commands (analyze, improve, doctor, quality) use formatter_utils
- ✅ All report commands (invert, coverage, gaps, effects) use formatter_utils
- ✅ Output preferences system working
- ✅ Tests passing
- ✅ Help text updated if needed

---

### 1.2 Standardize Error Output with error_handler

**Status**: 🟡 PARTIAL (25% complete)
**Priority**: HIGH
**Effort**: 6 hours

#### Current State
- error_handler module exists but only used in 3 commands
- Many commands have ad-hoc error reporting
- Inconsistent exit codes and error formatting

#### Action Items

**Task 1.2.1: Convert check command to use error_handler** (1.5 hours)
```gleam
// Current: Custom error output
Error(msg) -> {
  io.println_error("Error: " <> msg)
  halt(exit_error)
}

// New: Standardized via error_handler
Error(msg) -> {
  let error = error_handler.generic_error(
    msg,
    "Verify spec syntax and target URL",
    ["Run 'intent validate spec.cue' first", "Check --target URL is reachable"]
  )
  Int(error_handler.output_error(error, json_mode))
}
```

**Task 1.2.2: Convert validation commands to use error_handler** (1.5 hours)
- Update: validate, lint, analyze, show, export commands

**Task 1.2.3: Convert interview commands to use error_handler** (1.5 hours)
- Update: interview, beads, bead-status, history, diff, sessions commands

**Task 1.2.4: Convert KIRK commands to use error_handler** (1.5 hours)
- Update: quality, invert, coverage, gaps, effects, ears, parse commands

**Completion Criteria**:
- ✅ All command error paths use error_handler.generic_error()
- ✅ All exit codes normalized (0=pass, 1=fail, 2=blocked, 3=invalid, 4=error)
- ✅ Error messages include context dictionary where relevant
- ✅ Recovery steps provided for actionable errors
- ✅ Tests verify error output format

---

### 1.3 Add Test Coverage for Core Modules

**Status**: 🔴 NOT STARTED
**Priority**: MEDIUM
**Effort**: 3 hours

#### Current Coverage
| Module | Tests | Coverage |
|--------|-------|----------|
| cli_text_constants | Partial | ~30% |
| emoji_constants | None | 0% ❌ |
| formatter_utils | None | 0% ❌ |
| cli_flags | None | 0% ❌ |
| config | Good | 85% ✅ |
| error_handler | Good | 80% ✅ |

#### Action Items

**Task 1.3.1: Add emoji_constants tests** (0.5 hours)
```gleam
pub fn severity_icon_critical_test() {
  emoji_constants.severity_icon("critical")
  |> should.equal(emoji_constants.severity_critical)
}

pub fn status_icon_2xx_test() {
  emoji_constants.status_icon(200) |> should.equal(emoji_constants.status_2xx)
}

pub fn bool_icon_true_test() {
  emoji_constants.bool_icon(True) |> should.equal(emoji_constants.success)
}
// ... 5-10 more tests
```

**Task 1.3.2: Add formatter_utils tests** (1 hour)
```gleam
pub fn box_header_test() {
  let result = formatter_utils.box_header("Test")
  result |> string.contains("╔") |> should.equal(True)
}

pub fn progress_bar_test() {
  formatter_utils.progress_bar(50.0)
  |> string.should_equal("[█████░░░░]")
}

pub fn indent_test() {
  formatter_utils.indent_1() |> should.equal("  ")
  formatter_utils.indent_n(3) |> should.equal("      ")
}
// ... 8-12 more tests
```

**Task 1.3.3: Add cli_flags validation tests** (1.5 hours)
```gleam
pub fn validate_required_string_empty_test() {
  cli_flags.validate_required_string("", "target")
  |> should.be_error()
}

pub fn validate_enum_valid_test() {
  cli_flags.validate_enum("api", ["api", "cli", "event"], "profile")
  |> should.be_ok()
}

pub fn validate_dependency_test() {
  cli_flags.validate_dependency("value", "", "dependent", "required")
  |> should.be_error()
}
// ... 5-8 more tests
```

**Completion Criteria**:
- ✅ All new test files added to test/ directory
- ✅ Tests cover happy path and error cases
- ✅ Test names descriptive and follow convention
- ✅ All tests pass (`gleam test`)

---

### 1.4 Expand Module Documentation

**Status**: 🟡 PARTIAL (50% complete)
**Priority**: MEDIUM
**Effort**: 2 hours

#### Current State
- Module-level comments exist
- Function comments minimal
- No integration guide for contributors
- No troubleshooting section

#### Action Items

**Task 1.4.1: Enhance function documentation** (1 hour)
```gleam
/// Create a progress bar visualization from a percentage (0-100).
///
/// Fills with block_filled (█) up to percentage, rest with block_empty (░).
/// Output format: [████░░░░░░] 40.0%
///
/// Examples:
///   progress_bar_with_width(75.0, 10) -> "[███████░░░]"
///   progress_bar_with_width(0.0, 10) -> "[░░░░░░░░░░]"
///   progress_bar_with_width(100.0, 10) -> "[██████████]"
///
/// Arguments:
///   - percentage: Float 0.0 to 100.0
///   - width: Int > 0 (number of blocks)
///
/// Returns: String with formatted progress bar
pub fn progress_bar_with_width(percentage: Float, width: Int) -> String
```

**Task 1.4.2: Create integration guide** (1 hour)
- Add section to CLAUDE.md about adding new commands
- Document checklist from NEXT_STEPS.md
- Add example: complete walkthrough of adding a new command
- Link to help_text_test.gleam as reference

**Completion Criteria**:
- ✅ All public functions have comprehensive doc comments
- ✅ Documentation includes examples where helpful
- ✅ Integration guide added to CLAUDE.md
- ✅ Example command walkthrough included

---

## Phase 2: Quality Improvements (v1.1, Weeks 3-4)

### 2.1 Add Help Text Version Tracking

**Status**: 🟢 PLANNED
**Priority**: MEDIUM
**Effort**: 3 hours

#### Purpose
Track version changes to help text for AI agent compatibility

#### Implementation
```gleam
// In cli_text_constants.gleam

pub const CLI_TEXT_VERSION = "1.0.0"

pub type TextVersion {
  TextVersion(
    version: String,        // e.g., "1.0"
    introduced: String,     // e.g., "v1.0"
    deprecated: String,     // e.g., "N/A" or "v2.0"
  )
}

pub fn get_text_versions() -> Dict(String, TextVersion) {
  dict.from_list([
    #("cmd_check_desc", TextVersion("1.0", "v1.0", "N/A")),
    #("cmd_check_desc_v2", TextVersion("2.0", "v1.1", "N/A")),
    #("cmd_validate_desc", TextVersion("1.0", "v1.0", "N/A")),
    // ...
  ])
}

pub fn text_changelog() -> List(#(String, String, String)) {
  [
    #("v1.1", "2026-02-01", "Improved check command description"),
    #("v1.2", "2026-02-15", "Added plan command, deprecated plan-approve"),
    // ...
  ]
}

// Optional: Command for version info
// $ intent --text-version
// Output: {"version": "1.0.0", "total_texts": 54, "last_updated": "2026-01-18"}
```

#### Action Items
1. Add TextVersion type
2. Create version tracking for all texts
3. Generate version report command
4. Document changelog

---

### 2.2 Build Output Preferences System

**Status**: 🟢 PLANNED
**Priority**: MEDIUM
**Effort**: 2 hours

#### Purpose
Allow users to customize output (box width, emoji, colors)

#### Implementation
```gleam
// In config.gleam

pub type OutputPreferences {
  OutputPreferences(
    box_width: Int,        // default: 60 (env: INTENT_BOX_WIDTH)
    indent_size: Int,      // default: 2 (env: INTENT_INDENT_SIZE)
    emoji_enabled: Bool,   // default: True (env: INTENT_EMOJI)
    color_enabled: Bool,   // default: True (env: INTENT_COLORS)
  )
}

pub fn load_output_preferences(env_getter: fn(String) -> Result(String, Nil)) -> OutputPreferences {
  OutputPreferences(
    box_width: cli_flags.get_env_int(env_getter, "INTENT_BOX_WIDTH", 60),
    indent_size: cli_flags.get_env_int(env_getter, "INTENT_INDENT_SIZE", 2),
    emoji_enabled: cli_flags.get_env_bool(env_getter, "INTENT_EMOJI", True),
    color_enabled: cli_flags.get_env_bool(env_getter, "INTENT_COLORS", True),
  )
}
```

#### CLI Integration
```bash
# Disable emoji (for piping to non-Unicode systems)
$ INTENT_EMOJI=false intent check api.cue --target http://localhost:8080

# Custom box width for narrow terminals
$ INTENT_BOX_WIDTH=40 intent analyze api.cue

# Disable colors (for log files)
$ INTENT_COLORS=false intent doctor api.cue > report.txt
```

---

### 2.3 Plan cli_text_constants Split

**Status**: 🟢 PLANNED
**Priority**: MEDIUM (v1.1 → v2.0)
**Effort**: 2 hours planning + 1-2 hours implementation

#### Current Size
- 1,804 LOC (comfortable)
- 24 commands (4 categories)

#### Projected Size at v1.1
- ~2,500 LOC (still manageable)
- 35-40 commands

#### Decision Point for Split
- 🔴 Split needed at 4,000 LOC or 60+ commands
- Estimated timing: 6-12 months

#### Proposed Split Strategy
```
cli_text_constants.gleam           ← Core & Common (50%)
├── Command: check, validate, show, export
├── Command: lint, analyze, improve, doctor
├── Command: interview, beads, bead-status
├── Command: history, diff, sessions, sessions

cli_text_constants_analysis.gleam ← KIRK Analysis (25%)
├── Command: quality, invert, coverage, gaps, effects
├── Command: ears, parse

cli_text_constants_workflows.gleam ← Workflows (25%)
├── Command: plan, plan-approve, beads-regenerate
├── (Future) command: deploy, verify, rollback
```

#### Migration Path
```gleam
// Before (v1.0)
import intent/cli_text_constants

// After (v2.0)
import intent/cli_text_constants
import intent/cli_text_constants_analysis
import intent/cli_text_constants_workflows

// Or use module-level re-export:
import intent/cli_text_constants as {
  cmd_check_desc,
  cmd_quality_desc as cmd_quality_desc_deprecated,
  // ...
}
import intent/cli_text_constants_analysis as {
  cmd_quality_desc,
  cmd_invert_desc,
  // ...
}
```

---

## Phase 3: Long-term Improvements (v2.0+)

### 3.1 Data-Driven Help Text System

**Status**: 🟢 EXPLORATORY
**Priority**: LOW (v2.0 or later)
**Effort**: 8-12 hours

#### Purpose
Generate Gleam constants from structured data (YAML/TOML) at build time

#### Rationale
- Help text currently hardcoded
- At 100+ commands, maintenance becomes burden
- Data-driven approach enables tooling (generators, formatters)
- Enables future: multi-language support, versioning, AI hints

#### Proposed Implementation
```yaml
# .intent/help-texts.yaml
commands:
  check:
    name: "check"
    short: "Execute spec tests against target URL and verify behaviors"
    long: |
      WHAT IT DOES
        Execute all behaviors defined in a spec...
    flags:
      - name: "target"
        desc: "Target base URL to test against"
        required: true
        env: "INTENT_TARGET"

  validate:
    name: "validate"
    short: "Validate CUE spec file syntax and structure"
    long: |
      WHAT IT DOES
        Parses CUE syntax...
    flags: []
```

```bash
# Build-time command
$ gleam run -- build-help-texts .intent/help-texts.yaml src/intent/cli_text_constants.gleam
```

#### Build Script (Erlang FFI)
```erlang
% intent_ffi.erl
%% Generate Gleam module from YAML
-export([build_help_texts/2]).

build_help_texts(YamlFile, OutputFile) ->
  % Parse YAML
  % Generate Gleam code
  % Write to OutputFile
  ok.
```

---

### 3.2 Multi-Language Support

**Status**: 🟢 EXPLORATORY
**Priority**: LOW (v2.0 or later)
**Effort**: 6-8 hours

#### Purpose
Support multiple languages for help text

#### Implementation Strategy
```gleam
// Create wrapper layer
pub type Language {
  English
  Spanish
  French
  German
  // ...
}

pub fn cmd_check_desc(lang: Language) -> String {
  case lang {
    English -> "Execute spec tests against target URL and verify behaviors"
    Spanish -> "Ejecutar pruebas de especificación contra la URL de destino..."
    French -> "Exécuter les tests de spécification contre l'URL cible..."
    _ -> cmd_check_desc(English)  // fallback
  }
}

pub fn get_language_from_env() -> Language {
  case os.get_env("LANG") {
    Ok("es_ES") -> Spanish
    Ok("fr_FR") -> French
    Ok("de_DE") -> German
    _ -> English
  }
}
```

#### Per-Language Files
```
src/intent/
├── cli_text_constants.gleam         ← English (primary)
├── cli_text_constants_es.gleam      ← Spanish
├── cli_text_constants_fr.gleam      ← French
├── cli_text_constants_de.gleam      ← German
├── cli_text_constants_i18n.gleam    ← Routing/language selection
```

---

### 3.3 AI-Friendly Help Text Variants

**Status**: 🟢 EXPLORATORY
**Priority**: LOW (v2.0+ or when AI integration planned)
**Effort**: 4-6 hours

#### Purpose
Generate concise help text optimized for AI parsing

#### Implementation
```gleam
pub type HelpTextFormat {
  Human       // Full text for human users
  Compact     // Concise for LLM consumption
  Structured  // JSON-like with metadata
}

pub fn cmd_check_desc(format: HelpTextFormat) -> String {
  case format {
    Human -> "Execute spec tests against target URL and verify behaviors\n\nWHAT IT DOES\n..."
    Compact -> "Run spec tests against target API"
    Structured ->
      "{\"action\":\"test\",\"domain\":\"spec\",\"target\":\"api\",\"verb\":\"execute\"}"
  }
}

// Or separate module for AI hints
pub fn cmd_check_ai_hints() -> Dict(String, String) {
  dict.from_list([
    #("action", "execute_tests"),
    #("domain", "api_contract_testing"),
    #("required_context", "spec_file,target_url"),
    #("typical_output", "pass/fail_report"),
  ])
}
```

---

## Risk Mitigation

### Risk 1: Breaking Changes During Integration

**Probability**: LOW
**Impact**: HIGH

**Mitigation**:
- Always add new modules, never remove
- Keep old patterns alongside new (dual support period)
- Tag releases before major refactors
- Run full test suite after each phase

---

### Risk 2: Integration Complexity Grows

**Probability**: MEDIUM
**Impact**: MEDIUM

**Mitigation**:
- Small incremental changes per command
- Each command independently testable
- Pair integration work with new test coverage
- Document integration pattern per command type

---

### Risk 3: File Size Becomes Unwieldy

**Probability**: MEDIUM (at 4,000 LOC)
**Impact**: MEDIUM

**Mitigation**:
- Track file size metrics monthly
- Set alert at 3,500 LOC
- Plan split 3 months in advance
- Automate split process (script to organize constants)

---

## Success Metrics

### Phase 1 (v1.0 → v1.1)
- ✅ formatter_utils integrated into 10+ commands
- ✅ error_handler used in all command error paths
- ✅ Test coverage for emoji_constants, formatter_utils, cli_flags
- ✅ Documentation expanded for contributors
- ⏱️ 2-week timeline

### Phase 2 (v1.1)
- ✅ Help text versioning system working
- ✅ Output preferences system functional
- ✅ Split strategy documented and ready
- ⏱️ +2 weeks

### Phase 3 (v2.0+)
- ✅ Data-driven help text system (optional)
- ✅ Multi-language support (optional)
- ✅ AI-friendly help text variants (optional)
- ⏱️ Timeline TBD based on priority

---

## Rollout Timeline

```
Week 1-2 (Now):      ✅ Phase 1 Start
  ├─ formatter_utils integration
  ├─ error_handler standardization
  └─ Test coverage expansion

Week 3-4:            ✅ Phase 1 Complete
  ├─ Documentation expansion
  ├─ Version tracking setup
  ├─ Output preferences system
  └─ Phase 2 code review + merge

Week 5-8:            Phase 2 (if planned)
  ├─ Data-driven system exploration
  ├─ Multi-language investigation
  └─ AI integration planning

Month 2-3:           Phase 3 (if approved)
  ├─ Implement approved improvements
  ├─ Community feedback collection
  └─ v2.0 release planning
```

---

## Handoff Checklist

Before closing this review, ensure:

- [ ] ARCHITECTURAL_REVIEW.md shared with team
- [ ] SCORECARD.md reviewed by leads
- [ ] NEXT_STEPS.md approved for execution
- [ ] Integration tasks added to issue tracker
- [ ] Code review assigned for formatter_utils integration
- [ ] Test improvements scheduled for iteration 1
- [ ] Monitoring set up for file size growth
- [ ] v1.1 planning meeting scheduled

---

## Questions for Clarification

Before proceeding with Phase 1, confirm:

1. **Formatter integration scope**: Should we refactor ALL output or only new commands?
   - Current recommendation: All commands that have analysis/report output

2. **Error handler adoption**: How strict should exit codes be?
   - Current recommendation: Follow standard exit codes (0, 1, 2, 3, 4)

3. **Version tracking timeline**: Essential for v1.0 or can wait for v1.1?
   - Current recommendation: Nice-to-have, plan for v1.1

4. **Output preferences**: Worth implementing if limited current use?
   - Current recommendation: Yes, enables future customization

5. **Split strategy approval**: Split now at 1,800 LOC or wait until 4,000?
   - Current recommendation: Wait until 3,500 LOC (~v1.2)

---

**End of Next Steps Document**

---

*For questions or clarifications, refer to ARCHITECTURAL_REVIEW.md for detailed analysis.*
