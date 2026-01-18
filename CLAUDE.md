# Intent CLI

Contract-driven API testing in Gleam. CUE specs → HTTP tests → verification.

## Beads Workflow
```jsonl
{"cmd":"bd ready --json","desc":"find work"}
{"cmd":"bd update <id> --status in_progress --json","desc":"claim"}
{"cmd":"bd close <id> --reason '...' --json","desc":"complete"}
{"cmd":"bv --robot-triage","desc":"AI triage"}
{"cmd":"bv --robot-next","desc":"top pick"}
{"cmd":"bv --robot-plan","desc":"parallel tracks"}
{"cmd":"bv --robot-insights","desc":"PageRank,critical path"}
{"cmd":"bv --robot-graph --graph-format=json","desc":"dependency export"}
```
CRITICAL: Never bare `bv` - launches blocking TUI.

## Commands
```jsonl
{"cmd":"check","args":"<spec> [--target URL] [--json]","desc":"run spec against API"}
{"cmd":"plan","args":"<spec> [--json] [--rounds 1..5]","desc":"health + waves + beads"}
{"cmd":"doctor","args":"<spec>","desc":"prioritized improvements"}
{"cmd":"prompt","args":"<spec> [--bead ID] [--profile ai] [--format cin]","desc":"AI-ready prompts"}
{"cmd":"beads","args":"<spec|--session ID>","desc":"generate work items"}
{"cmd":"feedback","args":"<spec> --results <json>","desc":"failures → fix beads"}
{"cmd":"interview","args":"<profile> [--resume ID] [--export spec.cue]","desc":"guided spec discovery"}
{"cmd":"validate","args":"<spec>","desc":"CUE syntax check"}
{"cmd":"lint","args":"<spec>","desc":"anti-pattern detection"}
{"cmd":"analyze","args":"<spec>","desc":"quality scoring"}
{"cmd":"improve","args":"<spec>","desc":"improvement suggestions"}
{"cmd":"coverage","args":"<spec>","desc":"OWASP + edge case coverage"}
{"cmd":"invert","args":"<spec>","desc":"failure mode analysis"}
{"cmd":"gaps","args":"<spec>","desc":"mental model gap detection"}
{"cmd":"effects","args":"<spec>","desc":"second-order effects"}
{"cmd":"ears","args":"<file>","desc":"parse EARS requirements"}
```

## Key Files
```jsonl
{"file":"src/intent.gleam","desc":"CLI entry + glint commands"}
{"file":"src/intent/checker.gleam","desc":"response validation (~900 lines)"}
{"file":"src/intent/parser.gleam","desc":"JSON parsing + dynamic_to_json"}
{"file":"src/intent_ffi.erl","desc":"Erlang FFI"}
```

## Dev
```bash
gleam build && gleam test
gleam run -- check examples/user-api.cue --target http://localhost:8080
```

## Spec Shape (ALL FIELDS REQUIRED)
```jsonl
{"field":"name","type":"String"}
{"field":"description","type":"String"}
{"field":"version","type":"String"}
{"field":"audience","type":"String"}
{"field":"success_criteria","type":"List(String)"}
{"field":"config","type":"{base_url,timeout_ms,headers}"}
{"field":"features","type":"List(Feature)"}
{"field":"rules","type":"List(Rule)"}
{"field":"anti_patterns","type":"List(AntiPattern)"}
{"field":"ai_hints","type":"{implementation,entities,security,pitfalls}"}
```

## Feature Shape
```jsonl
{"field":"name","type":"String"}
{"field":"description","type":"String"}
{"field":"behaviors","type":"List(Behavior)","note":"non-empty"}
```

## Behavior Shape
```jsonl
{"field":"name","type":"String"}
{"field":"intent","type":"String"}
{"field":"request","type":"{method,path,headers,query,body}"}
{"field":"response","type":"{status,example,checks,headers}"}
{"field":"notes","type":"String","default":"''"}
{"field":"requires","type":"List(String)","default":"[]"}
{"field":"tags","type":"List(String)","default":"[]"}
{"field":"captures","type":"Dict(String,String)","default":"{}"}
```

## Check Shape
```jsonl
{"field":"rule","type":"String","note":"validation expression"}
{"field":"why","type":"String","note":"explanation"}
```

## Planning Vision

Intent owns Plan phase. All work decomposition flows from CUE specs.

### Glossary
```jsonl
{"term":"Spec","def":"CUE specification (features,behaviors,checks)"}
{"term":"Bead","def":"atomic 5-30min work unit from Spec"}
{"term":"Wave","def":"parallel bead group (same dependency depth)"}
{"term":"Round","def":"1 of 5 mental model passes"}
{"term":"RCS","def":"Round Completion Score 0-100%"}
{"term":"Doctor","def":"health report + prioritized fixes"}
{"term":"Environment","def":"named config (dev/staging/prod)"}
```

### 5-Round Mental Model System
```jsonl
{"round":1,"model":"EARS","output":"spec skeleton + ubiquitous/event/state/unwanted patterns","gate":"RCS₁=100%"}
{"round":2,"model":"Contracts","output":"response.checks with rule+why","gate":"RCS₂=100%"}
{"round":3,"model":"Inversion","output":"anti_patterns + error behaviors","gate":"RCS₃=100%"}
{"round":4,"model":"Effects","output":"requires[] + verification behaviors","gate":"RCS₄=100%"}
{"round":5,"model":"Pre-mortem","output":"ai_hints.pitfalls","gate":"RCS₅≥80%"}
```

### Bead Sources
```jsonl
{"source":"spec","flow":"feature/behavior → design/implement/test beads"}
{"source":"interview","flow":"answers → work items"}
{"source":"quality","flow":"issues → fix tasks"}
{"source":"round_gaps","flow":"missing requirements → fill-gap beads"}
{"source":"feedback","flow":"check failures → implementation fixes"}
```

## AI-Native Features

### Robot Mode
```jsonl
{"flag":"--json","desc":"standard JSON output"}
{"flag":"--robot","desc":"action metadata wrapper"}
{"flag":"--cin","desc":"Compact Intent Notation"}
{"flag":"--json-out FILE","desc":"write to file"}
```

### Bead-to-Prompt Pipeline
```jsonl
{"cmd":"intent prompt --bead <id>","desc":"bead-specific context"}
{"cmd":"intent prompt <spec> --profile ai","desc":"full spec + AI hints"}
{"cmd":"intent prompt <spec> --format cin","desc":"compact notation"}
{"cmd":"intent prompt <spec> --behavior <name>","desc":"single behavior"}
```

### Action JSON Schema
```json
{"action":"<cmd>_result","command":"<cmd>","exit_code":0,"data":{}}
```

### Auto-Verification Workflow
```jsonl
{"step":1,"cmd":"bd update <id> --status in_progress","desc":"claim bead"}
{"step":2,"cmd":"<implement>","desc":"do work"}
{"step":3,"cmd":"intent verify --bead <id>","desc":"run checks"}
{"step":4,"cmd":"bd close <id> --reason '...'","desc":"close with proof"}
```

### Feedback Loop
```jsonl
{"step":1,"cmd":"intent check <spec> --json-out results.json","desc":"capture failures"}
{"step":2,"cmd":"intent feedback <spec> --results results.json","desc":"generate fix beads"}
```

## Modules
```jsonl
{"mod":"loader","purpose":"CUE→Spec"}
{"mod":"parser","purpose":"JSON→types"}
{"mod":"runner","purpose":"HTTP execution"}
{"mod":"checker","purpose":"response validation"}
{"mod":"output","purpose":"format results"}
{"mod":"bead_templates","purpose":"interview→beads"}
{"mod":"plan_mode","purpose":"topo sort,waves,risk"}
{"mod":"prompt_builder","purpose":"AI prompts"}
{"mod":"quality_analyzer","purpose":"4D scoring (coverage,clarity,testability,ai_readiness)"}
{"mod":"spec_linter","purpose":"anti-patterns (6 warning types)"}
{"mod":"improver","purpose":"suggestions sorted by impact"}
{"mod":"kirk/coverage_analyzer","purpose":"OWASP Top 10 + edge cases"}
{"mod":"kirk/ears_parser","purpose":"EARS→behaviors (5 patterns)"}
{"mod":"kirk/effects_analyzer","purpose":"2nd-order effects + orphans"}
{"mod":"kirk/gap_detector","purpose":"5 gap types (inversion,2nd-order,checklist,coverage,security)"}
{"mod":"kirk/inversion_checker","purpose":"24 failure patterns (security,usability,integration)"}
{"mod":"interview","purpose":"5-round stateful engine"}
{"mod":"interview_storage","purpose":"JSONL + SQLite hybrid"}
{"mod":"spec_builder","purpose":"interview→CUE"}
```

### CLI Consistency Standards (NEW)
```jsonl
{"mod":"emoji_constants","purpose":"40+ centralized emoji/Unicode constants"}
{"mod":"cli_text_constants","purpose":"help text for 24 commands + 30+ flags"}
{"mod":"formatter_utils","purpose":"reusable box headers, progress bars, indentation"}
{"mod":"cli_flags","purpose":"20+ flag builders + validation helpers"}
{"mod":"config","purpose":"global config with env vars (INTENT_*)"}
{"mod":"error_handler","purpose":"centralized error formatting + exit codes"}
```

## CLI Consistency Standards

### Emoji Constants (`emoji_constants.gleam`)
Centralized Unicode/emoji definitions to eliminate duplication across 20+ modules.

**Constants (40+)**:
- Status icons: `success = "✓"`, `failure = "✗"`, `warning = "⚠"`, `info = "ℹ"`
- Severity: `critical = "🚨"`, `error = "❌"`, `warning = "⚠️"`, `info = "ℹ️"`
- UI elements: `box_tl/tr/bl/br`, `box_h/v`, `block_filled/empty`, `bullet`, `arrow`

**Helper functions**:
```gleam
pub fn severity_icon(severity: ErrorSeverity) -> String  // Returns emoji
pub fn status_icon(passed: Bool) -> String  // Returns ✓ or ✗
pub fn bool_icon(value: Bool) -> String  // Returns ✓ or ✗
```

### Text Constants (`cli_text_constants.gleam`)
Help text for all 24 commands and 30+ flags, ensuring consistent patterns.

**Command descriptions** (verb-first, 50-100 chars):
- Non-KIRK: `"Execute spec tests against target URL and verify behaviors"`
- KIRK: `"KIRK: Analyze spec quality across coverage, clarity, testability"`

**Flag helpers**:
```gleam
pub fn with_default(desc: String, default: String) -> String
pub fn required(desc: String) -> String
pub fn with_env(desc: String, env_var: String) -> String
pub fn with_default_and_env(desc: String, default: String, env_var: String) -> String
```

**Result**: `flag_target_desc |> required() |> with_env("INTENT_TARGET")`
→ `"Target base URL to test against (required) [env: INTENT_TARGET]"`

### Formatter Utils (`formatter_utils.gleam`)
Reusable formatting utilities for consistent multi-command output.

**Key functions**:
```gleam
pub fn box_header(title: String) -> String
pub fn box_header_with_subtitle(title: String, subtitle: String) -> String
pub fn progress_bar(percentage: Float) -> String  // [████████░░] 80.0%
pub fn progress_bar_with_width(percentage: Float, width: Int) -> String
pub fn score_with_status(score: Float) -> String  // [███░░░░░░] 30.0% ✗
pub fn indent_n(level: Int) -> String  // 0→"", 1→"  ", 2→"    ", etc (2-space per level)
pub fn section_header(icon: String, title: String) -> String
pub fn bullet_item(text: String) -> String
pub fn kv_pair(key: String, value: String) -> String
```

**Standards**:
- `box_width = 60` chars internal content
- Indentation: 2-space increments (levels 0-4 max)
- Progress bars: filled/empty block characters
- Scores: visual status based on threshold (≥90% ✓, ≥70% ⚠, <50% ✗)

### CLI Flags (`cli_flags.gleam`)
Reusable flag builders and validation helpers.

**Flag builders** (return `flag.FlagBuilder(T)`):
```gleam
pub fn target_flag() -> flag.FlagBuilder(String)
pub fn json_flag() -> flag.FlagBuilder(Bool)
pub fn verbose_flag() -> flag.FlagBuilder(Bool)
pub fn quiet_flag() -> flag.FlagBuilder(Bool)
pub fn output_file_flag() -> flag.FlagBuilder(String)
pub fn profile_flag() -> flag.FlagBuilder(String)  // api|cli|event|data|workflow|ui
pub fn allow_localhost_flag() -> flag.FlagBuilder(Bool)
pub fn session_flag() -> flag.FlagBuilder(String)
pub fn bead_id_flag() -> flag.FlagBuilder(String)
pub fn feature_flag() -> flag.FlagBuilder(String)
pub fn only_flag() -> flag.FlagBuilder(String)
```

**Validation helpers**:
```gleam
pub fn validate_required_string(value: String, flag_name: String) -> Result(String, String)
pub fn validate_range(value: Int, min: Int, max: Int, flag_name: String) -> Result(Int, String)
pub fn validate_enum(value: String, allowed: List(String), flag_name: String) -> Result(String, String)
pub fn validate_dependency(dependent: String, required: String, dep_name: String, req_name: String) -> Result(Nil, String)
```

**Environment variable getters** (exported for config):
```gleam
pub fn get_env_string(env_getter: fn(String) -> Result(String, Nil), env_var: String, default: String) -> String
pub fn get_env_bool(env_getter: fn(String) -> Result(String, Nil), env_var: String, default: Bool) -> Bool
pub fn get_env_int(env_getter: fn(String) -> Result(String, Nil), env_var: String, default: Int) -> Int
```

### Global Config (`config.gleam`)
Configuration management with environment variable and flag support.

**Config type**:
```gleam
pub type Config {
  Config(
    target_url: String,        // INTENT_TARGET
    allow_localhost: Bool,     // INTENT_ALLOW_LOCALHOST
    profile: String,           // INTENT_PROFILE (default: "api")
    output_file: String,       // INTENT_OUTPUT
    timeout_ms: Int,           // INTENT_TIMEOUT_MS (default: 30000)
  )
}
```

**Key functions**:
```gleam
pub fn default() -> Config
pub fn load_from_env(env_getter: fn(String) -> Result(String, Nil)) -> Config
pub fn merge_with_flags(base: Config, overrides: Config) -> Config  // Overrides take precedence for non-empty values
pub fn from_flags(target_url, allow_localhost, profile, output_file, timeout_ms) -> Config
pub fn validate_target_required(config: Config) -> Result(Nil, String)
pub fn has_target(config: Config) -> Bool
pub fn is_localhost_allowed(config: Config) -> Bool
```

**Usage pattern**:
```gleam
let env_config = config.load_from_env(os.get_env)
let flag_config = config.from_flags(target, localhost, profile, output_file, timeout)
let final_config = config.merge_with_flags(env_config, flag_config)
use Nil <- result.try(config.validate_target_required(final_config))
Ok(final_config)
```

### Error Handler (`error_handler.gleam`)
Centralized error formatting with severity levels, context, suggestions, and recovery steps.

**Severity levels**:
```gleam
pub type ErrorSeverity {
  Critical  // [CRITICAL] - process cannot continue
  High      // [ERROR] - operation failed
  Medium    // [WARNING] - degraded behavior
  Low       // [INFO] - informational
}
```

**Error structure**:
```gleam
pub type ErrorMessage {
  ErrorMessage(
    severity: ErrorSeverity,
    message: String,                    // Main error description
    context: Dict(String, String),      // Extra context (e.g., file: "spec.cue")
    suggestion: String,                 // Single actionable suggestion
    recovery_steps: List(String),       // Ordered steps to resolve
    exit_code: Int,                     // Process exit code (0=pass, 1=fail, 2=blocked, 3=invalid, 4=error)
  )
}
```

**Output functions**:
```gleam
pub fn format_error_text(error: ErrorMessage) -> String  // Pretty text output
pub fn format_error_json(error: ErrorMessage) -> Json    // Structured JSON
pub fn output_error(error: ErrorMessage, is_json: Bool) -> Int  // Output to stderr, return exit code

pub fn generic_error(message: String, suggestion: String, recovery_steps: List(String)) -> ErrorMessage
pub fn usage_error(command: String, usage: String) -> ErrorMessage
pub fn simple_error(message: String, exit_code: Int) -> ErrorMessage
```

**Output format** (text example):
```
[ERROR]: Failed to connect to target URL
Context:
  url: http://localhost:8080
  timeout_ms: 5000
Suggestion:
  Check that the target service is running
Recovery Steps:
  1. Verify the --target URL is correct
  2. Test connectivity with curl
  3. Check firewall rules
Exit code: 4
```

**Critical**: Error output goes to **stderr**, not stdout (Unix convention). Both JSON and text use `io.println_error()`.

### Integration Checklist

When adding a new command:

1. **Help text**: Use constants from `cli_text_constants.gleam`
   ```gleam
   command("my-command")
   |> command.description(cli_text_constants.cmd_my_desc)
   |> command.flag("--target", cli_flags.target_flag())
   |> command.flag("--json", cli_flags.json_flag())
   ```

2. **Flag validation**: Use helpers from `cli_flags.gleam`
   ```gleam
   use Nil <- result.try(cli_flags.validate_required_string(target, "target"))
   use Nil <- result.try(cli_flags.validate_enum(profile, ["api", "cli", "event", "data", "workflow", "ui"], "profile"))
   Ok(...)
   ```

3. **Configuration**: Load via `config.gleam`
   ```gleam
   let env_config = config.load_from_env(os.get_env)
   let flag_config = config.from_flags(target, localhost, profile, output_file, timeout)
   let config = config.merge_with_flags(env_config, flag_config)
   ```

4. **Error handling**: Use `error_handler.gleam`
   ```gleam
   case result {
     Ok(value) -> Ok(value)
     Error(msg) -> {
       let error = error_handler.generic_error(
         msg,
         "Check your spec file for syntax errors",
         ["Run 'intent validate spec.cue'", "Review error context above"]
       )
       Error(error_handler.output_error(error, is_json))
     }
   }
   ```

5. **Output formatting**: Use `formatter_utils.gleam`
   ```gleam
   let header = formatter_utils.box_header("Analysis Results")
   let score_line = formatter_utils.score_with_status(85.5)
   let indent = formatter_utils.indent_1() <> "→ "
   ```

6. **Emoji**: Use constants from `emoji_constants.gleam`, NOT hardcoded
   ```gleam
   import intent/emoji_constants as emoji
   let icon = emoji.success
   // Do NOT use "✓" or "✗" directly
   ```

## Style
Result types. Exhaustive matching. Small functions. Pipelines (`|>`). No defaults—all fields explicit in specs.

## 15-Phase TDD Workflow

### Overview
```jsonl
{"goal":"Fully automated 15-phase TDD workflow with LLM-as-judge quality gates","type":"sequential_pipeline","phases":15,"execution":"1→2→3→...→15 then STOP","integration":["beads","zjj","gleam-tdd-architect","omarchy","landing-skill"]}
{"deliverable":"~/.claude/skills/tdd15/SKILL.md","desc":"Main orchestrator skill (499 lines)"}
{"deliverable":"~/.claude/skills/tdd15/references/","desc":"15 phase reference docs (progressive disclosure)"}
{"principle":"fully_automated","desc":"No manual input - LLM-as-judge evaluates all quality gates"}
{"principle":"self_healing","desc":"All gates retry 3x with automatic fixes before surfacing failures"}
{"principle":"no_loop","desc":"Workflow runs once per bead, terminates at phase 15"}
{"principle":"gate_driven","desc":"Each phase halts on failure only after 3 self-healing attempts"}
{"principle":"state_tracked","desc":"TodoWrite + Beads track progress"}
```

### Invocation
```bash
/tdd15 <bead-id>
```

### Architecture
```jsonl
{"execution":"linear","flow":"User: /tdd15 <bead-id> → Init → Phase 1-15 → STOP"}
{"state_tracking":"TodoWrite","phases":15,"status":["pending","in_progress","completed","failed"]}
{"bead_lifecycle":"bd show → bd update in_progress → execute phases → bd close"}
{"zjj_lifecycle":"jjz add <bead> → work in isolation → jjz remove <bead>"}
```

### 15-Phase Definitions
```jsonl
{"phase":1,"name":"RESEARCH","skill":"Task(Explore)","output":"codebase context","gate":"sufficient_context","halt_on_fail":true}
{"phase":2,"name":"PLAN","skill":"Task(Plan)","output":"implementation design","gate":"plan_verified","halt_on_fail":true}
{"phase":3,"name":"VERIFY","skill":"Task(Plan) LLM-as-Judge","evaluator":"LLM scores criteria coverage >=80","gate":"plan_verified","halt_on_fail":true}
{"phase":4,"name":"RED","skill":"gleam-tdd-architect","action":"write failing tests","gate":"tests_fail","halt_on_fail":true}
{"phase":5,"name":"GREEN","skill":"gleam-tdd-architect","action":"minimal implementation","gate":"tests_pass","halt_on_fail":true}
{"phase":6,"name":"REFACTOR","skill":"gleam-tdd-architect","action":"clean code","gate":"tests_green","halt_on_fail":true}
{"phase":7,"name":"MARTIN FOWLER CHECK #1","type":"CRITICAL_GATE","skill":"Task(code-reviewer) LLM-as-Judge","questions":8,"evaluator":"LLM scores all 8 criteria >=80","gate":"martin_fowler_1","halt_on_fail":true,"critical":true}
{"phase":8,"name":"IMPLEMENT","action":"complete feature","standards":"cli_consistency","gate":"implementation_complete","halt_on_fail":true}
{"phase":9,"name":"VERIFY SUCCESS CRITERIA","check":"all criteria met","gate":"criteria_met","halt_on_fail":true}
{"phase":10,"name":"INTERROGATE","skill":"omarchy","action":"adversarial FP review","gate":"no_critical_issues","halt_on_fail":true}
{"phase":11,"name":"QA BATTLE TEST","action":"comprehensive testing","gate":"qa_pass","halt_on_fail":true}
{"phase":12,"name":"MARTIN FOWLER CHECK #2","type":"FINAL_GATE","skill":"Task(code-reviewer) LLM-as-Judge","questions":13,"evaluator":"LLM scores all 13 criteria >=80","gate":"martin_fowler_2","halt_on_fail":true,"critical":true}
{"phase":13,"name":"CONSISTENCY CHECK","skill":"Task(pr-review-toolkit:code-reviewer)","gate":"standards_met","halt_on_fail":true}
{"phase":14,"name":"CODE LIABILITY","action":"minimize code","gate":"minimized","halt_on_fail":true}
{"phase":15,"name":"LANDING","skill":"landing-skill","actions":["git commit","git push","bd close","jjz remove"],"gate":"push_succeeded","halt_on_fail":true,"critical":true}
```

### Phase Flow Diagram
```
/tdd15 <bead-id>
  ↓
[INIT: load bead, TodoWrite, bd update, jjz add]
  ↓
1-RESEARCH (Explore) → sufficient_context?
  ↓
2-PLAN (Plan + robot-insights) → plan_verified?
  ↓
3-VERIFY (LLM-as-Judge) → plan_verified (>=80)?
  ↓
4-RED (gleam-tdd-architect) → tests_fail?
  ↓
5-GREEN (gleam-tdd-architect) → tests_pass?
  ↓
6-REFACTOR (gleam-tdd-architect) → tests_green?
  ↓
7-MARTIN FOWLER #1 (LLM-as-Judge, 8 questions, CRITICAL) → martin_fowler_1 (>=80)?
  ↓
8-IMPLEMENT (complete feature) → implementation_complete?
  ↓
9-VERIFY (success criteria) → criteria_met?
  ↓
10-INTERROGATE (omarchy) → no_critical_issues?
  ↓
11-QA (battle test) → qa_pass?
  ↓
12-MARTIN FOWLER #2 (LLM-as-Judge, 13 questions, FINAL) → martin_fowler_2 (>=80)?
  ↓
13-CONSISTENCY (code-reviewer) → standards_met?
  ↓
14-CODE LIABILITY (minimize) → minimized?
  ↓
15-LANDING (landing-skill + git push) → push_succeeded? (CRITICAL)
  ↓
[COMPLETE: TodoWrite done, STOP]
```

### Beads Protocol Integration
```jsonl
{"cmd":"bd show <bead-id> --json","phase":"init","purpose":"Load EARS, DbC, success_criteria"}
{"cmd":"bd update <bead-id> --status in_progress","phase":"init","purpose":"Claim the bead"}
{"cmd":"bv --robot-insights","phase":"2","purpose":"PageRank + Betweenness Centrality insights"}
{"cmd":"bd create --title <title> --deps discovered-from:<bead-id>","phase":"10","purpose":"Track discovered issues"}
{"cmd":"bd close <bead-id> --reason <summary>","phase":"15","purpose":"Mark complete with proof"}
{"dependency":"discovered-from","semantic":"Genealogy tracking for discovered work"}
```

### ZJJ Workspace Isolation
```jsonl
{"cmd":"jjz add <bead-id> --bead <bead-id> --no-open","phase":"init","purpose":"Create isolated workspace at ../<bead-id>"}
{"cmd":"cd ../<bead-id>","phase":"4-14","purpose":"Work in isolation, no branch switching"}
{"cmd":"jjz sync <bead-id>","periodic":true,"purpose":"Rebase workspace on main if needed"}
{"cmd":"jjz remove <bead-id>","phase":"15","purpose":"Cleanup workspace after push"}
{"benefit":"no_branch_switching","desc":"All workspaces coexist on main"}
{"benefit":"no_conflicts","desc":"Isolated file changes, no merge conflicts"}
{"benefit":"parallel_agents","desc":"Multiple TDD workflows can run in parallel"}
```

### Martin Fowler Gate Checklists

#### Phase 7: MARTIN FOWLER CHECK #1 (8 Questions - CRITICAL)
```
1. Is this the simplest solution that works?
2. Has code been refactored for readability?
3. Do tests fully specify the behavior?
4. Is the code readable without extensive comments?
5. Does a live demo work end-to-end?
6. Have all success criteria been verified?
7. Is the UX smooth and intuitive?
8. Are edge cases handled gracefully?
→ HALT on any NO
```

#### Phase 12: MARTIN FOWLER CHECK #2 (13 Questions - FINAL)
```
1-8. [All from Phase 7 checks]
9. Does this integrate cleanly with existing codebase?
10. Is the code debuggable if issues arise?
11. Is performance acceptable for production?
12. Will this be maintainable 6 months from now?
13. Is each line of code necessary?
→ HALT on any NO; allow rewind to Phase 6/8/11
```

### Skills Used
```jsonl
{"skill":"Task","subagent":"Explore","phases":"1","purpose":"Codebase research and pattern discovery"}
{"skill":"Task","subagent":"Plan","phases":"2","purpose":"Implementation design and architecture"}
{"skill":"gleam-tdd-architect","phases":"4,5,6","purpose":"RED-GREEN-REFACTOR cycle"}
{"skill":"omarchy","phases":"10","purpose":"Adversarial FP review"}
{"skill":"Task","subagent":"pr-review-toolkit:code-reviewer","phases":"13","purpose":"Consistency standards validation"}
{"skill":"landing-skill","phases":"15","purpose":"Quality gates + git push + cleanup"}
```

### Success Criteria
```jsonl
{"criteria":"skill_created","check":"~/.claude/skills/tdd15/SKILL.md exists"}
{"criteria":"15_phases","check":"all phases execute sequentially"}
{"criteria":"stops_at_15","check":"workflow terminates, no loop"}
{"criteria":"todowrite_tracking","check":"15 phases tracked"}
{"criteria":"gates_work","check":"failures halt execution"}
{"criteria":"martin_fowler_gates","check":"phases 7 and 12 marked CRITICAL"}
{"criteria":"beads_integration","check":"bd show/update/close work"}
{"criteria":"zjj_integration","check":"workspace created/destroyed"}
{"criteria":"landing_succeeds","check":"git push + bd close + jjz remove"}
```

### CLI Consistency Standards for /tdd15

All code generated by /tdd15 must follow Intent CLI standards:
- **Emoji Constants**: Use `emoji_constants.gleam`, never hardcode ✓✗⚠️ etc
- **CLI Flags**: All flags use `cli_flags` builders
- **Error Handling**: Use `error_handler.gleam` module
- **Output Formatting**: Use `formatter_utils.gleam` for consistency
- **Gleam 7 Commandments**:
  - Immutability: No mutable state
  - No nulls: Use Result/Option types
  - Pipelines: Use `|>` operator idiomatically
  - Exhaustive matching: All pattern cases handled
  - Labeled arguments: Use labeled function arguments
  - Type safety: Leverage Gleam's type system
  - Formatting: `gleam format` passes
