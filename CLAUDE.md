# Claude Instructions for Intent CLI

## Project Metadata
```json
{"name":"intent-cli","description":"Requirements engineering and API testing CLI that transforms vague specifications into deterministic work items","language":"Gleam","runtime":"BEAM/Erlang VM","build_system":"moon","issue_tracker":"bd (beads)","vision":"Human writes requirements → CLI interviews systematically → CUE schemas control AI → Atomic beads generated → AI executes deterministically"}
```

## Critical Build Rules
```jsonl
{"rule":"never_run","commands":["gleam build","gleam test","gleam format"],"reason":"This project uses moon exclusively"}
{"rule":"always_run","commands":["moon run :ci","moon run :format","moon run :build","moon run :test"],"reason":"Moon manages all build/test tasks"}
{"rule":"pre_commit","command":"moon run :ci","mandatory":true,"gates":["format_check","build","test_1201+"]}
{"rule":"install_binary","command":"moon run :install","installs_to":"~/.local/bin/intent"}
```

## Moon CI/CD Commands
```jsonl
{"cmd":"moon run :ci","desc":"Full pipeline: format → build → test","use":"before commits"}
{"cmd":"moon run :install","desc":"Build and install binary to ~/.local/bin","use":"deploy binary"}
{"cmd":"moon run :format","desc":"Auto-format code","use":"format code"}
{"cmd":"moon run :test","desc":"Run test suite (1201+ tests)","use":"run tests"}
{"cmd":"moon run :build","desc":"Compile project","use":"build only"}
```

## Beads Workflow
```jsonl
{"cmd":"bd ready --json","desc":"Find ready work","output":"json"}
{"cmd":"bd update <id> --status in_progress --json","desc":"Claim work","output":"json"}
{"cmd":"bd close <id> --reason 'Done' --json","desc":"Complete work","output":"json"}
{"cmd":"bv --robot-triage","desc":"Comprehensive analysis with recommendations","output":"text"}
{"cmd":"bv --robot-next","desc":"Single top pick with claim command","output":"text"}
{"cmd":"bv --robot-plan","desc":"Parallel execution tracks","output":"text"}
{"cmd":"bv --robot-insights","desc":"Full metrics (PageRank, critical path)","output":"text"}
{"warning":"Never run bare 'bv' - launches interactive TUI that blocks session"}
```

## AI Agent Interview Mode
```jsonl
{"step":1,"cmd":"intent interview --cue --profile api","output":{"action":"ask_question","session":{"id":"interview-abc123"},"question":{},"progress":{}},"note":"Start interview, save session ID"}
{"step":2,"cmd":"intent interview --cue --session 'interview-abc123' --answer 'THE SYSTEM SHALL...'","output":{"action":"ask_question OR interview_complete"},"note":"Repeat until action=interview_complete"}
{"step":3,"cmd":"intent quality .interview/spec-interview-abc123.cue","output":"quality scores and recommendations","note":"Analyze spec"}
{"step":4,"cmd":"intent gaps .interview/spec-interview-abc123.cue","output":"missing requirements","note":"Find gaps"}
{"step":5,"cmd":"intent invert .interview/spec-interview-abc123.cue","output":"what could break","note":"Risk analysis"}
{"step":6,"cmd":"intent beads interview-abc123","output":{"action":"beads_generated","beads":[]},"note":"Generate work items"}
{"step":7,"cmd":"intent check .interview/spec-interview-abc123.cue --target http://localhost:8080 --json","output":{"summary":{"passed":0,"failed":0}},"note":"Test against live API"}
```

## Interview Profiles
```jsonl
{"profile":"api","desc":"REST/HTTP APIs","default":true}
{"profile":"cli","desc":"Command-line tools"}
{"profile":"event","desc":"Event-driven systems"}
{"profile":"data","desc":"Data processing systems"}
{"profile":"workflow","desc":"Business workflows"}
{"profile":"ui","desc":"User interfaces"}
```

## EARS Patterns
```jsonl
{"pattern":"ubiquitous","template":"THE SYSTEM SHALL [behavior]","use":"always-active behaviors"}
{"pattern":"event","template":"WHEN [trigger], THE SYSTEM SHALL [response]","use":"event-driven behaviors"}
{"pattern":"state","template":"WHILE [condition], THE SYSTEM SHALL [behavior]","use":"state-dependent behaviors"}
{"pattern":"unwanted","template":"IF [condition], THEN THE SYSTEM SHALL [behavior]","use":"error handling"}
{"pattern":"optional","template":"WHERE [feature enabled], THE SYSTEM SHALL [behavior]","use":"feature flags"}
```

## KIRK Analysis Commands
```jsonl
{"cmd":"intent quality <spec>","desc":"Score spec across 5 dimensions (completeness, clarity, testability, coverage, correctness)","scale":"0-100"}
{"cmd":"intent invert <spec>","desc":"Find what could break (second-order thinking)","output":"risks and recommendations"}
{"cmd":"intent coverage <spec>","desc":"Check HTTP method/status code coverage","output":"missing methods/codes"}
{"cmd":"intent gaps <spec>","desc":"Find missing requirements using mental lattice","output":"blocking and nice-to-have gaps"}
{"cmd":"intent effects <spec>","desc":"Identify side effects and state changes","output":"write effects, state changes, idempotency"}
```

## API Testing Commands
```jsonl
{"cmd":"intent check <spec> --target <url>","desc":"Run all tests against target API","output":"human-readable results"}
{"cmd":"intent check <spec> --target <url> --json","desc":"Run tests with JSON output","output":"machine-readable JSON"}
{"cmd":"intent check <spec> --target <url> --feature 'Feature Name'","desc":"Run specific feature only"}
{"cmd":"intent check <spec> --target <url> --behavior 'behavior-name'","desc":"Run specific behavior only"}
{"cmd":"intent check <spec> --target <url> --verbose","desc":"Include request/response details"}
{"cmd":"intent check <spec> --target <url> --quiet","desc":"Errors only"}
```

## Exit Codes
```jsonl
{"code":0,"meaning":"Success"}
{"code":1,"meaning":"Test failures"}
{"code":2,"meaning":"Blocked behaviors (dependencies failed)"}
{"code":3,"meaning":"Invalid specification"}
{"code":4,"meaning":"General error (file not found, network error, etc.)"}
```

## Key Files by Subsystem
```jsonl
{"subsystem":"core_entry","files":["src/intent.gleam"]}
{"subsystem":"interview","files":["src/intent/interview.gleam","src/intent/interview_questions.gleam","src/intent/interview_storage.gleam","src/intent/question_loader.gleam","src/intent/answer_loader.gleam"]}
{"subsystem":"kirk_analysis","files":["src/intent/kirk/quality_analyzer.gleam","src/intent/kirk/inversion_checker.gleam","src/intent/kirk/coverage_analyzer.gleam","src/intent/kirk/gap_detector.gleam","src/intent/kirk/ears_parser.gleam","src/intent/kirk/effects_analyzer.gleam"]}
{"subsystem":"api_testing","files":["src/intent/checker.gleam","src/intent/runner.gleam","src/intent/http_client.gleam","src/intent/rules_engine.gleam","src/intent/anti_patterns.gleam"]}
{"subsystem":"core_infra","files":["src/intent/types.gleam","src/intent/parser.gleam","src/intent/loader.gleam","src/intent/resolver.gleam","src/intent/validator.gleam","src/intent/interpolate.gleam","src/intent/output.gleam"]}
{"subsystem":"bead_system","files":["src/intent/bead_templates.gleam","src/intent/bead_feedback.gleam","src/intent/spec_builder.gleam"]}
{"subsystem":"utilities","files":["src/intent/rule.gleam","src/intent/formats.gleam","src/intent/errors.gleam","src/intent/ai_errors.gleam","src/intent_ffi.erl","src/intent/stdin.gleam","src/intent/cli_ui.gleam","src/intent/security.gleam"]}
```

## Required Spec Fields (NO DEFAULTS)
```jsonl
{"spec_fields":["name","description","audience","version","success_criteria","config","features","rules","anti_patterns","ai_hints"],"all_required":true}
{"config_fields":["base_url","timeout_ms","headers"],"all_required":true}
{"feature_fields":["name","description","behaviors"],"behaviors_cannot_be_empty":true}
{"behavior_fields":["name","intent","request","response","notes","requires","tags","captures"],"notes":"notes/requires/tags/captures can be empty but must be present"}
{"request_fields":["method","path","headers","query","body"]}
{"response_fields":["status","example","checks","headers"]}
{"check_fields":["rule","why"],"both_required":true}
```

## The 7 Gleam Commandments
```jsonl
{"commandment":1,"name":"Explicitness","rules":["No implicit type conversions","No operator overloading","No exceptions for control flow","Explicit transformations"],"example":"int.to_string(42) not string(42)"}
{"commandment":2,"name":"Immutability","rules":["Variables are labels for values","Shadowing creates new bindings","All data structures immutable"],"example":"let user = string.trim(user)"}
{"commandment":3,"name":"Type-First Design","rules":["Define custom types before logic","No null/nil - use Option(T)","Union types for states","Opaque types for validation"],"example":"pub type State { Connecting | Connected | Disconnected }"}
{"commandment":4,"name":"Exhaustive Pattern Matching","rules":["Prefer case over if","Compiler enforces exhaustiveness","Tuple matching for complex conditions","Guards limited to comparisons"],"example":"case user.role, is_authenticated { Admin, True -> ... }"}
{"commandment":5,"name":"Pipeline Flow","rules":["Transform with |> operator","Subject-first parameter order","Pipelines over nested calls","Use _ capture syntax"],"example":"raw |> string.trim |> string.lowercase"}
{"commandment":6,"name":"Railway-Oriented Errors","rules":["Never use exceptions","Return Result(value, error)","Chain with result.try or use","Map errors to domain types"],"example":"use content <- result.try(simplifile.read(...))"}
{"commandment":7,"name":"Strict Naming","rules":["Variables/functions: snake_case","Types/Constructors: PascalCase","Modules: snake_case","Constants: SCREAMING_SNAKE_CASE"],"enforced_by":"compiler"}
```

## Anti-Patterns to Avoid
```jsonl
{"antipattern":"bool_blindness","bad":"fn check_login() -> Bool","good":"fn check_login() -> Result(User, LoginError)"}
{"antipattern":"stringly_typed","bad":"status = \"connected\"","good":"status = Connected"}
{"antipattern":"index_iteration","bad":"Loop with list.length and indexing","good":"Use list.map, list.fold, pattern matching","reason":"Lists are linked lists O(n²)"}
{"antipattern":"primitive_obsession","bad":"fn get_user(id: Int)","good":"pub opaque type UserId { UserId(Int) }"}
{"antipattern":"manual_recursion","bad":"Write recursive functions for everything","good":"Use list.map, list.fold, list.filter"}
{"antipattern":"panic_in_libraries","bad":"panic as \"Invalid input\"","good":"Return Result(T, Error)"}
```

## Testing Conventions
```jsonl
{"rule":"mirror_structure","test_dir":"test/","mirrors":"src/"}
{"rule":"naming","test_files":"*_test.gleam","test_functions":"pub fn *_test()"}
{"rule":"assertions","use":"gleeunit/should","examples":["should.equal","should.be_ok","should.be_error"]}
{"rule":"speed","keep_fast":true,"timeout":"5 seconds"}
{"rule":"run_tests","cmd":"moon run :test","count":"1201+ tests"}
```

## Documentation Standards
```jsonl
{"doc_type":"module","syntax":"////","location":"top of file"}
{"doc_type":"function","syntax":"///","location":"immediately before pub fn"}
{"doc_type":"implementation","syntax":"//","location":"inline"}
{"rule":"always_document","scope":"public APIs"}
{"rule":"explain_why","not":"what"}
```

## AI-Friendly Error Format
```json
{"action":"error_category","error":{"type":"specific_error_type","message":"human-readable description","context":{}},"suggestion":"what to do next","recovery":["step 1","step 2","step 3"]}
```

## Common Error Builders
```jsonl
{"builder":"file_not_found","params":["path","expected_location"],"recovery":["mkdir -p <parent>","create file","verify permissions"]}
{"builder":"cue_validation_error","params":["message","file_path"],"recovery":["review schema","check field names","run cue vet"]}
{"builder":"session_not_found","params":["session_id","sessions_path"],"recovery":["list sessions","start new session","check spelling"]}
{"builder":"http_connection_error","params":["error_message","target_url"],"recovery":["check service running","verify port","check firewall"]}
```

## Git Workflow
```jsonl
{"step":1,"action":"Find work","cmd":"bd ready OR bv --robot-next"}
{"step":2,"action":"Claim task","cmd":"bd update <id> --status in_progress"}
{"step":3,"action":"Implement","note":"Stage code changes with git add"}
{"step":4,"action":"Beads auto-sync","note":"Daemon handles .beads/issues.jsonl automatically"}
{"step":5,"action":"Complete","cmd":"bd close <id> --reason 'Done'"}
{"step":6,"action":"Push code","cmd":"git push","note":"Beads already synced by daemon"}
{"warning":"NEVER commit without running 'moon run :ci' first"}
```

## Feature Development Checklist
```jsonl
{"step":1,"action":"Check for beads","cmd":"bd ready"}
{"step":2,"action":"Define types first","note":"Create custom types before logic"}
{"step":3,"action":"Write tests","location":"test/ directory"}
{"step":4,"action":"Implement with pipelines","use":"|> operator"}
{"step":5,"action":"Handle all errors","return":"Result, never panic"}
{"step":6,"action":"Run CI pipeline","cmd":"moon run :ci"}
{"step":7,"action":"Update docs","use":"/// comments for public functions"}
{"step":8,"action":"Commit changes","cmd":"git add . && git commit -m 'message'"}
{"step":9,"action":"Close bead","cmd":"bd close <id>"}
{"step":10,"action":"Install binary","cmd":"moon run :install"}
```

## AI Agent Best Practices
```jsonl
{"practice":1,"rule":"Always use --cue or --json flags for machine-readable output"}
{"practice":2,"rule":"Parse exit codes to determine success/failure/blocked states"}
{"practice":3,"rule":"Save session IDs for resumability across context resets"}
{"practice":4,"rule":"Parse recovery arrays from errors for automatic fixes"}
{"practice":5,"rule":"Check action field first in CUE responses to route logic"}
{"practice":6,"rule":"Use progress fields to show user progress indicators"}
{"practice":7,"rule":"Store .interview/ directory in version control for audit trail"}
{"practice":8,"rule":"Run KIRK analysis before generating beads to catch gaps"}
{"practice":9,"rule":"Execute beads in dependency order (use blocks and depends_on)"}
{"practice":10,"rule":"Mark beads as failed with reasons to improve regeneration"}
```

## AI Agent Pitfalls
```jsonl
{"pitfall":"interactive_commands","bad":"Run interactive commands","good":"Always use --cue or --json flags"}
{"pitfall":"ignore_exit_codes","bad":"Ignore exit codes","good":"Check exit code and parse error output"}
{"pitfall":"skip_kirk","bad":"Skip KIRK analysis","good":"Run quality, gaps, invert before generating beads"}
{"pitfall":"random_order","bad":"Execute beads in random order","good":"Respect depends_on dependencies"}
{"pitfall":"lose_sessions","bad":"Lose session IDs","good":"Save session ID from first response and reuse"}
{"pitfall":"assume_optional","bad":"Assume all fields are optional","good":"Provide all required CUE spec fields"}
{"pitfall":"relative_paths","bad":"Use relative paths","good":"Use absolute paths for all file operations"}
{"pitfall":"skip_ci","bad":"Commit without running moon run :ci","good":"Always run full pipeline before commits"}
```

## Resource Links
```jsonl
{"doc":"AI Protocol Quickstart","path":"docs/AI_PROTOCOL_QUICKSTART.md"}
{"doc":"EARS Syntax Guide","path":"docs/EARS_KIRK_WORKFLOW.md"}
{"doc":"Mental Lattice Framework","path":"docs/MENTAL_LATTICE_FRAMEWORK.md"}
{"doc":"API Reference","path":"docs/API_REFERENCE.md"}
{"doc":"Spec Format","path":"docs/SPEC_FORMAT.md"}
{"doc":"AI-Friendly Errors","path":"docs/ai-friendly-errors.md"}
{"external":"Gleam Language Tour","url":"https://tour.gleam.run/"}
{"external":"Gleam Standard Library","url":"https://hexdocs.pm/gleam_stdlib/"}
{"external":"bd (beads) Documentation","url":"https://github.com/steveyegge/beads"}
{"external":"CUE Language","url":"https://cuelang.org/"}
```

## Session Management
```jsonl
{"location":".interview/sessions.jsonl","format":"JSONL (one JSON object per line)","persistence":"auto-saved after each answer","resumability":"can resume from any point"}
{"cmd":"intent sessions","desc":"List all sessions"}
{"cmd":"intent sessions --profile api","desc":"Filter by profile"}
{"cmd":"intent history <session-id>","desc":"View session details"}
{"cmd":"intent diff <session1> <session2>","desc":"View diff between sessions"}
```

## Complete AI Agent Workflow
```jsonl
{"step":"1_start_interview","cmd":"intent interview --cue --profile api","output_action":"ask_question","save":"session.id"}
{"step":"2_answer_questions","cmd":"intent interview --cue --session '<id>' --answer 'THE SYSTEM SHALL...'","repeat":"until action=interview_complete"}
{"step":"3_quality_analysis","cmd":"intent quality .interview/spec-<id>.cue","output":"quality scores"}
{"step":"4_gap_analysis","cmd":"intent gaps .interview/spec-<id>.cue","output":"missing requirements"}
{"step":"5_risk_analysis","cmd":"intent invert .interview/spec-<id>.cue","output":"what could break"}
{"step":"6_generate_beads","cmd":"intent beads <session-id>","output_action":"beads_generated"}
{"step":"7_review_plan","cmd":"intent plan <session-id>","output":"phased execution plan"}
{"step":"8_approve_plan","cmd":"intent plan-approve <session-id>","output_action":"plan_approved"}
{"step":"9_execute_beads","cmd":"intent bead-status <bead-id> --status in_progress/completed","note":"in dependency order"}
{"step":"10_test_impl","cmd":"intent check .interview/spec-<id>.cue --target <url> --json","output":"test results"}
{"step":"11_regenerate_on_failure","cmd":"intent beads-regenerate <session-id>","condition":"if tests failed"}
```

## Moon Pipeline Details
```jsonl
{"task":"format-check","cmd":"gleam format --check","order":1}
{"task":"build","cmd":"gleam build --target erlang","order":2,"depends_on":["format-check"]}
{"task":"test","cmd":"gleam test","order":3,"depends_on":["build"],"count":"1201+ tests"}
{"task":"escript","cmd":"gleam run -m gleescript","order":4,"depends_on":["test"],"output":"dist/intent/intent"}
{"task":"install","deps":["escript"],"actions":["mkdir -p ~/.local/bin","cp dist/intent/intent ~/.local/bin/","chmod +x ~/.local/bin/intent"]}
{"feature":"smart_caching","mechanism":"hash-based","benefit":"skips unchanged tasks"}
{"feature":"parallel_execution","mechanism":"automatic","benefit":"independent tasks run in parallel"}
```
