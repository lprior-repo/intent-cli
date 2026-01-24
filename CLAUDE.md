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

## Commands (24 total)

**Core Spec Operations** (5):
```jsonl
{"cmd":"validate","args":"<spec>","desc":"CUE syntax check"}
{"cmd":"check","args":"<spec> [--target=URL] [--json=true]","desc":"run spec against API"}
{"cmd":"analyze","args":"<spec>","desc":"quality scoring (alias for quality)"}
{"cmd":"lint","args":"<spec>","desc":"anti-pattern detection"}
{"cmd":"improve","args":"<spec>","desc":"improvement suggestions"}
```

**KIRK Analysis** (6):
```jsonl
{"cmd":"quality","args":"<spec> [--json=true]","desc":"quality scoring (5 dimensions)"}
{"cmd":"coverage","args":"<spec> [--json=true]","desc":"OWASP + edge case coverage"}
{"cmd":"gaps","args":"<spec> [--json=true]","desc":"mental model gap detection"}
{"cmd":"invert","args":"<spec> [--json=true]","desc":"failure mode analysis"}
{"cmd":"effects","args":"<spec> [--json=true]","desc":"second-order effects"}
{"cmd":"ears","args":"<file> [--output=cue|json]","desc":"parse EARS requirements"}
```

**Interview Workflow** (5):
```jsonl
{"cmd":"interview","args":"<profile> [--resume=ID] [--export=spec.cue]","desc":"guided spec discovery"}
{"cmd":"sessions","args":"[--profile=api|cli]","desc":"list interview sessions"}
{"cmd":"history","args":"","desc":"show interview snapshots"}
{"cmd":"diff","args":"<session-id1> <session-id2>","desc":"compare interview sessions"}
{"cmd":"export","args":"<session-id> [--output=spec.cue]","desc":"export session to CUE spec"}
```

**Beads/Planning** (5):
```jsonl
{"cmd":"beads","args":"<session-id> [--json=true] [--max-items=N]","desc":"generate work items from interview session"}
{"cmd":"beads-regenerate","args":"<spec>","desc":"regenerate beads from spec"}
{"cmd":"bead-status","args":"--bead-id <id> --status success|failed|blocked [--reason 'text']","desc":"update individual bead execution status"}
{"cmd":"plan","args":"<session-id> [--json=true] [--rounds=1..5]","desc":"health + waves + beads"}
{"cmd":"plan-approve","args":"<session-id> [--yes] [--notes 'text']","desc":"approve execution plan"}
```
Note: Get session IDs with `intent sessions [--profile=api|cli]`

**Parsing** (2):
```jsonl
{"cmd":"parse","args":"<requirements.md>","desc":"quick EARS validation"}
{"cmd":"ears","args":"<file> [--output=cue|json]","desc":"detailed EARS analysis"}
```

**Utilities** (2):
```jsonl
{"cmd":"doctor","args":"<spec> [--json=true]","desc":"prioritized improvements"}
{"cmd":"show","args":"<spec>","desc":"display spec details"}
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

### JSON Output (Implemented)
```jsonl
{"flag":"--json=true","desc":"Machine-readable JSON output for all KIRK and analysis commands"}
{"commands":"quality, coverage, gaps, invert, effects, doctor, check, validate","status":"implemented"}
{"usage":"intent quality api.cue --json=true | jq '.data.overall_score'"}
```

### Planned Features (Not Yet Implemented)
```jsonl
{"flag":"--robot","desc":"Action metadata wrapper","status":"planned","task":"#10"}
{"flag":"--cin","desc":"Compact Intent Notation","status":"planned"}
{"flag":"--json-out FILE","desc":"Write JSON to file","status":"planned"}
{"cmd":"prompt","desc":"AI context generation from beads","status":"planned","task":"#5"}
{"cmd":"verify","desc":"Auto-verification workflow","status":"planned"}
{"cmd":"feedback","desc":"Generate fix beads from check failures","status":"planned","task":"#12"}
```

### Action JSON Schema
Commands with --json=true return this structure:
```json
{"success":true,"action":"<cmd>_result","command":"<cmd>","data":{...},"errors":[],"next_actions":[...],"metadata":{"timestamp":"...","version":"...","exit_code":0},"spec_path":"..."}
```

### next_actions Field
AI workflow guidance - commands suggest logical next steps:
```json
"next_actions": [
  {"command": "intent gaps spec.cue --json", "reason": "Find coverage gaps"},
  {"command": "intent invert spec.cue --json", "reason": "Analyze failure modes"}
]
```
Supported by: quality, coverage (more commands planned)

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

## Style
Result types. Exhaustive matching. Small functions. Pipelines (`|>`). No defaults—all fields explicit in specs.
