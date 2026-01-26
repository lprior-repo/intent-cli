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

## Commands (33 total)

**Core Spec Operations** (5):
```jsonl
{"cmd":"validate","args":"<spec>","desc":"CUE syntax check"}
{"cmd":"analyze","args":"<spec>","desc":"quality scoring (alias for quality)"}
{"cmd":"lint","args":"<spec>","desc":"anti-pattern detection"}
{"cmd":"improve","args":"<spec>","desc":"improvement suggestions"}
{"cmd":"diff","args":"<spec1> <spec2>","desc":"compare two spec versions"}
```

**KIRK Analysis** (6):
```jsonl
{"cmd":"quality","args":"<spec>","desc":"quality scoring (5 dimensions)"}
{"cmd":"coverage","args":"<spec>","desc":"OWASP + edge case coverage"}
{"cmd":"gaps","args":"<spec>","desc":"mental model gap detection"}
{"cmd":"invert","args":"<spec>","desc":"failure mode analysis"}
{"cmd":"effects","args":"<spec>","desc":"second-order effects"}
{"cmd":"ears","args":"<file> [--output=cue|json]","desc":"parse EARS requirements"}
```

**Interview Workflow** (4):
```jsonl
{"cmd":"interview","args":"<profile> [--resume=ID] [--export=spec.cue]","desc":"guided spec discovery"}
{"cmd":"sessions","args":"[--profile=api|cli]","desc":"list interview sessions"}
{"cmd":"history","args":"","desc":"show interview snapshots"}
{"cmd":"export","args":"<session-id> [--output=spec.cue]","desc":"export session to CUE spec"}
```

**Beads/Planning** (7):
```jsonl
{"cmd":"beads","args":"<session-id> [--max-items=N]","desc":"generate work items from interview session"}
{"cmd":"beads-regenerate","args":"<spec>","desc":"regenerate beads from spec"}
{"cmd":"bead-status","args":"--bead-id <id> --status success|failed|blocked [--reason 'text']","desc":"update individual bead execution status"}
{"cmd":"plan","args":"<session-id> [--rounds=1..5]","desc":"health + waves + beads"}
{"cmd":"plan-approve","args":"<session-id> [--yes] [--notes 'text']","desc":"approve execution plan"}
{"cmd":"prompt","args":"<session-id> [--max-items=N]","desc":"generate AI implementation prompts from session beads"}
{"cmd":"feedback","args":"--results <check-output.json>","desc":"generate fix beads from check command failures"}
```
Note: Get session IDs with `intent sessions [--profile=api|cli]`

**Parsing** (1):
```jsonl
{"cmd":"parse","args":"<requirements.md>","desc":"quick EARS validation"}
```
Note: `ears` command is in KIRK Analysis section above

**Utilities** (3):
```jsonl
{"cmd":"doctor","args":"<spec>","desc":"prioritized improvements"}
{"cmd":"show","args":"<spec>","desc":"display spec details"}
{"cmd":"help","args":"","desc":"display CLI help information"}
```

**AI Commands** (1):
```jsonl
{"cmd":"ai schema","args":"","desc":"generate action JSON schema documentation"}
```

**Shape Phase Commands** (5):
```jsonl
{"cmd":"shape start","args":"<spec>","desc":"initialize Shape phase session"}
{"cmd":"shape check","args":"<session-id>","desc":"validate Shape phase completeness"}
{"cmd":"shape critique","args":"<session-id>","desc":"generate critique questions for spec"}
{"cmd":"shape respond","args":"<session-id> --answers <file>","desc":"process critique responses"}
{"cmd":"shape agree","args":"<session-id>","desc":"finalize Shape phase agreement"}
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
gleam run -- validate examples/user-api.cue
gleam run -- quality examples/user-api.cue
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
All commands output JSON by default (except `help` which outputs plain text).
```jsonl
{"commands":"quality, coverage, gaps, invert, effects, doctor, check, validate, lint, sessions","status":"implemented"}
{"usage":"intent quality api.cue | jq '.data.overall_score'"}
```

### Implemented AI-Native Features
```jsonl
{"cmd":"prompt","desc":"AI context generation from beads","status":"implemented"}
{"cmd":"feedback","desc":"Generate fix beads from check failures","status":"implemented"}
{"output":"JSON native output for all analysis commands","status":"implemented"}
```

### Planned Features (Not Yet Implemented)
```jsonl
{"flag":"--robot","desc":"Action metadata wrapper","status":"planned","task":"#10"}
{"flag":"--cin","desc":"Compact Intent Notation","status":"planned"}
{"flag":"--json-out FILE","desc":"Write JSON to file","status":"planned"}
{"cmd":"verify","desc":"Auto-verification workflow","status":"planned"}
```

### Action JSON Schema
Commands return this structure:
```json
{"success":true,"action":"<cmd>_result","command":"<cmd>","data":{...},"errors":[],"next_actions":[...],"metadata":{"timestamp":"...","version":"...","exit_code":0},"spec_path":"..."}
```

### next_actions Field
AI workflow guidance - commands suggest logical next steps:
```json
"next_actions": [
  {"command": "intent gaps spec.cue", "reason": "Find coverage gaps"},
  {"command": "intent invert spec.cue", "reason": "Analyze failure modes"}
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
{"mod":"ai_schema","purpose":"action JSON schema generation"}
{"mod":"ai_errors","purpose":"AI-friendly error handling"}
{"mod":"vision_types","purpose":"Shape phase types"}
{"mod":"vision_storage","purpose":"Shape session persistence"}
{"mod":"vision_critique","purpose":"critique question generation"}
{"mod":"vision_session","purpose":"Shape phase state management"}
{"mod":"vision_commands","purpose":"Shape command implementations"}
```

## Style
Result types. Exhaustive matching. Small functions. Pipelines (`|>`). No defaults—all fields explicit in specs.
