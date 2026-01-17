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

## Style
Result types. Exhaustive matching. Small functions. Pipelines (`|>`). No defaults—all fields explicit in specs.
