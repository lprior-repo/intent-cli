# CLAUDE.md Enhancements - Next Level

## Suggested Additions

### 1. Quick Reference Index (Add at Top)
```jsonl
{"section":"critical_build_rules","line":8,"keywords":["moon","gleam","ci","build"],"priority":"critical"}
{"section":"ai_agent_interview","line":37,"keywords":["interview","cue","session","ears"],"priority":"high"}
{"section":"kirk_analysis","line":67,"keywords":["quality","invert","gaps","coverage","effects"],"priority":"high"}
{"section":"gleam_commandments","line":117,"keywords":["gleam","types","pipeline","result","pattern"],"priority":"high"}
{"section":"error_handling","line":156,"keywords":["error","recovery","ai-friendly"],"priority":"medium"}
{"section":"workflows","line":169,"keywords":["git","bd","workflow","checklist"],"priority":"medium"}
```

### 2. Command Chaining Patterns
```jsonl
{"pattern":"interview_to_test","chain":["intent interview --cue --profile api","intent interview --cue --session <id> --answer '...'","intent quality .interview/spec-<id>.cue","intent beads <id>","intent check .interview/spec-<id>.cue --target <url> --json"],"use_case":"Complete requirements to testing flow"}
{"pattern":"analysis_suite","chain":["intent quality <spec>","intent gaps <spec>","intent invert <spec>","intent coverage <spec>","intent effects <spec>"],"use_case":"Full KIRK analysis of spec"}
{"pattern":"bead_lifecycle","chain":["bd ready --json | jq -r '.[0].id'","bd update <id> --status in_progress","moon run :ci","bd close <id> --reason 'Done'","git push"],"use_case":"Claim, implement, complete, push"}
{"pattern":"failed_test_recovery","chain":["intent check <spec> --target <url> --json","intent beads-regenerate <session-id>","bd ready --json","bd update <new-bead-id> --status in_progress"],"use_case":"Test failure to regenerated beads"}
```

### 3. Common Error Patterns & Resolutions
```jsonl
{"error":"CUE export failed: field not allowed","cause":"Spec has extra fields not in schema","resolution":{"check":"cue vet <spec>","fix":"Remove invalid fields or update schema"},"frequency":"common"}
{"error":"Session not found","cause":"Session ID typo or expired","resolution":{"check":"intent sessions","fix":"Use correct ID or start new interview"},"frequency":"common"}
{"error":"Connection refused","cause":"Target API not running","resolution":{"check":"curl <url>/health","fix":"Start API server or fix URL"},"frequency":"very_common"}
{"error":"moon run :ci cached but tests should run","cause":"Moon cache hit despite changes","resolution":{"fix":"moon clean && moon run :ci"},"frequency":"rare"}
{"error":"gleam build ran instead of moon","cause":"Direct gleam command used","resolution":{"fix":"NEVER use gleam directly - use moon run :build"},"frequency":"user_error"}
```

### 4. Performance Expectations
```jsonl
{"operation":"moon run :ci (full)","duration":"~10s","includes":["format","build","test 1201+"],"first_run":true}
{"operation":"moon run :ci (cached)","duration":"<1s","includes":["all tasks cached"],"first_run":false}
{"operation":"moon run :test","duration":"~6s","test_count":"1201+"}
{"operation":"intent interview (5 rounds)","duration":"~5-10min","includes":["user interaction time"]}
{"operation":"intent check (10 behaviors)","duration":"~2-5s","depends_on":"target API latency"}
{"operation":"intent quality <spec>","duration":"~500ms","analysis":"5 dimensions"}
{"operation":"bd ready --json","duration":"~100ms","reads":".beads/issues.jsonl"}
```

### 5. Troubleshooting Decision Trees
```jsonl
{"problem":"Tests failing","step":1,"check":"Exit code = 1?","yes":"Parse JSON output for failures","no":"Check if exit code = 2 (blocked)"}
{"problem":"Tests failing","step":2,"check":"Failures in JSON?","yes":"Read .failures[].rule and .why","no":"Check logs for runtime errors"}
{"problem":"Tests failing","step":3,"check":"Expected vs actual mismatch?","yes":"Update spec or fix API","no":"Check for network/timeout issues"}
{"problem":"Build failing","step":1,"check":"Exit code = ?","format_error":"Run moon run :format","compile_error":"Check gleam error output","test_error":"Fix failing tests"}
{"problem":"Build failing","step":2,"check":"moon cache issue?","yes":"moon clean && moon run :ci","no":"Read compiler output carefully"}
```

### 6. Integration Patterns
```jsonl
{"integration":"intent_with_bd","pattern":"Generate beads from interview, track in bd","workflow":["intent interview --cue","intent beads <session-id>","Parse beads JSON","For each bead: bd create --title <title>"],"benefit":"Track requirements as beads"}
{"integration":"bd_with_moon","pattern":"Claim bead, implement, run CI, close","workflow":["bd ready --json | jq -r '.[0].id'","bd update <id> --status in_progress","<implement>","moon run :ci","bd close <id>"],"benefit":"Quality gates before closing"}
{"integration":"intent_with_git","pattern":"Test spec changes before commit","workflow":["<edit spec>","intent quality <spec>","intent gaps <spec>","git add <spec>","git commit"],"benefit":"Validate specs pre-commit"}
{"integration":"bv_with_bd","pattern":"Find best work, claim, execute","workflow":["bv --robot-next","Copy bd update command","Execute claim","Implement"],"benefit":"Smart work prioritization"}
```

### 7. Context Awareness Hints
```jsonl
{"context":"working_on_gleam_code","hints":["Check 7 Commandments section","Use pipelines |>","Return Result never panic","Pattern match exhaustively"],"section_ref":"line 117"}
{"context":"writing_tests","hints":["Mirror src/ structure in test/","Name files *_test.gleam","Use gleeunit/should","Keep tests fast <5s"],"section_ref":"line 138"}
{"context":"interview_session_active","hints":["Save session ID immediately","Use --cue flag always","Check progress.current_step","EARS format for answers"],"section_ref":"line 37"}
{"context":"api_testing","hints":["Use --json for parsing","Check exit code","Parse failures array","Use --verbose for debugging"],"section_ref":"line 76"}
{"context":"commit_time","hints":["MANDATORY: moon run :ci first","Check bd for open work","Stage code changes only","Beads auto-sync via daemon"],"section_ref":"line 169"}
```

### 8. Template Generators
```jsonl
{"template":"new_feature_bead","generate":"bd create --title 'Implement <feature>' --type feature --priority 2 --description 'Acceptance Criteria:\n- <criterion 1>\n- <criterion 2>\n\nTest Cases:\n- <test 1>\n- <test 2>'"}
{"template":"bug_fix_bead","generate":"bd create --title 'Fix <bug>' --type bug --priority 1 --description 'Bug: <description>\n\nSteps to Reproduce:\n1. <step>\n\nExpected: <expected>\nActual: <actual>\n\nRoot Cause: <cause>\nFix: <fix>'"}
{"template":"test_spec","generate":"intent interview --cue --profile api > /tmp/session.json && export SESSION_ID=$(jq -r '.session.id' /tmp/session.json)"}
{"template":"ci_commit","generate":"moon run :format && moon run :ci && git add . && git commit -m '<message>' && git push"}
```

### 9. State Machine for Interview
```jsonl
{"state":"init","action":"Start interview","cmd":"intent interview --cue --profile <profile>","next_state":"asking","output":"save session.id"}
{"state":"asking","action":"Receive question","check":"output.action == 'ask_question'","next_state":"answering"}
{"state":"answering","action":"Submit answer","cmd":"intent interview --cue --session <id> --answer '<answer>'","next_state":"asking OR complete"}
{"state":"complete","action":"Interview done","check":"output.action == 'interview_complete'","next_state":"analysis","output":"save spec_path"}
{"state":"analysis","action":"Run KIRK suite","cmds":["intent quality <spec>","intent gaps <spec>","intent invert <spec>"],"next_state":"beads"}
{"state":"beads","action":"Generate beads","cmd":"intent beads <session-id>","next_state":"execution","output":"save beads array"}
{"state":"execution","action":"Execute beads","loop":"for each bead","next_state":"testing"}
{"state":"testing","action":"Test implementation","cmd":"intent check <spec> --target <url> --json","next_state":"done OR regenerate"}
{"state":"regenerate","action":"Fix failures","cmd":"intent beads-regenerate <session-id>","next_state":"execution"}
{"state":"done","action":"All tests passing","final":true}
```

### 10. Metric Benchmarks
```jsonl
{"metric":"test_count","current":"1201+","target":"1500+","trend":"growing"}
{"metric":"test_pass_rate","current":"100%","target":"100%","enforcement":"moon blocks on failures"}
{"metric":"format_compliance","current":"100%","target":"100%","enforcement":"moon run :format-check"}
{"metric":"build_time_full","current":"~10s","target":"<15s","includes":"format+build+test"}
{"metric":"build_time_cached","current":"<1s","target":"<1s","benefit":"moon smart cache"}
{"metric":"loc_intent_cli","current":"~15k","trend":"stable","language":"Gleam"}
```

### 11. Keyboard Shortcuts (for AI mapping)
```jsonl
{"shortcut":"ci","expansion":"moon run :ci","context":"pre-commit"}
{"shortcut":"fmt","expansion":"moon run :format","context":"format code"}
{"shortcut":"test","expansion":"moon run :test","context":"run tests only"}
{"shortcut":"ready","expansion":"bd ready --json | jq -r '.[0]'","context":"find work"}
{"shortcut":"claim","expansion":"bd update <id> --status in_progress --json","context":"claim bead"}
{"shortcut":"done","expansion":"bd close <id> --reason 'Done' --json","context":"complete bead"}
{"shortcut":"next","expansion":"bv --robot-next","context":"smart work picker"}
```

### 12. Glossary (AI-optimized)
```jsonl
{"term":"EARS","full":"Easy Approach to Requirements Syntax","patterns":["ubiquitous","event","state","unwanted","optional"],"use":"Requirements format"}
{"term":"KIRK","full":"Knowledge Inversion Reasoning Kit","dimensions":["completeness","clarity","testability","coverage","correctness"],"use":"Design by Contract analysis"}
{"term":"CUE","full":"Configure Unify Execute","website":"https://cuelang.org/","use":"Schema and data validation language"}
{"term":"Bead","definition":"Atomic work item with dependencies","properties":["id","title","type","priority","depends_on","blocks"],"use":"Task tracking"}
{"term":"bd","full":"beads CLI","repo":"https://github.com/steveyegge/beads","use":"Issue tracking system"}
{"term":"bv","full":"beads viewer","flags":["robot-triage","robot-next","robot-plan","robot-insights"],"warning":"Never run bare bv (launches TUI)"}
{"term":"moon","website":"https://moonrepo.dev/","use":"Task runner and CI/CD pipeline","replaces":["gleam build","gleam test","gleam format"]}
```

### 13. Version Compatibility Matrix
```jsonl
{"component":"Gleam","version":"1.x","required":true,"check":"gleam --version"}
{"component":"Erlang/OTP","version":"25+","required":true,"check":"erl -version"}
{"component":"CUE","version":"0.6+","required":true,"check":"cue version"}
{"component":"moon","version":"latest","required":true,"check":"moon --version","install":"~/.moon/bin"}
{"component":"bd","version":"latest","required":true,"check":"bd --version","install":"via cargo"}
{"component":"bv","version":"latest","optional":true,"check":"bv --version","install":"via cargo"}
```

### 14. File Size & Complexity Metrics
```jsonl
{"module":"src/intent/checker.gleam","purpose":"Response validation","status":"largest module","complexity":"high","lines":"~800+"}
{"module":"src/intent/types.gleam","purpose":"Core type definitions","status":"critical","complexity":"medium","imports":"imported by all modules"}
{"module":"src/intent/parser.gleam","purpose":"JSON to Gleam parsing","status":"critical","complexity":"high","note":"Uses dynamic decoders"}
{"module":"src/intent/interview.gleam","purpose":"Interview orchestration","status":"core feature","complexity":"high","state":"session-based"}
{"module":"src/intent/runner.gleam","purpose":"Test execution","status":"core feature","complexity":"medium","note":"Handles dependency resolution"}
```

### 15. Signal Patterns (What to Watch For)
```jsonl
{"signal":"Tests suddenly failing","investigate":["Did spec change?","Did API change?","Network issue?"],"action":"Check git diff + API logs"}
{"signal":"Build slower than usual","investigate":["Moon cache miss?","New dependencies?","Test count increased?"],"action":"Check moon cache status"}
{"signal":"Session IDs not persisting","investigate":["File permissions?",".interview/ directory exists?","Disk space?"],"action":"Check .interview/sessions.jsonl"}
{"signal":"Bead daemon not syncing","investigate":["Daemon running?","Git repo clean?","Network connectivity?"],"action":"bd sync --status"}
{"signal":"gleam command ran directly","investigate":["Old habit?","Script using gleam?","Documentation wrong?"],"action":"Update script to use moon"}
```

## Implementation Suggestions

### Add These Sections to CLAUDE.md:

1. **After line 7 (Project Metadata)**: Add Quick Reference Index
2. **After line 46 (AI Agent Interview)**: Add Interview State Machine
3. **After line 93 (Exit Codes)**: Add Common Error Patterns
4. **After line 126 (Gleam Commandments)**: Add Context Awareness Hints
5. **After line 178 (Git Workflow)**: Add Command Chaining Patterns
6. **After line 206 (Best Practices)**: Add Integration Patterns
7. **After line 218 (Pitfalls)**: Add Troubleshooting Decision Trees
8. **After line 232 (Resources)**: Add Glossary + Version Matrix
9. **End of file**: Add Performance Expectations + Metrics

### Format All New Sections as JSONL

Keep the tight JSON blob format for consistency and AI parseability.

### Benefits

- **Faster lookups**: Index directs AI to right section
- **Error recovery**: Structured troubleshooting paths
- **Workflow automation**: Command chains for common tasks
- **Context switching**: Hints based on what AI is doing
- **Performance awareness**: Know what to expect
- **Integration knowledge**: How tools work together
- **Template acceleration**: Generate common patterns quickly
