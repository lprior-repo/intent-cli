# Intent CLI - Implementation Drive Reverse Prompt

**Active Session Context | Generated: 2026-01-15**

Use this prompt to systematically drive the Intent CLI codebase to completion through the beads workflow.

---

## CURRENT STATE SNAPSHOT

### Project Health
- **Total Issues**: 270 (210 in main database, 170 tombstones)
- **Open**: 214 actionable issues
- **In Progress**: 1 (intent-cli-zb9w: Regex ReDoS vulnerability)
- **Blocked**: 0 issues
- **Closed**: 56 issues
- **Recent Velocity**: 37 closed in last week

### Priority Breakdown
- **P0 (Critical)**: 45 issues - Security, data loss, broken functionality
- **P1 (High)**: 50 issues - Major features, important bugs
- **P2 (Medium)**: 138 issues - Nice-to-have, test coverage
- **P3 (Low)**: 35 issues - Polish, optimization
- **P4 (Backlog)**: 2 issues - Future ideas

### Critical Issues Requiring Immediate Attention

1. **intent-cli-buib** (P0) - CRITICAL: SSRF protection not applied to check --target URL
2. **intent-cli-zb9w** (P1, IN PROGRESS) - CRITICAL: Regex ReDoS vulnerability
3. **intent-cli-47g4** (P1) - Security: TOCTOU race condition in symlink validation
4. **intent-cli-s8ey** (P1) - Security: ETS regex cache has no size limit
5. **intent-cli-mcxx** (P1) - BUG: check command returns exit 0 for failures
6. **intent-cli-y2hh** (P1) - BUG: NULL byte in file path truncates path

---

## TOP PRIORITY WORK TRACKS

### Track 1: Security Hardening (P0-P1)
**Goal**: Eliminate all CRITICAL security vulnerabilities before production use

```bash
# Claim top security issue
bd update intent-cli-buib --status=in_progress --json

# Work items:
1. SSRF protection for --target URLs
2. Regex ReDoS mitigation (IN PROGRESS)
3. TOCTOU symlink race conditions
4. ETS cache memory limits
5. NULL byte security bypass
```

**Success Criteria**:
- All P0 security issues closed
- All P1 security issues closed or documented with mitigation
- Security test coverage >80%

---

### Track 2: Test Coverage Gaps (P2)
**Goal**: Achieve comprehensive test coverage for untested critical modules

**High-Value Test Files Needed**:
1. **intent-cli-7qna**: `test/effects_analyzer_test.gleam` (816 lines untested)
2. **intent-cli-ttnp**: `test/quality_analyzer_test.gleam` (674 lines untested)
3. **intent-cli-b1mn**: `test/gap_detector_test.gleam` (530 lines untested)
4. **intent-cli-uqbt**: `test/coverage_analyzer_test.gleam` (467 lines untested)

Each has detailed EARS specs, DbC contracts, and required test cases in the issue descriptions.

```bash
# Start with highest priority
bd update intent-cli-7qna --status=in_progress --json
```

---

### Track 3: Core Functionality Completion (P0-P1)
**Goal**: Complete Phase 1 interview/automation features

**Top Picks from bv --robot-triage**:
1. **intent-cli-qnf** (Score: 0.396) - Create answer_loader.gleam for CUE answers
2. **intent-cli-f5y** (Score: 0.396) - Modify interview.gleam to use answers dict
3. **intent-cli-xk8** (Score: 0.396) - Update intent.gleam to load answers file
4. **intent-cli-18a** (Score: 0.396) - Create bead_feedback.gleam for tracking
5. **intent-cli-bto** (Score: 0.396) - Add 'bead-status' command
6. **intent-cli-ozf** (Score: 0.396) - Add 'beads-regenerate' command
7. **intent-cli-8sg** (Score: 0.395) - Create plan_mode.gleam for execution plans
8. **intent-cli-woq** (Score: 0.395) - Add 'plan' command
9. **intent-cli-n16** (Score: 0.395) - Add 'plan-approve' command

These are all **high PageRank (100%)**, **low effort**, **high impact** items.

```bash
# These can be parallelized - all have zero dependencies
bd update intent-cli-qnf intent-cli-f5y intent-cli-xk8 --status=in_progress --json
```

---

### Track 4: Bug Fixes (P1)
**Goal**: Fix core functionality bugs affecting user experience

**Critical Bugs**:
1. **intent-cli-mcxx** - Exit code 0 for failures (breaks CI/CD)
2. **intent-cli-y2hh** - NULL byte path truncation
3. Additional bugs from `bd list --type=bug --status=open --json`

---

## WORKFLOW EXECUTION STRATEGY

### Phase 1: Security First (Week 1)
```bash
# Day 1-2: SSRF + ReDoS
bd update intent-cli-buib --status=in_progress
# Complete SSRF protection
bd close intent-cli-buib --reason "SSRF protection implemented"

bd update intent-cli-zb9w --status=in_progress
# Complete ReDoS mitigation
bd close intent-cli-zb9w --reason "Regex validation added"

# Day 3: Symlink + Cache
bd update intent-cli-47g4 --status=in_progress
bd update intent-cli-s8ey --status=in_progress
# ... work in parallel
bd close intent-cli-47g4 intent-cli-s8ey --reason "Fixed"

# Day 4-5: Remaining P1 bugs
bd update intent-cli-mcxx intent-cli-y2hh --status=in_progress
bd close intent-cli-mcxx intent-cli-y2hh --reason "Fixed"
```

### Phase 2: Test Coverage (Week 2)
```bash
# Parallel test file creation (4 agents)
bd update intent-cli-7qna --status=in_progress  # effects_analyzer
bd update intent-cli-ttnp --status=in_progress  # quality_analyzer
bd update intent-cli-b1mn --status=in_progress  # gap_detector
bd update intent-cli-uqbt --status=in_progress  # coverage_analyzer

# Each agent implements ~7-9 test cases per file
# Target: 100% function coverage for these modules
```

### Phase 3: Interview Feature Completion (Week 3)
```bash
# Sequential dependency chain
bd update intent-cli-qnf --status=in_progress   # answer_loader.gleam
bd close intent-cli-qnf --reason "Module created"

bd update intent-cli-f5y --status=in_progress   # interview.gleam mod
bd close intent-cli-f5y --reason "Integrated answers dict"

bd update intent-cli-xk8 --status=in_progress   # intent.gleam update
bd close intent-cli-xk8 --reason "CLI flag added"

# Parallel tracks for remaining commands
bd update intent-cli-18a intent-cli-bto intent-cli-ozf --status=in_progress
# ... implement in parallel
```

### Phase 4: Final Polish (Week 4)
```bash
# Complete remaining P2 issues
bd ready --json | jq -r '.[] | select(.priority == 2) | .id'
# Work through systematically
```

---

## RALPH (OMARCHY) COORDINATION

### Agent Swarm Strategy

When using `/omarchy` or the ralph skill:

**Agent 1: Security Sentinel**
- Focus: P0-P1 security issues only
- Track: intent-cli-buib, intent-cli-zb9w, intent-cli-47g4, intent-cli-s8ey, intent-cli-y2hh
- Mode: Sequential, thorough testing

**Agent 2: Test Coverage Specialist**
- Focus: Create test files for untested modules
- Track: intent-cli-7qna, intent-cli-ttnp, intent-cli-b1mn, intent-cli-uqbt
- Mode: Parallel execution (4 test files)

**Agent 3: Feature Implementer**
- Focus: Phase 1 interview features
- Track: intent-cli-qnf → intent-cli-f5y → intent-cli-xk8 → intent-cli-18a
- Mode: Sequential dependency chain

**Agent 4: CLI Expander**
- Focus: New CLI commands
- Track: intent-cli-bto, intent-cli-ozf, intent-cli-8sg, intent-cli-woq, intent-cli-n16
- Mode: Parallel (no dependencies)

**Agent 5: Bug Squasher**
- Focus: All P1 bugs
- Track: `bd list --type=bug --priority=1 --status=open --json`
- Mode: Sequential with testing

---

## QUALITY GATES

Before closing any issue:
- [ ] `gleam test` passes (all 583+ tests)
- [ ] `gleam build` succeeds with no warnings
- [ ] New code has test coverage
- [ ] Result types used for error handling
- [ ] Pattern matching is exhaustive
- [ ] No TODOs in code (use `bd create` instead)
- [ ] CUE files validated with `cue vet` if applicable
- [ ] Git commit with proper message format

---

## CONTINUOUS MONITORING

### Daily Health Check
```bash
# Morning standup
bd stats
bv --robot-triage | jq '.triage.quick_ref'
bd list --status=in_progress --json

# Identify blockers
bd blocked
```

### Weekly Review
```bash
# Velocity tracking
bd stats | grep "Closed"
bv --robot-insights | jq '.triage.project_health.velocity'

# Adjust priorities
bv --robot-triage | jq '.triage.blockers_to_clear'
```

---

## EXECUTION COMMANDS

### Start the Drive
```bash
# Get top recommendation
bv --robot-next

# Claim and start work
bd update <id> --status=in_progress --json

# Work on the issue (implement, test, commit)

# Mark complete
bd close <id> --reason "Implemented and tested" --json

# Repeat
```

### Parallel Execution
```bash
# Get parallel tracks
bv --robot-plan

# Claim multiple issues
bd update id1 id2 id3 --status=in_progress --json

# Work in parallel (multiple terminals/agents)

# Bulk close
bd close id1 id2 id3 --reason "Completed" --json
```

---

## RALPH SKILL INVOCATION

To activate autonomous multi-agent execution:

```bash
/omarchy "Drive intent-cli to completion following REVERSE_PROMPT_IMPLEMENTATION.md"
```

Or via skill:
```
Use the omarchy skill to coordinate 5 agents working the tracks defined in REVERSE_PROMPT_IMPLEMENTATION.md:
1. Security hardening (P0-P1 security issues)
2. Test coverage gaps (4 major untested modules)
3. Interview feature completion (Phase 1 automation)
4. CLI expansion (new commands)
5. Bug fixes (all P1 bugs)

Work systematically through each track, claiming issues via `bd update`, implementing with full test coverage, and closing via `bd close` with proper commit messages.
```

---

## SUCCESS CRITERIA

**Definition of Done for Intent CLI v1.0**:
- [ ] Zero P0 issues open
- [ ] <5 P1 issues open (all documented)
- [ ] Test coverage >85% overall
- [ ] All Phase 1 interview commands functional
- [ ] All KIRK analysis commands working
- [ ] Security audit passing
- [ ] Performance benchmarks met
- [ ] Documentation complete
- [ ] CI/CD pipeline green

**Current Progress**: 56/270 issues closed (21%)
**Target**: 260/270 issues closed (96%+ completion)

---

## REFERENCES

- Main reverse prompt: `REVERSE_PROMPT.md`
- Agent coordination: `AGENTS.md`
- Claude instructions: `CLAUDE.md`
- Beads documentation: `bd --help`, `bv --help`
- Triage analysis: `bv --robot-triage`
- Next action: `bv --robot-next`

---

**Last Updated**: 2026-01-15 21:34 CST
**Generated By**: Claude Code Session
**Next Review**: After 50 issues closed
