# PHASE 6: Implementation Checklist & Handoff

Complete checklist for LLM-based Help Text Quality Assessment Framework implementation.

---

## Document Map

Three documents provided in Phase 6 deliverables:

| Document | Purpose | Audience |
|----------|---------|----------|
| `PHASE_6_LLM_EVALUATION_FRAMEWORK.md` | Framework design, scoring rubrics, LLM prompts | Architects, LLM engineers |
| `PHASE_6_EVALUATION_EXAMPLES.md` | Concrete evaluation examples, scoring walkthroughs | Evaluators, QA |
| `PHASE_6_AUTOMATION_SCRIPT.md` | Implementation guide for automated scoring | Backend developers |
| `PHASE_6_IMPLEMENTATION_CHECKLIST.md` | This document - how to use framework | Project leads |

---

## Phase 6a: Evaluation Framework Design

**Status**: ✅ COMPLETE

### Deliverables

- [x] 5 scoring dimensions defined with rubrics
  - [x] AI-Friendliness (0-100): 3 submetrics
  - [x] Usability (0-100): 4 criteria
  - [x] Consistency (0-100): 4 criteria
  - [x] Coverage (0-100): 4 criteria
  - [x] Completeness (0-100): required categories
- [x] Scoring weights established: 35% + 25% + 20% + 10% + 10%
- [x] Quality tiers defined: ★★★★★ to ★☆☆☆☆
- [x] Exit criteria specified (≥85 overall, consistency ≥90)
- [x] LLM prompts written (3 primary evaluation contexts)
- [x] Terminology dictionary included
- [x] Standard section headers documented
- [x] Help text template provided

### Key Decisions Made

1. **Weighted Scoring Model**: AI-Friendliness (35%) prioritizes LLM usability
2. **5-Tier System**: Allows nuanced triage (good vs fair vs poor)
3. **Rubric Detail**: Point allocations transparent and reproducible
4. **LLM-First Design**: Prompts optimized for Claude/Opus understanding
5. **No Manual Thresholds**: All criteria automated or algorithmic

### Materials Location

```
/home/lewis/src/intent-cli/.planning/
├── PHASE_6_LLM_EVALUATION_FRAMEWORK.md          (Main specification)
└── PHASE_6_EVALUATION_EXAMPLES.md               (Reference examples)
```

---

## Phase 6b: Baseline Assessment

**Status**: 🔷 IN PROGRESS (awaiting implementation)

### Tasks

- [ ] **Run evaluation on all 24 commands**
  - [ ] Extract help text from `src/intent.gleam`
  - [ ] Parse `cli_text_constants.gleam` descriptions
  - [ ] Collect `glint.long_help()` content
  - [ ] Estimated: 2 hours manual collection + 30 min automated

- [ ] **Categorize by tier**
  - [ ] Excellent (90+): Expected 2-4 commands
  - [ ] Good (75-89): Expected 6-10 commands
  - [ ] Fair (60-74): Expected 8-12 commands
  - [ ] Poor (45-59): Expected 2-5 commands
  - [ ] Critical (<45): Expected 0-2 commands

- [ ] **Generate baseline report**
  - [ ] Overall statistics (mean, median, distribution)
  - [ ] Per-command scores with breakdown
  - [ ] Tier-by-tier summary
  - [ ] Common issue patterns
  - [ ] Format: JSON + Markdown

- [ ] **Identify highest-priority improvements**
  - [ ] Critical tier: Fix immediately
  - [ ] Poor tier: Plan for Phase 6c
  - [ ] Fair tier: Batch improvements

### Success Criteria

```
✓ All 24 commands evaluated with scores
✓ No undefined/missing help texts
✓ Baseline report complete
✓ Tier distribution identified
✓ Zero score disagreement >5 points
```

### Effort Estimate

- Time: 8-12 hours (mix of automated + manual review)
- Tools: Gleam regex, JSON generation, markdown output
- Reviews: 1 pass by QA, 1 pass by tech lead

---

## Phase 6c: Remediation & Improvements

**Status**: 🔲 PENDING (Phase 6b completion)

### Improvement Workflow

#### Step 1: Fix Critical Tier (score < 45)

For each critical command:
1. Read current help text
2. Identify blockers (missing sections, no examples)
3. Add minimal required content (from framework template)
4. Re-score
5. Target: reach 50+ (poor tier minimum)

**Example: Commands likely critical**
- `beads-regenerate` (likely underdocumented)
- Potentially `ears`, `parse` (newer KIRK features)

#### Step 2: Improve Poor Tier (45-59)

Prioritize by impact:
- Commands used frequently → higher priority
- Commands in documentation examples → higher priority
- Commands in typical workflow → higher priority

**Improvements (in order of ROI)**:
1. Add missing examples (typically +10-15 points)
2. Add MENTAL MODEL section (+5-8 points)
3. Add error scenarios (+5-7 points)
4. Clarify terminology (+3-5 points)

#### Step 3: Standardize Fair Tier (60-74)

Batch improvements:
1. Identify consistency issues across fair commands
2. Create standard templates
3. Apply systematically

**Focus areas**:
- Consistency score usually lowest for fair tier
- Flag documentation often incomplete
- Edge cases underdocumented

### Per-Command Effort Estimates

| Tier | Example | Current Score | Target | Effort | Owner |
|------|---------|----------------|--------|--------|-------|
| Critical | beads-regenerate | 42 | 55 | 2h | Developer |
| Poor | parse | 48 | 65 | 3h | Developer |
| Poor | ears | 51 | 68 | 3h | Developer + KIRK expert |
| Fair | show | 68 | 78 | 2h | Developer |
| Fair | export | 65 | 77 | 2h | Developer |
| Fair | history | 62 | 75 | 2h | Developer |
| Good | lint | 77 | 85 | 1.5h | Developer |
| Good | analyze | 79 | 87 | 1.5h | Developer |

### Success Criteria

```
✓ All critical tier commands reach ≥50
✓ All poor tier commands reach ≥60
✓ 60%+ of commands in good/excellent tiers
✓ Consistency score ≥85 across all commands
✓ No commands have <5 examples
✓ All commands have PREREQUISITES section
```

### Review Process

For each improved command:
1. Author updates help text
2. Auto-scorer generates new metrics
3. QA verifies accuracy (manual spot-check)
4. Tech lead reviews for clarity/tone
5. Merge to main

---

## Phase 6d: Automation Implementation

**Status**: 🔲 PENDING (design complete)

### Tasks

- [ ] **Implement `automated_checks.gleam`** (3-4 hours)
  - [ ] Structural clarity checks (sections, code blocks, readability)
  - [ ] Actionability checks (examples, flags, error docs)
  - [ ] Training checks (mental model, workflow, failure modes)
  - [ ] Consistency checks (terminology, formatting, tone)
  - [ ] Coverage checks (flag completeness, scenarios, integration)
  - [ ] Tests for each check function

- [ ] **Create report generation** (2 hours)
  - [ ] JSON schema for per-command report
  - [ ] JSON schema for summary report
  - [ ] Markdown summary generation
  - [ ] CSV export option

- [ ] **Integrate with Intent CLI** (1 hour)
  - [ ] New command: `intent eval-help-text`
  - [ ] Flags: `--output`, `--format`, `--command`
  - [ ] Help text for eval-help-text itself

- [ ] **CI/CD integration** (2 hours)
  - [ ] GitHub Actions workflow
  - [ ] Pre-commit hook
  - [ ] PR comment automation
  - [ ] Artifact uploads

- [ ] **Testing** (2-3 hours)
  - [ ] Unit tests for each check
  - [ ] Integration test (run on all 24 commands)
  - [ ] Golden file tests (known good/bad examples)
  - [ ] Performance testing (< 5 seconds for full eval)

### Code Structure

```
src/intent/
├── automated_checks.gleam          (Main checking logic)
├── help_text_metrics.gleam         (Data types: HelpTextMetrics)
├── evaluation_report.gleam         (JSON generation)
└── eval_command.gleam              (CLI integration)

tests/
├── automated_checks_test.gleam
└── evaluation_report_test.gleam
```

### Success Criteria

```
✓ All check functions implemented
✓ All tests passing (100% coverage)
✓ Full evaluation runs in <5 seconds
✓ JSON schema validates against examples
✓ CI/CD integration functional
✓ Pre-commit hook prevents critical commits
```

### Implementation Notes

- Use existing `emoji_constants.gleam` for ✓/✗ icons
- Use existing `formatter_utils.gleam` for markdown output
- Use existing `error_handler.gleam` for error reporting
- Regex patterns should be validated against real help text

---

## Phase 6e: Validation & Closure

**Status**: 🔲 PENDING (Phase 6d completion)

### Tasks

- [ ] **LLM re-evaluation of sample commands** (1-2 hours)
  - [ ] Run LLM evaluation prompts on 5 improved commands
  - [ ] Compare automated scores vs LLM scores
  - [ ] Analyze discrepancies
  - [ ] Adjust rubrics if needed

- [ ] **User acceptance testing** (1-2 hours)
  - [ ] New team member uses help text
  - [ ] Can they use all 24 commands without external docs?
  - [ ] Measure time-to-success for common tasks

- [ ] **Documentation of best practices** (1 hour)
  - [ ] Create help text writing guide (for future commands)
  - [ ] Document common improvement patterns
  - [ ] Include examples of before/after
  - [ ] Location: Update CLAUDE.md

- [ ] **Handoff & training** (1 hour)
  - [ ] Train team on framework
  - [ ] Demo automated scoring
  - [ ] Explain CI/CD integration
  - [ ] Set standards for new commands

### Success Criteria

```
✓ LLM evaluation correlates >0.85 with automated scoring
✓ New users complete 80%+ of common tasks from help text alone
✓ Best practices guide complete and added to CLAUDE.md
✓ Team trained and confident with framework
✓ Zero critical issues found in 5 sample commands
```

### Measurement Metrics

| Metric | Target | Current |
|--------|--------|---------|
| Average help text quality score | 80+ | TBD |
| Commands in good+ tier | ≥60% | TBD |
| Consistency score | ≥90 | TBD |
| Average AI-Friendliness | ≥80 | TBD |
| Average Usability | ≥80 | TBD |
| User task success rate | ≥80% | TBD |

---

## Quick Start for Evaluators

### Manual Evaluation (Without Automation)

If implementing Phase 6d automation takes too long, can manually evaluate using:

```bash
# 1. Print help text for a command
gleam run -- check --help

# 2. Score against rubric from PHASE_6_LLM_EVALUATION_FRAMEWORK.md
# Section 2.1: AI-Friendliness (40 points max)
#   - Structural clarity: ___/40
#   - Actionability: ___/30
#   - Training suitability: ___/30

# 3. Record scores in spreadsheet
# Columns: Command | AI (0-100) | Usability (0-100) | Consistency (0-100) |
#          Coverage (0-100) | Completeness (%) | Overall | Tier | Notes

# 4. Calculate overall: weighted average
# = (AI * 0.35) + (Usability * 0.25) + (Consistency * 0.20) +
#   (Coverage * 0.10) + (Completeness * 0.10)

# 5. Assign tier based on overall score
# 90+ = ★★★★★, 75-89 = ★★★★☆, 60-74 = ★★★☆☆, 45-59 = ★★☆☆☆, <45 = ★☆☆☆☆
```

### LLM-Assisted Evaluation

Use Claude via Claude Code:

```
Prompt Template:

"Here is the help text for the Intent CLI command 'check':

[PASTE HELP TEXT]

Please evaluate this help text using the framework in Section 4.1 of
PHASE_6_LLM_EVALUATION_FRAMEWORK.md:

1. Structural Clarity for LLM Parsing (0-40 points):
   - Are sections clearly delimited? (WHAT/WHY/WHEN/EXAMPLES)
   - Is the header format consistent?
   - Is information structured in lists?
   - Are code examples in markdown blocks?
   - Are technical terms defined?

2. Actionability for AI Agents (0-30 points):
   - Are examples copy-paste ready?
   - Are flags explained with values?
   - Are error scenarios documented?
   - Is output format described?

3. Suitability for Agent Training (0-30 points):
   - Is the mental model documented?
   - Is workflow integration shown?
   - Are failure modes documented?
   - Are related commands mentioned?

Provide individual scores and total /40, /30, /30. Then calculate:
AI-Friendliness = (X + Y + Z) / 100

Additionally, suggest improvements to reach 90+/100."
```

---

## Integration Checklist

### With Existing Modules

- [ ] **error_handler.gleam**
  - [ ] Use in eval command error reporting
  - [ ] Test error scenarios from help text

- [ ] **formatter_utils.gleam**
  - [ ] Use for markdown output generation
  - [ ] Use for score visualization (progress bars)
  - [ ] Test output formatting

- [ ] **cli_text_constants.gleam**
  - [ ] Pull all command descriptions
  - [ ] Verify against evaluated help texts
  - [ ] Add any missing descriptions

- [ ] **cli_flags.gleam**
  - [ ] Validate flag documentation matches definitions
  - [ ] Check all flags have help text

- [ ] **config.gleam**
  - [ ] Document env vars in help text
  - [ ] Verify precedence rules mentioned

### With CI/CD

- [ ] **GitHub Actions**
  - [ ] `.github/workflows/help-text-check.yml`
  - [ ] Runs on PR, push to main, nightly
  - [ ] Comments with score deltas

- [ ] **Pre-commit Hooks**
  - [ ] `.git/hooks/pre-commit` (local)
  - [ ] Blocks commits if critical tier detected
  - [ ] Outputs summary to developer

- [ ] **Git Configuration**
  - [ ] `.gitignore`: add evaluation_report.json
  - [ ] `.gitattributes`: diff config for JSON reports

### With Documentation

- [ ] **CLAUDE.md**
  - [ ] Add "Help Text Best Practices" section
  - [ ] Link to Phase 6 framework docs
  - [ ] Add examples of good help text

- [ ] **README.md**
  - [ ] Add "Help Text Quality" badge
  - [ ] Link to evaluation report
  - [ ] Explain scoring system briefly

- [ ] **CONTRIBUTING.md**
  - [ ] Require help text review for PRs
  - [ ] Reference framework for new commands
  - [ ] Explain pre-commit hook

---

## Risk Mitigation

### Risk: Automation doesn't catch subjective quality

**Mitigation**:
- Manual review pass after automation (1-2 hours)
- LLM validation on samples (correlate scores)
- Human judgment always final arbiter

### Risk: Help text becomes too verbose

**Mitigation**:
- Set maximum line length guideline (80 chars)
- Use nested sections for long content
- Separate detailed info into "ADVANCED" section

### Risk: Framework becomes outdated

**Mitigation**:
- Annual review of rubric (after 12 months)
- Monitor issue tracker for help text complaints
- Adapt evaluation prompts based on feedback

### Risk: Consistency enforcement is too strict

**Mitigation**:
- Consistency score is 20% of overall (not heavily weighted)
- Allow exceptions with documented reasons
- Review consistency rubric when implementing Phase 6d

---

## Resource Requirements

### Phase 6a (Completed): Framework Design
- **Time**: 20 hours (done)
- **Tools**: Markdown editor, spreadsheet
- **Team**: 1 architect, 1 technical writer

### Phase 6b: Baseline Assessment
- **Time**: 8-12 hours
- **Tools**: Text editor, spreadsheet, Gleam
- **Team**: 1 developer, 1 QA

### Phase 6c: Remediation
- **Time**: 15-25 hours (depends on number of fixes)
- **Tools**: Text editor, version control
- **Team**: 2-3 developers (parallel)

### Phase 6d: Automation
- **Time**: 10-15 hours (development + testing)
- **Tools**: Gleam, regex, JSON libraries, GitHub Actions
- **Team**: 1-2 backend developers

### Phase 6e: Validation
- **Time**: 4-6 hours
- **Tools**: Claude, user testing tools
- **Team**: 1 QA, 1 product owner, 1 developer

**Total Phase 6 Effort**: 57-73 hours (2-3 weeks for 2-3 person team)

---

## Success Definition

### Metrics

```
Phase 6 is successful when:

1. Evaluation Framework
   ✓ 5 scoring dimensions fully specified
   ✓ LLM prompts validated via manual testing
   ✓ Rubric applied to 3+ sample commands with consistency

2. Baseline Assessment
   ✓ All 24 commands evaluated
   ✓ Average score ≥ 75
   ✓ No undefined/missing help texts

3. Remediation
   ✓ 0 commands in critical tier
   ✓ ≥60% of commands in good+ tier
   ✓ Consistency score ≥ 90 across all commands

4. Automation
   ✓ Auto-scoring within 5% of manual scores
   ✓ CI/CD integration blocking critical PRs
   ✓ Full evaluation runs in <5 seconds

5. Validation
   ✓ LLM evaluation correlates >0.85 with automated
   ✓ 80%+ user task success from help text alone
   ✓ Zero critical issues post-publication
```

### Quality Gates

```
Before marking Phase 6 Complete:

✅ Overall avg score ≥ 80
✅ 90%+ of commands in good+ tier
✅ Consistency ≥ 90
✅ No commands without examples
✅ All required sections present in all commands
✅ LLM validation passed
✅ Automated scoring deployed to CI/CD
✅ Documentation added to CLAUDE.md
```

---

## File Locations & References

### Phase 6 Documents

```
/home/lewis/src/intent-cli/.planning/
├── PHASE_6_LLM_EVALUATION_FRAMEWORK.md      (Framework, rubrics, prompts)
├── PHASE_6_EVALUATION_EXAMPLES.md           (Scored examples with walkthroughs)
├── PHASE_6_AUTOMATION_SCRIPT.md             (Implementation guide for auto-scoring)
└── PHASE_6_IMPLEMENTATION_CHECKLIST.md      (This file - how to execute)
```

### Related Project Files

```
/home/lewis/src/intent-cli/
├── src/intent.gleam                         (Command definitions + long_help)
├── src/intent/cli_text_constants.gleam      (Descriptions + flag help)
├── src/intent/formatter_utils.gleam         (Output formatting)
├── src/intent/error_handler.gleam           (Error display)
├── src/intent/emoji_constants.gleam         (Icons)
├── src/intent/config.gleam                  (Configuration)
├── CLAUDE.md                                (Project documentation - update after Phase 6)
└── .planning/                               (All planning docs)
```

---

## Next Steps

### Immediate (This Week)

1. **Review Framework** (1-2 hours)
   - Read `PHASE_6_LLM_EVALUATION_FRAMEWORK.md`
   - Verify rubrics align with project goals
   - Discuss scoring weights with team

2. **Pilot Evaluation** (2-3 hours)
   - Manually evaluate 3 sample commands
   - Compare scores (verify consistency)
   - Validate against examples in Phase 6b

3. **Plan Phase 6b** (1 hour)
   - Assign evaluator(s)
   - Schedule baseline assessment
   - Set deadlines

### Short-term (Weeks 2-3)

1. **Execute Phase 6b**: Baseline assessment of all 24 commands
2. **Prioritize Phase 6c**: Identify high-impact improvements
3. **Start Phase 6d**: Begin automation implementation (if parallelizable)

### Medium-term (Weeks 4-6)

1. **Execute Phase 6c**: Implement improvements
2. **Complete Phase 6d**: Deploy automation
3. **Execute Phase 6e**: Validation and closure

---

## Appendix: Templates

### Help Text Review Template

```
Command: ________________
Reviewer: ________________
Date: ________________

STRUCTURAL CLARITY (0-40)
  ☐ WHAT IT DOES present: Yes / No / Partial
  ☐ WHY YOU'D USE IT present: Yes / No / Partial
  ☐ WHEN TO USE IT present: Yes / No / Partial
  ☐ EXAMPLES present: Yes / No / Partial (count: ___)
  ☐ Code blocks properly marked: Yes / No / Partial
  Score: ___ / 40

ACTIONABILITY (0-30)
  ☐ Examples copy-paste ready: Yes / No / Partial (count: ___)
  ☐ Flags documented: Yes / No / Partial (count: ___)
  ☐ Error scenarios documented: Yes / No / Partial (count: ___)
  ☐ Output format described: Yes / No / Partial
  Score: ___ / 30

TRAINING (0-30)
  ☐ Mental model explained: Yes / No / Partial
  ☐ Workflow shown: Yes / No / Partial
  ☐ Failure modes listed: Yes / No / Partial (count: ___)
  ☐ Related commands mentioned: Yes / No / Partial (count: ___)
  Score: ___ / 30

AI-FRIENDLINESS TOTAL: ___ / 100

Notes:
________________

Recommendations:
1. ________________
2. ________________
3. ________________
```

### Improvement Tracking Template

```
| Command | Phase 6b Score | Issues (5 max) | Phase 6c Target | Owner | Status | Phase 6c Score |
|---------|---|---|---|---|---|---|
| check | 89 | Example vague | 92 | Alice | In Progress | — |
| validate | 87 | — | 90 | Alice | Blocked | — |
| show | 68 | No errors, no examples | 78 | Bob | Pending | — |
```

---

**Phase 6 Framework Status**: ✅ Design Complete, Ready for Implementation

**Recommended Reading Order**:
1. This checklist (orientation)
2. `PHASE_6_LLM_EVALUATION_FRAMEWORK.md` (theory)
3. `PHASE_6_EVALUATION_EXAMPLES.md` (practical examples)
4. `PHASE_6_AUTOMATION_SCRIPT.md` (implementation details)

**Questions? Contact**: [Project Lead]

