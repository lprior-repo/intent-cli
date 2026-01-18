# PHASE 6: LLM-Based Help Text Quality Assessment Framework

**Phase Status**: Framework Design Complete ✅
**Date**: 2026-01-18
**Total Documentation**: 88 KB across 4 detailed specifications

---

## Overview

Phase 6 delivers a comprehensive, LLM-assessable framework for evaluating help text quality across all 24 Intent CLI commands. The framework ensures help text is simultaneously excellent for human users and optimized for language model understanding and agent reasoning.

### Why Phase 6 Matters

1. **LLM Readiness**: Help text becomes effective training data for downstream LLM operations
2. **User Experience**: Humans can execute any command confidently using help text alone
3. **Consistency**: All 24 commands follow predictable, learnable patterns
4. **Measurability**: Quality is quantified, tracked, and continuously improved

---

## Deliverables Summary

### 1. PHASE_6_LLM_EVALUATION_FRAMEWORK.md (28 KB)

**The Foundation Document**

Complete specification of the evaluation framework with:

- **5 Scoring Dimensions** (0-100 each):
  - AI-Friendliness (35% weight): How well LLMs parse and understand
  - Usability (25% weight): How well humans can use it
  - Consistency (20% weight): Adherence to patterns across commands
  - Coverage (10% weight): Completeness of documentation
  - Completeness (10% weight): Presence of required sections

- **Detailed Rubrics** for each dimension:
  - Structural Clarity (40 pts): Sections, formatting, code blocks
  - Actionability (30 pts): Examples, flags, error recovery
  - Training Suitability (30 pts): Mental models, workflows, failures
  - Plus 11 additional sub-rubrics

- **4 LLM Evaluation Prompts**:
  - Structural assessment (evaluates parsing clarity)
  - Actionability assessment (evaluates agent usability)
  - Training suitability (evaluates AI agent training)
  - Cross-command consistency (detects terminology/format drift)

- **Quality Tiers**:
  - ★★★★★ Excellent (90-100): Production ready
  - ★★★★☆ Good (75-89): Acceptable with minor improvements
  - ★★★☆☆ Fair (60-74): Needs significant work
  - ★★☆☆☆ Poor (45-59): High priority fixes
  - ★☆☆☆☆ Critical (<45): Urgent/blocking

- **Reference Materials**:
  - Terminology dictionary (spec, feature, behavior, bead, etc.)
  - Standard section headers template
  - Help text template for new commands

**Read This For**: Understanding the "why" and "what" of the evaluation framework

---

### 2. PHASE_6_EVALUATION_EXAMPLES.md (21 KB)

**The Practical Reference**

Concrete, scored examples showing how to apply the framework:

- **Example 1: Excellent Command** (`check`)
  - Current help text provided
  - Scored 89/100 with detailed breakdown
  - Shows all 5 dimensions evaluated
  - Recommendations to reach 90+

- **Example 2: Critical Command** (`interview` - worst case)
  - Before help text (1 sentence, very poor)
  - After help text (complete, well-structured)
  - Shows remediation process
  - Demonstrates 40→92 improvement potential

- **Example 3: Good Command** (`lint`)
  - Scores 77/100 (good but improvable)
  - Shows gaps that prevent reaching excellent
  - Concrete suggestions with point values

- **Scoring Walkthroughs**:
  - How to manually score a command
  - How to verify scoring consistency
  - How to identify improvement priorities

- **Quick Reference**:
  - Scoring tiers at a glance
  - Template for evaluating commands
  - Template for tracking improvements

**Read This For**: "How do I actually score a help text?" and concrete before/after examples

---

### 3. PHASE_6_AUTOMATION_SCRIPT.md (19 KB)

**The Implementation Guide**

Technical specification for implementing automated scoring:

- **6 Check Function Groups**:
  - Structural clarity checks (sections, code blocks, readability)
  - Actionability checks (examples, flags, error docs)
  - Training suitability checks (mental models, workflows)
  - Consistency checks (terminology, formatting, tone)
  - Coverage checks (flag completeness, scenarios)
  - Completeness checks (required categories)

- **Gleam Code Patterns**:
  - Example implementations of check functions
  - Regex patterns for help text parsing
  - JSON schema for metrics output
  - Type definitions (HelpTextMetrics)

- **Integration Approach**:
  - New CLI command: `intent eval-help-text`
  - JSON report format (per-command + summary)
  - Markdown summary generation
  - CSV export capability

- **CI/CD Integration**:
  - GitHub Actions workflow (runs on PR/push)
  - Pre-commit hook (blocks critical commits)
  - PR comment bot (shows score deltas)
  - Artifact uploading

- **Testing Strategy**:
  - Unit tests for each check function
  - Integration tests (evaluate all 24 commands)
  - Golden file tests (compare vs known good/bad)
  - Performance benchmarks (<5 seconds for full eval)

**Read This For**: "How do I build the automation?" and implementation details

---

### 4. PHASE_6_IMPLEMENTATION_CHECKLIST.md (20 KB)

**The Execution Guide**

Complete project management checklist and handoff document:

- **5 Phase Breakdown**:
  - Phase 6a: Framework Design (✅ Complete)
  - Phase 6b: Baseline Assessment (🔷 In Progress)
  - Phase 6c: Remediation & Improvements (🔲 Pending)
  - Phase 6d: Automation Implementation (🔲 Pending)
  - Phase 6e: Validation & Closure (🔲 Pending)

- **Per-Phase Details**:
  - Deliverables checklist
  - Success criteria for each phase
  - Effort estimates (hours)
  - Owner/team assignments
  - Dependencies and blockers

- **Baseline Assessment Tasks**:
  - How to extract help text from all 24 commands
  - Categorization by tier
  - Report generation requirements
  - Expected tier distribution

- **Improvement Workflow**:
  - Priority: Critical < Poor < Fair < Good
  - Improvement strategies (add examples, mental models, etc.)
  - Per-command effort estimates
  - ROI calculations

- **Automation Roadmap**:
  - Development tasks (3-4 weeks)
  - CI/CD integration (2 weeks)
  - Testing strategy
  - Code structure overview

- **Risk Mitigation**:
  - Automation limitations
  - Help text becoming too verbose
  - Framework becoming outdated
  - Consistency enforcement too strict

- **Resources & Budget**:
  - Total Phase 6 effort: 57-73 hours
  - Team composition (2-3 people)
  - Timeline: 2-3 weeks with parallel work

- **Success Metrics**:
  - Overall average ≥80
  - 60%+ in good+ tier
  - Zero critical tier
  - LLM validation correlation >0.85
  - User task success >80%

- **Templates**:
  - Help text review template (for evaluators)
  - Improvement tracking spreadsheet template

**Read This For**: "How do we execute this?" and project management

---

## Quick Navigation

### For Project Leads
→ Start with **PHASE_6_IMPLEMENTATION_CHECKLIST.md**
- High-level overview
- Resource requirements
- Timeline and phases
- Success criteria

### For Evaluators (Phase 6b)
→ Start with **PHASE_6_EVALUATION_EXAMPLES.md**
- Concrete scoring examples
- Manual evaluation process
- Templates to use

### For Developers (Phase 6c - Improvements)
→ Start with **PHASE_6_EVALUATION_EXAMPLES.md**, then **PHASE_6_LLM_EVALUATION_FRAMEWORK.md**
- Understand what makes good help text
- See examples of improvements
- Reference the standard sections template

### For Backend Developers (Phase 6d - Automation)
→ **PHASE_6_AUTOMATION_SCRIPT.md**
- Complete implementation specification
- Gleam code patterns
- CI/CD integration approach

### For Framework Users (Phase 6e - Validation)
→ All documents as reference
- Framework for ongoing evaluation
- Prompts for LLM assessment
- Templates for new commands

---

## Key Concepts

### Scoring Model

```
Overall Score = (
  (AI-Friendliness × 0.35) +
  (Usability × 0.25) +
  (Consistency × 0.20) +
  (Coverage × 0.10) +
  (Completeness × 0.10)
) / 100
```

### AI-Friendliness Breakdown

```
AI-Friendliness (0-100) = (
  (Structural Clarity × 0.40) +
  (Actionability × 0.30) +
  (Training Suitability × 0.30)
)
```

### Quality Tiers

| Tier | Score | Color | Status |
|------|-------|-------|--------|
| ★★★★★ | 90-100 | 🟢 Green | Production Ready |
| ★★★★☆ | 75-89 | 🟡 Yellow | Good |
| ★★★☆☆ | 60-74 | 🟠 Orange | Fair |
| ★★☆☆☆ | 45-59 | 🔴 Red | Poor |
| ★☆☆☆☆ | <45 | ⛔ Critical | Urgent |

### Exit Criteria (Phase 6 Complete)

```
✅ All 24 commands evaluated
✅ Average score ≥ 80
✅ 0 commands in critical tier
✅ ≥60% of commands in good+ tier
✅ Consistency ≥ 90
✅ Automation implemented and deployed
✅ LLM validation passed (correlation >0.85)
✅ Documentation added to CLAUDE.md
```

---

## File Structure

```
/home/lewis/src/intent-cli/.planning/

PHASE_6 Documentation:
├── PHASE_6_README.md                        (This file - orientation)
├── PHASE_6_LLM_EVALUATION_FRAMEWORK.md     (Framework spec, rubrics, prompts)
├── PHASE_6_EVALUATION_EXAMPLES.md          (Practical examples, walkthroughs)
├── PHASE_6_AUTOMATION_SCRIPT.md            (Implementation guide)
└── PHASE_6_IMPLEMENTATION_CHECKLIST.md     (Project management)

Related Project Files:
src/intent/
├── cli_text_constants.gleam        (Command descriptions)
├── formatter_utils.gleam           (Output formatting - reuse)
├── error_handler.gleam             (Error display - reuse)
├── emoji_constants.gleam           (Icons - reuse)
└── config.gleam                    (Configuration - reuse)

Implementation Targets (Future):
src/intent/
├── automated_checks.gleam          (New: scoring logic)
├── help_text_metrics.gleam         (New: data types)
├── evaluation_report.gleam         (New: JSON generation)
└── eval_command.gleam              (New: CLI integration)
```

---

## Next Steps

### Week 1: Orientation
- [ ] Read all 4 Phase 6 documents (aim for understanding, not memorization)
- [ ] Review `PHASE_6_EVALUATION_EXAMPLES.md` (concrete examples cement concepts)
- [ ] Pilot: Manually evaluate 3 sample commands to verify framework

### Week 2: Baseline Assessment (Phase 6b)
- [ ] Extract help text from all 24 commands
- [ ] Score each using framework
- [ ] Generate baseline report
- [ ] Categorize by tier

### Week 3-4: Improvements (Phase 6c)
- [ ] Fix critical tier commands (if any)
- [ ] Improve poor tier commands
- [ ] Standardize fair tier commands
- [ ] Re-score and document improvements

### Week 4-5: Automation (Phase 6d)
- [ ] Implement checking functions
- [ ] Build reporting system
- [ ] Integrate with CLI
- [ ] Set up CI/CD pipeline

### Week 6: Validation (Phase 6e)
- [ ] LLM validation of sample commands
- [ ] User acceptance testing
- [ ] Document best practices in CLAUDE.md
- [ ] Team training and handoff

---

## Usage Examples

### Manual Evaluation (No Automation)

```bash
# 1. Get help text
gleam run -- check --help

# 2. Score against rubric (see PHASE_6_EVALUATION_EXAMPLES.md)
# Record in spreadsheet:
#   Command: check
#   AI-Friendliness: 88/100
#   Usability: 92/100
#   Consistency: 85/100
#   Coverage: 82/100
#   Completeness: 100%
#   Overall: 89.0/100
#   Tier: ★★★★☆

# 3. Identify improvements from PHASE_6_EVALUATION_FRAMEWORK.md
```

### With LLM (Claude Code)

```
Paste this into Claude:

"Evaluate the following help text using the rubric in Section 4.1 of
PHASE_6_LLM_EVALUATION_FRAMEWORK.md:

[PASTE HELP TEXT HERE]

Provide scores for:
1. Structural Clarity (0-40)
2. Actionability (0-30)
3. Training Suitability (0-30)

Calculate AI-Friendliness = (A + B + C) / 100

Suggest 3 improvements to reach 90+/100."
```

### With Automation (Phase 6d - Future)

```bash
# Full evaluation
gleam run -- eval-help-text --output report.json

# View summary
cat report.json | jq '.statistics'

# Check specific command
cat report.json | jq '.commands[] | select(.name == "check")'

# Find critical issues
cat report.json | jq '.commands[] | select(.tier == "★☆☆☆☆")'
```

---

## Terminology Reference

| Term | Definition | Used For |
|------|-----------|----------|
| **Spec** | CUE specification file with features/behaviors | Core data structure |
| **Feature** | Named collection of behaviors (e.g., "Authentication") | Organizational unit |
| **Behavior** | Single request-response test case with validation rules | Test execution unit |
| **Bead** | Atomic 5-30min work unit | Project management |
| **Check** | Validation rule in behavior response | Test assertion |
| **Round** | Mental model pass (EARS, Contracts, Inversion, Effects, Pre-mortem) | Spec development phase |
| **KIRK** | Analysis framework (Quality, Inversion, Coverage, Gaps, Effects) | Deep analysis |

---

## Success Metrics

### Quantitative Targets
- Overall average help text score: ≥80/100
- Excellent tier (90+): ≥15% of commands
- Good tier (75-89): ≥45% of commands
- Zero critical tier (<45) commands
- Consistency score: ≥90
- LLM correlation with automated scoring: >0.85

### Qualitative Targets
- New users can execute any command from help text alone
- Help text serves as effective training corpus for LLMs
- All commands feel cohesive and follow predictable patterns
- No GitHub issues about "how do I use command X?"
- Team confidence in help text quality: ≥4/5

---

## FAQ

**Q: How long does Phase 6 take?**
A: 57-73 hours total (2-3 weeks with 2-3 person team). Can be parallelized.

**Q: Do we need to implement automation?**
A: No - manual evaluation (with spreadsheet) works for Phase 6b. Automation (Phase 6d) is optional but recommended for ongoing maintenance.

**Q: What if help text is already good?**
A: Framework still provides value for consistency, validation, and future improvements. Baseline assessment identifies gaps.

**Q: Can we use the framework for new commands?**
A: Yes! After Phase 6e, add "Help Text Best Practices" section to CLAUDE.md. New commands use template + framework for review.

**Q: How often do we re-evaluate?**
A: Recommended annually or when CI/CD detects degradation.

**Q: Who should evaluate?**
A: Any developer + 1 QA. Doesn't require domain expertise beyond general CLI knowledge.

---

## Contact & Support

**Phase 6 Owner**: [To be assigned]
**Questions**: Review relevant document section, then ask
**Issues**: Track in project backlog with "Phase 6" label

---

## Document Versions

| Document | Size | Version | Last Updated |
|----------|------|---------|--------------|
| PHASE_6_README.md | 8 KB | 1.0 | 2026-01-18 |
| PHASE_6_LLM_EVALUATION_FRAMEWORK.md | 28 KB | 1.0 | 2026-01-18 |
| PHASE_6_EVALUATION_EXAMPLES.md | 21 KB | 1.0 | 2026-01-18 |
| PHASE_6_AUTOMATION_SCRIPT.md | 19 KB | 1.0 | 2026-01-18 |
| PHASE_6_IMPLEMENTATION_CHECKLIST.md | 20 KB | 1.0 | 2026-01-18 |
| **Total** | **96 KB** | - | - |

---

**Status**: Framework Design Complete, Ready for Phase 6b Implementation

**Recommended First Read**: This document (10 minutes) → PHASE_6_IMPLEMENTATION_CHECKLIST.md (20 minutes) → PHASE_6_EVALUATION_EXAMPLES.md (30 minutes)

---

