---
name: intent:help
description: Show available Intent CLI commands and usage guide
---

<objective>
Display the complete Intent CLI command reference.

Output ONLY the reference content below. Do NOT add project-specific analysis, git status, or next-step suggestions.
</objective>

<reference>
# Intent CLI Command Reference

**Intent CLI** transforms vague requirements into crystal-clear beads with formal rigor (EARS, KIRK, mental lattices). A planning system for Claude Code.

## Quick Start

1. `/intent:interview` - Systematic EARS interview
2. `/intent:analyze` - Mental lattice gap detection
3. `/intent:contract` - Generate KIRK contracts
4. `/intent:plan-structure` - Create epic/feature/task hierarchy
5. `/intent:generate-beads` - Create beads in bd database
6. `/intent:quality` - Verify quality score (target: 90%+)

## Core Workflow

```
Interview → Analysis → Contracts → Structure → Beads → Quality Check
```

### Planning Commands

**`/intent:interview`**
Systematic EARS interview to gather requirements.

- Asks 18-24 questions across 6 EARS patterns
- Creates `.intent/REQUIREMENTS.md`
- Formats requirements in EARS notation

Usage: `/intent:interview`

**`/intent:analyze`**
Apply mental lattice analysis to discover gaps.

- Runs 5 thinking models (inversion, second-order, pre-mortem, checklist, circle)
- Discovers edge cases and failure modes
- Adds 40+ requirements to REQUIREMENTS.md
- Creates `.intent/ANALYSIS.md`

Usage: `/intent:analyze`

**`/intent:contract`**
Generate KIRK contracts from EARS requirements.

- Transforms requirements into formal contracts
- Validates with CUE schemas
- Creates `.intent/CONTRACTS.cue`

Usage: `/intent:contract`

**`/intent:plan-structure`**
Group contracts into epic/feature/task hierarchy.

- Creates semantic grouping
- Identifies dependencies
- Calculates parallel execution waves
- Creates `.intent/STRUCTURE.md`

Usage: `/intent:plan-structure`

**`/intent:generate-beads`**
Create beads in bd database with all metadata.

- Generates atomic work items from structure
- Includes preconditions, postconditions, invariants
- Sets dependencies and wave assignments
- Adds test cases and edge cases

Usage: `/intent:generate-beads`

**`/intent:quality`**
Verify quality score on 5 dimensions.

- Completeness (all fields present)
- Consistency (no contradictions)
- Testability (all beads have tests)
- Clarity (clear descriptions)
- Security (vulnerabilities covered)

Target: 90%+ overall score

Usage: `/intent:quality`

### Review Commands

**`/intent:review-requirements`**
Review and edit REQUIREMENTS.md.

Usage: `/intent:review-requirements`

**`/intent:review-analysis`**
Review and edit ANALYSIS.md.

Usage: `/intent:review-analysis`

**`/intent:review-contracts`**
Review and edit CONTRACTS.cue.

Usage: `/intent:review-contracts`

**`/intent:review-structure`**
Review and edit STRUCTURE.md.

Usage: `/intent:review-structure`

**`/intent:review-final`**
Final review before bead generation.

Usage: `/intent:review-final`

## Files & Structure

```
.intent/
├── PROJECT.md            # Project context
├── REQUIREMENTS.md       # EARS-formatted requirements
├── ANALYSIS.md           # Mental lattice discoveries
├── CONTRACTS.cue         # KIRK contracts (CUE validated)
├── STRUCTURE.md          # Epic/feature hierarchy
├── QUALITY.json          # Quality scores
└── DEPENDENCIES.mermaid  # Dependency graph
```

## Getting Help

- Read `.intent/REQUIREMENTS.md` for requirements
- Read `.intent/ANALYSIS.md` for discovered gaps
- Run `/intent:quality` to check quality
</reference>
