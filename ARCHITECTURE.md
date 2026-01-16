# Intent CLI - Hybrid Architecture

**The Planning System That Combines Formal Rigor with Meta-Prompting**

---

## Core Insight

```
Intent's Formal Methods + GSD's Context Engineering = Perfect Planning
```

This is NOT a testing tool. This is NOT an implementation tool.

**This is a PLANNING tool that outputs deterministic work items (beads).**

---

## The Three Pillars

### Pillar 1: Formal Methods (from Intent)

**EARS (Easy Approach to Requirements Syntax)**
- 6 patterns that eliminate ambiguity
- Forces explicit statement of triggers, states, conditions
- Makes negative requirements visible

**KIRK Contracts (Design by Contract)**
- Preconditions: What must be true BEFORE
- Postconditions: What must be true AFTER
- Invariants: What must ALWAYS be true

**Mental Lattices (5 Thinking Models)**
1. **Inversion**: What could fail?
2. **Second-Order**: What are the consequences?
3. **Pre-Mortem**: Why did this fail?
4. **Checklist**: What did we miss?
5. **Circle of Competence**: What's in scope?

### Pillar 2: Meta-Prompting (from GSD)

**Progressive Disclosure**
```
Commands (thin) → Workflows (detailed) → Templates (structure) → References (deep)
```

**Subagent Orchestration**
- Specialized agents for each phase
- Fresh context per agent (200k tokens)
- No quality degradation

**Context Engineering**
- Size limits based on Claude's quality curve
- @-references for lazy loading
- State files for persistence

### Pillar 3: Type Safety (CUE)

**All state validated**
- Bead schemas enforce completeness
- Contract schemas enforce structure
- Quality schemas enforce targets
- Session schemas enforce consistency

**Runtime validation**
```bash
cue vet schema/bead.cue .intent/beads/
```

If it doesn't validate, it doesn't proceed.

---

## Information Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                     1. EARS INTERVIEW                            │
│  Command: /intent:interview                                      │
│  Agent: ears-interviewer                                         │
│  Output: .intent/REQUIREMENTS.md (6 EARS patterns)              │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                   2. MENTAL LATTICE ANALYSIS                     │
│  Command: /intent:analyze                                        │
│  Agent: lattice-analyzer                                         │
│  Output: .intent/ANALYSIS.md (gaps, edge cases, failures)       │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                   3. KIRK CONTRACT GENERATION                    │
│  Command: /intent:contract                                       │
│  Agent: kirk-generator                                           │
│  Output: .intent/CONTRACTS.cue (validated schemas)              │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                    4. PARALLEL PHASE PLANNING                    │
│  Command: /intent:plan-phase N                                   │
│  Agent: phase-planner (GSD pattern)                              │
│  Output: .intent/phases/NN-name/PLAN.md (wave-based)            │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                     5. BEAD GENERATION                           │
│  Command: /intent:generate-beads                                 │
│  Agent: bead-generator                                           │
│  Output: Beads in bd database (atomic work items)               │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                    6. QUALITY VERIFICATION                       │
│  Command: /intent:quality                                        │
│  Agent: quality-verifier                                         │
│  Output: Quality score (5 dimensions, target 90%+)              │
└─────────────────────────────────────────────────────────────────┘
```

---

## File Structure (After Transformation)

```
.intent/                          # Project state (gitignored)
├── PROJECT.md                    # Project context (GSD pattern)
├── REQUIREMENTS.md               # EARS-formatted requirements
├── ANALYSIS.md                   # Mental lattice findings
├── CONTRACTS.cue                 # KIRK contracts (validated)
├── ROADMAP.md                    # Phase breakdown (GSD pattern)
├── QUALITY.json                  # 5-dimension scores
├── SESSION.cue                   # Interview state (resumable)
└── phases/
    ├── 01-foundation/
    │   ├── PLAN.md               # Phase plan (GSD template)
    │   └── beads.cue             # Generated beads (validated)
    ├── 02-core/
    │   └── ...
    └── ...

~/.claude/intent/                 # Installed system
├── workflows/
│   ├── ears-interview.md
│   ├── mental-lattice-analysis.md
│   ├── kirk-contract-generation.md
│   ├── parallel-planning.md
│   └── bead-generation.md
├── templates/
│   ├── project.md
│   ├── requirements.md
│   ├── contracts.md
│   ├── roadmap.md
│   ├── bead.cue
│   └── plan.md
├── references/
│   ├── ears-patterns.md
│   ├── kirk-contracts.md
│   ├── mental-lattices.md
│   ├── inversion.md
│   ├── second-order.md
│   ├── pre-mortem.md
│   └── quality-dimensions.md
├── agents/
│   ├── ears-interviewer.md
│   ├── lattice-analyzer.md
│   ├── kirk-generator.md
│   ├── bead-generator.md
│   └── quality-verifier.md
└── schema/
    ├── bead.cue
    ├── contract.cue
    ├── requirements.cue
    ├── quality.cue
    └── session.cue

src/intent/                       # Minimal Gleam code
├── validator.gleam               # CUE schema validation
└── quality.gleam                 # Quality scoring engine
```

---

## Agent Responsibilities

### ears-interviewer
**Input**: User answers via AskUserQuestion
**Output**: .intent/REQUIREMENTS.md (EARS-formatted)
**Process**:
1. Progress through 6 EARS patterns
2. Ask 3-5 questions per pattern
3. Validate answers for completeness
4. Format as EARS sentences
5. Save session state for resumption

### lattice-analyzer
**Input**: .intent/REQUIREMENTS.md
**Output**: .intent/ANALYSIS.md
**Process**:
1. Apply Inversion thinking (security, usability, integration failures)
2. Trace Second-Order consequences
3. Run Pre-Mortem scenarios
4. Execute Checklist review
5. Define Circle of Competence boundaries
6. Generate discovered requirements

### kirk-generator
**Input**: .intent/REQUIREMENTS.md + .intent/ANALYSIS.md
**Output**: .intent/CONTRACTS.cue
**Process**:
1. Parse EARS requirements
2. For each requirement:
   - Extract preconditions
   - Define postconditions
   - Identify invariants
3. Incorporate edge cases from analysis
4. Validate with CUE schema
5. Output structured contracts

### phase-planner
**Input**: .intent/CONTRACTS.cue
**Output**: .intent/phases/NN-name/PLAN.md
**Process**:
1. Group contracts by domain/feature
2. Identify dependencies
3. Calculate parallel waves
4. Apply GSD's goal-backward planning
5. Generate must-haves (truths, artifacts, key_links)
6. Create PLAN.md per GSD template

### bead-generator
**Input**: .intent/CONTRACTS.cue + .intent/phases/*/PLAN.md
**Output**: Beads in bd database
**Process**:
1. For each contract:
   - Generate bead ID (semantic prefix)
   - Extract title, what, why from contract
   - Copy preconditions/postconditions/invariants
   - Add edge cases from analysis
   - Generate test cases (given/when/then)
   - Calculate dependencies and wave
   - Estimate effort
2. Validate against bead.cue schema
3. Create in bd database
4. Generate dependency graph

### quality-verifier
**Input**: All .intent/ artifacts
**Output**: .intent/QUALITY.json
**Process**:
1. Score Completeness (all required fields?)
2. Score Consistency (no conflicts?)
3. Score Testability (all behaviors have tests?)
4. Score Clarity (all checks have 'why'?)
5. Score Security (OWASP coverage?)
6. Generate recommendations
7. Identify gaps

---

## Command Interface

### Setup Commands
```bash
/intent:init [project-name]    # Initialize .intent/ directory
/intent:help                   # Show all commands and usage
```

### Interview Commands
```bash
/intent:interview [project]    # Start EARS interview
/intent:resume-interview       # Resume interrupted interview
/intent:show-requirements      # Display current REQUIREMENTS.md
```

### Analysis Commands
```bash
/intent:analyze                # Apply mental lattices
/intent:invert                 # Run inversion thinking only
/intent:second-order           # Trace consequences only
/intent:pre-mortem             # Run failure scenarios only
```

### Contract Commands
```bash
/intent:contract               # Generate KIRK contracts
/intent:validate-contracts     # Validate CUE schemas
/intent:show-contracts         # Display contracts
```

### Planning Commands
```bash
/intent:plan-phase <N>         # Plan phase N (GSD pattern)
/intent:show-roadmap           # Display ROADMAP.md
/intent:plan-all               # Plan all phases
```

### Bead Commands
```bash
/intent:generate-beads [phase] # Generate beads from contracts
/intent:show-beads             # List generated beads
/intent:bead-graph             # Show dependency graph
```

### Quality Commands
```bash
/intent:quality                # Score on 5 dimensions
/intent:gaps                   # Show quality gaps
/intent:recommend              # Get improvement recommendations
```

---

## Quality Targets

Every planning session should achieve:

| Dimension | Target | Measurement |
|-----------|--------|-------------|
| **Completeness** | 100% | All beads have all required fields |
| **Consistency** | 100% | No circular deps, no conflicts |
| **Testability** | 100% | Every bead has test cases |
| **Clarity** | 100% | Every test has given/when/then |
| **Security** | 80%+ | OWASP top 10 covered |

**Overall Target: 90%+**

---

## Key Innovations

### 1. Interview-First Planning
GSD starts with user vision → Claude plans.
Intent starts with **systematic interview** → EARS requirements → KIRK contracts → beads.

The interview eliminates 90% of ambiguity upfront.

### 2. Type-Safe State
GSD uses markdown files (no validation).
Intent uses **CUE schemas** (validated at every step).

Invalid state cannot exist.

### 3. Mental Lattices as Agents
Traditional planning misses edge cases.
Intent **applies 5 thinking models systematically** as subagents.

What humans forget, agents remember.

### 4. Contracts → Beads
GSD plans are prompts.
Intent **generates deterministic beads from formal contracts**.

Same contracts always produce same beads.

### 5. Quality as Gate
GSD verifies post-execution.
Intent **scores quality before bead generation**.

Bad plans never become beads.

---

## Success Metrics

This system succeeds when:

1. **Determinism**: Same requirements → same beads (100% reproducible)
2. **Completeness**: Zero ambiguity in beads (AI can execute without questions)
3. **Coverage**: All edge cases enumerated (no surprises during execution)
4. **Quality**: 90%+ score on 5 dimensions
5. **Efficiency**: Beads execute in parallel waves (minimal sequential work)

---

## Anti-Patterns to Avoid

❌ **Skipping EARS interview** → Ambiguous requirements
❌ **Skipping mental lattices** → Missed edge cases
❌ **Skipping KIRK contracts** → Unclear acceptance criteria
❌ **Manual bead creation** → Inconsistent format, missing fields
❌ **Ignoring quality score** → Low-quality plans slip through
❌ **Sequential planning** → Missed parallelization opportunities

✅ **Follow the workflow**: Interview → Analyze → Contract → Plan → Generate → Verify

---

## Comparison to Other Systems

| Feature | Intent (Transformed) | GSD | Traditional PM |
|---------|---------------------|-----|----------------|
| **Requirements** | EARS (6 patterns) | User vision | User stories |
| **Contracts** | KIRK (pre/post/inv) | Plan templates | Acceptance criteria |
| **Edge Cases** | Mental lattices | Deviation rules | Manual |
| **Type Safety** | CUE schemas | None | None |
| **Quality** | 5-dimension score | Goal-backward | Manual review |
| **Work Items** | Beads (validated) | Tasks (markdown) | Tickets |
| **Parallelization** | Wave-based | Wave-based | Sprint planning |
| **Determinism** | 100% | ~80% | ~20% |

---

## The Vision

> "By the time a bead reaches the AI, every possible question has been answered, every edge case has been enumerated, and the implementation is purely mechanical translation from specification to code."

This is **deterministic AI-assisted development**.

Not "ask Claude to build something and hope."

But "systematically extract requirements, apply formal methods, generate validated work items, execute with zero ambiguity."

---

## Next Steps

See `TRANSFORMATION_PLAN.md` for detailed implementation roadmap.

Estimated time: 2 weeks for complete transformation.
