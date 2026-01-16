# Intent CLI

**Pure planning system that transforms requirements into trackable work items.**

Intent CLI provides a systematic workflow from requirements capture to bead generation, combining formal methods (EARS, KIRK contracts, mental lattices) with practical planning tools.

## Quick Start

**New to Intent?** See [QUICKSTART.md](QUICKSTART.md) for a 5-minute getting started guide.

**Complete reference**: See [PLANNING_WORKFLOW.md](PLANNING_WORKFLOW.md) for the full planning workflow documentation.

**Example requirements**: Try [example-requirements.txt](example-requirements.txt) to see EARS syntax in action.

## Planning Workflow

```
EARS Requirements → Mental Lattice Analysis → KIRK Contracts →
Structure Planning → Bead Generation
```

Intent transforms informal requirements into formal specifications, then organizes them into dependency-ordered work items in the bd issue tracker:

1. **EARS Interview**: Capture requirements using 6 structured patterns
2. **Mental Lattice**: Apply 5 thinking models to validate and refine
3. **KIRK Contracts**: Generate Design-by-Contract specifications
4. **Structure Planning**: Organize into epic/feature/task hierarchy with wave-based dependencies
5. **Bead Generation**: Create trackable work items in bd database

Each stage includes review gates and checkpoints for iteration.

## The Vision

```
Human writes requirements → CLI interviews systematically → CUE schemas control AI → AI executes perfectly
```

**CUE is the center of the universe.** Everything flows through typed, validated CUE schemas:
- Requirements are structured as CUE
- Interview state is tracked in CUE
- AI directives are output as CUE
- Beads (work items) are defined in CUE
- Feedback loops communicate via CUE

## How It Works

### For Humans: Write Requirements Naturally

```bash
# Start an interview
intent interview --profile api --cue

# CLI outputs CUE telling AI what to ask:
# {
#   action: "ask_question"
#   question: {
#     text: "In one sentence, what should this API do?"
#     pattern: "ubiquitous"
#   }
#   progress: { percent_complete: 0 }
# }
```

### For AI: Follow CUE Instructions Exactly

The AI parses the CUE and asks the human that exact question. No improvisation:

```bash
# AI submits human's answer
intent interview --session X --answer "Allow users to log in with email and password"

# CLI processes, outputs next CUE directive
# {
#   action: "ask_question"
#   question: {
#     text: "Who will use this API?"
#     ...
#   }
#   progress: { percent_complete: 20 }
# }
```

### Beads: Atomic Work Units

When the interview completes, CLI generates beads - tiny, perfectly-specified work items:

```cue
beads: [{
    id: "USR-001"
    title: "Implement login endpoint"
    what: "Create POST /login that validates email/password and returns JWT"
    why: "Core authentication for all API access"
    test: "Valid credentials return 200 with JWT; invalid return 401"
    done_when: "All tests pass, endpoint responds correctly"
    edge_cases: ["empty email", "very long password", "unicode characters"]
    dependencies: []
}]
```

## Key Concepts

### EARS Requirements Syntax

Six patterns that eliminate ambiguity:

| Pattern | Template | Use For |
|---------|----------|---------|
| Ubiquitous | THE SYSTEM SHALL [behavior] | Always true |
| Event-Driven | WHEN [trigger] THE SYSTEM SHALL | Cause-effect |
| State-Driven | WHILE [state] THE SYSTEM SHALL | State-dependent |
| Optional | WHERE [condition] THE SYSTEM SHALL | Feature flags |
| Unwanted | IF [condition] THE SYSTEM SHALL NOT | Security |
| Complex | WHILE [state] WHEN [trigger] | Combinations |

### KIRK Contracts

Design by Contract for APIs:
- **Preconditions**: What must be true before
- **Postconditions**: What must be true after
- **Invariants**: What must always be true

### Mental Lattices

Five thinking tools catch what humans miss:
1. **Inversion**: "What would make this fail?"
2. **Second-Order**: "What happens after that?"
3. **Pre-Mortem**: "Why did this fail?"
4. **Checklist**: "What did we miss?"
5. **Circle of Competence**: "What's in scope?"

## Commands

### Planning Workflow Commands

```bash
# EARS Requirements
gleam run -- ears <file>                          # Parse EARS requirements
gleam run -- ears-interview                       # Interactive EARS interview
gleam run -- review-requirements <file> --checkpoint  # Review with validation

# Mental Lattice Analysis
gleam run -- lattice-analyze <file>               # Apply all 5 thinking models
gleam run -- lattice-analyze <file> --model=inversion  # Apply specific model

# KIRK Contracts
gleam run -- generate-contract <file>             # Generate Design-by-Contract specs
gleam run -- review-contracts <file> --checkpoint # Review contracts

# Structure Planning
gleam run -- plan-structure <file> --project=<name>  # Plan epic/feature/task hierarchy
gleam run -- review-structure <file> --project=<name> --checkpoint  # Review structure

# Bead Generation
gleam run -- generate-beads <file> --project=<name>  # Create beads in bd database
gleam run -- review-beads                         # Review bead generation guidance
```

### bd Commands (Work with Generated Beads)

```bash
bd ready              # Show ready work (no blockers)
bd list --status=open # List all open beads
bd show <id>          # View bead details
bd update <id> --status=in_progress  # Claim work
bd close <id> --reason="..."         # Complete work
```

### Legacy API Testing Commands

```bash
# These commands work with CUE specification files
gleam run -- check <spec.cue> --target <url>  # Run tests against API
gleam run -- validate <spec.cue>              # Validate spec syntax
gleam run -- quality <spec.cue>               # Quality analysis
```

See [PLANNING_WORKFLOW.md](PLANNING_WORKFLOW.md) for complete command reference and examples.

## Installation

```bash
# Build from source
gleam build

# Run
gleam run -- check examples/user-api.cue --target http://localhost:8080
```

## Project Structure

```
src/intent/
├── intent.gleam              # CLI entry point (glint commands)
├── kirk/
│   └── ears_parser.gleam     # EARS requirements parser
├── kirk_contract.gleam       # KIRK contract generator
├── structure_planner.gleam   # Epic/feature/task planning with wave dependencies
├── bead_generator.gleam      # bd bead creation
├── review_gates.gleam        # Review checkpoints and validation
├── mental_lattice.gleam      # 5 thinking models
├── checker.gleam             # API response validation (legacy)
└── ...

docs/
├── PLANNING_WORKFLOW.md      # Complete planning workflow guide
├── QUICKSTART.md             # 5-minute getting started guide
├── example-requirements.txt  # Example EARS requirements
├── MENTAL_LATTICE_FRAMEWORK.md   # Theory
├── EARS_KIRK_WORKFLOW.md         # Workflow
└── INTERACTIVE_QUESTIONING.md    # Question system
```

## The Goal

> By the time a bead reaches the AI, every possible question has been answered, every edge case has been enumerated, and the implementation is purely mechanical translation from specification to code.

**This is deterministic AI-assisted development.**

## Status

**Planning Workflow (v1.0)**:
- ✅ EARS Requirements Parser: Complete
- ✅ Mental Lattice Analysis: Complete (5 models)
- ✅ KIRK Contract Generator: Complete
- ✅ Structure Planner: Complete (epic/feature/task + wave dependencies)
- ✅ Bead Generator: Complete (creates beads in bd database)
- ✅ Review Gates: Complete (checkpoints at each stage)
- ✅ Documentation: Complete (PLANNING_WORKFLOW.md, QUICKSTART.md)

**Legacy API Testing**:
- ✅ Core CLI: Working
- ✅ CUE Spec Validation: Working
- ✅ HTTP Request Execution: Working
- ✅ Response Checking: Working

## License

MIT
