# Intent CLI

**Contract-driven API testing with AI-powered planning.**

Intent transforms vague requirements into crystal-clear, atomic work items that an AI can execute deterministically.

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

### Analysis Tools

Five analysis techniques catch what humans miss:
1. **Inversion**: "What would make this fail?"
2. **Second-Order**: "What happens after that?"
3. **Pre-Mortem**: "Why did this fail?"
4. **Checklist**: "What did we miss?"
5. **Circle of Competence**: "What's in scope?"

## Commands

```bash
# Core
intent check <spec.cue> --target <url>   # Run tests against API
intent validate <spec.cue>                # Validate spec syntax

# Interview (AI-driven)
intent interview --profile api --cue      # Start interview, output CUE
intent interview --session X --answer Y   # Submit answer, get next directive
intent beads <session> --cue              # Generate beads as CUE

# KIRK Analysis (optional deep-dive tools)
intent quality <spec.cue>     # Quality scores (5 dimensions)
intent invert <spec.cue>      # What failure cases are missing?
intent coverage <spec.cue>    # HTTP method/status coverage
intent gaps <spec.cue>        # Gap detection via analysis tools

# EARS
intent ears <requirements.md> --output cue   # Parse EARS to CUE
```

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
├── interview.gleam        # Interview engine (722 lines)
├── bead_templates.gleam   # Bead generation
├── kirk/
│   ├── ears_parser.gleam      # EARS → behaviors
│   ├── quality_analyzer.gleam # 5-dimension scoring
│   ├── inversion_checker.gleam # What could fail?
│   └── coverage_analyzer.gleam # Test coverage
└── ...

schema/
├── questions.cue          # Interview questions database
├── ai_protocol.cue        # AI directive schemas (coming)
├── kirk.cue              # KIRK contract types
└── intent.cue            # Core spec schema

docs/
├── MENTAL_LATTICE_FRAMEWORK.md   # Analysis framework theory
├── EARS_KIRK_WORKFLOW.md         # EARS/KIRK workflow
└── INTERACTIVE_QUESTIONING.md    # Question system
```

## The Goal

> By the time a bead reaches the AI, every possible question has been answered, every edge case has been enumerated, and the implementation is purely mechanical translation from specification to code.

**This is deterministic AI-assisted development.**

## Status

- Core CLI: Working
- Interview Engine: Working
- KIRK Analysis: Working
- EARS Parser: Working
- AI-CUE Protocol: In Progress (see beads)

## Limitations

Intent is designed to help with specification clarity, not implementation infrastructure. It does **not** help with:

- **Unclear project structure** - Intent assumes your project layout is known
- **Missing dependencies** - Package installation is outside scope
- **Conflicting existing code** - Intent doesn't resolve merge conflicts
- **Build system issues** - Webpack, bundler, or compiler problems need separate tools
- **Runtime environment problems** - Docker, CI/CD, or deployment configuration

Intent focuses on capturing *what* should be built clearly. *How* to build it remains your responsibility.

## License

MIT
