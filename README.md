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

### Mental Lattices

Five thinking tools catch what humans miss:
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

# KIRK Analysis
intent quality <spec.cue>     # Quality scores (5 dimensions)
intent invert <spec.cue>      # What failure cases are missing?
intent coverage <spec.cue>    # HTTP method/status coverage
intent gaps <spec.cue>        # Gap detection via mental models

# EARS
intent ears <requirements.md> --output cue   # Parse EARS to CUE
```

## Installation

```bash
# Build from source
gleam build

# Run
gleam run -- check examples/user-api.cue --target=http://localhost:8080
```

## Common Issues

### Flag Syntax

**IMPORTANT**: All flags require `--flag=value` syntax (with equals sign), not `--flag value`.

```bash
# ✅ CORRECT
intent check api.cue --target=https://api.com
intent interview --profile=api --cue=true
intent quality api.cue --json=true

# ❌ WRONG
intent check api.cue --target https://api.com
intent interview --profile api
intent quality api.cue --json
```

**Why**: The CLI uses Glint which only supports the `=` syntax. Using spaces will cause "flag has no assigned value" errors.

**Tip**: Boolean flags can omit the value if true:
```bash
intent check api.cue --json           # Same as --json=true
intent interview --cue                # Same as --cue=true
```

### Command Aliases and Differences

**parse vs ears** - Both parse EARS requirements but serve different purposes:

```bash
# parse: Quick validation with pattern counts
intent parse requirements.md
# Output: ✓ Parsed 5 ubiquitous requirements, ✓ Parsed 3 event-driven requirements...

# ears: Detailed analysis with multiple output formats
intent ears requirements.md               # Detailed box format
intent ears requirements.md --output=cue  # Generate CUE spec
intent ears requirements.md --output=json # Machine-readable output
```

**When to use**:
- `parse`: Quick validation during editing, see pattern distribution
- `ears`: Full analysis, generating specs, or detailed requirement review

**analyze vs quality** - Identical output, different flags:

```bash
# analyze: Text output only (alias for quality)
intent analyze api.cue

# quality: Supports JSON output
intent quality api.cue        # Same text output as analyze
intent quality api.cue --json # Machine-readable scores
```

## Exit Codes

Intent CLI uses semantic exit codes to enable machine-readable error handling:

| Code | Meaning | Use Cases | CI/CD Action |
|------|---------|-----------|--------------|
| 0 | Success | Spec valid, tests pass, analysis complete | Continue pipeline |
| 1 | General failure | Tests failed, linting warnings found | Fail pipeline, review needed |
| 2 | Blocked behaviors | Check command found blocked behaviors | Investigate blocking issues |
| 3 | Invalid input | File not found, CUE parse error | Fix file path or syntax |
| 4 | Usage error | Missing required args, invalid flags | Fix command invocation |

**Examples**:

```bash
# Success (exit 0)
intent validate api.cue && echo "Valid spec"

# General failure (exit 1)
intent quality api.cue || echo "Quality issues found"

# Invalid input (exit 3)
intent validate missing.cue || echo "File not found or invalid"

# Usage error (exit 4)
intent check || echo "Missing required spec argument"
```

**CI/CD Integration**:

```yaml
# GitHub Actions example
- name: Validate API spec
  run: intent validate api.cue
  # Fails pipeline on exit code 1, 2, 3, or 4

- name: Quality check (non-blocking)
  run: intent quality api.cue || true
  # Continue pipeline even on failure

- name: Check for blocked behaviors
  run: |
    intent check api.cue --target=${{ secrets.API_URL }}
    if [ $? -eq 2 ]; then
      echo "::warning::Blocked behaviors detected"
    fi
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
├── MENTAL_LATTICE_FRAMEWORK.md   # Theory
├── EARS_KIRK_WORKFLOW.md         # Workflow
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

## License

Apache 2.0
