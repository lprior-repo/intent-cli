# AI Interview Protocol Documentation

Complete documentation for AI agents interfacing with the Intent CLI interview system.

## Document Overview

This protocol enables AI agents to conduct structured requirements interviews through a simple command-line interface using JSONL streaming.

### Quick Navigation

| Document | Purpose | Audience |
|----------|---------|----------|
| **[AI_PROTOCOL_QUICKSTART.md](./AI_PROTOCOL_QUICKSTART.md)** | 5-minute getting started guide | AI developers (start here) |
| **[AI_PROTOCOL.md](./AI_PROTOCOL.md)** | Complete protocol specification | AI developers, integrators |
| **[AI_PROTOCOL_DIAGRAM.md](./AI_PROTOCOL_DIAGRAM.md)** | Visual flow diagrams | Visual learners, architects |
| **[AI_PROTOCOL_TESTING.md](./AI_PROTOCOL_TESTING.md)** | Test cases and validation | QA engineers, testers |

---

## What is the AI Interview Protocol?

The AI Interview Protocol is a **JSONL-based streaming Q&A system** that enables AI agents to:

1. **Start** requirements interviews for 6 system types (API, CLI, Event, Data, Workflow, UI)
2. **Answer** questions through a structured 5-round interview process
3. **Generate** complete CUE specifications from interview answers
4. **Validate** and test the resulting specifications

### Key Features

- **5-Round Rigorous Methodology**: No shortcuts, full discovery → refinement → validation
- **EARS Pattern Integration**: Easy Approach to Requirements Syntax for structured answers
- **Automatic Session Management**: CLI handles session IDs and persistence
- **Progress Tracking**: Real-time progress (current_step, percent_complete)
- **Error Recovery**: Clear validation errors with retry guidance
- **Stateless Design**: Each message is self-contained JSON

---

## Getting Started (5 Minutes)

### Prerequisites

- Intent CLI installed and in PATH
- Basic understanding of JSON and CLI tools

### Your First Interview

```bash
# 1. Start an API interview
intent interview --cue --profile api

# Output: JSON with first question
{
  "action": "ask_question",
  "question": { "text": "In one sentence, what should this API do?", ... },
  "session": { "id": "interview-abc123", ... }
}

# 2. Submit an answer
intent interview --cue --session "interview-abc123" \
  --answer "THE SYSTEM SHALL authenticate users via JWT tokens"

# Output: Next question OR completion message

# 3. Repeat until complete (25 questions total)

# 4. Final output: Spec path
{
  "action": "interview_complete",
  "output": { "spec_path": ".interview/spec-interview-abc123.cue", ... }
}
```

**Next Steps:**
- Read [AI_PROTOCOL_QUICKSTART.md](./AI_PROTOCOL_QUICKSTART.md) for more details
- See [Complete Example](#complete-example-interaction) below

---

## Document Guide

### 1. Quick Start Guide

**File:** [AI_PROTOCOL_QUICKSTART.md](./AI_PROTOCOL_QUICKSTART.md)

**Length:** ~2 pages

**Contents:**
- Minimal working example
- Command reference
- EARS pattern quick reference
- Error recovery basics

**When to use:** You need to get an AI agent working in < 30 minutes

---

### 2. Full Protocol Specification

**File:** [AI_PROTOCOL.md](./AI_PROTOCOL.md)

**Length:** ~40 pages

**Contents:**
- Complete message type definitions
- JSON schemas for all responses
- Full EARS pattern explanations
- Complete example interaction (10+ turns)
- Error handling reference
- Session persistence details
- FAQ and troubleshooting

**When to use:**
- Building a production AI agent
- Need to understand all edge cases
- Implementing client libraries
- Debugging protocol issues

**Key Sections:**
- **Message Types** (start here for developers)
- **EARS Patterns** (for answer formatting)
- **Complete Example Interaction** (see full flow)
- **JSON Schema** (for validation)
- **Appendices** (profiles, rounds, FAQ)

---

### 3. Visual Diagrams

**File:** [AI_PROTOCOL_DIAGRAM.md](./AI_PROTOCOL_DIAGRAM.md)

**Length:** ~15 pages

**Contents:**
- Complete interview flow diagram
- Round progression visual
- Message type decision tree
- Error handling flow
- Session state machine
- EARS pattern decision tree
- Progress tracking visual
- Session storage structure
- JSON response structure map

**When to use:**
- Architecting an integration
- Understanding the big picture
- Explaining the protocol to stakeholders
- Debugging complex flows

**Highlights:**
- **Complete Interview Flow** - End-to-end interaction
- **Round Progression** - How rounds advance
- **Error Handling Flow** - Recovery paths

---

### 4. Testing Guide

**File:** [AI_PROTOCOL_TESTING.md](./AI_PROTOCOL_TESTING.md)

**Length:** ~25 pages

**Contents:**
- Happy path test cases
- Error handling test cases
- Edge case test cases
- Protocol compliance tests
- Performance benchmarks
- Sample Python test runner
- CI/CD integration examples
- Test data sets

**When to use:**
- Validating your implementation
- QA and testing
- Ensuring protocol compliance
- Performance testing

**Test Categories:**
- **HP (Happy Path)**: Complete flows
- **EH (Error Handling)**: Validation and recovery
- **EC (Edge Cases)**: Boundary conditions
- **PC (Protocol Compliance)**: JSON schema validation
- **PERF (Performance)**: Response time benchmarks

---

## Complete Example Interaction

Below is a condensed example showing the full protocol in action:

### Turn 1: Start

**Agent:**
```bash
intent interview --cue --profile api
```

**CLI:**
```json
{
  "action": "ask_question",
  "question": {
    "text": "In one sentence, what should this API do?",
    "round": 1,
    "priority": "critical",
    "pattern": "ubiquitous",
    "hint": "Use format: THE SYSTEM SHALL [behavior]",
    "examples": ["THE SYSTEM SHALL validate all API inputs", ...]
  },
  "progress": { "current_step": 1, "total_steps": 25, "percent_complete": 0 },
  "session": { "id": "interview-abc123", "profile": "api", ... }
}
```

### Turn 2: Answer

**Agent:**
```bash
intent interview --cue --session "interview-abc123" \
  --answer "THE SYSTEM SHALL authenticate users via JWT tokens"
```

**CLI:**
```json
{
  "action": "ask_question",
  "question": { "text": "Who will use this API? ...", "round": 1, ... },
  "progress": { "current_step": 2, "total_steps": 25, "percent_complete": 4 },
  "session": { "id": "interview-abc123", ... }
}
```

### Turns 3-24: Continue...

*[Questions progress through all 5 rounds]*

### Turn 25: Final Answer + Completion

**Agent:**
```bash
intent interview --cue --session "interview-abc123" \
  --answer "THE SYSTEM SHALL expose Prometheus metrics on /metrics endpoint"
```

**CLI:**
```json
{
  "action": "interview_complete",
  "output": {
    "spec_path": ".interview/spec-interview-abc123.cue",
    "behaviors_count": 25,
    "summary": "Interview complete. Generated spec with 25 behaviors."
  },
  "session": { "id": "interview-abc123", "completed_at": "2026-01-16T15:30:00Z" },
  "statistics": {
    "total_questions": 25,
    "rounds_completed": 5,
    "average_confidence": 0.85
  }
}
```

**Done!** Spec file ready at `.interview/spec-interview-abc123.cue`

---

## Architecture Overview

### Protocol Stack

```
┌─────────────────────────────────────────┐
│         AI Agent (Your Code)            │
│  - Generates answers                    │
│  - Parses JSON responses                │
│  - Tracks session state                 │
└──────────────────┬──────────────────────┘
                   │
                   │ JSONL over stdout
                   │
┌──────────────────▼──────────────────────┐
│      Intent CLI (--cue mode)            │
│  - Manages sessions                     │
│  - Loads questions                      │
│  - Validates answers                    │
│  - Generates specs                      │
└──────────────────┬──────────────────────┘
                   │
                   │ File I/O
                   │
┌──────────────────▼──────────────────────┐
│     .interview/ Storage                 │
│  - sessions.jsonl (state)               │
│  - history.jsonl (audit)                │
│  - spec-*.cue (generated)               │
└─────────────────────────────────────────┘
```

### Data Flow

```
1. Agent invokes CLI with --cue flag
2. CLI generates session ID
3. CLI loads question from CUE database
4. CLI outputs question as JSON to stdout
5. Agent parses JSON
6. Agent generates answer
7. Agent invokes CLI with --session and --answer
8. CLI validates answer
9. CLI extracts fields (behaviors, entities, etc.)
10. CLI checks for gaps/conflicts
11. CLI saves to sessions.jsonl
12. CLI gets next question (or completes)
13. Repeat 4-12 until all rounds complete
14. CLI generates spec from session
15. CLI outputs completion JSON
```

---

## System Profiles

The protocol supports 6 system profiles, each with tailored questions:

| Profile | Focus | Typical Use Cases | Question Count |
|---------|-------|-------------------|----------------|
| **api** | REST/HTTP APIs | Web services, microservices | ~25 |
| **cli** | Command-line tools | Dev tools, automation scripts | ~20 |
| **event** | Event-driven systems | Message queues, pub/sub | ~22 |
| **data** | Data systems | Databases, ETL, data pipelines | ~23 |
| **workflow** | Business workflows | State machines, BPM | ~24 |
| **ui** | User interfaces | Web apps, mobile apps | ~21 |

Each profile goes through the same 5-round structure but with profile-specific questions.

---

## The 5-Round Interview Structure

### Round 1: Discovery (Basic Info)

**Goal:** Understand core purpose, audience, and happy path

**Questions:** 5-7 questions

**Perspectives:** User, Developer, Security

**Examples:**
- What does the system do? (1 sentence)
- Who are the users?
- What's the happy path?
- What's the data model?
- What authentication is needed?

---

### Round 2: Discovery (Behaviors)

**Goal:** Identify common errors and error handling

**Questions:** 5-7 questions

**Perspectives:** User, Developer, Ops

**Examples:**
- Most common errors?
- Error response format?
- Status codes / exit codes?
- What information should never leak?

---

### Round 3: Refinement (Edge Cases)

**Goal:** Explore constraints, dependencies, unusual scenarios

**Questions:** 5-7 questions

**Perspectives:** Developer, Ops, Security

**Examples:**
- Edge cases and corner scenarios?
- External dependencies?
- Performance constraints?
- Concurrency handling?

---

### Round 4: Validation (Security)

**Goal:** Security, compliance, non-functional requirements

**Questions:** 5-7 questions

**Perspectives:** Security, Business, Ops

**Examples:**
- Authorization rules?
- Data sensitivity and encryption?
- Compliance requirements?
- Threat scenarios?

---

### Round 5: Validation (Completeness)

**Goal:** Final checks, non-functional requirements

**Questions:** 3-5 questions

**Perspectives:** All perspectives

**Examples:**
- Performance requirements?
- Scalability needs?
- Observability (metrics, traces)?
- Anything missing?

---

## EARS Patterns Reference

### Pattern Types

1. **Ubiquitous**: `THE SYSTEM SHALL [behavior]`
   - Universal requirements, no conditions

2. **Event-Driven**: `WHEN [trigger] THE SYSTEM SHALL [behavior]`
   - Triggered by specific events

3. **State-Driven**: `WHILE [state] THE SYSTEM SHALL [behavior]`
   - Depends on current state

4. **Optional**: `WHERE [condition] THE SYSTEM SHALL [behavior]`
   - Conditional features

5. **Unwanted**: `IF [condition] THE SYSTEM SHALL NOT [behavior]`
   - Prohibited behaviors

6. **Complex**: `WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]`
   - Multiple conditions

**Why EARS?**
- Reduces ambiguity in requirements
- Forces explicit conditions
- Enables automated parsing
- Improves testability

---

## Error Codes Reference

| Code | Retry? | Meaning | Recovery |
|------|--------|---------|----------|
| `ANSWER_TOO_SHORT` | Yes | Answer < 3 chars | Submit longer answer |
| `INVALID_FORMAT` | Yes | Malformed answer | Fix format, resubmit |
| `SESSION_NOT_FOUND` | No | Invalid session ID | Start new interview |
| `SESSION_EXPIRED` | No | > 24h old | Start new interview |
| `SESSION_COMPLETE` | No | Already finished | Review spec or start new |
| `PROFILE_UNKNOWN` | No | Invalid profile | Use valid profile |
| `INTERNAL_ERROR` | No | System error | Check logs, file perms |

---

## Implementation Checklist

### Minimal Implementation

- [ ] Parse JSON responses
- [ ] Track session ID
- [ ] Submit answers with proper escaping
- [ ] Handle 3 action types: `ask_question`, `interview_complete`, `validation_error`
- [ ] Loop until `interview_complete`

### Production Implementation

- [ ] Use JSON schema validation
- [ ] Implement retry logic for `retry_allowed` errors
- [ ] Track progress and show to user
- [ ] Use EARS patterns in answers
- [ ] Handle shell escaping correctly
- [ ] Support all 6 profiles
- [ ] Implement test suite
- [ ] Add performance monitoring
- [ ] Handle concurrent sessions
- [ ] Implement session resume

---

## FAQ

### Can I skip questions?

No. The protocol enforces answering all questions in sequence to preserve interview rigor.

### Can I change previous answers?

Not in v1.0. Sessions are append-only. To revise, start a new interview.

### How long do sessions last?

Sessions persist indefinitely in `.interview/sessions.jsonl` but expire for active use after 24 hours.

### Can I run multiple interviews in parallel?

Yes. Each session has a unique ID, allowing parallel interviews for different profiles.

### What if a question doesn't apply?

Answer with "Not applicable" or explain why. The system will continue.

### How do I validate my implementation?

See [AI_PROTOCOL_TESTING.md](./AI_PROTOCOL_TESTING.md) for comprehensive test cases.

---

## Support and Contributions

### Getting Help

- **Documentation Issues**: Open a GitHub issue with tag `docs`
- **Protocol Questions**: See [AI_PROTOCOL.md](./AI_PROTOCOL.md) FAQ section
- **Implementation Help**: Check [AI_PROTOCOL_TESTING.md](./AI_PROTOCOL_TESTING.md) troubleshooting

### Contributing

Contributions to improve this protocol are welcome:

1. Protocol enhancements (new message types, etc.)
2. Additional profiles
3. Documentation improvements
4. Test case additions
5. Example implementations

See `CONTRIBUTING.md` in the repository root.

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-01-16 | Initial protocol specification |

---

## License

This protocol documentation is part of the Intent CLI project and follows the same license terms.

---

**Ready to start?** Begin with [AI_PROTOCOL_QUICKSTART.md](./AI_PROTOCOL_QUICKSTART.md) for a 5-minute introduction.
