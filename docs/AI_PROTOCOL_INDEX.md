# AI Interview Protocol - Documentation Index

**Complete documentation suite for AI agents interfacing with Intent CLI**

Total: 4,317 lines | 6 documents | 127 KB

---

## Quick Links

| Need | Start Here | Time |
|------|------------|------|
| Get started fast | [Quickstart Guide](#1-quickstart-guide) | 5 min |
| Build production agent | [Full Specification](#2-full-specification) | 1 hour |
| Understand visually | [Diagrams](#3-visual-diagrams) | 15 min |
| See code examples | [Examples](#6-code-examples) | 30 min |
| Test implementation | [Testing Guide](#4-testing-guide) | 45 min |

---

## Document Catalog

### 1. Quickstart Guide
**[AI_PROTOCOL_QUICKSTART.md](./AI_PROTOCOL_QUICKSTART.md)**

- **Size:** 97 lines (~2 pages)
- **Time to read:** 5 minutes
- **Audience:** AI developers getting started

**Contents:**
- Minimal working example (3 steps)
- Command reference
- EARS pattern quick reference
- Error recovery basics

**When to use:**
- First time implementing
- Need working code fast
- Quick reference

---

### 2. Full Specification
**[AI_PROTOCOL.md](./AI_PROTOCOL.md)**

- **Size:** 1,431 lines (~40 pages)
- **Time to read:** 1-2 hours
- **Audience:** Production developers, integrators

**Contents:**
- Protocol flow (detailed)
- Message type definitions (6 types)
- JSON schemas (3 schemas)
- EARS patterns (6 patterns explained)
- Complete example interaction (10+ turns)
- Error handling reference (7 error codes)
- Session persistence details
- FAQ (10+ questions)
- Appendices (profiles, rounds, change log)

**Key Sections:**
- **§2 Protocol Flow** - Understand the sequence
- **§3 Message Types** - All request/response formats
- **§4 EARS Patterns** - Answer formatting guide
- **§5 Complete Example** - Full walkthrough
- **§6 Error Handling** - All error scenarios
- **Appendices** - Reference material

**When to use:**
- Building production systems
- Need complete understanding
- Handling edge cases
- Debugging issues

---

### 3. Visual Diagrams
**[AI_PROTOCOL_DIAGRAM.md](./AI_PROTOCOL_DIAGRAM.md)**

- **Size:** 460 lines (~15 pages)
- **Time to read:** 15 minutes
- **Audience:** Visual learners, architects

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

**Diagrams:**
1. Complete Interview Flow (end-to-end)
2. Round Progression (5 rounds)
3. Message Types Decision Tree
4. Error Handling Flow
5. Session State Machine
6. EARS Pattern Decision Tree
7. Progress Tracking Visual
8. Storage Structure
9. JSON Response Map

**When to use:**
- Planning architecture
- Understanding big picture
- Explaining to stakeholders
- Debugging complex flows

---

### 4. Testing Guide
**[AI_PROTOCOL_TESTING.md](./AI_PROTOCOL_TESTING.md)**

- **Size:** 818 lines (~25 pages)
- **Time to read:** 45 minutes
- **Audience:** QA engineers, developers

**Contents:**
- Test case catalog (30+ tests)
- Happy path tests (5 tests)
- Error handling tests (5 tests)
- Edge case tests (5 tests)
- Protocol compliance tests (5 tests)
- Performance tests (3 tests)
- Integration tests (3 tests)
- Sample Python test runner
- CI/CD integration examples
- Test data sets

**Test Categories:**
- **HP (Happy Path):** Complete interview flows
- **EH (Error Handling):** Validation and recovery
- **EC (Edge Cases):** Boundary conditions
- **PC (Protocol Compliance):** JSON schema validation
- **PERF (Performance):** Response time benchmarks
- **INT (Integration):** End-to-end testing

**When to use:**
- Validating implementation
- QA testing
- Ensuring compliance
- Performance testing
- CI/CD setup

---

### 5. Main README
**[AI_PROTOCOL_README.md](./AI_PROTOCOL_README.md)**

- **Size:** 557 lines (~18 pages)
- **Time to read:** 20 minutes
- **Audience:** Everyone (overview)

**Contents:**
- Document overview
- What is the protocol
- Getting started (5 minutes)
- Document guide
- Complete example interaction
- Architecture overview
- System profiles
- 5-round structure
- EARS patterns reference
- Error codes reference
- Implementation checklist
- FAQ

**When to use:**
- Starting point for all users
- Understanding what's available
- Choosing which doc to read next
- Quick reference

---

### 6. Code Examples
**[AI_PROTOCOL_EXAMPLES.md](./AI_PROTOCOL_EXAMPLES.md)**

- **Size:** 954 lines (~30 pages)
- **Time to read:** 30 minutes
- **Audience:** Developers implementing agents

**Contents:**
- Python implementation (minimal + production)
- Node.js implementation
- Bash script implementation
- Rust implementation
- Common patterns (5 patterns)
- Testing patterns
- Advanced patterns (LLM integration)

**Implementations:**
1. **Python Minimal** (~50 lines)
2. **Python Production** (~200 lines) - Full error handling, logging, retry
3. **Node.js Minimal** (~60 lines)
4. **Bash Script** (~80 lines)
5. **Rust Minimal** (~100 lines)

**Patterns:**
- Progress tracking
- Retry logic
- Answer caching
- Session resume
- Parallel interviews
- Mock CLI for testing
- LLM integration (OpenAI)

**When to use:**
- Starting implementation
- Need code examples
- Learning best practices
- Copy-paste starter code

---

## Reading Paths

### Path 1: Fast Start (15 minutes)
1. **Quickstart Guide** (5 min) - Get basic understanding
2. **Code Examples** - Python Minimal (5 min) - Copy working code
3. **Main README** - FAQ section (5 min) - Answer common questions

**Goal:** Have a working agent in 15 minutes

---

### Path 2: Production Ready (3 hours)
1. **Main README** (20 min) - Overview
2. **Full Specification** (2 hours) - Deep dive
3. **Testing Guide** (30 min) - Validation
4. **Code Examples** - Production Python (10 min) - Best practices

**Goal:** Production-grade implementation with full understanding

---

### Path 3: Visual Learner (30 minutes)
1. **Visual Diagrams** (15 min) - See the flow
2. **Full Specification** - Complete Example section (10 min) - See it in action
3. **Main README** - Getting Started (5 min) - Try it yourself

**Goal:** Understand through visuals and examples

---

### Path 4: QA/Testing (2 hours)
1. **Main README** (20 min) - Overview
2. **Testing Guide** (1 hour) - All test cases
3. **Code Examples** - Testing Patterns (20 min) - Test code
4. **Full Specification** - JSON Schema section (20 min) - Validation

**Goal:** Comprehensive test coverage

---

## Document Statistics

| Document | Lines | Pages* | Words** | Focus |
|----------|-------|--------|---------|-------|
| Quickstart | 97 | 2 | ~800 | Getting started |
| Full Spec | 1,431 | 40 | ~11,000 | Complete reference |
| Diagrams | 460 | 15 | ~3,500 | Visual learning |
| Testing | 818 | 25 | ~6,500 | QA/validation |
| README | 557 | 18 | ~4,500 | Overview |
| Examples | 954 | 30 | ~7,500 | Code samples |
| **Total** | **4,317** | **130** | **~33,800** | **Complete suite** |

*Estimated pages at ~35 lines per page
**Estimated words at ~8 words per line

---

## By Use Case

### Use Case: Building an AI Agent
**Documents:** Quickstart → Examples → Full Spec

1. Read Quickstart to understand basics
2. Copy Python Minimal example
3. Test with simple answers
4. Read Full Spec for production features
5. Upgrade to Python Production example
6. Add error handling from Full Spec
7. Run tests from Testing Guide

---

### Use Case: Understanding the Protocol
**Documents:** README → Diagrams → Full Spec

1. Read README overview
2. Study Diagrams for visual understanding
3. Read Full Spec for details
4. Try Quickstart example

---

### Use Case: Testing Implementation
**Documents:** Testing → Examples → Full Spec

1. Read Testing Guide test cases
2. Run sample tests from Examples
3. Implement missing tests
4. Validate against Full Spec JSON schemas
5. Run performance benchmarks

---

### Use Case: Integration Planning
**Documents:** README → Diagrams → Full Spec → Examples

1. Read README architecture
2. Study Diagrams for flow
3. Read Full Spec for details
4. Review Examples for implementation patterns
5. Create integration plan

---

## Protocol Overview

### What You Need to Know

**Input:** CLI commands with `--cue` flag
**Output:** JSONL (JSON Lines) to stdout
**State:** Managed automatically in `.interview/sessions.jsonl`
**Format:** Self-contained JSON objects

### Three Message Types

1. **ask_question** - CLI sends a question
2. **interview_complete** - Interview finished, spec generated
3. **validation_error** - Answer rejected, retry allowed

### Basic Flow

```
Start → Question → Answer → Question → ... → Complete
         ↑                     |
         |_____ Error _________|
```

---

## Key Concepts

### EARS Patterns (6 types)
Structured requirement syntax for consistent answers

### 5 Rounds
Progressive refinement: Discovery → Refinement → Validation

### 6 Profiles
System types: api, cli, event, data, workflow, ui

### Session Management
Automatic persistence with resume capability

---

## Protocol Version

**Current Version:** 1.0 (2026-01-16)

**Breaking Changes:** None

**Deprecations:** None

**Future Plans:**
- v1.1: Pause/resume, answer editing
- v2.0: Streaming spec generation, conflict resolution

---

## Support

### Documentation Issues
- GitHub issues with tag `docs`
- See [AI_PROTOCOL.md](./AI_PROTOCOL.md) FAQ section

### Implementation Help
- Check [AI_PROTOCOL_EXAMPLES.md](./AI_PROTOCOL_EXAMPLES.md) for code samples
- See [AI_PROTOCOL_TESTING.md](./AI_PROTOCOL_TESTING.md) troubleshooting

### Protocol Questions
- Read [AI_PROTOCOL.md](./AI_PROTOCOL.md) Appendix D: FAQ
- Review [AI_PROTOCOL_README.md](./AI_PROTOCOL_README.md) FAQ section

---

## Contributing

Contributions welcome for:
- Additional code examples (languages)
- Test case additions
- Documentation improvements
- Protocol enhancements

See repository CONTRIBUTING.md

---

## Quick Reference Card

```
┌─────────────────────────────────────────────────────────┐
│ AI INTERVIEW PROTOCOL - QUICK REFERENCE                 │
├─────────────────────────────────────────────────────────┤
│ START:                                                  │
│   intent interview --cue --profile <profile>            │
│                                                         │
│ ANSWER:                                                 │
│   intent interview --cue --session <id> --answer "..."  │
│                                                         │
│ RESUME:                                                 │
│   intent interview --cue --session <id>                 │
├─────────────────────────────────────────────────────────┤
│ PROFILES:                                               │
│   api cli event data workflow ui                        │
│                                                         │
│ ACTIONS:                                                │
│   ask_question interview_complete validation_error      │
│                                                         │
│ EARS PATTERNS:                                          │
│   THE SYSTEM SHALL [behavior]                           │
│   WHEN [trigger] THE SYSTEM SHALL [behavior]            │
│   WHILE [state] THE SYSTEM SHALL [behavior]             │
│   WHERE [condition] THE SYSTEM SHALL [behavior]         │
│   IF [condition] THE SYSTEM SHALL NOT [behavior]        │
├─────────────────────────────────────────────────────────┤
│ STORAGE:                                                │
│   .interview/sessions.jsonl    (session state)          │
│   .interview/spec-<id>.cue     (generated spec)         │
├─────────────────────────────────────────────────────────┤
│ DOCS:                                                   │
│   AI_PROTOCOL_QUICKSTART.md    (5 min start)           │
│   AI_PROTOCOL.md                (full spec)             │
│   AI_PROTOCOL_EXAMPLES.md      (code samples)          │
└─────────────────────────────────────────────────────────┘
```

---

## File Sizes

```
docs/
├── AI_PROTOCOL_QUICKSTART.md    2.5 KB  (tiny - start here)
├── AI_PROTOCOL.md               41 KB   (large - complete ref)
├── AI_PROTOCOL_DIAGRAM.md       24 KB   (medium - visuals)
├── AI_PROTOCOL_TESTING.md       19 KB   (medium - QA)
├── AI_PROTOCOL_README.md        15 KB   (medium - overview)
├── AI_PROTOCOL_EXAMPLES.md      25 KB   (medium - code)
└── AI_PROTOCOL_INDEX.md         6 KB    (this file)
```

---

**Start reading:** [AI_PROTOCOL_README.md](./AI_PROTOCOL_README.md)

**Get coding:** [AI_PROTOCOL_QUICKSTART.md](./AI_PROTOCOL_QUICKSTART.md)

**Full reference:** [AI_PROTOCOL.md](./AI_PROTOCOL.md)
