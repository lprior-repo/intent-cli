# Intent CLI 4.0: AI-Only Architecture

> **Design Principle**: Claude Code is the only user. Zero human interaction.

---

## Core Constraints

1. **NO interactive features**
   - No stdin prompts
   - No y/n confirmations
   - No spinners/progress bars on stdout
   - No colors/formatting
   - No `read_line()` or `prompt_*()` functions

2. **JSONL everywhere**
   - All input via stdin as JSONL
   - All output via stdout as JSONL
   - One JSON object per line
   - Correlation IDs for request/response matching

3. **CUE schemas for everything**
   - Input schemas validated before execution
   - Output schemas guaranteed
   - Schema introspection via `intent schema` command

4. **Stateless operations**
   - Each request is self-contained
   - State persisted in files (JSONL sessions, CUE specs)
   - No interactive "conversation" - just request/response

---

## Removed Features

### Delete from codebase:
```
src/intent/stdin.gleam           # All interactive input
- read_line()
- read_line_trimmed()
- read_non_empty_line()
- read_until_blank()
- prompt_for_answer()
- prompt_yes_no()

src/intent/cli_ui.gleam          # Interactive UI helpers
- print_error() with colors
- spinners
- progress bars

src/intent/progress_dashboard.gleam  # Progress UI
```

### Remove from all commands:
- Interactive confirmations
- Spinner animations
- Colored output
- Progress indicators on stdout
- Any blocking read from terminal

---

## New Architecture

### Command Protocol

Every command follows this pattern:

**Input (stdin):**
```jsonl
{"id":"req-001","command":"vision.start","args":{"profile":"api"}}
```

**Output (stdout):**
```jsonl
{"id":"req-001","success":true,"command":"vision.start","data":{"session_id":"sess-abc","questions":[...]},"next_actions":[...]}
```

### Command Namespacing

```
intent <domain>.<action> [--stdin]

Domains:
  vision    - Phase 1 operations
  shape     - Phase 2 operations
  spec      - Phase 3 operations (KIRK)
  ready     - Phase 4 operations
  bead      - Bead generation/management
  schema    - Schema introspection
  plan      - Plan management
```

### Example Command Flows

**Start a vision session:**
```bash
echo '{"id":"1","command":"vision.start","args":{"profile":"api"}}' | intent --stdin
# Output: {"id":"1","success":true,"data":{"session_id":"sess-123","phase":"vision","questions":[{"id":"q1","text":"What problem...","required":true}]}}
```

**Answer a question:**
```bash
echo '{"id":"2","command":"vision.answer","args":{"session_id":"sess-123","question_id":"q1","answer":"We are building..."}}' | intent --stdin
# Output: {"id":"2","success":true,"data":{"answered":true,"remaining_questions":7,"gaps":[]}}
```

**Get critique:**
```bash
echo '{"id":"3","command":"vision.critique","args":{"session_id":"sess-123"}}' | intent --stdin
# Output: {"id":"3","success":true,"data":{"persona":"skeptical_pm","issues":[...],"blocking_questions":[...],"gate_status":"locked"}}
```

**Check gate:**
```bash
echo '{"id":"4","command":"vision.gate","args":{"session_id":"sess-123"}}' | intent --stdin
# Output: {"id":"4","success":true,"data":{"gate":"vision","status":"unlocked","can_advance":true,"blockers":[]}}
```

**Advance to next phase:**
```bash
echo '{"id":"5","command":"vision.advance","args":{"session_id":"sess-123"}}' | intent --stdin
# Output: {"id":"5","success":true,"data":{"from_phase":"vision","to_phase":"shape","session_id":"sess-123"}}
```

---

## CUE Schema Structure

```
schema/
├── intent.cue              # Spec schema (existing)
├── plan.cue                # Plan schema
├── bead.cue                # Enhanced bead schema
│
├── commands/               # Per-command schemas
│   ├── vision/
│   │   ├── start.input.cue
│   │   ├── start.output.cue
│   │   ├── answer.input.cue
│   │   ├── answer.output.cue
│   │   ├── critique.input.cue
│   │   ├── critique.output.cue
│   │   ├── gate.input.cue
│   │   ├── gate.output.cue
│   │   └── advance.input.cue
│   │   └── advance.output.cue
│   ├── shape/
│   │   └── ... (same pattern)
│   ├── spec/
│   │   └── ... (same pattern)
│   ├── ready/
│   │   └── ... (same pattern)
│   ├── bead/
│   │   ├── generate.input.cue
│   │   ├── generate.output.cue
│   │   └── ...
│   └── schema/
│       ├── get.input.cue
│       └── get.output.cue
│
└── common/
    ├── envelope.cue        # Request/Response wrapper
    ├── errors.cue          # Error taxonomy
    └── types.cue           # Shared types
```

---

## Request Envelope Schema

```cue
// schema/common/envelope.cue
package common

#Request: {
    id: string                      // Correlation ID
    command: string                 // "domain.action" format
    args: {...}                     // Command-specific args
    options?: {
        timeout_ms?: int & >0
        dry_run?: bool
    }
}

#Response: {
    id: string                      // Echo correlation ID
    success: bool
    command: string                 // Echo command
    data: {...}                     // Command-specific data
    errors: [...#Error]
    next_actions: [...#NextAction]
    metadata: #Metadata
}

#Error: {
    code: string
    message: string
    location?: string
    fix_hint?: string
    fix_command?: string            // JSONL command to fix
}

#NextAction: {
    command: string                 // Full JSONL request
    reason: string
    priority: int & >=1 & <=5
    blocks?: [...string]            // Request IDs to wait for
}

#Metadata: {
    timestamp: string
    version: string
    duration_ms: int
    exit_code: int
}
```

---

## Command Schemas

### vision.start

```cue
// schema/commands/vision/start.input.cue
#VisionStartInput: {
    id: string
    command: "vision.start"
    args: {
        profile: "api" | "cli" | "event" | "data" | "workflow" | "ui"
        name?: string               // Optional project name
    }
}

// schema/commands/vision/start.output.cue
#VisionStartOutput: {
    id: string
    success: bool
    command: "vision.start"
    data: {
        session_id: string
        phase: "vision"
        profile: string
        questions: [...#Question]
        created_at: string
    }
    errors: [...#Error]
    next_actions: [...#NextAction]
    metadata: #Metadata
}

#Question: {
    id: string
    text: string
    context: string
    required: bool
    extract_fields: [...string]
    depends_on: [...string]
}
```

### vision.answer

```cue
// schema/commands/vision/answer.input.cue
#VisionAnswerInput: {
    id: string
    command: "vision.answer"
    args: {
        session_id: string
        question_id: string
        answer: string
        notes?: string
    }
}

// schema/commands/vision/answer.output.cue
#VisionAnswerOutput: {
    id: string
    success: bool
    command: "vision.answer"
    data: {
        answered: bool
        question_id: string
        extracted: {[string]: string}
        remaining_questions: int
        gaps: [...#Gap]
        conflicts: [...#Conflict]
    }
    errors: [...#Error]
    next_actions: [...#NextAction]
    metadata: #Metadata
}
```

### vision.critique

```cue
// schema/commands/vision/critique.input.cue
#VisionCritiqueInput: {
    id: string
    command: "vision.critique"
    args: {
        session_id: string
    }
}

// schema/commands/vision/critique.output.cue
#VisionCritiqueOutput: {
    id: string
    success: bool
    command: "vision.critique"
    data: {
        persona: "skeptical_pm"
        issues: [...#CritiqueIssue]
        blocking_questions: [...#BlockingQuestion]
        alignment_score: int & >=0 & <=100
        gate_status: "locked" | "unlocked"
        gate_blockers: [...string]
    }
    errors: [...#Error]
    next_actions: [...#NextAction]
    metadata: #Metadata
}

#CritiqueIssue: {
    id: string
    category: "reality" | "validation" | "scope" | "risk"
    severity: "critical" | "high" | "medium" | "low"
    description: string
    question: string                // Question to resolve this
    suggestion: string
}

#BlockingQuestion: {
    id: string
    question: string
    reason: string
    must_answer: bool
}
```

### schema.get

```cue
// schema/commands/schema/get.input.cue
#SchemaGetInput: {
    id: string
    command: "schema.get"
    args: {
        domain: string              // "vision" | "shape" | "spec" | "ready" | "bead"
        action?: string             // "start" | "answer" | etc.
        type?: "input" | "output" | "both"
    }
}

// schema/commands/schema/get.output.cue
#SchemaGetOutput: {
    id: string
    success: bool
    command: "schema.get"
    data: {
        domain: string
        action?: string
        schemas: {
            input?: string          // CUE schema as string
            output?: string
        }
    }
    errors: [...#Error]
    next_actions: [...#NextAction]
    metadata: #Metadata
}
```

---

## Batch Processing

Process multiple commands in one invocation:

```bash
cat <<'EOF' | intent --stdin
{"id":"1","command":"vision.start","args":{"profile":"api"}}
{"id":"2","command":"vision.answer","args":{"session_id":"$1.data.session_id","question_id":"q1","answer":"..."}}
{"id":"3","command":"vision.answer","args":{"session_id":"$1.data.session_id","question_id":"q2","answer":"..."}}
EOF
```

Output (one line per response):
```jsonl
{"id":"1","success":true,"command":"vision.start","data":{"session_id":"sess-abc",...}}
{"id":"2","success":true,"command":"vision.answer","data":{...}}
{"id":"3","success":true,"command":"vision.answer","data":{...}}
```

### Variable Substitution

Reference previous responses using `$<id>.<path>`:
- `$1.data.session_id` → value from request ID "1"
- `$prev.data.session_id` → value from previous response

---

## CLI Interface

```bash
# Primary mode: Read JSONL from stdin
intent --stdin

# Schema introspection (still JSONL output)
intent schema vision.start --type=input
intent schema --all

# Version (JSONL)
intent version
# {"success":true,"data":{"version":"4.0.0","git_sha":"abc123"}}

# Validate input against schema
intent validate --schema=vision.start.input < request.jsonl
```

### Flags

| Flag | Description |
|------|-------------|
| `--stdin` | Read JSONL from stdin (default mode) |
| `--schema` | Validate input against schema |
| `--timeout=N` | Global timeout in ms |
| `--dry-run` | Validate but don't execute |

---

## Error Handling

All errors are JSONL responses:

```jsonl
{"id":"req-001","success":false,"command":"vision.answer","data":{},"errors":[{"code":"SESSION_NOT_FOUND","message":"Session sess-xyz does not exist","fix_command":"{\"command\":\"vision.start\",\"args\":{\"profile\":\"api\"}}"}],"next_actions":[]}
```

### Error Codes

```
# Validation
INVALID_JSON              - Malformed JSON input
INVALID_COMMAND           - Unknown command
MISSING_REQUIRED_FIELD    - Required arg missing
SCHEMA_VALIDATION_FAILED  - Input doesn't match schema

# Session
SESSION_NOT_FOUND         - Session doesn't exist
SESSION_EXPIRED           - Session timed out
PHASE_MISMATCH            - Wrong phase for command

# Gate
GATE_LOCKED               - Cannot advance, blockers exist
BLOCKER_UNRESOLVED        - Blocking question not answered
CRITIQUE_REQUIRED         - Must run critique before advance

# System
IO_ERROR                  - File system error
TIMEOUT                   - Operation timed out
INTERNAL_ERROR            - Unexpected error
```

---

## Session Persistence

Sessions stored as JSONL files:

```
.intent/
├── sessions/
│   ├── sess-abc.jsonl           # Session state
│   ├── sess-abc.answers.jsonl   # Answer history
│   └── sess-abc.critique.jsonl  # Critique history
├── plans/
│   ├── plan-xyz.cue             # Generated plans
│   └── plan-xyz.beads.jsonl     # Generated beads
└── schemas/                      # Cached schemas
```

### Session State Format

```jsonl
{"type":"session_created","session_id":"sess-abc","profile":"api","phase":"vision","timestamp":"..."}
{"type":"question_answered","session_id":"sess-abc","question_id":"q1","answer":"...","extracted":{...},"timestamp":"..."}
{"type":"critique_run","session_id":"sess-abc","persona":"skeptical_pm","issues":[...],"timestamp":"..."}
{"type":"gate_checked","session_id":"sess-abc","gate":"vision","status":"unlocked","timestamp":"..."}
{"type":"phase_advanced","session_id":"sess-abc","from":"vision","to":"shape","timestamp":"..."}
```

---

## Claude Code Integration

### Recommended Usage Pattern

```python
# Claude Code can:
# 1. Get schemas to understand available commands
# 2. Build valid JSONL requests
# 3. Execute and parse responses
# 4. Follow next_actions for workflow

# Example workflow:
requests = [
    {"id": "1", "command": "schema.get", "args": {"domain": "vision"}},
]
# → Learn available vision commands

requests = [
    {"id": "2", "command": "vision.start", "args": {"profile": "api"}},
]
# → Get session_id and questions

# Answer all questions...
for q in questions:
    requests.append({
        "id": str(next_id()),
        "command": "vision.answer",
        "args": {"session_id": session_id, "question_id": q["id"], "answer": "..."}
    })

# Get critique
requests.append({"id": "N", "command": "vision.critique", "args": {"session_id": session_id}})

# Check gate and advance
requests.append({"id": "N+1", "command": "vision.gate", "args": {"session_id": session_id}})
requests.append({"id": "N+2", "command": "vision.advance", "args": {"session_id": session_id}})
```

### next_actions Guidance

Every response includes `next_actions` with suggested commands:

```json
"next_actions": [
  {
    "command": "{\"id\":\"auto\",\"command\":\"vision.answer\",\"args\":{\"session_id\":\"sess-abc\",\"question_id\":\"q2\"}}",
    "reason": "Answer next required question",
    "priority": 1
  },
  {
    "command": "{\"id\":\"auto\",\"command\":\"vision.critique\",\"args\":{\"session_id\":\"sess-abc\"}}",
    "reason": "Run critique after answering all questions",
    "priority": 2
  }
]
```

Claude Code can parse these and decide what to do next.

---

## Removed Beads

These beads are no longer needed with AI-only architecture:

- ~~Interactive stdin functions~~
- ~~CLI UI helpers (colors, spinners)~~
- ~~Progress dashboard~~
- ~~Prompt confirmation flows~~

## Modified Beads

These beads change scope:

| Bead | Change |
|------|--------|
| Vision Commands | JSONL request/response only |
| Shape Commands | JSONL request/response only |
| Spec Commands | JSONL request/response only |
| Ready Commands | JSONL request/response only |
| All Critique | Returns structured issues, no conversation |

---

## Summary

**Before (Human-Interactive):**
```
User types → CLI prompts → User responds → CLI shows results
```

**After (AI-Only):**
```
JSONL request → Validate schema → Execute → JSONL response
```

**Benefits:**
- Deterministic behavior
- Schema-validated I/O
- Batch processing
- Easy testing
- Perfect for Claude Code
