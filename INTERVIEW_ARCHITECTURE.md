# Intent Interview Architecture

**The Missing Front Door**: Structured interrogation system for turning vague ideas into solid specifications.

## Executive Summary

The Intent Interview engine **fills the gap that Kiro, OpenSpec, and SpecKit all miss**: the discovery phase. Instead of assuming you already know what you want, we interrogate you systematically to figure it out.

```
┌──────────────────────────────────────────────────────────────┐
│ VAGUE IDEA:  "I want users to log in"                       │
└──────────────────────────────────────────────────────────────┘
                            ↓
┌──────────────────────────────────────────────────────────────┐
│ INTENT INTERVIEW ENGINE                                      │
│  • 5 rounds × 5+ perspectives = 25+ questions               │
│  • Gap detection (blocking/non-blocking)                    │
│  • Conflict detection (CAP theorem, scope creep, etc)      │
│  • AI-driven extraction from free-form answers              │
│  • Session persistence (SQLite + JSONL for git)            │
└──────────────────────────────────────────────────────────────┘
                            ↓
┌──────────────────────────────────────────────────────────────┐
│ SOLID SPEC: Complete CUE file with all edge cases,          │
│ constraints, security rules, performance requirements,      │
│ and AI implementation hints                                 │
└──────────────────────────────────────────────────────────────┘
                            ↓
┌──────────────────────────────────────────────────────────────┐
│ BEAD TEMPLATES                                               │
│  • Auto-generate work items from spec                       │
│  • Pre-filled acceptance criteria                           │
│  • Dependency graph                                         │
│  • AI hints embedded                                        │
└──────────────────────────────────────────────────────────────┘
                            ↓
┌──────────────────────────────────────────────────────────────┐
│ BEADS (via bd)                                               │
│  • Ready for AI to implement                                │
│  • Tests verify implementation                              │
│  • Close beads on success                                   │
└──────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. Interview Schema (`schema/interview.cue`)

Defines the data model for persistent interview sessions:

```cue
#InterviewSession: {
  id: string                          // Unique session ID
  profile: "api" | "cli" | "event" | "data" | "workflow" | "ui"
  stage: "discovery" | "refinement" | "validation" | "complete" | "paused"
  rounds_completed: int & >=0 & <=5

  answers: [...#Answer]               // All collected answers
  extracted_spec: #PartialSpec        // Evolving spec being built
  gaps: [...#Gap]                     // Missing information (blocking/non-blocking)
  conflicts: [...#Conflict]           // Detected contradictions
}

#Answer: {
  question_id: string
  response: string                    // Raw human text
  extracted: {...}                    // Parsed fields (AI-extracted)
  confidence: float & >=0 & <=1       // 0-1 confidence score
  round: int & >=1 & <=5
  perspective: "user" | "developer" | "ops" | "security" | "business"
  timestamp: string                   // ISO 8601
}
```

### 2. Question Database (`schema/questions.cue`)

Comprehensive question library organized by:
- **5 rounds**: Discovery → Implementation → Validation → Complete
- **5+ perspectives**: User, Developer, Ops, Security, Business
- **Categories**: HappyPath, ErrorCase, EdgeCase, Constraint, Dependency, NonFunctional
- **Priority levels**: Critical, Important, NiceTohave

Example questions:

```
Round 1: Core Intent (What are we building?)
  R1-User-1: "In one sentence, what should this do?"
  R1-User-2: "Who will use this? What's their goal?"
  R1-User-3: "Walk me through the happy path step-by-step"
  R1-User-4: "What's the MOST important thing this MUST do correctly?"
  R1-User-5: "What would make this a failure, even if it works?"
  R1-Dev-1: "What data model does this operate on?"
  ...

Round 2: Error Cases (What can go wrong?)
  R2-Dev-1: "What if the input is invalid? Examples?"
  R2-Security-1: "What if the user isn't authorized?"
  R2-Dev-2: "What if a dependency fails?"
  R2-Dev-3: "What if they try to do this twice?"
  R2-Dev-4: "What if two users try this at the same time?"
  ...

Round 3: Edge Cases (Where are the boundaries?)
Round 4: Security & Compliance (How do we keep this safe?)
Round 5: Operations (How does this run in production?)
```

### 3. Interview Engine (`src/intent/interview.gleam`)

Core logic for:

#### Answer Extraction
```gleam
pub fn extract_from_answer(
  question_id: String,
  response: String,
  extract_fields: List(String),
) -> Dict(String, String)
```

Implements pattern-based extraction (extensible for LLM):
- `auth_method`: identifies "JWT", "OAuth", "session", "API key", etc.
- `entities`: extracts capitalized nouns (User, Order, Product)
- `audience`: identifies "mobile", "web", "API", "CLI", "internal"
- Generic: returns any non-empty response for unknown fields

#### Gap Detection
```gleam
pub fn detect_gaps(
  profile: Profile,
  answers: List(Answer),
) -> List(Gap)
```

Profile-aware gap checking:
- **API**: base_url, auth_method, happy_path, error_cases, response_format
- **CLI**: command_name, happy_path, help_text, exit_codes
- **Event**: event_type, payload_schema, trigger
- etc.

Flags gaps as **blocking** (must answer) or **non-blocking** (nice-to-have).

#### Conflict Detection
```gleam
pub fn detect_conflicts(answers: List(Answer)) -> List(Conflict)
```

Identifies common conflicts:
1. **CAP Theorem**: "fast" + "strongly_consistent" (impossible at scale)
2. **Audit Paradox**: "anonymous" + "audit_trail" (can't trace anon users)
3. **Scope Creep**: "simple" + 20_requirements

Presents resolution options with tradeoffs.

#### Confidence Scoring
```gleam
pub fn calculate_confidence(
  question_id: String,
  response: String,
  extracted: Dict(String, String),
) -> Float  // 0-1
```

Simple heuristic: longer responses + more fields = higher confidence.
Can be extended with NLP/LLM scoring.

### 4. Session Storage (`src/intent/interview_storage.gleam`)

**Dual persistence strategy** (mirrors Beads):

#### JSONL (Source of Truth for Git)
- Location: `.interview/sessions.jsonl`
- One line per session (last entry for each ID wins)
- Git-native: commit together with code changes
- Format: `session_to_jsonl_line(session)`

```json
{"id":"interview-abc123","profile":"api","stage":"discovery","rounds_completed":2,"answers":[...],"gaps":[...],"conflicts":[...],"raw_notes":"..."}
```

#### SQLite (Local Query Cache)
- Location: `.interview/interview.db`
- Schema: `sessions`, `answers`, `gaps`, `conflicts` tables
- Index on `session_id`, `status`, `profile` for fast queries
- Syncs bidirectionally with JSONL

**Sync Strategy**:
1. JSONL is source of truth
2. On startup: `sync_from_jsonl()` loads JSONL into SQLite
3. During interview: save to both SQLite + append to JSONL
4. On conflicts: JSONL wins (it's in git history)

### 5. CLI Integration (`src/intent.gleam`)

New `interview` command:

```bash
# Start new API interview
intent interview --profile=api

# Resume existing session
intent interview --resume=interview-abc123

# Non-interactive mode (for CI/testing)
intent interview --profile=api --answers=answers.yaml

# Export completed interview to spec
intent interview --resume=interview-abc123 --export=user-api.cue

# Help
intent interview --help
```

Command flags:
- `--profile`: api, cli, event, data, workflow, ui (default: api)
- `--resume`: Session ID to continue
- `--answers`: Path to YAML/JSON with pre-filled answers
- `--export`: Output path for generated spec (CUE)

### 6. Question Delivery (TBD - Next Phase)

**Interactive TUI** (stub in place, ready for implementation):
1. Load questions from `schema/questions.cue`
2. Filter by: round, perspective, profile, priority, dependencies
3. Show one question at a time with context/examples
4. Parse answer → extract fields → calculate confidence
5. Detect gaps (pause for critical ones)
6. Detect conflicts (offer resolution options)
7. Save session incrementally to JSONL + SQLite
8. Export final spec on completion

## Data Flow

```
User Input (conversation)
         ↓
Question Matching (round, perspective, dependencies)
         ↓
Answer Capture (raw text)
         ↓
Extraction (parse into structured fields)
         ↓
Confidence Scoring (how sure are we?)
         ↓
Gap Detection (what's missing?)
    ↓          ↓
Blocking    Non-blocking
(pause)      (continue)
    ↓          ↓
    └────┬─────┘
         ↓
Conflict Detection (any contradictions?)
         ↓
  [Offer Resolution]
         ↓
Persist to JSONL + SQLite
         ↓
[Next Question] or [Advance Round]
         ↓
[Repeat]
         ↓
[Round Complete]
    ↓         ↓
5 rounds?   → Next Round
    │
    └─→ Interview Complete
         ↓
Export to CUE Spec
```

## Multi-Profile Support

Interview adapts questions based on system profile:

### API Profile
- Questions focus on: endpoints, HTTP methods, authentication, error responses, rate limiting
- Required fields: base_url, auth_method, endpoint descriptions, status codes
- Gap check: all critical endpoints must have happy path + at least one error case

### CLI Profile
- Questions focus on: commands, flags, help text, exit codes, output format
- Required fields: command names, flag descriptions, success output, error messages
- Gap check: all commands need documentation and error handling

### Event Profile
- Questions focus on: event types, payload schema, triggers, delivery guarantees
- Required fields: event_type, payload_schema, idempotency rules
- Gap check: all events need schema validation

### Data Profile
- Questions focus on: data model, schemas, retention, access patterns
- Required fields: entity definitions, field constraints, retention policy
- Gap check: sensitive fields marked

### Workflow Profile
- Questions focus on: steps, state transitions, error recovery, retries
- Required fields: step descriptions, transition rules, failure handling
- Gap check: all state transitions covered

### UI Profile
- Questions focus on: user flows, states, interactions, accessibility
- Required fields: flow descriptions, state definitions, transitions
- Gap check: happy path + error states for all flows

## Why This Matters

### Kiro / OpenSpec / SpecKit
```
You: "Write your spec"
Tool: "OK, we'll help you format it"
Result: Incomplete spec with silent assumptions
```

### Intent Interview
```
You: "Users can log in"
Tool: "Who are these users?"
       "What happens if they enter wrong password?"
       "Do we send 'Invalid email' or 'Wrong password'?"
       "What if someone tries 100 passwords/second?"
       "How fast must login be?"
       "Can admins reset passwords?"
Tool: "OK, here's your complete spec with all edge cases"
Result: Solid specification, zero ambiguity
```

## Implementation Phases

### Phase 1: Foundation (COMPLETE ✓)
- ✓ Schema design (CUE)
- ✓ Question database (CUE)
- ✓ Interview engine (Gleam)
- ✓ Session persistence (JSONL + SQLite stubs)
- ✓ CLI command integration
- ✓ Gap & conflict detection

### Phase 2: TUI & Interactivity (NEXT)
- [ ] Question loading from CUE
- [ ] Interactive question-answer loop
- [ ] Real-time gap/conflict detection UI
- [ ] Session save on Ctrl+C
- [ ] Resume session workflow
- [ ] Progress display

### Phase 3: Bead Generation (FUTURE)
- [ ] Bead template engine
- [ ] Auto-generate work items from spec
- [ ] Template customization
- [ ] Integration with `bd` commands

### Phase 4: AI Enhancement (FUTURE)
- [ ] LLM-based answer extraction (vs. pattern matching)
- [ ] Smarter gap suggestions
- [ ] Follow-up question generation
- [ ] Requirement synthesis

## Testing Strategy

### Unit Tests
```gleam
// interview.gleam tests
#[test]
fn test_extract_auth_method() { ... }

#[test]
fn test_detect_gaps_api_profile() { ... }

#[test]
fn test_detect_cap_theorem_conflict() { ... }

#[test]
fn test_calculate_confidence() { ... }
```

### Integration Tests
```bash
# Non-interactive test
intent interview --profile=api \
  --answers=test_data/sample_answers.yaml \
  --export=/tmp/test_spec.cue

# Verify spec is valid CUE
intent validate /tmp/test_spec.cue

# Verify spec has expected content
intent show /tmp/test_spec.cue --json | grep -q "auth_method"
```

## File Structure

```
intent-cli/
├── schema/
│   ├── interview.cue          # Interview session schema
│   ├── questions.cue          # Question database (25+ questions)
│   └── intent.cue             # Main spec schema (extended with ai_hints)
│
├── src/intent/
│   ├── interview.gleam        # Core engine (extraction, gaps, conflicts)
│   ├── interview_storage.gleam # JSONL + SQLite persistence
│   └── [...other modules]
│
├── src/intent.gleam           # CLI entry point (new interview command)
│
├── .interview/                # (created at runtime)
│   ├── sessions.jsonl         # All sessions (git-tracked)
│   ├── interview.db           # SQLite cache (git-ignored)
│   └── [session-id]/          # Workspace for in-progress sessions
│
└── examples/
    └── interviews/
        ├── user-api-session.jsonl    # Sample completed session
        └── user-api.cue              # Exported spec
```

## Next Steps

1. **Implement TUI** (interactive question loop)
2. **Load questions from CUE** at runtime
3. **Connect to real Question database**
4. **Implement SQLite persistence** (currently stubs)
5. **Add Bead template engine**
6. **Test with real users**

## Appendix: Design Decisions

### Why 5 Rounds?
- Matches the 5 W's (What, Why, Who, When, Where) conceptually
- Allows progression: Intent → Failures → Boundaries → Security → Operations
- Natural break points for user fatigue
- Aligns with typical project planning phases

### Why JSONL + SQLite?
- **JSONL**: Version control friendly (one line per session), easy to merge
- **SQLite**: Fast queries, supports joining/aggregating multiple sessions
- **Mirrors Beads**: Proven approach for AI-friendly issue tracking

### Why Multiple Perspectives?
- **User**: Understands success criteria and UX
- **Developer**: Knows implementation constraints
- **Ops**: Thinks about scale, reliability, deployments
- **Security**: Identifies risks and compliance needs
- **Business**: Understands cost/revenue implications

Asking the same question from 5 angles catches gaps that single-perspective teams miss.

### Why AI-Driven Extraction?
- Humans are better at answering questions than filling forms
- AI can learn what matters from free-form answers
- Extraction can improve over time (LLM scoring)
- Maintains conversational flow vs. rigid questionnaires

## References

- Beads (Steve Yegge): https://github.com/steveyegge/beads
- CUE Language: https://cuelang.org
- Gleam Language: https://gleam.run
