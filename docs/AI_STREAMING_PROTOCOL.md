# AI Streaming Q&A Protocol

## Overview

The AI Streaming Q&A Protocol provides a stateless, deterministic interface for AI agents to conduct full interview sessions while maintaining complete rigor. Unlike the interactive human mode, this protocol streams questions one at a time with full context, enabling AI agents to provide thoughtful answers without managing complex state.

## Design Principles

1. **Complete Rigor**: All 5 rounds, all questions, all perspectives - no shortcuts
2. **Stateless Operations**: Each request is self-contained with session ID
3. **Full Context**: Every question includes round info, perspective, examples, and EARS patterns
4. **Progress Tracking**: Clear visibility into interview progress at every step
5. **Type Safety**: CUE-based output ensures machine-parseable responses
6. **Resumability**: Sessions persist to `.interview/sessions.jsonl` for git-friendly storage

## Protocol Flow

### 1. Start Interview

**Command:**
```bash
intent interview --cue --profile api
```

**Response:**
```cue
{
	action: "ask_question"

	session: {
		id: "interview-abc123"
		profile: "api"
		created_at: "2026-01-17T10:30:00Z"
		stage: "discovery"
	}

	question: {
		id: "api-r1-q1-happy"
		text: "In one sentence, what should this API do?"
		round: 1
		perspective: "user"
		category: "happy_path"
		priority: "critical"

		context: "Start with the core purpose. What problem does this solve?"
		example: "THE SYSTEM SHALL accept Pokemon names and return their stats"

		ears_pattern: "ubiquitous"
		ears_hint: "Format: THE SYSTEM SHALL [behavior]"
		ears_examples: [
			"THE SYSTEM SHALL accept HTTP POST requests to /users",
			"THE SYSTEM SHALL return JSON responses with 200 status"
		]

		expected_type: "text"
		extract_into: ["name", "purpose"]
	}

	progress: {
		current_step: 1
		total_steps: 47
		percent_complete: 2
		round: 1
		round_name: "Discovery - Happy Path"
		rounds_completed: 0
		total_rounds: 5
	}

	metadata: {
		can_skip: false  // Critical questions cannot be skipped
		depends_on: []   // No dependencies
		blocks: ["api-r2-q1-error"]  // This answer blocks later questions
	}
}
```

### 2. Submit Answer

**Command:**
```bash
intent interview --cue --session interview-abc123 --answer "THE SYSTEM SHALL accept Pokemon names via GET /pokemon/{name} and return JSON with stats"
```

**Response (Next Question):**
```cue
{
	action: "ask_question"

	session: {
		id: "interview-abc123"
		profile: "api"
		updated_at: "2026-01-17T10:31:00Z"
		stage: "discovery"
		answers_count: 1
	}

	question: {
		id: "api-r1-q2-auth"
		text: "How should users authenticate?"
		round: 1
		perspective: "security"
		category: "constraint"
		priority: "critical"

		context: "Authentication controls access. Consider who needs to use this API."
		example: "No authentication required - public API"

		ears_pattern: "ubiquitous"
		ears_hint: "Format: THE SYSTEM SHALL [behavior]"
		ears_examples: [
			"THE SYSTEM SHALL require JWT tokens in Authorization header",
			"THE SYSTEM SHALL accept API keys via X-API-Key header",
			"THE SYSTEM SHALL allow anonymous access"
		]

		expected_type: "text"
		extract_into: ["auth_method"]
	}

	progress: {
		current_step: 2
		total_steps: 47
		percent_complete: 4
		round: 1
		round_name: "Discovery - Happy Path"
		rounds_completed: 0
		total_rounds: 5
	}

	previous_answer: {
		question_id: "api-r1-q1-happy"
		response: "THE SYSTEM SHALL accept Pokemon names via GET /pokemon/{name} and return JSON with stats"
		extracted: {
			name: "Pokemon API"
			purpose: "Return Pokemon stats by name"
		}
		confidence: 0.85
		timestamp: "2026-01-17T10:31:00Z"
	}

	metadata: {
		can_skip: false
		depends_on: []
		blocks: ["api-r4-q3-auth-failure"]
	}
}
```

### 3. Interview Complete

**Response (After Final Answer):**
```cue
{
	action: "interview_complete"

	session: {
		id: "interview-abc123"
		profile: "api"
		created_at: "2026-01-17T10:30:00Z"
		completed_at: "2026-01-17T10:45:00Z"
		stage: "complete"
		duration_minutes: 15
	}

	output: {
		spec_path: ".interview/spec-interview-abc123.cue"
		behaviors_count: 23
		anti_patterns_count: 2
		gaps_count: 0
		conflicts_count: 1

		summary: "Interview complete. Generated spec with 23 behaviors across 5 rounds."

		quality_preview: {
			completeness: 92
			clarity: 88
			testability: 95
			coverage: 85
		}
	}

	statistics: {
		total_questions: 47
		questions_answered: 47
		questions_skipped: 0
		critical_answered: 15
		important_answered: 20
		nice_to_have_answered: 12

		rounds: [
			{round: 1, name: "Discovery", questions: 10, answered: 10}
			{round: 2, name: "Behaviors", questions: 12, answered: 12}
			{round: 3, name: "Edge Cases", questions: 8, answered: 8}
			{round: 4, name: "Security & Ops", questions: 10, answered: 10}
			{round: 5, name: "Validation", questions: 7, answered: 7}
		]

		perspectives: [
			{perspective: "user", questions: 12}
			{perspective: "developer", questions: 15}
			{perspective: "ops", questions: 8}
			{perspective: "security", questions: 7}
			{perspective: "business", questions: 5}
		]
	}

	next_steps: [
		"Run: intent quality .interview/spec-interview-abc123.cue"
		"Run: intent gaps .interview/spec-interview-abc123.cue"
		"Run: intent invert .interview/spec-interview-abc123.cue"
		"Run: intent beads interview-abc123"
	]

	conflicts: [
		{
			id: "conflict-cap"
			between: ["latency", "consistency"]
			description: "You want both speed AND strong consistency"
			impact: "CAP theorem: impossible to have both at scale"
			resolution_required: true
		}
	]
}
```

### 4. Resume Interview

**Command:**
```bash
intent interview --cue --session interview-abc123
```

**Response:**
- If incomplete: Returns next unanswered question (same format as step 2)
- If complete: Returns `interview_complete` action with full summary

### 5. Error Handling

**Invalid Session:**
```cue
{
	action: "validation_error"

	error: {
		type: "session_not_found"
		message: "Session 'interview-xyz' not found"
		suggestion: "Start a new session with: intent interview --cue --profile api"
		retry_allowed: false
	}

	recovery: [
		"List sessions: intent sessions"
		"Start new: intent interview --cue --profile api"
		"Check spelling of session ID"
	]
}
```

**Missing Answer:**
```cue
{
	action: "validation_error"

	error: {
		type: "answer_required"
		message: "--answer flag is required when --session is provided"
		suggestion: "Provide your answer using --answer \"THE SYSTEM SHALL...\""
		retry_allowed: true
	}

	recovery: [
		"Add --answer flag with your response"
		"Use EARS pattern (ubiquitous, event, state, unwanted, optional)"
	]
}
```

## Protocol Features

### Progress Tracking

Every response includes comprehensive progress information:

- **current_step**: 1-indexed question number
- **total_steps**: Total questions for this profile
- **percent_complete**: Integer percentage (0-100)
- **round**: Current round number (1-5)
- **round_name**: Human-readable round description
- **rounds_completed**: Number of fully completed rounds
- **total_rounds**: Always 5

### Question Metadata

Each question includes:

- **id**: Unique identifier (stable across sessions)
- **text**: The actual question
- **round**: Round number (1-5)
- **perspective**: Who asks this (user, developer, ops, security, business)
- **category**: Type of question (happy_path, error_case, edge_case, constraint, dependency, non_functional)
- **priority**: Importance level (critical, important, nice_to_have)
- **context**: Why this question matters
- **example**: Sample answer to guide responses
- **ears_pattern**: Which EARS pattern applies (ubiquitous, event, state, unwanted, optional)
- **ears_hint**: How to format the answer
- **ears_examples**: Concrete examples of well-formed answers
- **expected_type**: Data type expected (text, number, boolean, list)
- **extract_into**: Fields to extract from the answer

### Session Persistence

Sessions are stored in `.interview/sessions.jsonl` using JSONL format:

- **Git-friendly**: One line per session, easy to diff and merge
- **Resumable**: Can resume from any point, even across context resets
- **Auditable**: Full history of all answers with timestamps
- **Portable**: Standard JSON format, language-agnostic

## Implementation Details

### Question Loading

Questions are loaded from `schema/questions.cue` using the CUE loader:

1. Parse CUE schema for profile and round
2. Filter by priority (if using `--required-only`)
3. Sort by round, then priority, then perspective
4. Track answered questions to find next unanswered

### Answer Processing

When an answer is submitted:

1. Load session from JSONL
2. Validate answer format (check for EARS pattern adherence)
3. Extract structured data using pattern matching
4. Calculate confidence score based on answer quality
5. Detect gaps (missing critical information)
6. Detect conflicts (contradictory requirements)
7. Save updated session to JSONL
8. Return next question or completion

### Conflict Detection

The protocol automatically detects common conflicts:

- **CAP Theorem**: Consistency vs. Availability vs. Partition Tolerance
- **Security vs. Usability**: Anonymous users vs. Audit trails
- **Performance vs. Correctness**: Caching vs. Real-time accuracy
- **Simplicity vs. Features**: Minimal scope vs. Rich functionality

Conflicts are surfaced in the final `interview_complete` response.

### Gap Detection

Gaps are identified when:

- Critical questions receive brief answers (< 10 chars)
- Required fields are missing from extracted data
- Dependencies are not satisfied
- Profile-specific requirements are not addressed

## AI Agent Integration Guide

### Step 1: Start Interview
```python
import subprocess
import json

result = subprocess.run(
    ["intent", "interview", "--cue", "--profile", "api"],
    capture_output=True,
    text=True
)

# Parse CUE output (simplified - use proper CUE parser)
response = parse_cue(result.stdout)
session_id = response["session"]["id"]
question = response["question"]
```

### Step 2: Answer Questions in Loop
```python
while response["action"] == "ask_question":
    # Generate answer using LLM
    answer = generate_answer(question["text"], question["ears_hint"])

    # Submit answer
    result = subprocess.run(
        ["intent", "interview", "--cue",
         "--session", session_id,
         "--answer", answer],
        capture_output=True,
        text=True
    )

    response = parse_cue(result.stdout)

    # Save progress
    save_progress(session_id, response["progress"])
```

### Step 3: Handle Completion
```python
if response["action"] == "interview_complete":
    spec_path = response["output"]["spec_path"]

    # Run quality analysis
    quality = run_command(["intent", "quality", spec_path])
    gaps = run_command(["intent", "gaps", spec_path])
    risks = run_command(["intent", "invert", spec_path])

    # Generate beads
    beads = run_command(["intent", "beads", session_id])
```

## Advanced Features

### Bulk Answers (Not Yet Implemented)

For AI agents with full context, bulk answering could be supported:

```bash
intent interview --cue --session interview-abc123 --bulk-answer answers.jsonl
```

Where `answers.jsonl` contains:
```jsonl
{"question_id": "api-r1-q1-happy", "response": "THE SYSTEM SHALL..."}
{"question_id": "api-r1-q2-auth", "response": "THE SYSTEM SHALL..."}
```

### Skip to Section (Not Yet Implemented)

Jump to specific sections for focused interviews:

```bash
intent interview --cue --profile api --skip-to refinement
```

### Required-Only Mode (Not Yet Implemented)

Answer only critical questions for rapid prototyping:

```bash
intent interview --cue --profile api --required-only
```

## Comparison to Human Interactive Mode

| Feature | Human Mode | AI Streaming Mode |
|---------|------------|-------------------|
| Interface | Interactive prompts | Stateless CUE responses |
| State | Terminal session | JSONL persistence |
| Progress | Live display | JSON progress object |
| Examples | Inline hints | Structured EARS examples |
| Errors | Terminal messages | Structured CUE errors |
| Resume | Session ID required | Automatic from session ID |
| Output | Terminal text | Machine-parseable CUE |
| Rigor | All questions | All questions (same rigor) |

## Testing the Protocol

### Manual Test

```bash
# Start interview
intent interview --cue --profile api > response1.cue
cat response1.cue  # Inspect first question

# Extract session ID
SESSION_ID=$(cue eval response1.cue -e session.id)

# Answer first question
intent interview --cue --session $SESSION_ID \
  --answer "THE SYSTEM SHALL accept Pokemon names and return stats" \
  > response2.cue

# Continue until complete
# ...
```

### Automated Test

```python
def test_full_interview():
    # Start
    resp = cue_interview(["--profile", "api"])
    assert resp["action"] == "ask_question"
    session = resp["session"]["id"]

    # Answer all questions
    while resp["action"] == "ask_question":
        answer = generate_test_answer(resp["question"])
        resp = cue_interview(["--session", session, "--answer", answer])

    # Verify completion
    assert resp["action"] == "interview_complete"
    assert resp["output"]["behaviors_count"] > 0
    assert os.path.exists(resp["output"]["spec_path"])
```

## Error Recovery

### Lost Session Context

If AI agent loses context mid-interview:

```bash
# List all sessions
intent sessions

# Resume specific session
intent interview --cue --session interview-abc123
```

### Corrupted Session

If JSONL is corrupted:

1. Check `.interview/sessions.jsonl` for syntax errors
2. Fix JSON manually (each line must be valid JSON)
3. Resume interview

### Missing Dependencies

If questions depend on earlier answers:

- The protocol tracks `depends_on` and `blocks` relationships
- Questions are presented in dependency order
- Skipping dependencies is prevented for critical paths

## Future Enhancements

1. **Parallel Answering**: Answer independent questions in parallel
2. **Answer Validation**: Real-time validation against EARS patterns
3. **Smart Defaults**: Suggest answers based on profile and context
4. **Conflict Resolution UI**: Interactive resolution for detected conflicts
5. **Gap Filling**: Targeted questions to fill detected gaps
6. **Export to Multiple Formats**: GraphQL, OpenAPI, AsyncAPI, etc.
