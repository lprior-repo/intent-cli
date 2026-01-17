# AI Interview Protocol Specification

**Version:** 1.0
**Status:** Draft
**Last Updated:** 2026-01-16

## Overview

This document specifies a streaming Q&A protocol for AI agents to interface with the Intent CLI interview system. The protocol preserves the full 5-round rigorous interview methodology while providing an AI-friendly request/response cycle using JSONL (JSON Lines) format.

### Design Principles

1. **Rigor Preservation**: Full 5-round interview process with no shortcuts
2. **Stateless Messages**: Each message is self-contained JSON
3. **Progress Transparency**: Clear tracking of position in interview flow
4. **Rich Context**: Questions include round, perspective, examples, and hints
5. **Automatic Session Management**: Sessions handled internally, no manual tracking needed
6. **Error Recovery**: Clear, actionable error messages with retry guidance

### Key Features

- Simple JSONL streaming (one JSON object per line to stdout)
- Auto-managed session IDs (agent receives ID, doesn't create it)
- Full question context (EARS patterns, examples, category, priority)
- Progress tracking (current_step/total_steps, percent_complete)
- Explicit completion signal
- Validation errors with retry capability

---

## Protocol Flow

### High-Level Sequence

```
1. Agent → CLI: Start Interview Request
2. CLI → Agent: Question Response (with session ID)
3. Agent → CLI: Answer Submission
4. CLI → Agent: Next Question Response
   ... (repeat steps 3-4 for all questions across 5 rounds)
5. CLI → Agent: Completion Response (with spec path)
```

### Session Lifecycle

```
┌─────────────────────────────────────────────────────────┐
│ DISCOVERY (Rounds 1-2)                                  │
│ - Round 1: Core intent, audience, happy path           │
│ - Round 2: Common errors, security concerns             │
└─────────────────────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────────────────────┐
│ REFINEMENT (Round 3)                                    │
│ - Edge cases, constraints, dependencies                 │
└─────────────────────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────────────────────┐
│ VALIDATION (Rounds 4-5)                                 │
│ - Non-functional requirements, completeness checks      │
└─────────────────────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────────────────────┐
│ COMPLETE                                                │
│ - Spec generated, session archived                      │
└─────────────────────────────────────────────────────────┘
```

---

## Message Types

### 1. Start Interview Request

**Agent → CLI Command:**

```bash
intent interview --cue --profile <profile>
```

**Parameters:**

- `--cue`: Enable CUE output mode for AI agents
- `--profile`: One of: `api`, `cli`, `event`, `data`, `workflow`, `ui`

**Example:**

```bash
intent interview --cue --profile api
```

---

### 2. Question Response

**CLI → Agent (stdout):**

```json
{
  "action": "ask_question",
  "question": {
    "text": "In one sentence, what should this API do?",
    "id": "r1-user-api-1",
    "round": 1,
    "perspective": "user",
    "category": "happy_path",
    "priority": "critical",
    "context": "We're starting with the core intent. Give us the simplest possible description.",
    "example": "Allow users to log in with email and password",
    "pattern": "ubiquitous",
    "hint": "Use format: THE SYSTEM SHALL [behavior]",
    "examples": [
      "THE SYSTEM SHALL validate all API inputs",
      "THE SYSTEM SHALL log all requests"
    ],
    "extract_into": ["name"],
    "expected_type": "text"
  },
  "progress": {
    "current_step": 1,
    "total_steps": 25,
    "percent_complete": 0,
    "round": 1,
    "round_name": "Discovery",
    "category": "basic_info"
  },
  "session": {
    "id": "interview-a3f9b2c1-4d5e-6789-abcd-ef0123456789",
    "profile": "api",
    "started_at": "2026-01-16T14:32:10Z"
  }
}
```

**Field Descriptions:**

- `action`: Always `"ask_question"` for this message type
- `question.text`: The actual question to answer
- `question.id`: Unique identifier for this question (internal use)
- `question.round`: Interview round (1-5)
- `question.perspective`: One of: `user`, `developer`, `ops`, `security`, `business`
- `question.category`: One of: `happy_path`, `error_case`, `edge_case`, `constraint`, `dependency`, `nonfunctional`
- `question.priority`: One of: `critical`, `important`, `nice_to_have`
- `question.context`: Additional context explaining why this question matters
- `question.example`: Example answer for guidance
- `question.pattern`: EARS pattern type (see EARS Patterns section)
- `question.hint`: Formatting hint for EARS-style answers
- `question.examples`: List of 2-3 example answers following the pattern
- `question.extract_into`: Fields to extract from answer (internal use)
- `question.expected_type`: Expected response type (usually `"text"`)
- `progress.current_step`: Current question number (1-indexed)
- `progress.total_steps`: Total questions in interview
- `progress.percent_complete`: Percentage (0-100)
- `progress.round`: Current round (1-5)
- `progress.round_name`: Human-readable stage name
- `progress.category`: Question category grouping
- `session.id`: Session identifier (use for answer submission)
- `session.profile`: The profile being interviewed
- `session.started_at`: ISO 8601 timestamp of session start

---

### 3. Answer Submission Request

**Agent → CLI Command:**

```bash
intent interview --cue --session <session_id> --answer "<answer_text>"
```

**Parameters:**

- `--cue`: Enable CUE output mode
- `--session`: Session ID from previous question response
- `--answer`: The answer text (properly escaped for shell)

**Example:**

```bash
intent interview --cue --session "interview-a3f9b2c1-4d5e-6789-abcd-ef0123456789" \
  --answer "THE SYSTEM SHALL authenticate users via JWT tokens"
```

**Answer Requirements:**

- Minimum length: 3 characters
- Should follow EARS pattern hints when provided
- Should address the question context
- Be specific and actionable

**Best Practices:**

- Use EARS format when `question.hint` is provided
- Reference the `question.example` for guidance
- Include specific entities/values mentioned in `question.context`
- For critical questions, provide detailed answers (50+ characters)

---

### 4. Next Question Response

**CLI → Agent (stdout):**

After submitting an answer, the CLI will respond with either:

1. **Another Question** (same format as Message Type 2)
2. **Completion Response** (see Message Type 5)
3. **Validation Error** (see Message Type 6)

The interview automatically progresses through rounds. When all questions in a round are complete, the next question will be from the subsequent round (indicated by `progress.round`).

---

### 5. Completion Response

**CLI → Agent (stdout):**

```json
{
  "action": "interview_complete",
  "output": {
    "spec_path": ".interview/spec-interview-a3f9b2c1-4d5e-6789-abcd-ef0123456789.cue",
    "behaviors_count": 25,
    "anti_patterns_count": 3,
    "summary": "Interview complete. Generated spec with 25 behaviors.",
    "next_steps": [
      "Review the generated spec",
      "Run 'intent validate <spec_path>' to verify",
      "Run 'intent check <spec_path> --target <url>' to test"
    ]
  },
  "session": {
    "id": "interview-a3f9b2c1-4d5e-6789-abcd-ef0123456789",
    "profile": "api",
    "started_at": "2026-01-16T14:32:10Z",
    "completed_at": "2026-01-16T14:45:23Z"
  },
  "statistics": {
    "total_questions": 25,
    "total_answers": 25,
    "rounds_completed": 5,
    "gaps_detected": 3,
    "conflicts_detected": 0,
    "average_confidence": 0.82
  }
}
```

**Field Descriptions:**

- `action`: Always `"interview_complete"`
- `output.spec_path`: Path to generated CUE specification file
- `output.behaviors_count`: Number of behaviors extracted from answers
- `output.anti_patterns_count`: Number of anti-patterns detected
- `output.summary`: Human-readable summary
- `output.next_steps`: Suggested actions after completion
- `session.*`: Session metadata
- `statistics.*`: Interview statistics and quality metrics

**Agent Responsibilities:**

1. Save the `spec_path` for future reference
2. Optionally validate the spec using `intent validate`
3. Optionally run tests using `intent check`

---

### 6. Validation Error Response

**CLI → Agent (stdout):**

```json
{
  "action": "validation_error",
  "error": {
    "code": "ANSWER_TOO_SHORT",
    "message": "Answer too short",
    "suggestion": "Please provide a more detailed response (minimum 3 characters)",
    "retry_allowed": true,
    "context": {
      "question_id": "r1-user-api-1",
      "answer_length": 2,
      "minimum_length": 3
    }
  },
  "session": {
    "id": "interview-a3f9b2c1-4d5e-6789-abcd-ef0123456789",
    "profile": "api"
  }
}
```

**Error Codes:**

- `ANSWER_TOO_SHORT`: Answer below minimum length
- `INVALID_FORMAT`: Answer doesn't match expected format
- `SESSION_NOT_FOUND`: Invalid session ID
- `SESSION_EXPIRED`: Session older than 24 hours
- `SESSION_COMPLETE`: Attempt to answer after completion
- `PROFILE_UNKNOWN`: Invalid profile specified
- `INTERNAL_ERROR`: System error during processing

**Field Descriptions:**

- `action`: Always `"validation_error"`
- `error.code`: Machine-readable error code
- `error.message`: Human-readable error message
- `error.suggestion`: Actionable suggestion for recovery
- `error.retry_allowed`: Whether the agent can retry (always `true` for validation errors)
- `error.context`: Additional error context (optional)

**Recovery:**

When `retry_allowed` is `true`, resubmit the answer with corrections:

```bash
intent interview --cue --session "<session_id>" --answer "<corrected_answer>"
```

---

## EARS Patterns

The interview system uses EARS (Easy Approach to Requirements Syntax) to guide answer formatting. Questions include a `pattern` field and `hint` to help structure responses.

### Pattern Types

#### 1. Ubiquitous (Universal Requirements)

**Format:** `THE SYSTEM SHALL [behavior]`

**Example Question:** "What security measures are always required?"

**Example Answers:**
- "THE SYSTEM SHALL validate all API inputs"
- "THE SYSTEM SHALL encrypt data at rest"
- "THE SYSTEM SHALL log all authentication attempts"

**When to use:** Requirements that apply globally without conditions

---

#### 2. Event-Driven (Trigger-Based)

**Format:** `WHEN [trigger] THE SYSTEM SHALL [behavior]`

**Example Question:** "What happens when a user logs in?"

**Example Answers:**
- "WHEN user submits valid credentials THE SYSTEM SHALL issue a JWT token"
- "WHEN token expires THE SYSTEM SHALL return 401 Unauthorized"
- "WHEN user logs out THE SYSTEM SHALL invalidate the session"

**When to use:** Behaviors triggered by specific events

---

#### 3. State-Driven (Condition-Based)

**Format:** `WHILE [state] THE SYSTEM SHALL [behavior]`

**Example Question:** "What behavior is required during active sessions?"

**Example Answers:**
- "WHILE user is authenticated THE SYSTEM SHALL include user_id in all logs"
- "WHILE request is processing THE SYSTEM SHALL hold the database connection"
- "WHILE rate limit is exceeded THE SYSTEM SHALL queue requests"

**When to use:** Behaviors that depend on current state

---

#### 4. Optional (Feature-Specific)

**Format:** `WHERE [condition] THE SYSTEM SHALL [behavior]`

**Example Question:** "What optional features can be configured?"

**Example Answers:**
- "WHERE caching is enabled THE SYSTEM SHALL store responses for 5 minutes"
- "WHERE debug mode is active THE SYSTEM SHALL include stack traces"
- "WHERE rate limiting is configured THE SYSTEM SHALL enforce limits per API key"

**When to use:** Conditional features or configuration-dependent behavior

---

#### 5. Unwanted (Prohibited Behavior)

**Format:** `IF [condition] THE SYSTEM SHALL NOT [behavior]`

**Example Question:** "What should the system never do?"

**Example Answers:**
- "IF authentication fails THE SYSTEM SHALL NOT leak user existence"
- "IF input is invalid THE SYSTEM SHALL NOT execute SQL directly"
- "IF rate limit exceeded THE SYSTEM SHALL NOT process the request"

**When to use:** Security constraints or anti-patterns

---

#### 6. Complex (Combined Conditions)

**Format:** `WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]`

**Example Question:** "What happens when specific events occur in certain states?"

**Example Answers:**
- "WHILE user is authenticated WHEN token nears expiry THE SYSTEM SHALL refresh the token"
- "WHILE batch job is running WHEN error occurs THE SYSTEM SHALL rollback partial changes"
- "WHILE circuit breaker is open WHEN request arrives THE SYSTEM SHALL return cached response"

**When to use:** Complex requirements with multiple conditions

---

## Complete Example Interaction

Below is a full interaction showing an AI agent completing a 2-round abbreviated interview (for demonstration; real interviews are 5 rounds).

### Turn 1: Start Interview

**Agent → CLI:**

```bash
intent interview --cue --profile api
```

**CLI → Agent:**

```json
{
  "action": "ask_question",
  "question": {
    "text": "In one sentence, what should this API do?",
    "id": "r1-user-api-1",
    "round": 1,
    "perspective": "user",
    "category": "happy_path",
    "priority": "critical",
    "context": "We're starting with the core intent. Give us the simplest possible description.",
    "example": "Allow users to log in with email and password",
    "pattern": "ubiquitous",
    "hint": "Use format: THE SYSTEM SHALL [behavior]",
    "examples": [
      "THE SYSTEM SHALL validate all API inputs",
      "THE SYSTEM SHALL log all requests"
    ],
    "extract_into": ["name"],
    "expected_type": "text"
  },
  "progress": {
    "current_step": 1,
    "total_steps": 10,
    "percent_complete": 0,
    "round": 1,
    "round_name": "Discovery",
    "category": "basic_info"
  },
  "session": {
    "id": "interview-abc123",
    "profile": "api",
    "started_at": "2026-01-16T14:00:00Z"
  }
}
```

---

### Turn 2: Answer Question 1

**Agent → CLI:**

```bash
intent interview --cue --session "interview-abc123" \
  --answer "THE SYSTEM SHALL authenticate users via email and password and issue JWT tokens"
```

**CLI → Agent:**

```json
{
  "action": "ask_question",
  "question": {
    "text": "Who will use this API? What are they trying to accomplish?",
    "id": "r1-user-api-2",
    "round": 1,
    "perspective": "user",
    "category": "happy_path",
    "priority": "critical",
    "context": "Understanding your audience helps us design the right behavior.",
    "example": "Mobile app users, web frontend, and third-party integrations",
    "pattern": "ubiquitous",
    "hint": "List the different types of clients and their goals",
    "examples": [
      "Mobile apps need fast authentication for user login",
      "Web SPAs need session management for browser clients"
    ],
    "extract_into": ["audience", "success_criteria"],
    "expected_type": "text"
  },
  "progress": {
    "current_step": 2,
    "total_steps": 10,
    "percent_complete": 10,
    "round": 1,
    "round_name": "Discovery",
    "category": "basic_info"
  },
  "session": {
    "id": "interview-abc123",
    "profile": "api",
    "started_at": "2026-01-16T14:00:00Z"
  }
}
```

---

### Turn 3: Answer Question 2

**Agent → CLI:**

```bash
intent interview --cue --session "interview-abc123" \
  --answer "Mobile applications and web frontends will use this API to authenticate users and access protected resources. Third-party developers may integrate for SSO."
```

**CLI → Agent:**

```json
{
  "action": "ask_question",
  "question": {
    "text": "Walk me through the happy path. What happens step-by-step?",
    "id": "r1-user-api-3",
    "round": 1,
    "perspective": "user",
    "category": "happy_path",
    "priority": "critical",
    "context": "Describe the ideal flow from start to finish. Don't worry about errors yet.",
    "example": "Client sends POST /login with email/password → validates → returns JWT token",
    "pattern": "event_driven",
    "hint": "Use format: WHEN [trigger] THE SYSTEM SHALL [behavior]",
    "examples": [
      "WHEN client sends POST /login THE SYSTEM SHALL validate credentials",
      "WHEN validation succeeds THE SYSTEM SHALL issue JWT token"
    ],
    "extract_into": ["behaviors"],
    "expected_type": "text"
  },
  "progress": {
    "current_step": 3,
    "total_steps": 10,
    "percent_complete": 20,
    "round": 1,
    "round_name": "Discovery",
    "category": "basic_info"
  },
  "session": {
    "id": "interview-abc123",
    "profile": "api",
    "started_at": "2026-01-16T14:00:00Z"
  }
}
```

---

### Turn 4: Answer Question 3

**Agent → CLI:**

```bash
intent interview --cue --session "interview-abc123" \
  --answer "WHEN client sends POST /auth/login with email and password THE SYSTEM SHALL validate credentials against database. WHEN credentials are valid THE SYSTEM SHALL generate JWT token with user_id and expiration. WHEN token is generated THE SYSTEM SHALL return 200 OK with token in response body."
```

**CLI → Agent:**

```json
{
  "action": "ask_question",
  "question": {
    "text": "What data model does this operate on? List the key entities.",
    "id": "r1-dev-api-1",
    "round": 1,
    "perspective": "developer",
    "category": "constraint",
    "priority": "important",
    "context": "Understanding the domain helps us catch inconsistencies.",
    "example": "Users (id, email, password_hash), Tokens (token, user_id, expires_at)",
    "pattern": "ubiquitous",
    "hint": "List entities with their key attributes",
    "examples": [
      "User: id, email, password_hash, created_at",
      "Session: token, user_id, expires_at, ip_address"
    ],
    "extract_into": ["entities"],
    "expected_type": "text"
  },
  "progress": {
    "current_step": 4,
    "total_steps": 10,
    "percent_complete": 30,
    "round": 1,
    "round_name": "Discovery",
    "category": "basic_info"
  },
  "session": {
    "id": "interview-abc123",
    "profile": "api",
    "started_at": "2026-01-16T14:00:00Z"
  }
}
```

---

### Turn 5: Answer Question 4

**Agent → CLI:**

```bash
intent interview --cue --session "interview-abc123" \
  --answer "User entity: id (UUID), email (unique), password_hash (bcrypt), created_at, last_login_at. RefreshToken entity: token (UUID), user_id, expires_at, revoked (boolean), created_at."
```

**CLI → Agent:**

```json
{
  "action": "ask_question",
  "question": {
    "text": "What kind of authentication does this need?",
    "id": "r1-security-api-1",
    "round": 1,
    "perspective": "security",
    "category": "constraint",
    "priority": "critical",
    "context": "Auth method cascades through the whole design.",
    "example": "JWT for mobile, session cookies for web, API keys for server-to-server",
    "pattern": "ubiquitous",
    "hint": "Specify the authentication mechanism(s)",
    "examples": [
      "JWT tokens with RS256 signing",
      "OAuth 2.0 with authorization code flow"
    ],
    "extract_into": ["auth_method"],
    "expected_type": "text"
  },
  "progress": {
    "current_step": 5,
    "total_steps": 10,
    "percent_complete": 40,
    "round": 1,
    "round_name": "Discovery",
    "category": "basic_info"
  },
  "session": {
    "id": "interview-abc123",
    "profile": "api",
    "started_at": "2026-01-16T14:00:00Z"
  }
}
```

---

### Turn 6: Answer Question 5 (Last in Round 1)

**Agent → CLI:**

```bash
intent interview --cue --session "interview-abc123" \
  --answer "JWT tokens with HS256 signing for access tokens (15 min expiry), and refresh tokens stored in database (7 day expiry). Include Bearer authentication in Authorization header."
```

**CLI → Agent:**

```json
{
  "action": "ask_question",
  "question": {
    "text": "What's the most common error users will hit?",
    "id": "r2-user-api-1",
    "round": 2,
    "perspective": "user",
    "category": "error_case",
    "priority": "critical",
    "context": "The error that happens 80% of the time.",
    "example": "Wrong password, email already exists, invalid token",
    "pattern": "event_driven",
    "hint": "Use format: WHEN [trigger] THE SYSTEM SHALL [behavior]",
    "examples": [
      "WHEN password is incorrect THE SYSTEM SHALL return 401 Unauthorized",
      "WHEN token is expired THE SYSTEM SHALL return 401 with error code"
    ],
    "extract_into": ["error_cases"],
    "expected_type": "text"
  },
  "progress": {
    "current_step": 6,
    "total_steps": 10,
    "percent_complete": 50,
    "round": 2,
    "round_name": "Discovery",
    "category": "behaviors"
  },
  "session": {
    "id": "interview-abc123",
    "profile": "api",
    "started_at": "2026-01-16T14:00:00Z"
  }
}
```

**Note:** Progress automatically moved to Round 2. The `round` field incremented from 1 to 2, and `category` changed from `"basic_info"` to `"behaviors"`.

---

### Turn 7-9: Continue answering Round 2 questions...

*[Omitted for brevity - same pattern continues]*

---

### Turn 10: Final Answer + Completion

**Agent → CLI:**

```bash
intent interview --cue --session "interview-abc123" \
  --answer "IF authentication fails THE SYSTEM SHALL NOT reveal whether email exists. IF rate limit exceeded THE SYSTEM SHALL NOT process request. IF SQL injection attempted THE SYSTEM SHALL NOT execute raw query."
```

**CLI → Agent:**

```json
{
  "action": "interview_complete",
  "output": {
    "spec_path": ".interview/spec-interview-abc123.cue",
    "behaviors_count": 10,
    "anti_patterns_count": 2,
    "summary": "Interview complete. Generated spec with 10 behaviors.",
    "next_steps": [
      "Review the generated spec at .interview/spec-interview-abc123.cue",
      "Run 'intent validate .interview/spec-interview-abc123.cue' to verify",
      "Run 'intent check .interview/spec-interview-abc123.cue --target http://localhost:3000' to test"
    ]
  },
  "session": {
    "id": "interview-abc123",
    "profile": "api",
    "started_at": "2026-01-16T14:00:00Z",
    "completed_at": "2026-01-16T14:12:34Z"
  },
  "statistics": {
    "total_questions": 10,
    "total_answers": 10,
    "rounds_completed": 2,
    "gaps_detected": 2,
    "conflicts_detected": 0,
    "average_confidence": 0.84
  }
}
```

**Interview Complete!** The agent can now access the generated spec file and proceed with validation/testing.

---

## Error Handling

### Common Error Scenarios

#### 1. Session Not Found

**Cause:** Invalid or expired session ID

**Response:**

```json
{
  "action": "validation_error",
  "error": {
    "code": "SESSION_NOT_FOUND",
    "message": "Session not found: interview-invalid123",
    "suggestion": "Start a new interview with 'intent interview --cue --profile <profile>'",
    "retry_allowed": false
  }
}
```

**Recovery:** Start a new interview session

---

#### 2. Answer Too Short

**Cause:** Answer below minimum length (3 characters)

**Response:**

```json
{
  "action": "validation_error",
  "error": {
    "code": "ANSWER_TOO_SHORT",
    "message": "Answer too short",
    "suggestion": "Please provide a more detailed response (minimum 3 characters)",
    "retry_allowed": true,
    "context": {
      "answer_length": 2,
      "minimum_length": 3
    }
  },
  "session": {
    "id": "interview-abc123",
    "profile": "api"
  }
}
```

**Recovery:** Resubmit with a longer answer

---

#### 3. Critical Question Answered Briefly

**Cause:** Critical priority question answered with less than 10 characters

**Response:**

The system will accept the answer but may detect a "gap" internally. The interview continues, but the generated spec may have warnings.

**No Error Response** - Interview proceeds normally, but `statistics.gaps_detected` will be > 0 in the completion response.

---

#### 4. File System Error

**Cause:** Cannot write session storage

**Response:**

```json
{
  "action": "validation_error",
  "error": {
    "code": "INTERNAL_ERROR",
    "message": "Failed to save session - permission denied",
    "suggestion": "Check file permissions for .interview/ directory",
    "retry_allowed": false
  }
}
```

**Recovery:** Fix file system permissions and restart interview

---

## Session Persistence

### Storage Location

Sessions are automatically persisted to:

```
.interview/
├── sessions.jsonl         # All sessions (git-friendly)
├── history.jsonl          # Session snapshots (audit trail)
└── spec-<session-id>.cue  # Generated specs (on completion)
```

### Session Recovery

If an agent disconnects and wants to resume:

```bash
intent interview --cue --session "interview-abc123"
```

The CLI will load the session and return the next unanswered question (or completion if all done).

### Session Expiry

Sessions expire after 24 hours of inactivity. After expiry:

```json
{
  "action": "validation_error",
  "error": {
    "code": "SESSION_EXPIRED",
    "message": "Session expired (older than 24 hours)",
    "suggestion": "Start a new interview with 'intent interview --cue --profile <profile>'",
    "retry_allowed": false
  }
}
```

---

## Implementation Notes

### For AI Agent Developers

#### Parsing Responses

All responses are valid JSON objects. Parse them as:

```python
import json
import subprocess

# Start interview
result = subprocess.run(
    ["intent", "interview", "--cue", "--profile", "api"],
    capture_output=True,
    text=True
)

response = json.loads(result.stdout)

if response["action"] == "ask_question":
    session_id = response["session"]["id"]
    question_text = response["question"]["text"]
    hint = response["question"]["hint"]

    # Generate answer (implement your logic here)
    answer = generate_answer(question_text, hint)

    # Submit answer
    result = subprocess.run(
        ["intent", "interview", "--cue", "--session", session_id, "--answer", answer],
        capture_output=True,
        text=True
    )

    next_response = json.loads(result.stdout)
    # Continue loop...
```

#### Shell Escaping

Answers may contain special characters. Properly escape for shell:

```python
import shlex

answer = 'THE SYSTEM SHALL validate "email" and \'password\' fields'
escaped_answer = shlex.quote(answer)

subprocess.run([
    "intent", "interview", "--cue",
    "--session", session_id,
    "--answer", escaped_answer
])
```

#### Progress Tracking

Track progress with `progress.percent_complete`:

```python
progress = response["progress"]["percent_complete"]
print(f"Interview {progress}% complete")

# Visual progress bar
bar_length = 50
filled = int(bar_length * progress / 100)
bar = "=" * filled + " " * (bar_length - filled)
print(f"[{bar}] {progress}%")
```

#### EARS Pattern Assistance

Use the `question.pattern`, `question.hint`, and `question.examples` fields to guide answer generation:

```python
def generate_answer_with_pattern(question_text, pattern, hint, examples):
    if pattern == "ubiquitous":
        # Format: THE SYSTEM SHALL [behavior]
        return f"THE SYSTEM SHALL {extract_behavior(question_text)}"
    elif pattern == "event_driven":
        # Format: WHEN [trigger] THE SYSTEM SHALL [behavior]
        trigger = extract_trigger(question_text)
        behavior = extract_behavior(question_text)
        return f"WHEN {trigger} THE SYSTEM SHALL {behavior}"
    # ... handle other patterns
```

#### Error Handling

Always check the `action` field:

```python
response = json.loads(result.stdout)

if response["action"] == "validation_error":
    error = response["error"]
    if error["retry_allowed"]:
        # Retry with corrected answer
        corrected_answer = fix_answer(original_answer, error["suggestion"])
        # Submit again...
    else:
        # Fatal error, restart interview
        print(f"Fatal error: {error['message']}")
        print(f"Suggestion: {error['suggestion']}")
        exit(1)
elif response["action"] == "ask_question":
    # Process question...
elif response["action"] == "interview_complete":
    # Interview done!
    spec_path = response["output"]["spec_path"]
    print(f"Spec generated: {spec_path}")
```

---

## JSON Schema

### Question Response Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["action", "question", "progress", "session"],
  "properties": {
    "action": {
      "type": "string",
      "enum": ["ask_question"]
    },
    "question": {
      "type": "object",
      "required": ["text", "id", "round", "perspective", "category", "priority", "context", "example", "pattern", "hint", "examples", "extract_into", "expected_type"],
      "properties": {
        "text": { "type": "string", "minLength": 1 },
        "id": { "type": "string", "pattern": "^r[1-5]-.+$" },
        "round": { "type": "integer", "minimum": 1, "maximum": 5 },
        "perspective": { "type": "string", "enum": ["user", "developer", "ops", "security", "business"] },
        "category": { "type": "string", "enum": ["happy_path", "error_case", "edge_case", "constraint", "dependency", "nonfunctional"] },
        "priority": { "type": "string", "enum": ["critical", "important", "nice_to_have"] },
        "context": { "type": "string" },
        "example": { "type": "string" },
        "pattern": { "type": "string", "enum": ["ubiquitous", "event_driven", "state_driven", "optional", "unwanted", "complex"] },
        "hint": { "type": "string" },
        "examples": { "type": "array", "items": { "type": "string" }, "minItems": 1 },
        "extract_into": { "type": "array", "items": { "type": "string" } },
        "expected_type": { "type": "string" }
      }
    },
    "progress": {
      "type": "object",
      "required": ["current_step", "total_steps", "percent_complete", "round", "round_name", "category"],
      "properties": {
        "current_step": { "type": "integer", "minimum": 1 },
        "total_steps": { "type": "integer", "minimum": 1 },
        "percent_complete": { "type": "integer", "minimum": 0, "maximum": 100 },
        "round": { "type": "integer", "minimum": 1, "maximum": 5 },
        "round_name": { "type": "string" },
        "category": { "type": "string" }
      }
    },
    "session": {
      "type": "object",
      "required": ["id", "profile", "started_at"],
      "properties": {
        "id": { "type": "string", "pattern": "^interview-.+$" },
        "profile": { "type": "string", "enum": ["api", "cli", "event", "data", "workflow", "ui"] },
        "started_at": { "type": "string", "format": "date-time" }
      }
    }
  }
}
```

### Completion Response Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["action", "output", "session", "statistics"],
  "properties": {
    "action": {
      "type": "string",
      "enum": ["interview_complete"]
    },
    "output": {
      "type": "object",
      "required": ["spec_path", "behaviors_count", "anti_patterns_count", "summary", "next_steps"],
      "properties": {
        "spec_path": { "type": "string" },
        "behaviors_count": { "type": "integer", "minimum": 0 },
        "anti_patterns_count": { "type": "integer", "minimum": 0 },
        "summary": { "type": "string" },
        "next_steps": { "type": "array", "items": { "type": "string" } }
      }
    },
    "session": {
      "type": "object",
      "required": ["id", "profile", "started_at", "completed_at"],
      "properties": {
        "id": { "type": "string" },
        "profile": { "type": "string" },
        "started_at": { "type": "string", "format": "date-time" },
        "completed_at": { "type": "string", "format": "date-time" }
      }
    },
    "statistics": {
      "type": "object",
      "required": ["total_questions", "total_answers", "rounds_completed", "gaps_detected", "conflicts_detected", "average_confidence"],
      "properties": {
        "total_questions": { "type": "integer", "minimum": 0 },
        "total_answers": { "type": "integer", "minimum": 0 },
        "rounds_completed": { "type": "integer", "minimum": 0, "maximum": 5 },
        "gaps_detected": { "type": "integer", "minimum": 0 },
        "conflicts_detected": { "type": "integer", "minimum": 0 },
        "average_confidence": { "type": "number", "minimum": 0, "maximum": 1 }
      }
    }
  }
}
```

### Validation Error Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["action", "error"],
  "properties": {
    "action": {
      "type": "string",
      "enum": ["validation_error"]
    },
    "error": {
      "type": "object",
      "required": ["code", "message", "suggestion", "retry_allowed"],
      "properties": {
        "code": { "type": "string", "enum": ["ANSWER_TOO_SHORT", "INVALID_FORMAT", "SESSION_NOT_FOUND", "SESSION_EXPIRED", "SESSION_COMPLETE", "PROFILE_UNKNOWN", "INTERNAL_ERROR"] },
        "message": { "type": "string" },
        "suggestion": { "type": "string" },
        "retry_allowed": { "type": "boolean" },
        "context": { "type": "object" }
      }
    },
    "session": {
      "type": "object",
      "properties": {
        "id": { "type": "string" },
        "profile": { "type": "string" }
      }
    }
  }
}
```

---

## Appendix A: Profile Descriptions

### api (REST/HTTP API)

**Focus:** HTTP endpoints, request/response patterns, authentication, error codes

**Typical Questions:**
- Core functionality and endpoints
- Authentication/authorization methods
- Error handling and status codes
- Rate limiting and security

**Generated Spec:** CUE file with behaviors mapped to HTTP methods, paths, and status codes

---

### cli (Command-Line Tool)

**Focus:** Commands, flags, exit codes, input/output

**Typical Questions:**
- Main commands and subcommands
- Input sources (files, stdin, arguments)
- Output formats and destinations
- Error handling and exit codes

**Generated Spec:** CUE file with command behaviors and expected outputs

---

### event (Event-Driven System)

**Focus:** Event types, payloads, triggers, handlers

**Typical Questions:**
- Event types and schemas
- Event sources and triggers
- Event handlers and side effects
- Error recovery and dead-letter queues

**Generated Spec:** CUE file with event behaviors and payload schemas

---

### data (Data System)

**Focus:** Data models, access patterns, consistency, retention

**Typical Questions:**
- Data entities and relationships
- CRUD operations and queries
- Consistency guarantees
- Backup and retention policies

**Generated Spec:** CUE file with data behaviors and schema definitions

---

### workflow (Business Workflow)

**Focus:** Process steps, state transitions, error recovery

**Typical Questions:**
- Workflow steps and transitions
- Success/failure paths
- Rollback and compensation logic
- Long-running process handling

**Generated Spec:** CUE file with workflow behaviors and state machine

---

### ui (User Interface)

**Focus:** User flows, states, interactions, accessibility

**Typical Questions:**
- User journeys and flows
- UI states and transitions
- Input validation and feedback
- Accessibility requirements

**Generated Spec:** CUE file with UI behaviors and interaction patterns

---

## Appendix B: Round Breakdown

### Round 1: Discovery (Basic Info)

**Goal:** Understand core purpose, audience, and happy path

**Perspectives:** User, Developer, Security

**Typical Questions:**
- What does the system do? (1 sentence)
- Who are the users?
- What's the happy path?
- What's the data model?
- What authentication is needed?

**Expected Output:** 5-7 questions covering basics

---

### Round 2: Discovery (Behaviors)

**Goal:** Identify common errors, edge cases, and error handling

**Perspectives:** User, Developer, Ops

**Typical Questions:**
- Most common errors?
- Error response format?
- Status codes / exit codes?
- Logging and monitoring?
- What information should never leak?

**Expected Output:** 5-7 questions on error scenarios

---

### Round 3: Refinement (Edge Cases)

**Goal:** Explore constraints, dependencies, and unusual scenarios

**Perspectives:** Developer, Ops, Security

**Typical Questions:**
- Edge cases and corner scenarios?
- External dependencies?
- Performance constraints?
- Concurrency handling?
- Resource limits?

**Expected Output:** 5-7 questions on edge cases

---

### Round 4: Validation (Security)

**Goal:** Security, compliance, and non-functional requirements

**Perspectives:** Security, Business, Ops

**Typical Questions:**
- Authorization rules?
- Data sensitivity and encryption?
- Compliance requirements?
- Audit logging?
- Threat scenarios?

**Expected Output:** 5-7 questions on security

---

### Round 5: Validation (Completeness)

**Goal:** Final checks, non-functional requirements, completeness

**Perspectives:** All perspectives

**Typical Questions:**
- Performance requirements?
- Scalability needs?
- Observability (metrics, traces)?
- Deployment constraints?
- Anything missing?

**Expected Output:** 3-5 questions on final details

---

## Appendix C: FAQ

### Why JSONL instead of JSON arrays?

**Answer:** JSONL (JSON Lines) allows streaming one message at a time without needing to parse an entire array. Each line is a complete, valid JSON object. This is more robust for CLI tools and easier to parse incrementally.

---

### Can I skip questions?

**Answer:** No. The protocol enforces answering all questions in sequence. This preserves interview rigor. If you don't have an answer, provide your best estimate or use "Unknown - need more information".

---

### Can I go back and change answers?

**Answer:** Not in the current protocol version. Sessions are append-only. To revise answers, start a new interview session.

---

### How do I handle network timeouts?

**Answer:** The CLI operations are stateless and idempotent. If a submission times out, retry with the same session ID and answer. The system will detect duplicate answers and proceed to the next question.

---

### What if the question doesn't apply to my system?

**Answer:** Answer with "Not applicable" or explain why it doesn't apply. The system will continue. Example: "This system doesn't require authentication - it's a public read-only API."

---

### Can I run multiple interviews in parallel?

**Answer:** Yes. Each session has a unique ID. You can run interviews for different profiles simultaneously.

---

### How long do sessions persist?

**Answer:** Sessions are stored in `.interview/sessions.jsonl` indefinitely. They expire for active use after 24 hours, but the data remains for audit purposes.

---

## Appendix D: Change Log

### Version 1.0 (2026-01-16)

- Initial protocol specification
- Support for 6 profiles (api, cli, event, data, workflow, ui)
- 5-round interview structure
- EARS pattern integration
- JSON schema definitions
- Complete example interaction

---

## Appendix E: Future Enhancements

### Planned for v1.1

- **Pause/Resume:** Explicit pause command for long-running interviews
- **Answer Editing:** Allow revising previous answers within same session
- **Parallel Questioning:** Ask multiple non-dependent questions at once
- **Confidence Feedback:** Return confidence scores in question responses
- **Gap Detection:** Include detected gaps in real-time during interview

### Planned for v2.0

- **Streaming Spec Generation:** Output partial spec after each round
- **Interactive Conflict Resolution:** Prompt agent to resolve detected conflicts
- **Multi-language Support:** Questions in multiple languages
- **Custom Question Sets:** Allow loading custom question databases
- **Webhook Integration:** POST answers to external validation services

---

## License

This protocol specification is part of the Intent CLI project and follows the same license terms.

---

## Contact

For questions, issues, or contributions to this protocol:

- GitHub: [intent-cli repository](https://github.com/your-org/intent-cli)
- Issues: [Protocol discussions](https://github.com/your-org/intent-cli/discussions)

---

**End of Specification**
