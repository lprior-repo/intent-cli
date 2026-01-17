# AI Protocol Quick Start

This is a condensed guide for AI agents interfacing with Intent CLI's interview system. For full details, see [AI_PROTOCOL.md](./AI_PROTOCOL.md).

## Minimal Example

### 1. Start Interview

```bash
intent interview --cue --profile api
```

**Response:**

```json
{
  "action": "ask_question",
  "question": { "text": "In one sentence, what should this API do?", ... },
  "progress": { "current_step": 1, "total_steps": 25, ... },
  "session": { "id": "interview-abc123", ... }
}
```

### 2. Submit Answer

```bash
intent interview --cue --session "interview-abc123" \
  --answer "THE SYSTEM SHALL authenticate users via JWT tokens"
```

**Response:** Next question (same format as above) OR completion message

### 3. Repeat Until Complete

Continue submitting answers until you receive:

```json
{
  "action": "interview_complete",
  "output": { "spec_path": ".interview/spec-interview-abc123.cue", ... }
}
```

## Key Points

1. **JSONL Format**: Each CLI invocation outputs one JSON object to stdout
2. **Session Persistence**: Use the same `session.id` for all answers
3. **5 Rounds**: Interview progresses through 5 rounds automatically
4. **EARS Patterns**: Use `question.hint` and `question.examples` to format answers
5. **Error Handling**: Check `action` field for `"validation_error"` responses

## Profiles

- `api`: REST/HTTP APIs
- `cli`: Command-line tools
- `event`: Event-driven systems
- `data`: Data systems
- `workflow`: Business workflows
- `ui`: User interfaces

## EARS Quick Reference

```
Ubiquitous:   THE SYSTEM SHALL [behavior]
Event-Driven: WHEN [trigger] THE SYSTEM SHALL [behavior]
State-Driven: WHILE [state] THE SYSTEM SHALL [behavior]
Optional:     WHERE [condition] THE SYSTEM SHALL [behavior]
Unwanted:     IF [condition] THE SYSTEM SHALL NOT [behavior]
Complex:      WHILE [state] WHEN [trigger] THE SYSTEM SHALL [behavior]
```

## Error Recovery

If you receive `"action": "validation_error"`:

```json
{
  "action": "validation_error",
  "error": {
    "code": "ANSWER_TOO_SHORT",
    "suggestion": "Please provide a more detailed response",
    "retry_allowed": true
  }
}
```

**Recovery:** Resubmit the same command with a corrected answer.

## Full Documentation

See [AI_PROTOCOL.md](./AI_PROTOCOL.md) for:
- Complete JSON schemas
- Detailed EARS pattern explanations
- Full example interaction (10+ turns)
- Error code reference
- Session persistence details
- Implementation examples in Python
