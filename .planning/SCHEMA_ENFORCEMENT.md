# Schema Enforcement Architecture

> **Principle**: CUE validates, Gleam enforces. Every byte in, every byte out.

---

## Validation Pipeline

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           REQUEST FLOW                                       │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  stdin (JSONL)                                                               │
│       │                                                                      │
│       ▼                                                                      │
│  ┌─────────────────┐                                                        │
│  │ Parse JSON line │ → Error? → {"success":false,"error":"INVALID_JSON"}    │
│  └────────┬────────┘                                                        │
│           │                                                                  │
│           ▼                                                                  │
│  ┌─────────────────────────┐                                                │
│  │ Extract command name    │                                                │
│  │ (e.g., "vision.start")  │                                                │
│  └────────┬────────────────┘                                                │
│           │                                                                  │
│           ▼                                                                  │
│  ┌─────────────────────────────────┐                                        │
│  │ Load CUE schema for command     │                                        │
│  │ schema/commands/{domain}/{action}.input.cue                              │
│  └────────┬────────────────────────┘                                        │
│           │                                                                  │
│           ▼                                                                  │
│  ┌─────────────────────────────────┐                                        │
│  │ Validate JSON against CUE       │ → Error? → {"success":false,           │
│  │ (cue vet - or embedded)         │            "error":"SCHEMA_INVALID",   │
│  └────────┬────────────────────────┘            "details":[...]}            │
│           │                                                                  │
│           ▼                                                                  │
│  ┌─────────────────────────────────┐                                        │
│  │ Parse to Gleam types            │ → Error? → {"success":false,           │
│  │ (dynamic.decode with schema)    │            "error":"PARSE_FAILED"}     │
│  └────────┬────────────────────────┘                                        │
│           │                                                                  │
│           ▼                                                                  │
│  ┌─────────────────────────────────┐                                        │
│  │ Execute command handler         │                                        │
│  │ Returns Result(Response, Error) │                                        │
│  └────────┬────────────────────────┘                                        │
│           │                                                                  │
└───────────┼──────────────────────────────────────────────────────────────────┘
            │
┌───────────┼──────────────────────────────────────────────────────────────────┐
│           │                      RESPONSE FLOW                               │
├───────────┼──────────────────────────────────────────────────────────────────┤
│           ▼                                                                  │
│  ┌─────────────────────────────────┐                                        │
│  │ Gleam Response type             │                                        │
│  │ (statically typed)              │                                        │
│  └────────┬────────────────────────┘                                        │
│           │                                                                  │
│           ▼                                                                  │
│  ┌─────────────────────────────────┐                                        │
│  │ Serialize to JSON               │                                        │
│  │ (response_to_json)              │                                        │
│  └────────┬────────────────────────┘                                        │
│           │                                                                  │
│           ▼                                                                  │
│  ┌─────────────────────────────────┐                                        │
│  │ Validate against output CUE     │ → Panic if invalid (internal bug)     │
│  │ schema/commands/{domain}/{action}.output.cue                             │
│  └────────┬────────────────────────┘                                        │
│           │                                                                  │
│           ▼                                                                  │
│  stdout (JSONL line)                                                         │
│                                                                              │
└──────────────────────────────────────────────────────────────────────────────┘
```

---

## Gleam Type ↔ CUE Schema Correspondence

Every Gleam type has a corresponding CUE schema. They MUST match exactly.

### Example: vision.start

**CUE Schema (source of truth):**
```cue
// schema/commands/vision/start.input.cue
package vision

#StartInput: {
    id: string & =~"^[a-zA-Z0-9_-]+$"
    command: "vision.start"
    args: {
        profile: "api" | "cli" | "event" | "data" | "workflow" | "ui"
        name?: string & strings.MinRunes(1) & strings.MaxRunes(100)
    }
    options?: {
        timeout_ms?: int & >0 & <300000
    }
}
```

**Gleam Type (enforces at runtime):**
```gleam
pub type VisionStartInput {
  VisionStartInput(
    id: String,
    command: String,  // Must be "vision.start"
    args: VisionStartArgs,
    options: Option(RequestOptions),
  )
}

pub type VisionStartArgs {
  VisionStartArgs(
    profile: Profile,
    name: Option(String),
  )
}

pub type Profile {
  Api
  Cli
  Event
  Data
  Workflow
  Ui
}
```

**Decoder (validates during parse):**
```gleam
pub fn decode_vision_start_input(data: Dynamic) -> Result(VisionStartInput, List(DecodeError)) {
  use id <- result.try(dynamic.field("id", dynamic.string)(data))
  use command <- result.try(dynamic.field("command", dynamic.string)(data))

  // Validate command matches
  use _ <- result.try(case command {
    "vision.start" -> Ok(Nil)
    _ -> Error([DecodeError(expected: "vision.start", found: command, path: ["command"])])
  })

  use args <- result.try(dynamic.field("args", decode_vision_start_args)(data))
  use options <- result.try(dynamic.optional_field("options", decode_request_options)(data))

  Ok(VisionStartInput(id, command, args, options))
}

fn decode_vision_start_args(data: Dynamic) -> Result(VisionStartArgs, List(DecodeError)) {
  use profile_str <- result.try(dynamic.field("profile", dynamic.string)(data))
  use profile <- result.try(parse_profile(profile_str))
  use name <- result.try(dynamic.optional_field("name", dynamic.string)(data))

  // Validate name length if present
  use _ <- result.try(case name {
    Some(n) if string.length(n) < 1 ->
      Error([DecodeError(expected: "non-empty string", found: "empty", path: ["args", "name"])])
    Some(n) if string.length(n) > 100 ->
      Error([DecodeError(expected: "max 100 chars", found: int.to_string(string.length(n)), path: ["args", "name"])])
    _ -> Ok(Nil)
  })

  Ok(VisionStartArgs(profile, name))
}
```

---

## Schema Registry

Schemas are embedded in the binary for fast access:

```gleam
// src/intent/schema_registry.gleam

pub type SchemaType {
  Input
  Output
}

pub fn get_schema(command: String, schema_type: SchemaType) -> Result(String, String) {
  let path = case schema_type {
    Input -> "schema/commands/" <> command_to_path(command) <> ".input.cue"
    Output -> "schema/commands/" <> command_to_path(command) <> ".output.cue"
  }

  // Embedded at compile time or loaded from disk
  load_schema(path)
}

fn command_to_path(command: String) -> String {
  // "vision.start" -> "vision/start"
  string.replace(command, ".", "/")
}

pub fn list_commands() -> List(String) {
  // Return all available commands
  [
    "vision.start", "vision.answer", "vision.critique", "vision.gate", "vision.advance",
    "shape.start", "shape.answer", "shape.critique", "shape.gate", "shape.advance",
    "spec.analyze", "spec.quality", "spec.coverage", "spec.gaps", "spec.invert", "spec.effects",
    "ready.check", "ready.critique", "ready.gate",
    "bead.generate", "bead.list", "bead.get",
    "plan.create", "plan.export",
    "schema.get", "schema.list", "schema.validate",
  ]
}
```

---

## CUE Validation Strategy

### Option 1: Shell out to `cue vet` (Current)
```gleam
pub fn validate_against_cue(json: String, schema_path: String) -> Result(Nil, List(ValidationError)) {
  // Write JSON to temp file
  // Run: cue vet schema.cue temp.json
  // Parse stderr for errors
}
```

**Pros:** Uses official CUE tooling
**Cons:** Subprocess overhead, requires cue installed

### Option 2: Embed CUE validation (Preferred for AI-only)
```gleam
// Use a CUE library or port validation logic
pub fn validate_against_schema(json: Json, schema: Schema) -> Result(Nil, List(ValidationError))
```

**Pros:** Fast, no subprocess, portable
**Cons:** Need to implement/find CUE validator

### Option 3: Gleam-only validation (Pragmatic)
```gleam
// CUE schemas are documentation, Gleam decoders are enforcement
// Validate CUE schemas in tests, trust Gleam decoders at runtime
```

**Pros:** Fastest, simplest
**Cons:** Schema drift risk (mitigated by tests)

**Recommendation:** Option 3 for runtime, Option 1 for testing.

---

## AI-Agent Optimized Command Design

### Principles for AI Agents

1. **Predictable structure** - Same envelope for all commands
2. **Self-documenting** - Schema introspection built-in
3. **Rich errors** - Include fix_command with valid JSONL
4. **Actionable next_actions** - Complete commands ready to execute
5. **Idempotent where possible** - Safe to retry
6. **Batch-friendly** - Process multiple commands efficiently
7. **Stateless requests** - All context in the request, not hidden state

### Command Response Design

Every response includes everything an AI needs to continue:

```json
{
  "id": "req-001",
  "success": true,
  "command": "vision.start",
  "data": {
    "session_id": "sess-abc123",
    "phase": "vision",
    "profile": "api",
    "questions": [
      {
        "id": "q1",
        "text": "What problem are you solving?",
        "context": "Describe the core problem in 2-3 sentences",
        "required": true,
        "example_answer": "Users struggle to track their fitness goals across multiple apps..."
      }
    ],
    "state": {
      "answered_count": 0,
      "total_questions": 8,
      "gaps": [],
      "conflicts": [],
      "gate_status": "locked"
    }
  },
  "errors": [],
  "next_actions": [
    {
      "command": "{\"id\":\"auto\",\"command\":\"vision.answer\",\"args\":{\"session_id\":\"sess-abc123\",\"question_id\":\"q1\",\"answer\":\"\"}}",
      "reason": "Answer the first required question",
      "priority": 1
    }
  ],
  "metadata": {
    "timestamp": "2024-01-25T10:30:00Z",
    "version": "4.0.0",
    "duration_ms": 45
  }
}
```

### Error Response Design

Errors include everything needed to fix them:

```json
{
  "id": "req-002",
  "success": false,
  "command": "vision.answer",
  "data": {},
  "errors": [
    {
      "code": "SESSION_NOT_FOUND",
      "message": "Session 'sess-invalid' does not exist",
      "path": "args.session_id",
      "fix_hint": "Create a new session or use an existing session ID",
      "fix_command": "{\"id\":\"fix-001\",\"command\":\"vision.start\",\"args\":{\"profile\":\"api\"}}",
      "available_sessions": ["sess-abc123", "sess-def456"]
    }
  ],
  "next_actions": [
    {
      "command": "{\"id\":\"auto\",\"command\":\"vision.start\",\"args\":{\"profile\":\"api\"}}",
      "reason": "Create a new vision session",
      "priority": 1
    },
    {
      "command": "{\"id\":\"auto\",\"command\":\"session.list\",\"args\":{}}",
      "reason": "List existing sessions",
      "priority": 2
    }
  ],
  "metadata": {
    "timestamp": "2024-01-25T10:30:05Z",
    "version": "4.0.0",
    "duration_ms": 12
  }
}
```

### Batch Response Design

For batch requests, return array or JSONL:

**Input:**
```jsonl
{"id":"1","command":"vision.start","args":{"profile":"api"}}
{"id":"2","command":"vision.answer","args":{"session_id":"$1.data.session_id","question_id":"q1","answer":"Solving fitness tracking"}}
```

**Output:**
```jsonl
{"id":"1","success":true,"command":"vision.start","data":{"session_id":"sess-new123",...}}
{"id":"2","success":true,"command":"vision.answer","data":{"answered":true,...}}
```

### Schema Introspection

AI can discover the API:

```jsonl
{"id":"discover","command":"schema.list","args":{}}
```

Response:
```json
{
  "id": "discover",
  "success": true,
  "command": "schema.list",
  "data": {
    "commands": [
      {
        "name": "vision.start",
        "description": "Start a new vision session for Phase 1",
        "input_schema": "schema/commands/vision/start.input.cue",
        "output_schema": "schema/commands/vision/start.output.cue"
      },
      // ... all commands
    ],
    "phases": ["vision", "shape", "spec", "ready"],
    "workflow": "vision → shape → spec → ready → beads"
  }
}
```

Get specific schema:
```jsonl
{"id":"get-schema","command":"schema.get","args":{"command":"vision.start","type":"input"}}
```

Response includes the full CUE schema as a string that AI can parse.

---

## Implementation Beads

### New Beads for Schema Enforcement

| ID | Title | Priority | Effort |
|----|-------|----------|--------|
| SCHEMA-01 | Create schema/common/envelope.cue | P1 | 20min |
| SCHEMA-02 | Create schema/common/errors.cue | P1 | 15min |
| SCHEMA-03 | Create schema/common/types.cue | P1 | 20min |
| SCHEMA-04 | Create schema_registry.gleam | P1 | 30min |
| SCHEMA-05 | Create input_validator.gleam | P1 | 45min |
| SCHEMA-06 | Create output_validator.gleam | P1 | 30min |
| SCHEMA-07 | Create command_router.gleam | P1 | 45min |
| SCHEMA-08 | Schema validation tests | P1 | 30min |

### Modified Beads

All command beads now include:
- Input schema definition
- Output schema definition
- Gleam type that matches schema
- Decoder with validation
- Tests that verify schema ↔ type correspondence

---

## Testing Strategy

### Schema ↔ Type Correspondence Tests

```gleam
// test/schema_correspondence_test.gleam

pub fn vision_start_input_matches_schema_test() {
  // 1. Load CUE schema
  let schema = schema_registry.get_schema("vision.start", Input)

  // 2. Generate valid examples from schema (or use fixtures)
  let valid_json = "{\"id\":\"test\",\"command\":\"vision.start\",\"args\":{\"profile\":\"api\"}}"

  // 3. Validate JSON against CUE (using cue vet)
  let cue_result = cue_validate(valid_json, schema)
  assert cue_result == Ok(Nil)

  // 4. Parse with Gleam decoder
  let gleam_result = decode_vision_start_input(json.decode(valid_json))
  assert gleam_result |> result.is_ok

  // 5. Encode back to JSON
  let roundtrip = gleam_result |> result.unwrap |> vision_start_input_to_json

  // 6. Validate roundtrip against CUE
  let roundtrip_result = cue_validate(roundtrip, schema)
  assert roundtrip_result == Ok(Nil)
}
```

### Invalid Input Tests

```gleam
pub fn vision_start_rejects_invalid_profile_test() {
  let invalid_json = "{\"id\":\"test\",\"command\":\"vision.start\",\"args\":{\"profile\":\"invalid\"}}"

  // Both CUE and Gleam should reject
  let cue_result = cue_validate(invalid_json, schema)
  assert cue_result |> result.is_error

  let gleam_result = decode_vision_start_input(json.decode(invalid_json))
  assert gleam_result |> result.is_error
}
```

---

## Summary

| Layer | Responsibility |
|-------|---------------|
| **CUE Schemas** | Source of truth for API contracts |
| **Gleam Types** | Runtime enforcement, type safety |
| **Decoders** | Validation during parse |
| **Tests** | Verify schema ↔ type correspondence |
| **Output Validators** | Ensure responses match output schemas |

**Guarantee:** If a request passes input validation, it WILL produce a valid output. If something goes wrong, the error response WILL match the error schema.
