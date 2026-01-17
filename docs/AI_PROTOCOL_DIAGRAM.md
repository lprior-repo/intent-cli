# AI Protocol Flow Diagrams

Visual reference for the AI Interview Protocol.

## Complete Interview Flow

```
┌──────────────┐
│  AI Agent    │
└──────┬───────┘
       │
       │ 1. intent interview --cue --profile api
       ↓
┌──────────────────────────────────────────────────────────┐
│                     Intent CLI                           │
│  ┌────────────────────────────────────────────────────┐  │
│  │ Generate Session ID: interview-abc123              │  │
│  │ Load Question Database for Profile                 │  │
│  │ Get First Question (Round 1)                       │  │
│  │ Persist Session to .interview/sessions.jsonl      │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────┬───────────────────────────────────────┘
                   │
                   │ 2. Output Question Response (JSON)
                   ↓
       ┌───────────────────────────────────┐
       │ {                                 │
       │   "action": "ask_question",       │
       │   "question": {...},              │
       │   "progress": {...},              │
       │   "session": {"id": "..."}        │
       │ }                                 │
       └───────────┬───────────────────────┘
                   │
┌──────────────┐   │
│  AI Agent    │◄──┘
│              │
│  Parse JSON  │
│  Extract:    │
│  - question  │
│  - session   │
│  - hint      │
│              │
│  Generate    │
│  Answer      │
└──────┬───────┘
       │
       │ 3. intent interview --cue --session "interview-abc123"
       │                           --answer "THE SYSTEM SHALL..."
       ↓
┌──────────────────────────────────────────────────────────┐
│                     Intent CLI                           │
│  ┌────────────────────────────────────────────────────┐  │
│  │ Load Session from .interview/sessions.jsonl       │  │
│  │ Validate Answer (length, format)                  │  │
│  │ Extract Fields (entities, behaviors, etc.)        │  │
│  │ Check for Gaps/Conflicts                          │  │
│  │ Save Updated Session                              │  │
│  │ Get Next Question                                 │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────┬───────────────────────────────────────┘
                   │
                   │ 4. Output Next Question OR Completion
                   ↓
       ┌───────────────────────────────────┐
       │ IF more questions:                │
       │   { "action": "ask_question" }    │
       │                                   │
       │ ELSE:                             │
       │   { "action": "interview_complete"│
       │     "output": {"spec_path": ...} }│
       └───────────┬───────────────────────┘
                   │
┌──────────────┐   │
│  AI Agent    │◄──┘
└──────┬───────┘
       │
       │ Repeat steps 3-4 until complete
       ↓
   [Loop continues through all 5 rounds]
```

---

## Round Progression

```
Round 1: Basic Info          Round 2: Behaviors
┌──────────────────┐        ┌──────────────────┐
│ Q1: Core intent  │        │ Q6: Common errors│
│ Q2: Audience     │───────▶│ Q7: Error format │
│ Q3: Happy path   │        │ Q8: Status codes │
│ Q4: Data model   │        │ Q9: Logging      │
│ Q5: Auth method  │        │ Q10: Info leaks  │
└──────────────────┘        └──────────┬───────┘
     Discovery                          │
          ▲                             │
          │                             ↓
          │                    Round 3: Edge Cases
          │                    ┌──────────────────┐
          │                    │ Q11: Edge cases  │
          │                    │ Q12: Dependencies│
          │                    │ Q13: Constraints │
          │                    │ Q14: Concurrency │
          │                    │ Q15: Limits      │
          │                    └──────────┬───────┘
          │                               │
          │                               ↓
          │                    Round 4: Security
          │                    ┌──────────────────┐
          │                    │ Q16: AuthZ rules │
          │                    │ Q17: Encryption  │
          │                    │ Q18: Compliance  │
          │                    │ Q19: Audit logs  │
          │                    │ Q20: Threats     │
          │                    └──────────┬───────┘
          │                               │
          │                               ↓
          │                    Round 5: Validation
          │                    ┌──────────────────┐
          │                    │ Q21: Performance │
          │                    │ Q22: Scalability │
          └────────────────────┤ Q23: Observability│
                 Complete      │ Q24: Deployment  │
                               │ Q25: Completeness│
                               └──────────────────┘
```

---

## Message Types Decision Tree

```
                    Start Interview
                          │
                          ↓
               ┌──────────────────────┐
               │ CLI Execution        │
               └──────────┬───────────┘
                          │
              ┌───────────▼───────────┐
              │   Check Action Type   │
              └───────────┬───────────┘
                          │
        ┌─────────────────┼─────────────────┐
        │                 │                 │
        ↓                 ↓                 ↓
 ┌──────────────┐  ┌──────────────┐  ┌──────────────┐
 │ ask_question │  │interview_    │  │ validation_  │
 │              │  │  complete    │  │   error      │
 └──────┬───────┘  └──────┬───────┘  └──────┬───────┘
        │                 │                 │
        ↓                 ↓                 ↓
 ┌──────────────┐  ┌──────────────┐  ┌──────────────┐
 │Extract:      │  │Extract:      │  │Check:        │
 │- session.id  │  │- spec_path   │  │- retry_allowed│
 │- question    │  │- statistics  │  │- suggestion  │
 │- hint        │  │              │  │              │
 └──────┬───────┘  └──────┬───────┘  └──────┬───────┘
        │                 │                 │
        ↓                 ↓                 ↓
 ┌──────────────┐  ┌──────────────┐  ┌──────────────┐
 │Generate      │  │Save spec path│  │Fix answer and│
 │answer        │  │End interview │  │retry         │
 └──────┬───────┘  └──────────────┘  └──────┬───────┘
        │                                    │
        ↓                                    │
 ┌──────────────┐                            │
 │Submit answer │                            │
 └──────┬───────┘                            │
        │                                    │
        └────────────────────────────────────┘
```

---

## Error Handling Flow

```
                Submit Answer
                      │
                      ↓
          ┌───────────────────────┐
          │  Validate Answer      │
          └───────────┬───────────┘
                      │
        ┌─────────────┴─────────────┐
        │                           │
        ↓                           ↓
   Valid Answer               Invalid Answer
        │                           │
        ↓                           ↓
┌──────────────┐          ┌──────────────────┐
│Process Answer│          │ Check Error Type │
│Save Session  │          └─────────┬────────┘
│Return Next Q │                    │
└──────────────┘        ┌───────────┼───────────┐
                        │           │           │
                        ↓           ↓           ↓
                  ┌─────────┐ ┌─────────┐ ┌─────────┐
                  │Too Short│ │Session  │ │Internal │
                  │         │ │Not Found│ │Error    │
                  └────┬────┘ └────┬────┘ └────┬────┘
                       │           │           │
                       ↓           ↓           ↓
                  ┌─────────┐ ┌─────────┐ ┌─────────┐
                  │retry:   │ │retry:   │ │retry:   │
                  │ true    │ │ false   │ │ false   │
                  └────┬────┘ └────┬────┘ └────┬────┘
                       │           │           │
                       ↓           ↓           ↓
                  ┌─────────┐ ┌─────────┐ ┌─────────┐
                  │Resubmit │ │Start New│ │Fix and  │
                  │w/ longer│ │Interview│ │Retry    │
                  └─────────┘ └─────────┘ └─────────┘
```

---

## Session State Machine

```
                         ┌──────┐
                         │ NULL │
                         └───┬──┘
                             │
                             │ Start Interview
                             ↓
                      ┌────────────┐
                      │ DISCOVERY  │
                      │ (Rounds 1-2)│
                      └─────┬──────┘
                            │
                            │ Rounds 1-2 Complete
                            ↓
                      ┌────────────┐
                      │ REFINEMENT │
                      │ (Round 3)   │
                      └─────┬──────┘
                            │
                            │ Round 3 Complete
                            ↓
                      ┌────────────┐
                      │ VALIDATION │
                      │ (Rounds 4-5)│
                      └─────┬──────┘
                            │
                            │ Round 5 Complete
                            ↓
                      ┌────────────┐
                      │  COMPLETE  │
                      └────────────┘
                            │
                            │ Spec Generated
                            ↓
                      ┌────────────┐
                      │  ARCHIVED  │
                      └────────────┘

   ┌────────────────────────────────────────┐
   │ Can Pause/Resume at any state          │
   │ Sessions expire after 24h inactivity   │
   │ All states persisted to JSONL          │
   └────────────────────────────────────────┘
```

---

## EARS Pattern Decision Tree

```
                   Question Text
                         │
                         ↓
            ┌────────────────────────┐
            │ Contains "when"?       │
            └────────┬───────────────┘
                     │
         ┌───────────┴───────────┐
         │YES                    │NO
         ↓                       ↓
    ┌─────────┐          ┌──────────────┐
    │Contains │          │Contains      │
    │"while"? │          │"while"?      │
    └────┬────┘          └──────┬───────┘
         │                      │
    ┌────┴────┐          ┌──────┴───────┐
    │YES  │NO │          │YES       │NO │
    ↓     ↓   ↓          ↓          ↓   ↓
  COMPLEX EVENT  ┌────────┴─┐   ┌───┴───────┐
                 │Contains  │   │Contains   │
                 │"if"+"not"│   │"if"+"not" │
                 └────┬─────┘   └─────┬─────┘
                      │               │
                 ┌────┴────┐     ┌────┴────┐
                 │YES  │NO │     │YES  │NO │
                 ↓     ↓   ↓     ↓     ↓   ↓
              STATE  UNWANTED UNWANTED UBIQUITOUS
              DRIVEN         │
                             ↓
                       ┌──────────┐
                       │Contains  │
                       │"optional"│
                       └────┬─────┘
                            │
                       ┌────┴────┐
                       │YES  │NO │
                       ↓     ↓
                   OPTIONAL UBIQUITOUS
```

---

## Progress Tracking Visual

```
Interview Progress: 12/25 questions (48% complete)

Round 1: Discovery (Basic Info)
[█████████████████████] 5/5 ✓ Complete

Round 2: Discovery (Behaviors)
[█████████████████████] 5/5 ✓ Complete

Round 3: Refinement (Edge Cases)
[████████░░░░░░░░░░░░░] 2/5 ← Current

Round 4: Validation (Security)
[░░░░░░░░░░░░░░░░░░░░░] 0/5

Round 5: Validation (Completeness)
[░░░░░░░░░░░░░░░░░░░░░] 0/3


Current Question:
─────────────────────────────────────────
Q12 [IMPORTANT] (Developer perspective)

"What external dependencies does this system have?"

Context: Dependencies affect reliability and testing.
Example: PostgreSQL database, Redis cache, SendGrid email API

Pattern: ubiquitous
Hint: Use format: THE SYSTEM SHALL [behavior]
─────────────────────────────────────────
```

---

## Session Storage Structure

```
Project Root
│
├── .interview/
│   ├── sessions.jsonl              ← All session state
│   │   └── One line per session (latest state)
│   │       {
│   │         "id": "interview-abc123",
│   │         "profile": "api",
│   │         "stage": "refinement",
│   │         "rounds_completed": 2,
│   │         "answers": [...],
│   │         "gaps": [...],
│   │         "conflicts": [...]
│   │       }
│   │
│   ├── history.jsonl               ← Audit trail
│   │   └── One line per snapshot
│   │       {
│   │         "session_id": "interview-abc123",
│   │         "snapshot_id": "interview-abc123-2026-01-16T14:32:10Z",
│   │         "timestamp": "2026-01-16T14:32:10Z",
│   │         "description": "Round 1 complete",
│   │         "answers": {...}
│   │       }
│   │
│   └── spec-interview-abc123.cue   ← Generated spec (on completion)
│       package spec
│
│       spec: {
│         name: "User Authentication API"
│         ...
│       }
│
└── (Your project files)
```

---

## JSON Response Structure Map

```
┌─────────────────────────────────────────────────────────┐
│                   CLI Response JSON                     │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  action: String ────────┬── "ask_question"             │
│  (Required)             ├── "interview_complete"       │
│                         └── "validation_error"         │
│                                                         │
├─────────────────────────────────────────────────────────┤
│  IF action == "ask_question":                           │
│  ├── question: Object                                   │
│  │   ├── text: String (the actual question)            │
│  │   ├── id: String (e.g., "r1-user-api-1")           │
│  │   ├── round: Integer (1-5)                          │
│  │   ├── perspective: Enum                             │
│  │   ├── category: Enum                                │
│  │   ├── priority: Enum                                │
│  │   ├── context: String                               │
│  │   ├── example: String                               │
│  │   ├── pattern: String (EARS pattern)                │
│  │   ├── hint: String                                  │
│  │   ├── examples: Array[String]                       │
│  │   ├── extract_into: Array[String]                   │
│  │   └── expected_type: String                         │
│  ├── progress: Object                                   │
│  │   ├── current_step: Integer                         │
│  │   ├── total_steps: Integer                          │
│  │   ├── percent_complete: Integer (0-100)             │
│  │   ├── round: Integer (1-5)                          │
│  │   ├── round_name: String                            │
│  │   └── category: String                              │
│  └── session: Object                                    │
│      ├── id: String (use for answers!)                 │
│      ├── profile: String                               │
│      └── started_at: String (ISO 8601)                 │
├─────────────────────────────────────────────────────────┤
│  IF action == "interview_complete":                     │
│  ├── output: Object                                     │
│  │   ├── spec_path: String                             │
│  │   ├── behaviors_count: Integer                      │
│  │   ├── anti_patterns_count: Integer                  │
│  │   ├── summary: String                               │
│  │   └── next_steps: Array[String]                     │
│  ├── session: Object (same as above + completed_at)    │
│  └── statistics: Object                                 │
│      ├── total_questions: Integer                      │
│      ├── total_answers: Integer                        │
│      ├── rounds_completed: Integer                     │
│      ├── gaps_detected: Integer                        │
│      ├── conflicts_detected: Integer                   │
│      └── average_confidence: Float (0-1)               │
├─────────────────────────────────────────────────────────┤
│  IF action == "validation_error":                       │
│  ├── error: Object                                      │
│  │   ├── code: String (error code)                     │
│  │   ├── message: String                               │
│  │   ├── suggestion: String (how to fix)               │
│  │   ├── retry_allowed: Boolean                        │
│  │   └── context: Object (optional details)            │
│  └── session: Object (partial - id, profile only)      │
└─────────────────────────────────────────────────────────┘
```

---

**End of Diagrams**
