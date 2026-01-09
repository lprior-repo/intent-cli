# Intent CLI Design Decisions

## File Locking Strategy for Concurrent Append

### Context

The `bead_feedback.gleam` module needs to append feedback entries to `.intent/feedback-{session}.cue` files. Multiple beads may complete concurrently, requiring atomic append operations.

### Decision: Erlang File Locking + Atomic Write

Erlang/OTP provides robust file handling through the `file` module. Our strategy:

1. **Exclusive Lock via file:open/2 with `{exclusive, true}`**
   - Uses OS-level file locking (flock on Unix, LockFile on Windows)
   - Prevents multiple processes from writing simultaneously

2. **Atomic Append Pattern**
   ```
   1. Read current file content
   2. Append new feedback entry
   3. Write to temp file (same directory)
   4. Call file:sync to flush to disk
   5. Rename temp to target (atomic on POSIX)
   ```

3. **Retry on Lock Failure**
   - If lock acquisition fails, wait 50ms and retry (max 10 attempts)
   - After max retries, return error with helpful message

### Implementation Location

- `src/intent_ffi.erl`: `atomic_append/2` function
- `src/intent/bead_feedback.gleam`: Gleam wrapper

### Trade-offs

- **Simplicity over performance**: File locking adds overhead but ensures correctness
- **No external dependencies**: Uses built-in Erlang primitives
- **Platform portable**: Works on Linux, macOS, Windows

### Edge Cases Handled

- Disk full during write: Temp file deleted, error returned
- Process crash during write: Temp file left behind (cleaned on next run)
- Symlinked file: Follow symlink, lock resolved path
- Permission denied: Clear error message with path

### Testing

```bash
# Concurrent append test
gleam test -- --filter concurrent_append
```

---

## Regeneration Strategies

### Context

When beads fail execution, the `beads-regenerate` command analyzes failures and generates improved beads. Four explicit strategies are defined.

### Strategy Definitions

1. **Inversion-Driven** (`inversion_driven`)
   - Analyze what went wrong by inverting expectations
   - Ask: "What would make this fail?" then prevent that
   - Best for: Security failures, edge case misses

2. **Second-Order-Driven** (`second_order_driven`)
   - Trace cascading effects of the failure
   - Ask: "What happened after the initial failure?"
   - Best for: Integration failures, dependency issues

3. **Pre-Mortem-Driven** (`premortem_driven`)
   - Imagine the failure already happened, work backwards
   - Ask: "We failed - what was the root cause?"
   - Best for: Complex failures, unclear error messages

4. **Hybrid** (`hybrid`)
   - Apply all three methods in sequence
   - Combine insights for comprehensive regeneration
   - Default strategy, highest quality but slowest

### Strategy Selection

- **Auto-select based on error type**:
  - `error.type == "security"` → inversion_driven
  - `error.type == "integration"` → second_order_driven
  - `error.type == "unknown"` → hybrid
  - User can override with `--strategy` flag

### Implementation Location

- `src/intent/regenerate.gleam`: Strategy implementations
- `schema/beads.cue#RegenerationStrategy`: Type definitions

---

## Quality Threshold

All executable beads must meet an 80+ quality threshold before execution.

Quality dimensions:
- **Completeness** (0-100): Are all required fields specified?
- **Testability** (0-100): Can this bead be verified automatically?
- **Clarity** (0-100): Is the intent unambiguous?

Overall score = weighted average (testability × 1.5, others × 1.0)

---

## Feedback Loop Integration (FEEDBACK-CONNECT)

### Flow: Light Specs → Beads → Feedback → Regeneration

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         FEEDBACK LOOP                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. LIGHT SPEC CREATION (--light flag)                                  │
│     ├── 2-5 questions per profile                                       │
│     ├── LightSpec CUE output (simplified behaviors)                     │
│     └── Context scan (language, framework auto-detected)                │
│                                                                          │
│  2. BEAD GENERATION (intent beads <session>)                           │
│     ├── Convert LightSpec to BeadRecord with AI-friendly format         │
│     ├── Include: input_example, output_example, must_return, must_not   │
│     └── Generate edge_cases from interview answers                      │
│                                                                          │
│  3. BEAD EXECUTION (AI implements each bead)                            │
│     ├── AI receives simplified bead format                              │
│     ├── Executes implementation                                         │
│     └── Reports result via: intent bead-status <id> --status X         │
│                                                                          │
│  4. FEEDBACK CAPTURE (bead_feedback.gleam)                              │
│     ├── Appends #BeadFeedback to .intent/feedback-{session}.cue        │
│     ├── Tracks: result, reason, error, duration_ms                      │
│     └── Uses atomic append (file locking strategy above)                │
│                                                                          │
│  5. REGENERATION (intent beads-regenerate <session>)                    │
│     ├── Loads feedback for failed/blocked beads                         │
│     ├── Applies regeneration strategy (inversion/second-order/etc)      │
│     ├── Generates improved beads with updated edge_cases                │
│     └── Preserves successful beads unchanged                            │
│                                                                          │
│  6. ITERATION                                                           │
│     └── Repeat steps 3-5 until all beads pass                          │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### How Light Specs Flow Through Feedback

1. **Light spec captures intent quickly** (2-5 questions vs 25+)
2. **Context scan provides environment info** (language, framework)
3. **Beads include both in AI-friendly format**
4. **When bead fails**, feedback captures:
   - Error type and message
   - Context from codebase scan
   - Original light spec intent
5. **Regeneration uses all context** to improve the bead

### Integration Points

- `src/intent/spec_builder.gleam`: `build_light_spec_from_session()`
- `src/intent/context_scanner.gleam`: `detect_from_directory()`
- `src/intent/bead_templates.gleam`: `generate_beads_from_session()` + AI fields
- `src/intent/bead_feedback.gleam`: `append_feedback()`, `load_feedback()`
- `src/intent/regenerate.gleam`: 4 regeneration strategies
