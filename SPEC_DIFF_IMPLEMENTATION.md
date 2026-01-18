# Spec Diff Tracking Implementation

## Overview

This document describes the implementation of spec diff tracking for the Intent CLI. The feature enables tracking how specs change between consecutive interview rounds and provides diff summaries for iteration tracking.

**Bead ID**: intent-cli-6nqr
**Priority**: 2
**Status**: Implementation Complete

## Architecture

The implementation follows the **Railway-Oriented Programming (ROP)** and **Functional Core / Imperative Shell** patterns:

- **Pure Functions**: All diff logic is pure, deterministic, and testable
- **No Side Effects**: Core comparison logic has no I/O operations
- **Type Safety**: Exhaustive pattern matching ensures correctness
- **Integration Ready**: Designed to integrate with interview_storage.gleam

## Module: spec_diff.gleam

Located at: `/home/lewis/src/intent-cli/src/intent/spec_diff.gleam`

### Key Types

#### SpecSnapshot
Represents a spec at a point in time:
```gleam
pub type SpecSnapshot {
  SpecSnapshot(
    spec_id: String,           // "api-1.0.0"
    snapshot_id: String,       // "api-1.0.0-round-1"
    timestamp: String,         // ISO timestamp or metadata
    round: Int,                // Interview round number
    spec: Spec,                // Full spec
    description: String,       // "After EARS round"
  )
}
```

#### SpecDiff
Comprehensive diff between two spec versions:
```gleam
pub type SpecDiff {
  SpecDiff(
    // Identification and timing
    from_id: String,           // snapshot_id of previous version
    to_id: String,             // snapshot_id of current version
    from_timestamp: String,    // Timestamp of previous
    to_timestamp: String,      // Timestamp of current
    from_round: Int,           // Previous round number
    to_round: Int,             // Current round number

    // Metadata changes (optional)
    name_changed: Option(#(String, String)),
    description_changed: Option(#(String, String)),
    version_changed: Option(#(String, String)),
    audience_changed: Option(#(String, String)),

    // Spec content changes
    success_criteria_changed: CriteriaChange,
    config_changed: ConfigChange,
    features_added: List(Feature),
    features_modified: List(#(Feature, Feature)),
    features_removed: List(Feature),
    behaviors_added: List(BehaviorChange),
    behaviors_modified: List(BehaviorChange),
    behaviors_removed: List(BehaviorChange),
    rules_added: List(Rule),
    rules_modified: List(#(Rule, Rule)),
    rules_removed: List(Rule),
    anti_patterns_added: List(AntiPattern),
    anti_patterns_modified: List(#(AntiPattern, AntiPattern)),
    anti_patterns_removed: List(AntiPattern),

    // Summary metrics
    total_changes: Int,
    significant: Bool,         // > 5% of base size
  )
}
```

#### BehaviorChange
Tracks behavior modifications with feature context:
```gleam
pub type BehaviorChange {
  BehaviorChange(
    feature_name: String,      // Parent feature
    behavior_name: String,     // Behavior name
    old_intent: Option(String), // Previous intent
    new_intent: String,        // Current intent
    change_type: ChangeType,   // Added/Modified/Removed
  )
}
```

#### Change Types

- **CriteriaChange**: Tracks success criteria additions and removals
- **ConfigChange**: Tracks base_url, timeout_ms, headers modifications
- **ChangeType**: Enum for Added/Modified/Removed

### Public API

#### `create_spec_snapshot/3`
Creates a snapshot of a spec at a point in time:
```gleam
pub fn create_spec_snapshot(
  spec: Spec,
  round: Int,
  description: String,
) -> SpecSnapshot
```

**Pure function** - no I/O, deterministic.

#### `diff_specs/2`
Compares two spec snapshots and produces a comprehensive diff:
```gleam
pub fn diff_specs(
  from_snapshot: SpecSnapshot,
  to_snapshot: SpecSnapshot,
) -> SpecDiff
```

**Algorithm**:
1. Compare metadata fields (name, description, version, audience)
2. Compare success criteria lists (set difference)
3. Compare config (base_url, timeout_ms, headers)
4. Compare features by name (keyed dictionary comparison)
5. Compare behaviors across all features (keyed by "feature::behavior")
6. Compare rules by name
7. Compare anti-patterns by name
8. Calculate total changes and significance threshold

**Significance Threshold**: > 5% of base size (features + rules + anti-patterns)

#### `format_diff/1`
Formats a SpecDiff as human-readable text:
```gleam
pub fn format_diff(diff: SpecDiff) -> String
```

**Output Format**:
```
Spec Diff: Round 1 → Round 2
Time: <timestamp> → <timestamp>

⚠ SIGNIFICANT CHANGE (12 changes)

Metadata Changes:
  • Version: 1.0.0 → 1.1.0

Features:
  + new_feature: Handles new payment method
  ~ existing_feature
    Before: Old description
    After: New description
  - removed_feature

Behaviors:
  + [auth] validate_2fa: New two-factor authentication flow
  ~ [payments] process_refund
    - Requires card number
    + Requires transaction ID
  - [legacy] old_behavior

Rules:
  + new_rule: All responses must include request ID
  ~ existing_rule
    - Checks status code
    + Checks status code and body

Anti-Patterns:
  + hardcoded_passwords: Don't hardcode passwords
  ~ sql_injection: Definition changed

Success Criteria:
  + Support 2FA authentication
  - Support legacy password auth

Config Changes:
  base_url: http://api.example.com → https://api.example.com
  timeout_ms: 5000 → 10000
  authorization: Bearer token → OAuth 2.0
```

#### JSON Serialization

```gleam
pub fn snapshot_to_json(snapshot: SpecSnapshot) -> json.Json
pub fn diff_to_json(diff: SpecDiff) -> json.Json
```

## Design Decisions

### 1. Round-Aware Tracking
- Each diff includes `from_round` and `to_round` for tracking evolution through 5-round mental model
- Integration point with interview engine (Discovery → Refinement → Validation → Complete)

### 2. Per-Feature Behavior Changes
- Behaviors are keyed by "feature_name::behavior_name" to enable accurate tracking
- Supports adding/modifying/removing behaviors within features

### 3. Significance Calculation
- Dynamic threshold: `max(1, base_size / 20)` = 5% change detection
- Helps distinguish minor iterations from significant rewrites

### 4. Optional Changes
- Metadata changes (name, description, etc.) are `Option` types
- `None` means no change, `Some(#(old, new))` means changed
- This supports specs that keep constant metadata during rounds

### 5. Pure Functions for Testability
- All comparison logic is pure
- No dependency on I/O, system time, or external state
- Enables comprehensive unit testing

## Integration Points

### With interview_storage.gleam
The spec diff module can integrate with interview session storage:

```gleam
// Proposed integration in interview_storage.gleam
pub fn append_spec_diff_to_history(
  session: InterviewSession,
  previous_spec: Spec,
  current_spec: Spec,
  round: Int,
  history_path: String,
) -> Result(Nil, String) {
  let from_snapshot = spec_diff.create_spec_snapshot(
    previous_spec,
    round - 1,
    "End of round " <> string.inspect(round - 1)
  )
  let to_snapshot = spec_diff.create_spec_snapshot(
    current_spec,
    round,
    "After round " <> string.inspect(round) <> " updates"
  )

  let diff = spec_diff.diff_specs(from_snapshot, to_snapshot)
  let line = spec_diff.diff_to_json_line(diff)

  // Append to .interview/spec_diffs.jsonl
  use existing <- result.try(read_file(history_path))
  let content = case existing {
    "" -> line
    _ -> existing <> "\n" <> line
  }
  write_file(history_path, content)
}
```

### With check command
When running `intent check` on successive rounds, capture before/after specs:

```gleam
// Pseudo-code for integration
let previous_spec = load_previous_round_spec(round - 1)
let current_spec = loader.load_spec(spec_path)

let diff = spec_diff.diff_specs(
  spec_diff.create_spec_snapshot(previous_spec, round - 1, "Previous round"),
  spec_diff.create_spec_snapshot(current_spec, round, "Current round")
)

case diff.significant {
  True -> io.println("⚠ Significant spec changes detected")
  False -> io.println("Minor spec updates")
}
```

## Testing Strategy

### Unit Tests (test/intent/spec_diff_test.gleam)

Tests cover:

1. **Snapshot Creation**
   - Create snapshot from spec with round number
   - Verify snapshot_id format
   - Verify description storage

2. **No-Change Diff**
   - Diff identical specs returns 0 total_changes
   - Significance is False
   - All change lists are empty

3. **Feature Addition**
   - Add feature to spec
   - Diff shows feature in `features_added`
   - Increments total_changes

4. **Behavior Changes**
   - Modify behavior intent
   - Diff shows behavior in `behaviors_modified`
   - Tracks old_intent and new_intent

5. **Complex Changes**
   - Multiple types of changes in one round
   - Verify total_changes calculation
   - Verify significant flag threshold

6. **Formatting**
   - Format diff produces valid output
   - Includes all change categories
   - Properly truncates long strings

### Integration Tests (Future)

- Load real interview sessions
- Capture spec diffs across rounds
- Verify diffs match expected evolution

## Performance Characteristics

### Time Complexity
- Snapshot creation: O(1) constant
- Diff comparison: O(n + m) where n = features, m = behaviors
  - Dictionary lookups: O(log n) per item
  - Total: O(n log n) for features, O(m log m) for behaviors

### Space Complexity
- Snapshot: O(n) where n = spec size
- Diff: O(changes) - only stores changed items
- Memory efficient for typical specs (< 100 features, < 1000 behaviors)

## Future Enhancements

1. **Structured Diff Output**
   - JSON format optimized for machine parsing
   - Include change IDs for cross-referencing

2. **Contextual Suggestions**
   - AI hints based on detected changes
   - Suggest rule additions when new features detected

3. **Diff Analytics**
   - Track frequency of each change type per round
   - Identify patterns in specification evolution

4. **Merge Support**
   - Detect conflicting changes between parallel interviews
   - Suggest resolution strategies

5. **Backwards Compatibility**
   - Old spec → new spec schema migration tracking
   - Detection of deprecated fields

## Code Quality Metrics

- **Lines of Code**: ~950 (spec_diff.gleam)
- **Cyclomatic Complexity**: Low (max depth: 5 nested cases)
- **Test Coverage Target**: 90%+ for diff_specs
- **Documentation**: Full inline doc comments
- **Type Safety**: 100% - no unwrap(), all cases handled

## Integration Checklist

- [x] Create spec_diff.gleam module
- [x] Implement SpecSnapshot type
- [x] Implement SpecDiff type
- [x] Implement diff_specs comparison logic
- [x] Implement format_diff output
- [x] Add JSON serialization
- [x] Create spec_diff_test.gleam tests
- [ ] Integrate with interview_storage.gleam
- [ ] Add CLI command: `intent spec-diff`
- [ ] Add flag to check command: `--track-diffs`
- [ ] Update CLAUDE.md with diff command documentation
- [ ] Add metrics to quality analyzer
- [ ] Create examples in documentation

## Files Modified/Created

### Created
- `/home/lewis/src/intent-cli/src/intent/spec_diff.gleam` (950 lines)
- `/home/lewis/src/intent-cli/test/intent/spec_diff_test.gleam` (120 lines)
- `/home/lewis/src/intent-cli/SPEC_DIFF_IMPLEMENTATION.md` (this file)

### To Be Modified
- `src/intent.gleam` - Add CLI integration
- `src/intent/interview_storage.gleam` - Add diff storage methods
- `CLAUDE.md` - Document new command
- `.gitignore` - Ensure .interview/spec_diffs.jsonl is tracked

## References

- **RFC 0001**: Spec evolution tracking for interview rounds
- **CLAUDE.md**: Section "5-Round Mental Model System"
- **interview_storage.gleam**: Answer history tracking pattern
- **Types**: Spec, Feature, Behavior, Rule, AntiPattern structures
