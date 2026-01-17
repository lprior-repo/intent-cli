# Quality Analyzer Error Handling Implementation

## Summary

Added comprehensive error handling to the `quality_analyzer` module following the Railway-Oriented Programming pattern used throughout Intent CLI.

## Changes Made

### 1. Error Type Definition

Created `QualityAnalyzerError` type with four variants:

```gleam
pub type QualityAnalyzerError {
  EmptySpec(message: String)
  InvalidSpec(message: String)
  MissingRequiredField(field: String, location: String)
  IncompleteData(message: String)
}
```

### 2. Updated Function Signatures

**Before:**
```gleam
pub fn analyze_spec(spec: Spec) -> QualityReport
```

**After:**
```gleam
pub fn analyze_spec(spec: Spec) -> Result(QualityReport, QualityAnalyzerError)
```

### 3. Validation Logic

Added `validate_spec_structure()` to check for required fields before analysis:
- Validates that `spec.name` is not empty
- Can be extended to check other required fields

### 4. Error Conditions

The analyzer now returns errors for:

- **EmptySpec**: Spec has no features or behaviors (cannot analyze)
- **IncompleteData**: Features exist but have no behaviors
- **MissingRequiredField**: Required spec fields are missing
- **InvalidSpec**: Spec structure is invalid (reserved for future use)

### 5. AI-Friendly Error Formatting

Added two error formatters following the pattern in `answer_loader.gleam`:

#### `format_error_ai(error)` - CUE Format for AI Agents

Returns structured CUE with:
- `action` - Error category
- `error.type` - Specific error type
- `error.message` - What went wrong
- `error.context` - Additional context fields
- `suggestion` - What to do next
- `recovery` - List of concrete recovery steps

Example output:
```cue
{
    action: "validation_error"
    error: {
        type: "empty_spec"
        message: "Cannot analyze empty specification"
        context: {
            reason: "Spec has no features or behaviors"
            analysis_stage: "quality_analysis"
        }
    }
    suggestion: "Add features and behaviors to the specification"
    recovery: [
        "Add at least one feature to the spec",
        "Ensure each feature has at least one behavior",
        "Run 'intent validate <spec.cue>' to verify structure",
        "Use 'intent interview' to generate complete specification"
    ]
}
```

#### `format_error_text(error)` - Human-Readable Format

Returns formatted text with:
- Error message
- Context section
- Suggestion
- Numbered recovery steps

Example output:
```
Error: Cannot analyze empty specification

Context:
  reason: Spec has no features or behaviors
  analysis_stage: quality_analysis

Suggestion: Add features and behaviors to the specification

Recovery Steps:
  1. Add at least one feature to the spec
  2. Ensure each feature has at least one behavior
  3. Run 'intent validate <spec.cue>' to verify structure
  4. Use 'intent interview' to generate complete specification
```

### 6. CLI Integration

Updated both CLI commands to handle Result type:

**`analyze` command:**
```gleam
case quality_analyzer.analyze_spec(spec) {
  Ok(report) -> {
    io.println(quality_analyzer.format_report(report))
    halt(exit_pass)
  }
  Error(e) -> {
    io.println_error(quality_analyzer.format_error_text(e))
    halt(exit_invalid)
  }
}
```

**`improve` command:**
```gleam
case quality_analyzer.analyze_spec(spec) {
  Ok(quality_report) -> {
    // Continue with improvement suggestions
  }
  Error(e) -> {
    io.println_error(quality_analyzer.format_error_text(e))
    halt(exit_invalid)
  }
}
```

### 7. Test Coverage

Created `test/intent/quality_analyzer_test.gleam` with tests for:
- Empty spec error handling
- Spec with features but no behaviors
- AI-friendly error formatting (CUE structure)
- Human-readable error formatting
- Error message content validation

### 8. Example Files

Added `examples/empty-spec.cue` for testing error handling with invalid specs.

## Benefits

### For AI Agents
- **Self-Recovery**: Parse recovery steps and attempt automatic fixes
- **Deterministic**: Same error always produces same structured output
- **Actionable**: Every error includes concrete next steps
- **Machine-Readable**: CUE format is parseable for automated workflows

### For Human Users
- **Clear Guidance**: Specific recovery steps for each error type
- **Context Awareness**: Understand why the error occurred
- **Actionable Feedback**: Commands to run to fix the issue
- **Consistent Format**: Same error structure across all modules

## Error Propagation

The error handling follows Gleam's Railway-Oriented Programming pattern:

1. Spec loads → `Result(Spec, LoadError)`
2. Quality analysis → `Result(QualityReport, QualityAnalyzerError)`
3. CLI displays appropriate error message

Each stage can fail independently with specific, actionable errors.

## Files Modified

- `src/intent/quality_analyzer.gleam` - Core error handling implementation
- `src/intent.gleam` - CLI command updates for analyze and improve
- `test/intent/quality_analyzer_test.gleam` - Test coverage (new file)
- `examples/empty-spec.cue` - Test fixture (new file)

## Consistency with Existing Patterns

This implementation follows the same patterns established in:
- `intent/answer_loader.gleam` - AI-friendly error formatting
- `intent/loader.gleam` - Result-based error handling
- `intent/http_client.gleam` - Execution error types

## Future Enhancements

Potential additions:
- More granular validation errors (missing description, invalid version, etc.)
- JSON output format for `--json` flag support
- Error recovery suggestions based on spec profile (API vs CLI vs Event)
- Integration with KIRK analysis for deeper validation

## Testing

Test with empty spec:
```bash
intent analyze examples/empty-spec.cue
```

Expected output:
```
Error: Cannot analyze empty specification

Context:
  reason: Spec has no features or behaviors. Cannot perform quality analysis on empty specification.
  analysis_stage: quality_analysis

Suggestion: Add features and behaviors to the specification

Recovery Steps:
  1. Add at least one feature to the spec
  2. Ensure each feature has at least one behavior
  3. Run 'intent validate <spec.cue>' to verify structure
  4. Use 'intent interview' to generate complete specification
```

## Implementation Notes

- All error formatters include `escape_json_string()` helper to properly escape special characters in CUE/JSON strings
- Error messages are specific and include the field/location information where applicable
- Recovery steps are ordered from most specific to most general
- Validation happens before expensive analysis operations (fail fast)

## Adherence to Gleam Principles

1. **Explicit Error Handling**: No panics, all failures return Result
2. **Type-Safe**: Compiler enforces error handling at call sites
3. **Immutable**: All error data is immutable
4. **Pattern Matching**: CLI uses case to handle Result types
5. **Pipeline Flow**: Error formatters use string concatenation pipeline
6. **No Exceptions**: Pure functional error handling with Result type

---

**Implemented**: 2026-01-17
**Framework**: R→P→A→D→C (Research → Plan → Act → Do → Check)
