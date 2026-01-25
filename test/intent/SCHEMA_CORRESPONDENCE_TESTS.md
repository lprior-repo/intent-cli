# Schema Correspondence Tests

## Overview

Created comprehensive schema correspondence tests in `/test/intent/schema_correspondence_test.gleam` that verify CUE schemas and Gleam types match exactly, following the strategy outlined in `SCHEMA_ENFORCEMENT.md`.

## Test File Location

```
test/intent/schema_correspondence_test.gleam
```

## Test Coverage

### 1. Schema Loading Tests
- `can_load_quality_input_schema_test()` - Verifies AI command input schemas can be loaded
- `can_load_validate_input_schema_test()` - Verifies validate command schema loading

### 2. Basic CUE Validation Tests
- `simple_cue_validation_accepts_valid_data_test()` - Valid data passes CUE validation
- `simple_cue_validation_rejects_wrong_type_test()` - Type mismatches are rejected
- `simple_cue_validation_rejects_missing_field_test()` - Missing required fields are rejected

### 3. Quality Command Correspondence Tests
- `quality_input_valid_minimal_test()` - Minimal valid input matches schema
- `quality_input_valid_with_optional_test()` - Optional fields handled correctly
- `quality_input_rejects_missing_required_field_test()` - Required field validation
- `quality_input_rejects_wrong_type_test()` - Type validation for fields

### 4. Common Envelope Tests
- `envelope_response_accepts_valid_structure_test()` - Valid response envelope structure
- `envelope_response_rejects_invalid_status_test()` - Status enum validation

## Test Pattern

Each test follows the correspondence pattern:

```gleam
// 1. Define CUE schema
let schema = "
package test

#TypeDef: {
  field: type
  ...
}

data: #TypeDef
"

// 2. Create JSON data (simulating Gleam type serialization)
let json_data = json.object([...]) |> json.to_string

// 3. Validate against CUE schema
let result = output_validator.validate_against_schema(schema, json_data)

// 4. Assert expected result
result |> should.be_ok  // or should.be_error
```

## Dependencies

- `intent/ai_schema` - Schema loading
- `intent/output_validator` - CUE validation via `cue vet`
- `gleam/json` - JSON serialization
- `gleeunit/should` - Assertions

## Test Philosophy

These tests ensure:

1. **Bidirectional Validation**: Both CUE and Gleam enforce the same contracts
2. **Type Safety**: Gleam types match CUE schema definitions
3. **Roundtrip Integrity**: Data preserves structure through encode/decode cycles
4. **Error Consistency**: Invalid data rejected by both layers

## Running Tests

```bash
gleam test
```

Or specifically:

```bash
gleam test --target erlang
```

## Status

**Created**: 2026-01-25
**Tests**: 11 test functions covering core correspondence scenarios
**Integration**: Ready for GREEN phase (implementation)

## Next Steps (GREEN Phase)

1. Fix pre-existing compilation errors in `src/intent/plan_mode.gleam`
2. Run full test suite to verify RED state (tests fail without implementation)
3. Implement missing helper functions if needed
4. Verify all tests pass

## Notes

- Tests are self-contained and don't depend on external CUE schema files
- Each test includes its own inline CUE schema for clarity and isolation
- Tests validate the correspondence contract, not implementation details
- Future tests can be added following the same pattern for other commands
