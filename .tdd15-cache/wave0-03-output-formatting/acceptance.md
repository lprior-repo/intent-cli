# Acceptance Criteria for WAVE0-03: Output Formatting

## Test Strategy
Write an integration test that verifies ALL commands with --json flag return the consistent JsonResponse schema.

## Required Test
**File**: `test/intent/json_consistency_test.gleam`

### Test Cases:
1. `all_kirk_commands_support_json_flag_test()`
   - quality --json → JsonResponse with next_actions
   - coverage --json → JsonResponse with next_actions  
   - gaps --json → JsonResponse with next_actions
   - invert --json → JsonResponse with next_actions
   - effects --json → JsonResponse with next_actions

2. `all_json_responses_have_required_fields_test()`
   - Verify success field
   - Verify action field
   - Verify command field
   - Verify data field
   - Verify errors array
   - Verify next_actions array
   - Verify metadata object
   - Verify spec_path field

3. `next_actions_populated_intelligently_test()`
   - quality → suggests gaps, invert
   - coverage → suggests effects, doctor
   - gaps → suggests quality, doctor
   - invert → suggests gaps, effects
   - effects → suggests gaps, coverage

4. `error_responses_are_machine_readable_test()`
   - Invalid spec path → JsonResponse with errors
   - Parse failure → JsonResponse with fix_hint
   - Validation failure → JsonResponse with fix_command

## Success Gate
- gleam test passes all new json_consistency tests
- No command returns raw strings when --json=true
- All next_actions arrays contain at least 1 suggestion
