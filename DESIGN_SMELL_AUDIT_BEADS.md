# Design Audit: Code Smell Conversion to Executable Beads

**Audit Score**: 90/100 (Design Audit Code Smells)
**Date**: 2026-01-18
**Source**: Design Audit Findings → 5 Executable Beads
**Total Effort**: 90 minutes
**Total Priority Sum**: 5 (3 Critical P0 + 1 Medium P2 + 1 Low P3)

---

## Bead Summary

All 5 code smells have been converted into executable work items in `.beads/design-smell-beads.jsonl`:

### Critical Priority (P0 - 40 minutes total)

| ID | Title | File | Lines | Effort | Status |
|---|---|---|---|---|---|
| `intent-cli-design-smell-1` | Fix center_in_box negative padding code smell | `src/intent/formatter_utils.gleam` | 39-49 | 15min | open |
| `intent-cli-design-smell-2` | Fix progress_bar_with_width unvalidated parameter code smell | `src/intent/formatter_utils.gleam` | 62-71 | 15min | open |
| `intent-cli-design-smell-3` | Fix config merge always-override logic bug | `src/intent/config.gleam` | 65 | 10min | open |

### Medium Priority (P2 - 20 minutes total)

| ID | Title | File | Lines | Effort | Status |
|---|---|---|---|---|---|
| `intent-cli-design-smell-4` | Consolidate indent functions to eliminate duplication | `src/intent/formatter_utils.gleam` | 99-126 | 20min | open |

### Low Priority (P3 - 30 minutes total)

| ID | Title | File | Lines | Effort | Status |
|---|---|---|---|---|---|
| `intent-cli-design-smell-5` | Simplify float_to_string_1dp arithmetic | `src/intent/formatter_utils.gleam` | 226-235 | 30min | open |

---

## Bead Details

### Bead 1: center_in_box Negative Padding (CRITICAL)

**ID**: `intent-cli-design-smell-1`
**File**: `/home/lewis/src/intent-cli/src/intent/formatter_utils.gleam` (lines 39-49)
**Priority**: 0 (Critical Bug)
**Effort**: 15 minutes

**Code Smell Type**: Unhandled Negative Integer
**Severity**: CRITICAL - Silent Failure
**Root Cause**: Missing bounds checking on calculated padding value

**Problem**:
```gleam
fn center_in_box(text: String, width: Int) -> String {
  let text_len = string.length(text)
  let padding = width - text_len  // Can be negative!
  let left_pad = padding / 2
  // ... string.repeat(" ", left_pad) with negative value → silent failure
}
```

**Solution**:
```gleam
fn center_in_box(text: String, width: Int) -> String {
  let text_len = string.length(text)
  let actual_width = case text_len > width {
    True -> text_len   // Expand box if text too long
    False -> width
  }
  let padding = actual_width - text_len  // Always >= 0
  // ... proceed safely
}
```

**Tags**: `code-smell`, `bug-fix`, `critical`, `defensive-programming`, `formatter-utils`, `unhandled-negative-value`

---

### Bead 2: progress_bar_with_width Unvalidated Parameter (CRITICAL)

**ID**: `intent-cli-design-smell-2`
**File**: `/home/lewis/src/intent-cli/src/intent/formatter_utils.gleam` (lines 62-71)
**Priority**: 0 (Critical DOS Vector)
**Effort**: 15 minutes

**Code Smell Type**: Unvalidated Input Parameter
**Severity**: CRITICAL - Resource Exhaustion
**Root Cause**: No bounds checking on user-provided width parameter

**Problem**:
```gleam
pub fn progress_bar_with_width(percentage: Float, width: Int) -> String {
  let filled_count = float.round(clamped /. 100.0 *. int.to_float(width))
  // SMELL: width never validated - could be 1_000_000
  // string.repeat allocates huge string → DOS attack vector
  string.repeat(emoji.block_filled, filled_count)
}
```

**Solution**:
```gleam
pub fn progress_bar_with_width(percentage: Float, width: Int) -> String {
  let validated_width = int.max(1, int.min(width, 100))  // Clamp to 1-100
  let filled_count = float.round(clamped /. 100.0 *. int.to_float(validated_width))
  string.repeat(emoji.block_filled, filled_count)
}
```

**Tags**: `code-smell`, `bug-fix`, `critical`, `input-validation`, `formatter-utils`, `resource-exhaustion`

---

### Bead 3: config.gleam merge_with_flags Data Loss (CRITICAL)

**ID**: `intent-cli-design-smell-3`
**File**: `/home/lewis/src/intent-cli/src/intent/config.gleam` (line 65)
**Priority**: 0 (Critical Data Loss)
**Effort**: 10 minutes

**Code Smell Type**: Incomplete Merge Logic
**Severity**: CRITICAL - Silent Data Loss
**Root Cause**: timeout_ms field always uses override without checking if it's default

**Problem**:
```gleam
pub fn merge_with_flags(base: Config, overrides: Config) -> Config {
  Config(
    // ... other fields check for empty/default ...
    timeout_ms: overrides.timeout_ms,  // SMELL: Always uses override!
  )
}
```

**Solution**:
```gleam
pub fn merge_with_flags(base: Config, overrides: Config) -> Config {
  Config(
    // ... other fields ...
    timeout_ms: case overrides.timeout_ms == 30_000 {
      True -> base.timeout_ms      // If default, use base value
      False -> overrides.timeout_ms // If explicit, use override
    },
  )
}
```

**Tags**: `code-smell`, `bug-fix`, `critical`, `logic-error`, `config`, `data-loss`

---

### Bead 4: Consolidate Indent Functions (MEDIUM - DRY Violation)

**ID**: `intent-cli-design-smell-4`
**File**: `/home/lewis/src/intent-cli/src/intent/formatter_utils.gleam` (lines 99-126)
**Priority**: 2 (Code Smell, Maintenance)
**Effort**: 20 minutes

**Code Smell Type**: Duplicate Code
**Severity**: MEDIUM - Maintenance Burden
**Root Cause**: indent_0 through indent_4 duplicate indent_n logic

**Problem**:
```gleam
pub fn indent_0() -> String { "" }
pub fn indent_1() -> String { "  " }
pub fn indent_2() -> String { "    " }
pub fn indent_3() -> String { "      " }
pub fn indent_4() -> String { "        " }
pub fn indent_n(level: Int) -> String { string.repeat("  ", level) }
// DRY VIOLATION: Single source of truth exists but not used
```

**Solution** (Option A - Recommended):
```gleam
pub fn indent(level: Int) -> String {
  indent_n(level)
}

pub fn indent_n(level: Int) -> String {
  string.repeat("  ", level)
}
// Remove indent_0 through indent_4, refactor callers
```

**Solution** (Option B - Backward Compatible):
```gleam
pub fn indent_0() -> String { indent_n(0) }
pub fn indent_1() -> String { indent_n(1) }
pub fn indent_2() -> String { indent_n(2) }
pub fn indent_3() -> String { indent_n(3) }
pub fn indent_4() -> String { indent_n(4) }
pub fn indent_n(level: Int) -> String { string.repeat("  ", level) }
```

**Tags**: `code-smell`, `refactoring`, `medium`, `duplication`, `formatter-utils`, `dry-violation`

---

### Bead 5: Simplify float_to_string_1dp (LOW - Clarity)

**ID**: `intent-cli-design-smell-5`
**File**: `/home/lewis/src/intent-cli/src/intent/formatter_utils.gleam` (lines 226-235)
**Priority**: 3 (Code Quality)
**Effort**: 30 minutes

**Code Smell Type**: Over-Engineering
**Severity**: LOW - Code Clarity
**Root Cause**: Unnecessarily complex float formatting logic

**Problem**:
```gleam
pub fn float_to_string_1dp(f: Float) -> String {
  let rounded = float.round(f *. 10.0) |> int.to_float  // Unnecessary conversion
  let divided = rounded /. 10.0
  let int_part = float.floor(divided) |> float.round
  let decimal_part = float.round({ divided -. int.to_float(int_part) } *. 10.0)
  int.to_string(int_part) <> "." <> int.to_string(decimal_part)
}
// 9 lines for simple 1-decimal formatting
```

**Solution**:
```gleam
pub fn float_to_string_1dp(f: Float) -> String {
  let rounded = float.round(f *. 10.0) /. 10.0  // Direct division
  let int_part = float.floor(rounded) |> float.round
  let decimal_part = float.round({ rounded -. float.floor(rounded) } *. 10.0)
  int.to_string(int_part) <> "." <> int.to_string(decimal_part)
}
// 5 lines - same logic, clearer intent
```

**Tags**: `code-smell`, `refactoring`, `low`, `clarity`, `formatter-utils`, `over-engineering`

---

## Testing Strategy

### Bead 1 & 2 & 3 (Critical - Functional Correctness)

**Before/After Testing**:
- Test case: Call function with edge case inputs
- Verify no silent failures (Bead 1)
- Verify reasonable resource usage (Bead 2)
- Verify config values preserved (Bead 3)

### Bead 4 (Refactoring - API Compatibility)

**Backward Compatibility Check**:
- Option A: Audit all call sites, refactor to use `indent_n()` directly
- Option B: Keep wrapper functions, delegate to `indent_n()`

### Bead 5 (Clarity - Behavioral Equivalence)

**Before/After Equivalence**:
- Property test: `float_to_string_1dp(x)` produces same output before/after
- Edge cases: 0.0, 1.5, 99.9, -1.1, very large/small values

---

## Files to Review

1. **Primary**: `/home/lewis/src/intent-cli/.beads/design-smell-beads.jsonl` - Complete bead definitions
2. **Secondary**: `/home/lewis/src/intent-cli/src/intent/formatter_utils.gleam` - Lines 39-49, 62-71, 99-126, 226-235
3. **Secondary**: `/home/lewis/src/intent-cli/src/intent/config.gleam` - Lines 50-67 (entire merge_with_flags function)

---

## Execution Plan

### Wave 1 (Critical Bugs - 40 minutes)
1. Execute Bead 1: Fix center_in_box negative padding
2. Execute Bead 2: Fix progress_bar_with_width unvalidated parameter
3. Execute Bead 3: Fix config merge always-override logic
4. Run tests: `gleam test` → All should pass

### Wave 2 (Code Quality - 50 minutes)
5. Execute Bead 4: Consolidate indent functions (refactor + audit call sites)
6. Execute Bead 5: Simplify float_to_string_1dp arithmetic
7. Run tests: `gleam test` → All should pass

### Completion
- All beads marked closed in BD
- Build succeeds: `gleam build`
- Tests pass: `gleam test`

---

## Metrics

**Code Smell Density**: 5 issues across 2 files (formatter_utils.gleam, config.gleam)
**Critical Issues**: 3 (silent failures, DOS, data loss)
**Refactoring Issues**: 2 (DRY violation, over-engineering)
**Total Resolution Time**: ~90 minutes
**Build Status**: Currently passing ✓
**Test Status**: Currently passing ✓

---

## Notes

- All beads use standard `.beads/issues.jsonl` format (JSONL per line)
- Each bead includes before/after code examples and clear success criteria
- Critical beads (P0) should be executed first
- All changes are backward compatible or include migration path
- No breaking API changes for critical bugs
