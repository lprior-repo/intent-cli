# Schema/Parser Mismatch Analysis Report

**Date**: 2025-02-09  
**Project**: intent-cli  
**Issue**: Critical schema/parser mismatch preventing spec parsing

## Executive Summary

The Intent CLI project has migrated from **v2.0 (HTTP-based)** to **v3.0 (declarative)** schema, but:
- **Schema** (`schema/intent.cue`) uses v3.0 declarative format
- **Parser** (`src/intent/parser.gleam`) and **Types** (`src/intent/types.gleam`) still use v2.0 HTTP format
- **8 of 9 example files** still use v2.0 HTTP format and fail CUE validation

**Impact**: CRITICAL - Most example specs cannot be exported or parsed.

---

## Current State Analysis

### Schema (v3.0 - Declarative)

**File**: `schema/intent.cue`

**Structure**:
```cue
#Spec: {
  name!, description!, audience!, version!,
  success_criteria!, 
  features!: [...#Feature],
  invariants!: [...#Invariant],      // NEW: v3.0
  anti_patterns!: [...#AntiPattern],
  ai_hints!: #AIHints
}

#Behavior: {
  name!, intent!,
  notes?, requires?, tags?,
  preconditions?: [...string],       // NEW: v3.0
  postconditions?: [...string],      // NEW: v3.0
  verifications?: [...#Verification] // NEW: v3.0
  // NO: request, response, captures
}

#Invariant: {                           // NEW: v3.0
  name!, description!,
  criteria!: [...string]
}
```

**Key Changes from v2.0**:
- `config` field removed
- `rules` field replaced with `invariants`
- Behavior `request` field removed
- Behavior `response` field removed
- Behavior `captures` field removed
- Added `preconditions`, `postconditions`, `verifications`
- Added `#Invariant` type

---

### Parser & Types (v2.0 - HTTP-based)

**Files**: 
- `src/intent/parser.gleam` (lines 37, 135-137, 253)
- `src/intent/types.gleam` (lines 7-19, 33-43, 85-102, 111-119)

**Expected Structure**:
```gleam
pub type Spec {
  Spec(
    name, description, audience, version,
    success_criteria,
    config: Config,              // REMOVED in v3.0
    features,
    rules: List(Rule),           // REMOVED in v3.0
    anti_patterns,
    ai_hints
  )
}

pub type Behavior {
  Behavior(
    name, intent, notes, requires, tags,
    request: Request,            // REMOVED in v3.0
    response: Response,          // REMOVED in v3.0
    captures: Dict(String, String) // REMOVED in v3.0
  )
}

pub type Config { ... }           // REMOVED in v3.0
pub type Request { ... }          // REMOVED in v3.0
pub type Response { ... }         // REMOVED in v3.0
pub type Rule { ... }             // REMOVED in v3.0 (replaced by Invariant)
```

**Parser Expects** (lines 37, 135-137, 253):
```gleam
use config <- result.try(dynamic.field("config", parse_config)(data))
use request <- result.try(dynamic.field("request", parse_request)(data))
use response <- result.try(dynamic.field("response", parse_response)(data))
use captures <- result.try(dynamic.field("captures", parse_string_dict)(data))
use rules <- result.try(dynamic.field("rules", dynamic.list(parse_rule))(data))
```

---

## Evidence: Actual Test Results

### Test 1: Declarative Spec Exports Successfully

```bash
$ cue export examples/declarative-spec.cue
✓ SUCCESS
```

**Output structure**:
```json
{
  "spec": {
    "name": "Declarative Specification Example",
    "features": [...],
    "invariants": [...],        // Present
    "ai_hints": {...}
  }
}
```

**Behavior structure**:
```json
{
  "name": "successful-login",
  "intent": "User can log in...",
  "preconditions": [...],       // Present
  "postconditions": [...],      // Present
  "verifications": [...]        // Present
}
```

### Test 2: HTTP Specs Fail Schema Validation

```bash
$ cue export examples/user-api.cue
❌ FAILS with errors:
```

**Errors**:
```
spec.config: field not allowed:                      ./examples/user-api.cue:25:2
spec.features.0.behaviors.0.captures: field not allowed:  ./examples/user-api.cue:85:6
spec.features.0.behaviors.0.request: field not allowed:    ./examples/user-api.cue:45:6
spec.features.0.behaviors.0.response: field not allowed:   ./examples/user-api.cue:55:6
...
spec.rules: field not allowed:                       ./examples/user-api.cue:364:2
```

---

## Example Files Status

| File | Format | Status |
|------|--------|--------|
| `array-validation.cue` | HTTP (v2.0) | FAILS schema validation |
| `conflicts-gaps.cue` | HTTP (v2.0) | FAILS schema validation |
| `declarative-spec.cue` | Declarative (v3.0) | ✓ Passes |
| `interview-workflow.cue` | HTTP (v2.0) | FAILS schema validation |
| `meal-planner-api.cue` | HTTP (v2.0) | FAILS schema validation |
| `nested-paths.cue` | HTTP (v2.0) | FAILS schema validation |
| `pokemon-api.cue` | HTTP (v2.0) | FAILS schema validation |
| `regex-rules.cue` | HTTP (v2.0) | FAILS schema validation |
| `user-api.cue` | HTTP (v2.0) | FAILS schema validation |

**Result**: 8 of 9 example specs are broken.

---

## Code Impact Analysis

### Files Using v2.0 Types (Must Change)

**Direct imports of old types**:
1. `src/intent/parser.gleam` - Parser expects v2.0 fields
2. `src/intent/types.gleam` - Defines v2.0 types
3. `src/intent/validator.gleam` - Validates `behavior.response.checks`
4. `src/intent/spec_validator.gleam` - Uses Spec type
5. `src/intent/spec_linter.gleam` - Uses Behavior, Spec types
6. `src/intent/runner.gleam` - Executes HTTP requests (Config, Request)
7. `src/intent/rules_engine.gleam` - Processes Rule types
8. `src/intent/quality_analyzer.gleam` - Analyzes Rule, Behavior
9. `src/intent/checker.gleam` - Checks Response against Check
10. `src/intent/ready_document.gleam` - Uses Rule, Behavior
11. `src/intent/vision_document.gleam` - Uses Behavior, Feature
12. `src/intent/output.gleam` - Uses Behavior types
13. `src/intent/kirk/quality_analyzer.gleam` - Uses Check, Behavior
14. `src/intent/kirk/inversion_checker.gleam` - Uses Method enum
15. `src/intent/effects_analyzer.gleam` - Uses Method enum
16. `src/intent/spec_builder.gleam` - Builds with Config
17. `src/intent/loader.gleam` - Loads Spec
18. `src/intent/http_client.gleam` - HTTP client (Config, Request)
19. `src/intent/config.gleam` - Config type
20. `src/intent/rule.gleam` - Rule expression parsing

**Total**: 20+ files reference v2.0 types

### Dependencies Chain

```
parser.gleam (v2.0)
  → types.gleam (v2.0)
    → validator.gleam
      → runner.gleam
        → http_client.gleam
        → checker.gleam
          → rules_engine.gleam
```

---

## Mismatch Points (Exact File:Line References)

### 1. Spec Field Mismatch

**Schema** (`schema/intent.cue:6-20`):
- Has: `invariants`
- Missing: `config`, `rules`

**Parser** (`src/intent/parser.gleam:21-61`):
- Expects: `config` (line 37)
- Expects: `rules` (line 42)
- Missing: `invariants`

### 2. Behavior Field Mismatch

**Schema** (`schema/intent.cue:30-51`):
- Has: `preconditions`, `postconditions`, `verifications`
- Missing: `request`, `response`, `captures`

**Parser** (`src/intent/parser.gleam:124-148`):
- Expects: `request` (line 135)
- Expects: `response` (line 136)
- Expects: `captures` (line 137)
- Missing: `preconditions`, `postconditions`, `verifications`

### 3. Type Definition Mismatch

**Types** (`src/intent/types.gleam`):
- `Config` (line 23) - Not in v3.0 schema
- `Request` (line 85) - Not in v3.0 schema
- `Response` (line 96) - Not in v3.0 schema
- `Rule` (line 111) - Replaced by Invariant in v3.0
- `Check` (line 106) - Not in v3.0 schema

**Missing Types**:
- `Invariant` - v3.0 requirement
- `Verification` - v3.0 requirement

---

## Decision: Parser Update vs Schema Revert

### Recommendation: UPDATE PARSER TO v3.0

**Rationale**:

1. **Schema is authoritative** - CUE schema defines the contract
2. **Declarative is the future** - v3.0 is more flexible and domain-agnostic
3. **Single example works** - `declarative-spec.cue` proves v3.0 is viable
4. **Breaking change acknowledged** - Migration guide needed anyway

### Alternative: Revert Schema to v2.0

**Pros**:
- Preserves 8 existing example files
- Less code changes (20+ files already work)
- HTTP testing is a valid use case

**Cons**:
- Loses declarative flexibility
- Abandons v3.0 design goals
- Schema regression

**NOT RECOMMENDED** - This would undo the migration work.

---

## Implementation Approach

### Phase 1: Type System Migration

**File**: `src/intent/types.gleam`

**Actions**:
1. Add `Invariant` type
2. Add `Verification` type
3. Update `Behavior` type:
   - Remove: `request`, `response`, `captures`
   - Add: `preconditions`, `postconditions`, `verifications`
4. Update `Spec` type:
   - Remove: `config`, `rules`
   - Add: `invariants`
5. Mark `Config`, `Request`, `Response`, `Rule`, `Check` as deprecated

### Phase 2: Parser Migration

**File**: `src/intent/parser.gleam`

**Actions**:
1. Remove parsing for `config` (line 37)
2. Remove parsing for `rules` (line 42)
3. Add parsing for `invariants`
4. Update `parse_behavior` (lines 124-148):
   - Remove: `request`, `response`, `captures`
   - Add: `preconditions`, `postconditions`, `verifications`
5. Remove obsolete parsers:
   - `parse_config` (lines 63-80)
   - `parse_request` (lines 168-175)
   - `parse_response` (lines 231-240)
   - `parse_rule` (lines 253-263)

### Phase 3: Dependent Code Updates

**Files to update** (20+ files):

**HTTP Execution Layer** (Mark as deprecated):
- `src/intent/runner.gleam` - HTTP execution
- `src/intent/http_client.gleam` - HTTP client
- `src/intent/checker.gleam` - Response validation
- `src/intent/rules_engine.gleam` - Rule processing

**Validation Layer** (Update for v3.0):
- `src/intent/validator.gleam` - Remove HTTP validation, add invariant checks
- `src/intent/spec_validator.gleam` - Update structure validation
- `src/intent/semantic_validator.gleam` - Check invariants

**Analysis Layer** (Update for v3.0):
- `src/intent/quality_analyzer.gleam` - Remove rule analysis
- `src/intent/effects_analyzer.gleam` - Remove method enum usage
- `src/intent/kirk/quality_analyzer.gleam` - Update checks
- `src/intent/kirk/inversion_checker.gleam` - Remove method dependency

**Document Generation** (Update for v3.0):
- `src/intent/vision_document.gleam` - Use new Behavior structure
- `src/intent/ready_document.gleam` - Use invariants instead of rules
- `src/intent/output.gleam` - Update reporting

### Phase 4: Example Migration

**Files to update** (8 examples):
1. `examples/array-validation.cue`
2. `examples/conflicts-gaps.cue`
3. `examples/interview-workflow.cue`
4. `examples/meal-planner-api.cue`
5. `examples/nested-paths.cue`
6. `examples/pokemon-api.cue`
7. `examples/regex-rules.cue`
8. `examples/user-api.cue`

**Migration Strategy**:
- Keep HTTP examples in v2.0 format temporarily
- Create v3.0 declarative equivalents
- Update migration guide

---

## Affected Files Summary

### Must Change (Critical):
1. `src/intent/types.gleam` - Type definitions
2. `src/intent/parser.gleam` - Parsing logic
3. `src/intent/validator.gleam` - Validation
4. `src/intent/spec_validator.gleam` - Schema validation
5. `examples/*.cue` (8 files) - Example specs

### Should Update (High Priority):
6. `src/intent/quality_analyzer.gleam`
7. `src/intent/effects_analyzer.gleam`
8. `src/intent/spec_linter.gleam`
9. `src/intent/vision_document.gleam`
10. `src/intent/ready_document.gleam`

### Deprecate (HTTP-specific):
11. `src/intent/runner.gleam`
12. `src/intent/http_client.gleam`
13. `src/intent/checker.gleam`
14. `src/intent/rules_engine.gleam`
15. `src/intent/config.gleam`

### May Update (Nice to have):
16-20. Various kirk/ and output/ files

**Total**: 20+ files need changes

---

## Estimated Complexity

| Phase | Complexity | Time Estimate | Risk |
|-------|-----------|---------------|------|
| Type System Migration | MEDIUM | 2-4 hours | Low |
| Parser Migration | MEDIUM | 2-4 hours | Low |
| Dependent Code Updates | HIGH | 8-16 hours | Medium |
| Example Migration | MEDIUM | 4-8 hours | Low |
| Testing | HIGH | 4-8 hours | Medium |
| **TOTAL** | **HIGH** | **20-40 hours** | **Medium** |

---

## Risk Assessment

### High Risk Areas:
1. **Breaking changes** - Existing HTTP-based workflows will break
2. **Test suite** - 758 tests may fail (need to verify)
3. **Dependent tools** - Any external tools using HTTP specs

### Mitigation Strategies:
1. **Version support** - Maintain backward compatibility for v2.0 specs
2. **Migration guide** - Document v2.0 → v3.0 migration
3. **Feature flag** - Allow choosing schema version
4. **Comprehensive testing** - Update all tests

---

## Recommended Next Steps

1. **IMMEDIATE**: Run full test suite to assess damage
   ```bash
   gleam test
   ```
   Expected: Many failures due to missing types

2. **Phase 1**: Update type definitions
   - Add `Invariant` and `Verification` types
   - Mark old types as deprecated

3. **Phase 2**: Update parser
   - Parse v3.0 fields
   - Maintain backward compatibility

4. **Phase 3**: Update validation
   - Remove HTTP-specific validation
   - Add invariant validation

5. **Phase 4**: Update examples incrementally
   - Start with simple examples
   - Document migration patterns

6. **Documentation**: Update all docs
   - Migration guide
   - Examples
   - Schema reference

---

## Conclusion

The schema/parser mismatch is **CRITICAL** and requires **immediate attention**. The recommended approach is to **update the parser and types to match the v3.0 declarative schema**, while maintaining backward compatibility for v2.0 HTTP specs if possible.

**Key Decision Point**: Should v2.0 HTTP specs continue to be supported?
- **Yes**: Maintain dual-mode parser (complexity +50%)
- **No**: Breaking change, migrate all examples (recommended for v3.0)

**Severity**: CRITICAL  
**Priority**: IMMEDIATE  
**Complexity**: HIGH  
**Risk**: MEDIUM

---

**Report Generated**: 2025-02-09  
**Generated By**: QA Enforcer (Ruthless Testing)  
**Evidence**: Actual command execution and file inspection
