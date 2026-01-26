# Phase 0: TRIAGE - intent-cli-ct6

## Bead Summary
- **ID**: intent-cli-ct6
- **Title**: AI-13: Add --parallel flag to batch command
- **Description**: Add --parallel flag to the batch command for concurrent processing
- **Dependencies**: AI-06 (intent ai batch command)

## Complexity Assessment: SIMPLE

### Routing Decision
Following **SIMPLE** path: Phases 0 → 4 → 5 → 6 → 14 → 15

### Rationale
1. **Single flag addition**: Adding one boolean flag to existing command
2. **Clear scope**: Enable concurrent processing in batch mode
3. **No architectural changes**: Leverages existing batch infrastructure
4. **Limited file impact**: ~2 files (intent.gleam, test file)
5. **Well-defined success**: Flag exists, concurrent processing works

### Complexity Factors
| Factor | Value | Impact |
|--------|-------|--------|
| Criteria count | 1 | Low |
| File count | ~2 | Low |
| Dependency depth | Low | Low |
| New modules | No | Low |
| Architectural change | No | Low |

## Language: Gleam

### Test Command
```bash
gleam test
```

### Format Command
```bash
gleam format
```

### FP Checks
Gleam 7 Commandments:
1. Immutability
2. No nulls
3. Pipelines
4. Exhaustive matching
5. Labeled args
6. Type safety
7. Formatting

## Success Criteria
1. ✅ --parallel flag added to interview command
2. ✅ When --parallel=true, batch processes answers concurrently
3. ✅ Tests verify concurrent processing
4. ✅ All existing tests pass
5. ✅ gleam format passes

## Implementation Notes
- Current batch mode in `run_interview_batch()` uses `list.fold()` for sequential processing
- Need to switch to concurrent processing when --parallel flag is true
- Gleam has limited built-in concurrency - may need to use Erlang processes
- Consider using `process` module or OTP tasks

## Files to Modify
1. `/home/lewis/src/intent-cli/src/intent.gleam` - Add flag, implement concurrent logic
2. `/home/lewis/src/intent-cli/test/interview_batch_test.gleam` - Add concurrency tests

## Phase Routing
Skipping phases: 1, 2, 3, 7, 8, 9, 10, 11, 12, 13

Proceeding to Phase 4 (RED).
