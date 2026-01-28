# Phase 2: Red Queen + Product Owner Review

You are reviewing intent-cli, an AI-native specification and planning system in Gleam. It has 33 commands covering spec operations, KIRK analysis (quality, coverage, gaps, inversion, effects), interview workflow, bead/planning generation, parsing, Shape phase, and AI schema output.

## Step 1: Red Queen Review

Adversarial QA against all commands. Test:

### JSON Output Validity
Every command (except help) must produce valid JSON matching the Action JSON Schema.
```bash
gleam build
for cmd in validate quality coverage gaps invert effects lint show doctor improve; do
  echo "Testing: $cmd"
  gleam run -- $cmd examples/user-api.cue 2>/dev/null | python3 -c 'import sys,json; json.load(sys.stdin)' && echo "  PASS" || echo "  FAIL"
done
gleam run -- diff examples/user-api.cue examples/meal-planner-api.cue 2>/dev/null | python3 -c 'import sys,json; json.load(sys.stdin)' && echo "diff: PASS" || echo "diff: FAIL"
```

### Exit Code Consistency
Missing files → exit 3. Unknown commands → exit 4. Success → exit 0.
```bash
for cmd in validate quality show ears parse lint; do
  gleam run -- $cmd nonexistent.file 2>/dev/null; echo "$cmd missing file: $?"
done
gleam run -- ai schema 2>/dev/null; echo "ai schema no-flags: $?"
gleam run -- help 2>/dev/null; echo "help: $?"
gleam run -- nonexistent 2>/dev/null; echo "unknown: $?"
```

### Edge Cases
- Empty input files, malformed CUE, missing required args
- Interview/session commands with invalid session IDs
- Bead generation from nonexistent sessions

## Step 2: Product Owner Review

From a user perspective:
1. Are error messages clear and actionable?
2. Is JSON output consistent across all 33 commands?
3. Do next_actions suggestions make sense in workflow context?
4. Are the KIRK analysis outputs useful for improving specs?

## Step 3: Generate Beads

For every issue found:
```bash
bd create --title="[Review] <title>" --type=bug --priority=<0-4> --description="<EARS requirement + findings + where to look>"
```

## Completion Signal

When all reviews are done and beads are created:
COMPLETE
