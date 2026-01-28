# Phase 2: Red Queen + Product Owner Review

You are reviewing intent-cli, an AI-native specification and planning system in Gleam. It has 33 commands covering spec operations, KIRK analysis (quality, coverage, gaps, inversion, effects), interview workflow, bead/planning generation, parsing, Shape phase, and AI schema output.

## First: Load Skills

```
skill({ name: "red-queen" })
skill({ name: "bitter-truth" })
```

Red Queen enforces adversarial evolutionary QA — attack, document, fix, regress. Every generation must defeat all previous generations.

## Step 1: Red Queen Adversarial QA

Attack every command systematically. For each attack category:

### Attack 1: JSON Output Contract
Every command (except help) must produce valid JSON matching the Action JSON Schema.
```bash
gleam build
for cmd in validate quality coverage gaps invert effects lint show doctor improve; do
  echo "Testing: $cmd"
  gleam run -- $cmd examples/user-api.cue 2>/dev/null | python3 -c 'import sys,json; json.load(sys.stdin)' && echo "  PASS" || echo "  FAIL"
done
gleam run -- diff examples/user-api.cue examples/meal-planner-api.cue 2>/dev/null | python3 -c 'import sys,json; json.load(sys.stdin)' && echo "diff: PASS" || echo "diff: FAIL"
```

### Attack 2: Exit Code Consistency
Missing files → exit 3. Unknown commands → exit 4. Success → exit 0.
```bash
for cmd in validate quality show ears parse lint; do
  gleam run -- $cmd nonexistent.file 2>/dev/null; echo "$cmd missing file: $?"
done
gleam run -- ai schema 2>/dev/null; echo "ai schema no-flags: $?"
gleam run -- help 2>/dev/null; echo "help: $?"
gleam run -- nonexistent 2>/dev/null; echo "unknown: $?"
```

### Attack 3: Input Boundary
- Empty input files, malformed CUE, missing required args
- Interview/session commands with invalid session IDs
- Bead generation from nonexistent sessions
- Special characters in spec paths

### Attack 4: Cross-Command Consistency
- Same error type → same exit code across all commands?
- JSON schema consistent across all commands?
- Error messages actionable?

## Step 2: Product Owner Review

From a user perspective:
1. Are error messages clear and actionable?
2. Is JSON output consistent across all 33 commands?
3. Do next_actions suggestions make sense in workflow context?
4. Are the KIRK analysis outputs useful for improving specs?

## Step 3: Generate Beads

For EVERY finding, create a bead with EARS format:
```bash
bd create --title="[Review] <title>" --type=bug --priority=<0-4> --description="WHEN <trigger> THE SYSTEM SHALL <expected> BUT INSTEAD <actual>. Severity: P<0-3>. Reproduction: <command>. Where to look: <file:line>"
```

## Completion Signal

When all attacks are exhausted and beads are created:
COMPLETE
