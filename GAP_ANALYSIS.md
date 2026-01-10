# Gap Analysis: What We're Not Doing

Comparing this repo against the AI Coding Manifesto principles.

## The Good News

This repo **already does well**:

| Manifesto Principle | How Intent Does It |
|---------------------|-------------------|
| Contracts, not descriptions | `checks` with explicit `rule` + `why` |
| Pattern examples | `anti_patterns` with `bad_example` + `good_example` |
| Explicit constraints | `rules` with `when` conditions |
| Dependency visibility | `requires` field in behaviors |

**The interview questions (questions.cue) are excellent** - context, examples, clear extraction targets.

---

## The Gaps

### 1. CLAUDE.md Violates Its Own Principles

**Problem**: CLAUDE.md tells AI *about* the project but doesn't *show* how to work with it.

```markdown
# Current (descriptions)
- Use Result types for error handling
- Pattern match exhaustively
- Prefer pipelines for data transformation
```

**Should have examples**:
```markdown
# Pattern: Error handling with Result
## From src/intent/parser.gleam:45
pub fn parse_spec(json: String) -> Result(Spec, ParseError) {
  json.decode(json, spec_decoder())
  |> result.map_error(fn(e) { ParseError(e.message) })
}

## Anti-pattern:
pub fn parse_spec(json: String) -> Spec {
  // NEVER: throws, no Result wrapper
  json.decode(json, spec_decoder())
}
```

**The manifesto says**: "Show me one, I'll give you ten."

---

### 2. Interview Questions Exist But Aren't Executable

**Problem**: `questions.cue` has 30+ brilliant questions but:
- No `intent interview` command
- No session persistence
- No spec generation from answers

**Gap**: The interview schema (`interview.cue`) defines `#InterviewSession` but there's no implementation.

```bash
# Should exist but doesn't:
intent interview start --profile api
intent interview answer r1-user-1 "Login with email and password"
intent interview export user-api.cue
```

**The manifesto says**: "Executable feedback beats intuition."

---

### 3. No Feedback Loop From Spec to Validation

**Problem**: You can write a spec, but there's no:
- Spec quality scoring
- Coverage analysis
- "What's missing?" detector

The `intent-self.cue` defines a `quality_score` behavior, but it's not implemented.

**Should exist**:
```bash
intent lint examples/user-api.cue
# Output:
# ⚠️  behavior "create-user" missing error case for rate limiting
# ⚠️  no behavior tests 429 response
# ✓  14/15 anti-patterns avoided
# Score: 87/100
```

---

### 4. Questions Don't Map to Spec Fields

**Problem**: Questions have `extract_into` fields, but the extraction isn't implemented.

```cue
question: "What's the MOST important thing this MUST do correctly?"
extract_into: "critical_constraint,anti_patterns"
```

But there's no code that:
1. Parses the answer
2. Generates an `anti_patterns` entry
3. Validates completeness

**Gap**: The data flow is designed but not built.

---

### 5. AGENTS.md Doesn't Follow Its Own Advice

**Problem**: AGENTS.md says "use bd for ALL task tracking" but:
- No actual beads for the interview system
- No beads for the missing features

```bash
# Should exist:
bd-42: "Implement intent interview command"
bd-43: "Add spec quality scoring"
bd-44: "Connect interview extraction to spec generation"
```

**Gap**: Tracking system exists but isn't populated.

---

### 6. Missing "What Success Looks Like" for the CLI

**Problem**: `intent-self.cue` has behaviors but no executable tests.

The spec says:
```cue
name: "execute_behaviors"
intent: "Run all behaviors from a valid spec against target API"
```

But there's no:
```bash
intent self-test  # Run intent against its own spec
```

**The manifesto says**: "I can verify contracts. I can only guess at descriptions."

---

### 7. No Error Message Examples

**Problem**: The CLI defines exit codes but no example error outputs.

What an AI needs:
```markdown
## Example Error Outputs

### Invalid CUE syntax (exit 3)
$ intent check broken.cue
Error: Invalid CUE at line 42
  expected '}' but found ']'

### Network failure (exit 4)
$ intent check spec.cue --target http://unreachable
Error: Connection refused
  target: http://unreachable
  timeout: 5000ms
```

**Gap**: AI can't match error output patterns without examples.

---

## Priority Fixes

### High Priority (Unblocks Everything Else)

1. **Add examples to CLAUDE.md** - Show code patterns from actual files
2. **Create beads for missing features** - Use your own tracking system
3. **Implement `intent interview`** - Make the question system executable

### Medium Priority (Improves AI Experience)

4. **Add spec linting** - Quality feedback without running against target
5. **Show error output examples** - What does each exit code look like?
6. **Map questions → spec fields** - Extraction pipeline

### Low Priority (Polish)

7. **Self-test command** - Dogfooding builds confidence
8. **Interview → Spec generation** - Automate the extraction

---

## The Meta-Insight

This repo is a case study in **good design, incomplete execution**:

- ✅ Designed the interview system (schema, questions, session model)
- ❌ Never built the CLI command
- ✅ Designed spec quality checks
- ❌ Never implemented the linter
- ✅ Wrote AGENTS.md about using bd
- ❌ Didn't populate beads for the work

**The manifesto warning applies**: "Factory gave 10/10 green but tested the wrong code."

This repo has the architecture for AI-friendly development but isn't using it for its own development.

---

## Recommended Next Steps

```bash
# 1. Create the beads
bd create "Implement intent interview command" -t feature -p 1
bd create "Add code examples to CLAUDE.md" -t task -p 1
bd create "Implement spec quality linting" -t feature -p 2
bd create "Map interview answers to spec extraction" -t feature -p 2

# 2. Show the dependency graph
bd list --json | jq '.[] | {id, title, deps}'

# 3. Pick one and build it
bd ready --json
```

The happy path is to **use Intent's principles to build Intent**.
