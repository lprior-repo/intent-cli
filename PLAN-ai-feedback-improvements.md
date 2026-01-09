# Plan: AI Feedback Improvements

Based on honest AI feedback about what would actually help when receiving prompts from Intent.

## Summary of Feedback

### Keep (Already Valuable)
- The `why` field on every check
- Anti-patterns with bad/good examples
- Explicit edge cases and error behaviors
- Behavior dependencies (`requires`)
- EARS patterns for unambiguous requirements

### Concerns Raised
1. **Overhead vs simple tasks** - 500 lines of CUE for "add a login endpoint"
2. **Mental Lattices feel like marketing** - Value is in individual techniques, not the branding
3. **Interview assumes humans have answers** - Questions like "What security attacks are possible?" go unanswered
4. **API-centric design** - How well does it generalize?

### Requested Improvements
1. "Light mode" spec format for simple tasks
2. Make mental models optional analysis tools, not mandatory
3. Add codebase context (existing patterns, frameworks)
4. Add entry points ("edit this file, call this function")
5. Add clear boundaries ("don't touch X, Y, Z")

---

## Implementation Plan

### Phase 1: Light Mode Spec Format

**Goal**: Create a minimal spec format that can be used for simple tasks without the full 5x5 matrix overhead.

#### 1.1 Define `#LightSpec` schema in `schema/intent.cue`

```cue
// Minimal spec for simple, well-understood tasks
#LightSpec: {
    name:        string
    description: string

    // What to implement (EARS format encouraged but not required)
    behaviors: [...#LightBehavior]

    // Optional fields - only if relevant
    anti_patterns?: [...#AntiPattern]
    ai_hints?:      #AIHints
}

#LightBehavior: {
    name:   #Identifier
    intent: string  // Plain English purpose with "why"

    // Simplified request/response
    request: {
        method: #Method
        path:   string
        body?:  _
    }
    response: {
        status: int
        checks?: #Checks  // Optional - only add what matters
    }
}
```

**Files to modify**:
- `schema/intent.cue` - Add `#LightSpec` and `#LightBehavior` types
- `src/intent/parser.gleam` - Parse light specs
- `src/intent/loader.gleam` - Detect and load light specs

#### 1.2 Add `--light` flag to interview command

```bash
intent interview --profile api --light
```

Only asks Round 1 questions (5 questions total instead of 25):
- What should it do?
- Who uses it?
- Happy path flow
- Key entities
- Auth method

**Files to modify**:
- `src/intent.gleam` - Add `--light` flag
- `src/intent/interview.gleam` - Support abbreviated interview
- `schema/questions.cue` - Mark which questions are "light mode" eligible

#### 1.3 Auto-detect complexity

If the user's first answer suggests a simple task, offer to switch to light mode:

> "This sounds like a straightforward task. Would you like to use light mode (5 questions) instead of the full interview (25 questions)?"

---

### Phase 2: Make Mental Models Optional

**Goal**: Mental models (inversion, pre-mortem, second-order thinking) should be analysis tools you run when needed, not mandatory parts of every spec.

#### 2.1 Remove mental model fields from default spec output

Currently `#KirkSpec` adds these to every spec:
- `inversions?`
- `pre_mortem?`
- `second_order_effects?`

Change to: Only add these if user explicitly requests analysis.

**Files to modify**:
- `src/intent/spec_builder.gleam` - Don't add KIRK fields by default
- `schema/kirk.cue` - Keep schema, but document as "analysis output"

#### 2.2 Keep KIRK as separate analysis commands

The existing commands are good - make them clearly optional:
```bash
intent analyze <spec.cue>     # Quality analysis
intent invert <spec.cue>      # Inversion analysis (what could fail?)
intent gaps <spec.cue>        # Gap detection
intent coverage <spec.cue>    # Coverage analysis
```

Add documentation that these are optional deep-dive tools.

#### 2.3 Add `--with-analysis` flag for those who want it

```bash
intent interview --profile api --with-analysis
```

Only then does the interview ask inversion/pre-mortem questions.

---

### Phase 3: Add Codebase Context Fields

**Goal**: Help AI understand where and how to implement, not just what to implement.

#### 3.1 Add `#CodebaseContext` to `#AIHints`

```cue
#AIHints: {
    // Existing fields...

    // NEW: Codebase context
    codebase?: #CodebaseContext
}

#CodebaseContext: {
    // What patterns exist in this codebase?
    patterns?: {
        error_handling?:  string  // e.g., "Use Result types, not exceptions"
        auth_middleware?: string  // e.g., "See src/middleware/auth.ts"
        validation?:      string  // e.g., "Use zod schemas in src/schemas/"
        testing?:         string  // e.g., "Jest with RTL, tests in __tests__/"
    }

    // What frameworks/libraries are in use?
    stack?: {
        language?:  string
        framework?: string
        database?:  string
        orm?:       string
        testing?:   string
    }

    // Where should implementation go?
    entry_points?: [...#EntryPoint]

    // What NOT to touch?
    boundaries?: [...#Boundary]
}

#EntryPoint: {
    file:        string  // e.g., "src/routes/users.ts"
    line?:       int     // Optional line number
    function?:   string  // e.g., "registerRoutes"
    description: string  // e.g., "Add new endpoint handlers here"
}

#Boundary: {
    path:   string  // e.g., "src/legacy/**"
    reason: string  // e.g., "Legacy code, don't modify"
}
```

**Files to modify**:
- `schema/intent.cue` - Add `#CodebaseContext`, `#EntryPoint`, `#Boundary`
- `src/intent/parser.gleam` - Parse new fields

#### 3.2 Add codebase context questions to interview

Add optional questions (only asked if user opts in with `--with-context`):

```
"What existing patterns should the AI follow?"
"What files should the AI edit?"
"What files should the AI NOT touch?"
"What testing patterns does the codebase use?"
```

**Files to modify**:
- `schema/questions.cue` - Add codebase context questions
- `src/intent/interview.gleam` - Support `--with-context` flag

#### 3.3 Auto-detect from project files

```bash
intent context-scan
```

Scans project and suggests:
- Detected stack (from package.json, gleam.toml, go.mod, etc.)
- Common patterns (from existing code structure)
- Suggested entry points (from file naming conventions)

**Files to create**:
- `src/intent/context_scanner.gleam` - Project analysis
- Add `context-scan` command to `src/intent.gleam`

---

### Phase 4: Improve Documentation & Honesty

#### 4.1 Update claims in README

- Change "90% one-shot success rate" to "Designed for high first-attempt success" (no unproven percentages)
- Change "50% token reduction" to "Significant token reduction" (actual varies)
- Rename "Mental Lattices" to "Analysis Tools" or "Quality Checks"

#### 4.2 Add honest limitations section

Document what Intent does NOT help with:
- Unclear project structure
- Missing dependencies
- Conflicting existing code
- Build system issues
- Runtime environment problems

---

## File Changes Summary

### Schema Changes (`schema/`)
| File | Changes |
|------|---------|
| `intent.cue` | Add `#LightSpec`, `#LightBehavior`, `#CodebaseContext`, `#EntryPoint`, `#Boundary` |
| `questions.cue` | Mark light-mode questions, add context questions |
| `kirk.cue` | Document as optional analysis (no structural changes) |

### Source Changes (`src/intent/`)
| File | Changes |
|------|---------|
| `intent.gleam` | Add `--light`, `--with-analysis`, `--with-context` flags; add `context-scan` command |
| `interview.gleam` | Support abbreviated interview modes |
| `parser.gleam` | Parse `#LightSpec` and `#CodebaseContext` |
| `loader.gleam` | Detect spec type (full vs light) |
| `spec_builder.gleam` | Don't add KIRK fields by default |
| `context_scanner.gleam` (new) | Project analysis for auto-detecting context |

---

## Bead Breakdown

### Epic: AI Feedback Improvements
Parent bead for all work below.

### Phase 1 Beads (Light Mode)
1. **[LIGHT-1]** Add `#LightSpec` schema to `schema/intent.cue`
2. **[LIGHT-2]** Add `#LightBehavior` schema to `schema/intent.cue`
3. **[LIGHT-3]** Update parser.gleam to parse light specs
4. **[LIGHT-4]** Update loader.gleam to detect light vs full specs
5. **[LIGHT-5]** Add `--light` flag to interview command
6. **[LIGHT-6]** Mark light-mode questions in questions.cue
7. **[LIGHT-7]** Implement abbreviated interview flow

### Phase 2 Beads (Optional Mental Models)
8. **[OPT-1]** Remove auto-generated KIRK fields from spec_builder.gleam
9. **[OPT-2]** Add `--with-analysis` flag to interview command
10. **[OPT-3]** Document KIRK commands as optional deep-dive tools

### Phase 3 Beads (Codebase Context)
11. **[CTX-1]** Add `#CodebaseContext` schema to intent.cue
12. **[CTX-2]** Add `#EntryPoint` schema to intent.cue
13. **[CTX-3]** Add `#Boundary` schema to intent.cue
14. **[CTX-4]** Update parser.gleam to parse codebase context
15. **[CTX-5]** Add `--with-context` flag to interview command
16. **[CTX-6]** Add codebase context questions to questions.cue
17. **[CTX-7]** Create context_scanner.gleam module
18. **[CTX-8]** Add `context-scan` command to CLI

### Phase 4 Beads (Documentation)
19. **[DOC-1]** Update README claims to be evidence-based
20. **[DOC-2]** Add limitations section to documentation
21. **[DOC-3]** Rename "Mental Lattices" to "Analysis Tools" in docs

---

## Dependencies

```
[LIGHT-1] ─┬─> [LIGHT-3] ──> [LIGHT-4]
[LIGHT-2] ─┘
                  │
[LIGHT-5] ──> [LIGHT-6] ──> [LIGHT-7]
                  │
[OPT-1] ──> [OPT-2] ──> [OPT-3]

[CTX-1] ─┬─> [CTX-4] ──> [CTX-5] ──> [CTX-6]
[CTX-2] ─┤
[CTX-3] ─┘
              │
         [CTX-7] ──> [CTX-8]

[DOC-1], [DOC-2], [DOC-3] (independent)
```

---

## Success Criteria

1. **Light mode works**: `intent interview --light --profile api` completes in 5 questions
2. **Mental models optional**: Default spec output has no KIRK fields
3. **Codebase context captured**: Specs can include entry points and boundaries
4. **Context scanning works**: `intent context-scan` detects stack and suggests entry points
5. **Honest documentation**: No unproven percentage claims

---

## Skeptical Review - Issues Found & Fixed

After creating the initial beads, a skeptical review identified these issues:

### Issue 1: #LightSpec is still API-centric
**Problem**: The original feedback criticized API-centric design, but #LightBehavior still uses HTTP method/path/status.
**Fix**: Updated LIGHT-2 description to note this limitation and flag need for CLI/data/UI profiles.

### Issue 2: Missing test beads
**Problem**: 21 task beads created, zero test beads.
**Fix**: Added LIGHT-TEST-1 and LIGHT-TEST-2 for schema and parser tests.

### Issue 3: Context scanner overengineered
**Problem**: Detecting patterns and entry points from code structure is error-prone heuristics.
**Fix**: Simplified CTX-7 to only detect language/framework from manifest files.

### Issue 4: Missing simplified bead output format
**Problem**: Feedback specifically wanted: task, input_example, output_example, must_return, must_not, edge_cases.
**Fix**: Added BEAD-FORMAT bead to implement this.

### Issue 5: Flag proliferation
**Problem**: Three new flags (--light, --with-analysis, --with-context) adds complexity.
**Fix**: Added warning to epic description to review for UX simplification.

### Issue 6: No feedback loop connection
**Problem**: P1 beads implement feedback tracking, but this work doesn't connect to it.
**Fix**: Added FEEDBACK-CONNECT bead to integrate with P1 feedback loop.

### Issue 7: Incomplete dependency graph
**Problem**: LIGHT-5 (--light flag) didn't depend on LIGHT-1/LIGHT-2 (the schema).
**Fix**: Added missing dependencies.

### Issue 8: AI-CUE protocol overlap
**Problem**: AI-CUE epic (h43) also deals with AI-directed output.
**Fix**: Added AI-CUE-COMPAT bead to coordinate.

### Issue 9: Still requires interview
**Problem**: Feedback wanted to skip interview entirely for experienced devs.
**Fix**: Added DIRECT-SPEC bead for generating beads from manual specs.

### Issue 10: No progressive output
**Problem**: Feedback wanted early output, not just at end.
**Fix**: Added PROGRESSIVE-BEADS bead.

### Issue 11: CUE-only output
**Problem**: Feedback wanted YAML, Markdown, direct AI prompt formats.
**Fix**: Added MULTI-OUTPUT bead.

### New Beads Added in Review
- LIGHT-TEST-1: Schema tests
- LIGHT-TEST-2: Parser tests
- BEAD-FORMAT: Simplified bead output
- FEEDBACK-CONNECT: Integration with P1
- AI-CUE-COMPAT: Coordination with AI-CUE protocol
- DIRECT-SPEC: Skip interview for experienced devs
- PROGRESSIVE-BEADS: Show output early
- MULTI-OUTPUT: Multiple output formats
