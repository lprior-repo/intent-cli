# Intent CLI Help Text Consistency Initiative - Ralph Loop Prompt

## Mission
Transform Intent CLI help system from partial (7/24 commands complete) to comprehensive and AI-friendly by ensuring ALL 24 commands have:
- Consistent extended help (WHAT/WHY/WHEN/EXAMPLES/FLAGS/EXIT CODES)
- Detailed subcommand documentation
- Examples for every command
- Proper --help implementation
- AI-friendly formatting and structure

## Research Phase (COMPLETE)
✓ Current state mapped: 24 commands, 7 have extended help, 17 lack it
✓ Gaps identified: missing extended help, inline usage scattered, flag inconsistencies
✓ Patterns documented: extended help structure, flag builders, text constants
✓ Opportunities listed: text helpers, help section module, subcommand organization

**Key Findings**:
- 17/24 commands need extended help implementation
- interview, beads, bead-status need comprehensive documentation
- 6 KIRK commands (quality, invert, coverage, gaps, effects, ears) need extended help
- Inline usage messages in ears, history, quality commands should be migrated to centralized help
- Flag descriptions need examples and constraints documented

---

## Phase 1: Planning Phase (IN PROGRESS)

**Deliverables**:
1. Create detailed implementation plan documenting:
   - Which 17 commands need extended help (group by category)
   - Extended help template for each command
   - Flag documentation improvements needed
   - Order of implementation (dependencies first)
   - Testing strategy for each command

2. File structure plan:
   - Expand cli_text_constants.gleam with all 24 extended help texts
   - Create help_sections module for reusable formatting
   - Document help text categories and templates

3. Validation checklist:
   - 24/24 commands have 1-line descriptions ✓
   - 24/24 commands have extended help (new)
   - 24/24 commands document all flags
   - 24/24 commands include USAGE EXAMPLES section
   - 24/24 commands include EXIT CODES
   - All inline usage messages migrated to centralized system

**Action**: If plan exists, review it. If not, create HELP_TEXT_IMPLEMENTATION_PLAN.md with details above.

---

## Phase 2: Implementation Phase (PENDING)

**Deliverables**:
1. Expand cli_text_constants.gleam:
   - Add extended help for lint, analyze, improve, doctor (quality commands)
   - Add extended help for interview, beads, bead-status, history, diff, sessions (workflow commands)
   - Add extended help for quality, invert, coverage, gaps, effects, ears, parse (KIRK + parsing commands)
   - Ensure all follow WHAT/WHY/WHEN/PREREQUISITES/EXAMPLES/FLAGS/EXIT CODES structure
   - Each command: 200-400 lines of help text

2. Migrate inline usage messages:
   - Find inline io.println("Usage: ...") messages in intent.gleam
   - Move into extended help sections
   - Remove from code

3. Create help_sections module (formatter_utils enhancement):
   - pub fn extended_help_section(title, content) -> String
   - pub fn usage_examples(examples: List(#(String, String))) -> String
   - pub fn flag_details(flags: List(#(String, String))) -> String
   - pub fn see_also(commands: List(#(String, String))) -> String
   - Provides consistent formatting for all help text

4. Enhance cli_flags.gleam:
   - Add examples to all flag descriptions
   - Document constraints (incompatible flags, dependencies)
   - Add validation for interview profile flag
   - Add validation helpers for interview session ID

5. Update intent.gleam to use new help text:
   - Replace command.description() calls to pull from cli_text_constants
   - Ensure extended help accessible via --help flag
   - Update interview_command to use cli_flags.validate_enum for profile

**Action**: Check if cli_text_constants.gleam has been expanded to 24 extended help texts. If not, implement them systematically (4 per commit).

---

## Phase 3: Testing Phase (PENDING)

**Deliverables**:
1. Manual testing:
   - Run each command with --help flag
   - Verify extended help displays correctly
   - Check all sections present (WHAT/WHY/WHEN/EXAMPLES/FLAGS/EXIT CODES)
   - Verify examples are runnable
   - Check flag descriptions include all constraints

2. Automated testing:
   - Create test/help_text_test.gleam with:
     - Test that all 24 commands have help descriptions
     - Test that extended help contains required sections
     - Test that all flags document their defaults
     - Test that examples syntax is valid
   - Create scripts/test-help-*.sh test suite:
     - test-help-invocation.sh: Verify all 24 commands respond to --help
     - test-help-sections.sh: Verify extended help has all sections
     - test-help-examples.sh: Parse and validate examples
     - test-help-flags.sh: Check all flags documented
     - test-help-quality.sh: Scoring for help completeness

3. Output validation:
   - Verify colored output formatting works correctly
   - Verify box drawing characters render properly
   - Test help on different terminal widths
   - Verify no text truncation

**Action**: If test files exist, run them. If not, create test/help_text_test.gleam with basic coverage tests.

---

## Phase 4: Code Review Phase (PENDING)

**Deliverables**:
1. Style consistency review:
   - All extended help follows established WHAT/WHY/WHEN pattern
   - All command descriptions use verb-first, 50-100 character format
   - All KIRK commands have "KIRK:" prefix consistently
   - All flag descriptions include environment variable references where applicable
   - All examples follow consistent formatting

2. Quality analysis:
   - Check for typos and grammar in all help text
   - Verify terminology consistency (spec vs specification, target vs URL, etc.)
   - Ensure technical accuracy in all descriptions
   - Check for completeness (no vague or circular descriptions)

3. Architecture review:
   - Verify cli_text_constants.gleam stays <1500 lines (split if needed)
   - Confirm flag builders follow DRY principle
   - Check that validation helpers are used consistently
   - Verify no duplicated help text across commands

**Action**: Run code review on all modified files. Check for inconsistencies in the previous phases.

---

## Phase 5: Deep Interrogation Phase (PENDING)

**Deliverables**:
1. Adversarial testing:
   - Test every command with invalid flag combinations
   - Test help output with extremely long terminal widths (120+ chars)
   - Test help output with narrow terminal widths (40 chars)
   - Test all commands individually: `intent COMMAND --help`
   - Test subcommand structure (if refactored)
   - Verify exit codes are correct when help is displayed

2. Edge case testing:
   - Commands with zero flags (validate, export, show)
   - Commands with many flags (interview: 8 flags, bead-status: 4 flags)
   - Commands that take positional arguments
   - Commands that use environment variables
   - Commands with platform-specific behavior

3. Cross-command consistency:
   - Verify all error messages use error_handler module
   - Check all outputs respect --json flag
   - Verify all commands have consistent ordering of sections
   - Test that related commands reference each other in "SEE ALSO"

**Action**: Create interrogation test suite and run against all 24 commands systematically.

---

## Phase 6: Validation Phase (PENDING)

**Deliverables**:
1. Completeness validation:
   - ✓ 24/24 commands have 1-line descriptions
   - ✓ 24/24 commands have extended help in cli_text_constants.gleam
   - ✓ 24/24 commands document all flags with examples
   - ✓ 24/24 commands show USAGE EXAMPLES section
   - ✓ 24/24 commands list EXIT CODES
   - ✓ All inline usage messages removed from intent.gleam
   - ✓ All validation helpers used in appropriate commands

2. Consistency validation:
   - All extended help follows WHAT/WHY/WHEN/PREREQUISITES/EXAMPLES/FLAGS/EXIT CODES
   - All command descriptions follow "verb + object + scope" pattern
   - All flag descriptions include practical examples
   - All examples are realistic and runnable
   - All exit codes documented consistently (0=pass, 1=fail, 2=blocked, 3=invalid, 4=error)

3. Quality metrics:
   - CLI help text consistency score (0-100%)
   - Coverage completeness score
   - Flag documentation score
   - Example comprehensiveness score
   - AI-friendliness score

**Action**: Create VALIDATION_REPORT.md documenting all 24 commands against checklist.

---

## Phase 7: LLM Quality Assessment (PENDING)

**Deliverables**:
1. Comprehensive LLM evaluation:
   - AI Model evaluates ALL 24 commands for:
     - Clarity: Are descriptions clear to first-time users?
     - Completeness: Are there enough examples and details?
     - Consistency: Do all commands follow same patterns?
     - AI-Friendliness: Can LLMs parse and understand easily?
     - Accuracy: Is technical information correct?
     - Usability: Are flags and options well-organized?

2. Detailed feedback:
   - Per-command scores (0-100)
   - Category scores (Testing, Quality, Interview, KIRK, etc.)
   - Overall CLI help system score
   - Top 3 strengths
   - Top 3 weaknesses with fixes

3. Final recommendations:
   - Specific improvements needed
   - Priority order for remaining work
   - Patterns to standardize further
   - Documentation additions

**Action**: Use strong LLM (Claude Opus) to evaluate all help text and produce LLM_QUALITY_ASSESSMENT.md.

---

## Phase 8: Iteration & Polish (PENDING)

**Deliverables**:
1. Based on LLM assessment, make improvements:
   - Fix clarity issues identified
   - Add missing examples
   - Improve organization where needed
   - Enhance consistency where found gaps

2. Final validation:
   - Re-run all tests
   - Verify no regressions
   - Confirm all metrics improved

3. Documentation:
   - Final implementation report
   - Before/after metrics
   - Help text best practices guide for future commands
   - Integration checklist for new commands

---

## Success Criteria (Completion Promise)

ALL of the following must be true:

- ✓ 24/24 commands have comprehensive extended help in cli_text_constants.gleam
- ✓ All extended help follows WHAT/WHY/WHEN/PREREQUISITES/EXAMPLES/FLAGS/EXIT CODES structure
- ✓ All 24 commands document their flags with descriptions and examples
- ✓ All inline usage messages migrated to centralized help system
- ✓ No inline io.println("Usage: ...") messages remain in intent.gleam
- ✓ All tests passing (help_text_test.gleam + original test suite)
- ✓ Code review completed with zero critical issues
- ✓ Interrogation testing completed on all 24 commands
- ✓ Validation report shows 100% compliance
- ✓ LLM assessment score ≥85/100
- ✓ All commits pushed to remote
- ✓ Final report generated

<promise>HELP TEXT CONSISTENCY COMPLETE</promise>

---

## Current Phase Status

Review all phases above. If phase is marked COMPLETE ✓, move to next. If marked PENDING, implement that phase.

**Progress**: Research complete. About to enter Planning phase.

## Next Action

1. Check if HELP_TEXT_IMPLEMENTATION_PLAN.md exists
2. If not, create it with detailed 8-phase roadmap
3. If yes, review it and proceed to implementation
4. Create test suite structure
5. Begin expanding cli_text_constants.gleam systematically

When all phases complete and success criteria met, output:
```
<promise>HELP TEXT CONSISTENCY COMPLETE</promise>
```

This will signal Ralph Loop completion.
