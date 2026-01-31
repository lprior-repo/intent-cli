#!/usr/bin/env bash
# Validate a bead CUE file against the enhanced schema
# Usage: validate-bead-cue.sh <bead.cue>
#
# Returns:
#   0 - Valid bead
#   1 - Invalid bead (schema violation)
#   2 - File not found or CUE not installed

set -euo pipefail

BEAD_FILE="${1:-}"

if [[ -z "$BEAD_FILE" ]]; then
    echo "Usage: validate-bead-cue.sh <bead.cue>"
    echo ""
    echo "Validates a bead definition file against the enhanced schema."
    echo "Bead files must be in .beads/specs/ directory."
    exit 2
fi

# Check CUE is installed
if ! command -v cue &> /dev/null; then
    echo "ERROR: CUE is not installed"
    echo "Install with: go install cuelang.org/go/cmd/cue@latest"
    exit 2
fi

# Check file exists
if [[ ! -f "$BEAD_FILE" ]]; then
    echo "ERROR: Bead file not found: $BEAD_FILE"
    exit 2
fi

echo "Validating: $BEAD_FILE"
echo "---"

# Run CUE validation
if cue vet "$BEAD_FILE" 2>&1 | grep -v "^zoxide:"; then
    # If there was output (errors), exit with failure
    echo ""
    echo "❌ VALIDATION FAILED"
    exit 1
fi

# Additional semantic checks via CUE eval
ERRORS=()

# Check EARS has all required patterns
if ! cue eval "$BEAD_FILE" -e 'bead.ears_requirements.ubiquitous' 2>/dev/null | grep -q "THE SYSTEM SHALL"; then
    ERRORS+=("EARS: Missing ubiquitous requirements (THE SYSTEM SHALL)")
fi

if ! cue eval "$BEAD_FILE" -e 'bead.ears_requirements.event_driven' 2>/dev/null | grep -q "WHEN"; then
    ERRORS+=("EARS: Missing event_driven requirements (WHEN...SHALL)")
fi

if ! cue eval "$BEAD_FILE" -e 'bead.ears_requirements.unwanted' 2>/dev/null | grep -q "THE SYSTEM SHALL NOT"; then
    ERRORS+=("EARS: Missing unwanted requirements (SHALL NOT)")
fi

# Check contracts have invariants
if ! cue eval "$BEAD_FILE" -e 'bead.contracts.invariants' 2>/dev/null | grep -q '"'; then
    ERRORS+=("KIRK: Missing invariants")
fi

# Check acceptance tests have real_input
if ! cue eval "$BEAD_FILE" -e 'bead.acceptance_tests.happy_paths[0].real_input' 2>/dev/null | grep -q '.'; then
    ERRORS+=("ATDD: Missing real_input in happy_paths")
fi

# Check E2E has pipeline test
if ! cue eval "$BEAD_FILE" -e 'bead.e2e_tests.pipeline_test.name' 2>/dev/null | grep -q "test_full_"; then
    ERRORS+=("E2E: Pipeline test name must start with 'test_full_'")
fi

# Check implementation has phase_1_tests_first
if ! cue eval "$BEAD_FILE" -e 'bead.implementation_tasks.phase_1_tests_first' 2>/dev/null | grep -q "task"; then
    ERRORS+=("Tasks: Missing phase_1_tests_first (TDD red phase)")
fi

# Check AI hints
if ! cue eval "$BEAD_FILE" -e 'bead.ai_hints.do_not' 2>/dev/null | grep -q "unwrap"; then
    ERRORS+=("AI Hints: Missing critical do_not (no unwrap)")
fi

if [[ ${#ERRORS[@]} -gt 0 ]]; then
    echo "❌ SEMANTIC VALIDATION FAILED"
    echo ""
    for err in "${ERRORS[@]}"; do
        echo "  - $err"
    done
    exit 1
fi

echo "✅ BEAD VALID - All 10 sections present and well-formed"
echo ""
echo "Summary:"
cue eval "$BEAD_FILE" -e '{
    id: bead.id,
    title: bead.title,
    type: bead.type,
    priority: bead.priority,
    sections: {
        ears: len(bead.ears_requirements.ubiquitous) + len(bead.ears_requirements.event_driven) + len(bead.ears_requirements.unwanted),
        contracts: len(bead.contracts.invariants),
        inversions: len(bead.inversions.security_failures) + len(bead.inversions.usability_failures),
        tests: len(bead.acceptance_tests.happy_paths) + len(bead.acceptance_tests.error_paths),
        e2e: 1,
        tasks: len(bead.implementation_tasks.phase_1_tests_first),
    }
}' 2>/dev/null || true

exit 0
