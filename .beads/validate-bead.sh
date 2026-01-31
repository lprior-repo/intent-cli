#!/usr/bin/env bash
# Validate that a bead conforms to the enhanced template schema
# Usage: validate-bead.sh <bead-id>
#
# Returns:
#   0 - Valid bead
#   1 - Invalid bead (missing sections)
#   2 - Bead not found

set -euo pipefail

BEAD_ID="${1:-}"

if [[ -z "$BEAD_ID" ]]; then
    echo "Usage: validate-bead.sh <bead-id>"
    exit 2
fi

# Get bead description
DESCRIPTION=$(bd show "$BEAD_ID" 2>/dev/null | sed -n '/^DESCRIPTION$/,/^LABELS/p' | head -n -1 | tail -n +2)

if [[ -z "$DESCRIPTION" ]]; then
    echo "ERROR: Bead $BEAD_ID not found"
    exit 2
fi

# Required sections checklist
MISSING_SECTIONS=()

# Section 1: EARS Requirements
if ! echo "$DESCRIPTION" | grep -qi "ears_requirements\|THE SYSTEM SHALL"; then
    MISSING_SECTIONS+=("EARS Requirements (Section 1)")
fi

# Section 2: KIRK Contracts
if ! echo "$DESCRIPTION" | grep -qi "contracts\|preconditions\|postconditions\|invariants"; then
    MISSING_SECTIONS+=("KIRK Contracts (Section 2)")
fi

# Section 3: Inversion Analysis
if ! echo "$DESCRIPTION" | grep -qi "inversions\|failure.*prevention\|THE SYSTEM SHALL NOT"; then
    MISSING_SECTIONS+=("Inversion Analysis (Section 3)")
fi

# Section 4: ATDD Tests
if ! echo "$DESCRIPTION" | grep -qi "acceptance_tests\|happy_paths\|error_paths\|real_input"; then
    MISSING_SECTIONS+=("ATDD Acceptance Tests (Section 4)")
fi

# Section 5: E2E Tests
if ! echo "$DESCRIPTION" | grep -qi "e2e_tests\|pipeline_test\|test_full_"; then
    MISSING_SECTIONS+=("E2E Tests (Section 5)")
fi

# Section 6: Implementation Tasks
if ! echo "$DESCRIPTION" | grep -qi "implementation_tasks\|phase_1.*tests.*first\|phase_2.*implementation"; then
    MISSING_SECTIONS+=("Implementation Tasks (Section 6)")
fi

# Section 7: Failure Modes
if ! echo "$DESCRIPTION" | grep -qi "failure_modes\|symptom\|likely_cause\|where_to_look"; then
    MISSING_SECTIONS+=("Failure Modes (Section 7)")
fi

# Section 8: Completion Checklist
if ! echo "$DESCRIPTION" | grep -qi "completion_checklist\|\[ \].*tests\|\[ \].*code"; then
    MISSING_SECTIONS+=("Completion Checklist (Section 8)")
fi

# Section 9: Context
if ! echo "$DESCRIPTION" | grep -qi "context\|related_files\|similar_implementations"; then
    MISSING_SECTIONS+=("Context (Section 9)")
fi

# Section 10: AI Hints
if ! echo "$DESCRIPTION" | grep -qi "ai_hints\|do:\|do_not:"; then
    MISSING_SECTIONS+=("AI Hints (Section 10)")
fi

# Report results
if [[ ${#MISSING_SECTIONS[@]} -eq 0 ]]; then
    echo "✅ Bead $BEAD_ID is valid - all 10 sections present"
    exit 0
else
    echo "❌ Bead $BEAD_ID is INVALID - missing ${#MISSING_SECTIONS[@]} sections:"
    for section in "${MISSING_SECTIONS[@]}"; do
        echo "   - $section"
    done
    echo ""
    echo "See .beads/BEAD_TEMPLATE.md for required format"
    exit 1
fi
