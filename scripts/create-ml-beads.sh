#!/bin/bash
# create-ml-beads.sh
# Generate Mental Lattice beads from CUE schema specification
#
# This script reads schema/mental-lattice-beads.cue and uses bd to create
# all 29 beads in the correct order with proper dependencies.
#
# Usage:
#   ./scripts/create-ml-beads.sh [phase]
#   phase: 0, 1a, 1b, 2, 3, 4, 5, or omit for all
#
# Dog-fooding: This script demonstrates how the Mental Lattice framework
# converts structured CUE specifications into executable work items.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Color codes for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

log_success() {
    echo -e "${GREEN}✅${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}⚠${NC} $1"
}

log_error() {
    echo -e "${RED}❌${NC} $1"
}

# Create a single bead
# Returns the created bead ID
create_bead() {
    local id=$1
    local title=$2
    local what=$3
    local why=$4
    local effort=$5
    local file=$6
    local test_cmd=$7
    local depends_on=$8

    # Build description combining what, effort, and file
    local description="${what}

Effort: $effort
File: $file"

    # Build design notes combining why and test
    local design="${why}

Test: $test_cmd"

    # Convert depends_on string to --deps format
    local deps_args=""
    if [ ! -z "$depends_on" ]; then
        deps_args="--deps $depends_on"
    fi

    # Create the bead using bd and capture the ID
    # Redirect output to a temp file to avoid mixing stdout
    local output
    output=$(bd create \
        --title "$title" \
        --description "$description" \
        --design "$design" \
        $deps_args \
        --type task \
        2>&1)

    # Extract the created bead ID from output
    local created_id=$(echo "$output" | grep -oE "intent-cli-[a-z0-9]+" | head -1)

    if [ -n "$created_id" ]; then
        # Only output the ID, nothing else
        echo "$created_id"
    else
        # Return empty on failure
        echo ""
    fi

    # Small delay to avoid race conditions
    sleep 0.5
}

# Phase 0: Schema Extensions
create_phase_0() {
    log_info "===== PHASE 0: Schema Extensions (5 beads) ====="

    # Create Phase 0 beads with sequential dependencies
    log_info "Creating ML-SCHEMA-1..."
    schema_1=$(create_bead \
        "ML-SCHEMA-1" \
        "[ML-SCHEMA-1] Add #BeadKirkMetadata to schema/beads.cue" \
        "THE SYSTEM SHALL add optional kirk?: #BeadKirkMetadata field to #Bead type" \
        "Enable beads to carry mental model validation metadata without breaking existing beads" \
        "5min" \
        "schema/beads.cue" \
        "cue vet schema/beads.cue" \
        "")
    [ -n "$schema_1" ] && log_success "Created: $schema_1" || log_error "Failed to create ML-SCHEMA-1"

    log_info "Creating ML-SCHEMA-2..."
    schema_2=$(create_bead \
        "ML-SCHEMA-2" \
        "[ML-SCHEMA-2] Add EARS, Contract, Inversion, Quality types" \
        "THE SYSTEM SHALL add #BeadEARS, #BeadContract, #BeadInversion, #BeadQuality types to schema/beads.cue" \
        "Provide structured types for capturing mental model analysis on beads" \
        "10min" \
        "schema/beads.cue" \
        "cue eval -l '#BeadEARS | #BeadContract | #BeadInversion | #BeadQuality' schema/beads.cue" \
        "$schema_1")
    [ -n "$schema_2" ] && log_success "Created: $schema_2" || log_error "Failed to create ML-SCHEMA-2"

    log_info "Creating ML-SCHEMA-3..."
    schema_3=$(create_bead \
        "ML-SCHEMA-3" \
        "[ML-SCHEMA-3] Add RegenerationStrategy and BeadKirkAudit types" \
        "THE SYSTEM SHALL add #RegenerationStrategy and #BeadKirkAudit types" \
        "Track how/when beads were improved when they fail and capture audit trail" \
        "5min" \
        "schema/beads.cue" \
        "cue eval -l '#RegenerationStrategy | #BeadKirkAudit' schema/beads.cue" \
        "$schema_2")
    [ -n "$schema_3" ] && log_success "Created: $schema_3" || log_error "Failed to create ML-SCHEMA-3"

    log_info "Creating ML-SCHEMA-4..."
    schema_4=$(create_bead \
        "ML-SCHEMA-4" \
        "[ML-SCHEMA-4] Add MentalLatticeConfig type with strategic decisions" \
        "THE SYSTEM SHALL add #MentalLatticeConfig type capturing 5 strategic decisions" \
        "Provide single source of truth for how mental lattice tools should behave" \
        "5min" \
        "schema/beads.cue" \
        "cue eval -l '#MentalLatticeConfig' schema/beads.cue" \
        "$schema_3")
    [ -n "$schema_4" ] && log_success "Created: $schema_4" || log_error "Failed to create ML-SCHEMA-4"

    log_info "Creating ML-SCHEMA-5..."
    schema_5=$(create_bead \
        "ML-SCHEMA-5" \
        "[ML-SCHEMA-5] Create schema/mental-lattice-config.cue with Phase 1 analysis" \
        "THE SYSTEM SHALL create schema/mental-lattice-config.cue file with ml_config instance and all 8 Phase 1 bead KIRK metadata" \
        "Document the mental lattice analysis in machine-readable CUE format, ready for tools to consume" \
        "15min" \
        "schema/mental-lattice-config.cue" \
        "cue vet schema/mental-lattice-config.cue schema/beads.cue && cue eval schema/mental-lattice-config.cue | grep -c 'p1_t1'" \
        "$schema_4")
    [ -n "$schema_5" ] && log_success "Created: $schema_5" || log_error "Failed to create ML-SCHEMA-5"

    log_success "Phase 0 beads created"
    # Export the final schema_5 ID for use by other phases
    echo "$schema_5"
}

# Phase 1A: P1 Enhancement
create_phase_1a() {
    log_info "===== PHASE 1A: P1 Bead Enhancement (8 beads) ====="

    # Get the schema_5 ID from Phase 0 (or allow it to be passed in)
    local schema_5_id=${1:-"intent-cli-9uu"}
    log_info "Phase 1A depends on: $schema_5_id"

    # All 8 P1A beads depend on ML-SCHEMA-5
    for i in 1 2 3 4 5 6 7 8; do
        case $i in
            1)
                id="ML-P1A-1"
                title="[ML-P1A-1] Enhance [P1-T1.1] answer_loader.gleam with KIRK"
                what="WHEN bead qnf (answer_loader) is analyzed THEN THE SYSTEM SHALL update it with complete KIRK metadata"
                why="Ensure bead meets 80+ quality threshold and has explicit contracts before execution"
                file=".beads/ (bead qnf)"
                test_cmd="bd show intent-cli-qnf | jq -r '.kirk.quality.overall' | grep -E '^(9[0-9]|100)$'"
                ;;
            2)
                id="ML-P1A-2"
                title="[ML-P1A-2] Enhance [P1-T1.2] ask_single_question with KIRK"
                what="WHEN bead f5y is analyzed THEN THE SYSTEM SHALL update with KIRK metadata documenting dict/prompt interaction"
                why="Ensure bead handles backwards compatibility and edge cases correctly"
                file=".beads/ (bead f5y)"
                test_cmd="bd show intent-cli-f5y | jq -r '.kirk.quality.overall' | grep -E '^(8[0-9]|9[0-9]|100)$'"
                ;;
            3)
                id="ML-P1A-3"
                title="[ML-P1A-3] Enhance [P1-T1.3] intent.gleam CLI with KIRK"
                what="WHEN bead xk8 is analyzed THEN THE SYSTEM SHALL update with KIRK metadata documenting flag interaction"
                why="Ensure CLI flags work correctly and interactions are documented"
                file=".beads/ (bead xk8)"
                test_cmd="bd show intent-cli-xk8 | jq '.kirk.quality.overall' | grep -E '^83$'"
                ;;
            4)
                id="ML-P1A-4"
                title="[ML-P1A-4] Enhance [P1-T1.4] bead_feedback.gleam with KIRK"
                what="WHEN bead 18a is analyzed THEN THE SYSTEM SHALL update with KIRK metadata flagging concurrent write risk"
                why="Ensure bead highlights critical file locking need before execution"
                file=".beads/ (bead 18a)"
                test_cmd="bd show intent-cli-18a | jq '.kirk.quality.overall' | grep -E '^87$'"
                ;;
            5)
                id="ML-P1A-5"
                title="[ML-P1A-5] Enhance [P1-T1.5] bead-status command with KIRK"
                what="WHEN bead bto is analyzed THEN THE SYSTEM SHALL update with KIRK metadata documenting validation needs"
                why="Ensure CLI command validates input and shows clear errors"
                file=".beads/ (bead bto)"
                test_cmd="bd show intent-cli-bto | jq '.kirk.quality.overall' | grep -E '^87$'"
                ;;
            6)
                id="ML-P1A-6"
                title="[ML-P1A-6] Enhance [P1-T1.6] beads-regenerate with KIRK"
                what="WHEN bead ozf is analyzed THEN THE SYSTEM SHALL update with KIRK metadata flagging low quality score"
                why="Highlight that this bead is below 80 quality threshold and needs gap fixes first"
                file=".beads/ (bead ozf)"
                test_cmd="bd show intent-cli-ozf | jq '.kirk.quality.overall' | grep -E '^67$'"
                ;;
            7)
                id="ML-P1A-7"
                title="[ML-P1A-7] Enhance [P1-T1.7] plan_mode.gleam with KIRK"
                what="WHEN bead 8sg is analyzed THEN THE SYSTEM SHALL update with KIRK metadata documenting high quality"
                why="Well-defined algorithmic task - should pass quality gate easily"
                file=".beads/ (bead 8sg)"
                test_cmd="bd show intent-cli-8sg | jq '.kirk.quality.overall' | grep -E '^93$'"
                ;;
            8)
                id="ML-P1A-8"
                title="[ML-P1A-8] Enhance [P1-T1.8] plan command with KIRK"
                what="WHEN bead woq is analyzed THEN THE SYSTEM SHALL update with KIRK metadata documenting high quality"
                why="Simple wrapper command - should pass quality gate"
                file=".beads/ (bead woq)"
                test_cmd="bd show intent-cli-woq | jq '.kirk.quality.overall' | grep -E '^93$'"
                ;;
        esac

        create_bead \
            "$id" \
            "$title" \
            "$what" \
            "$why" \
            "10min" \
            "$file" \
            "$test_cmd" \
            "$schema_5_id"
    done

    log_success "Phase 1A beads created"
}

# Phase 1B: Critical Gaps
create_phase_1b() {
    log_info "===== PHASE 1B: Critical Gaps (5 beads) ====="

    # Get the schema_5 ID from Phase 0 (or allow it to be passed in)
    local schema_5_id=${1:-"intent-cli-9uu"}
    log_info "Phase 1B depends on: $schema_5_id (CRITICAL GATE)"

    # ML-GAP-1: File locking strategy
    log_info "Creating ML-GAP-1..."
    gap_1=$(create_bead \
        "ML-GAP-1" \
        "[ML-GAP-1] Design file locking strategy for concurrent append" \
        "THE SYSTEM SHALL design and document file locking mechanism for concurrent append operations in feedback file (18a bead)" \
        "Critical: P1-T1.4 (bead_feedback) has race condition - must prevent concurrent append without locking" \
        "15min" \
        "DESIGN.md" \
        "grep -l 'file locking' DESIGN.md && echo 'Strategy documented'" \
        "$schema_5_id")
    [ -n "$gap_1" ] && log_success "Created: $gap_1" || log_error "Failed to create ML-GAP-1"

    # ML-GAP-2: Regeneration strategies
    log_info "Creating ML-GAP-2..."
    gap_2=$(create_bead \
        "ML-GAP-2" \
        "[ML-GAP-2] Define 4 regeneration strategies explicitly" \
        "THE SYSTEM SHALL define 4 explicit regeneration strategies: inversion-driven, second-order-driven, pre-mortem-driven, hybrid" \
        "Critical: P1-T1.6 (beads-regenerate) is vague on 'adjusted approach' - must specify all strategies before execution" \
        "15min" \
        "src/intent/regenerate.gleam" \
        "grep -E 'inversion|second_order|pre_mortem|hybrid' src/intent/regenerate.gleam | wc -l | grep -E '^[4-9]'" \
        "$gap_1")
    [ -n "$gap_2" ] && log_success "Created: $gap_2" || log_error "Failed to create ML-GAP-2"

    # ML-GAP-3: Quality validation gates
    log_info "Creating ML-GAP-3..."
    gap_3=$(create_bead \
        "ML-GAP-3" \
        "[ML-GAP-3] Implement quality validation gates (80+ threshold)" \
        "THE SYSTEM SHALL implement automated validation that enforces 80+ quality threshold on all beads before execution" \
        "High: Framework specifies 80+ quality but no validation enforces it - blocks low-quality beads from running" \
        "25min" \
        "src/intent/validate_quality.gleam" \
        "grep -l 'quality.*80' src/intent/validate_quality.gleam && grep -l 'must_pass' src/intent/validate_quality.gleam" \
        "$gap_2")
    [ -n "$gap_3" ] && log_success "Created: $gap_3" || log_error "Failed to create ML-GAP-3"

    # ML-GAP-4: EARS parser integration
    log_info "Creating ML-GAP-4..."
    gap_4=$(create_bead \
        "ML-GAP-4" \
        "[ML-GAP-4] Integrate EARS parser into bead analysis" \
        "THE SYSTEM SHALL integrate ears_parser to automatically classify bead requirements into EARS patterns (ubiquitous, event-driven, state-driven, optional)" \
        "Medium: Mental Lattice analysis documents EARS patterns but no automation extracts them - limits scale" \
        "20min" \
        "src/intent/ears_parser.gleam" \
        "grep -l 'pattern.*ubiquitous\\|event_driven\\|state_driven\\|optional' src/intent/ears_parser.gleam" \
        "$gap_3")
    [ -n "$gap_4" ] && log_success "Created: $gap_4" || log_error "Failed to create ML-GAP-4"

    # ML-GAP-5: Inversion coverage checker
    log_info "Creating ML-GAP-5..."
    gap_5=$(create_bead \
        "ML-GAP-5" \
        "[ML-GAP-5] Implement inversion coverage checker" \
        "THE SYSTEM SHALL implement tool to identify missing edge cases and inversion risks in bead specifications" \
        "Medium: Inversion analysis identifies risks but no tool prevents incomplete coverage - must automate gap detection" \
        "20min" \
        "src/intent/inversion_checker.gleam" \
        "grep -l 'edge_case\\|risk\\|coverage' src/intent/inversion_checker.gleam && grep -c 'failure' src/intent/inversion_checker.gleam | grep -E '[5-9]|[0-9]{2,}'" \
        "$gap_4")
    [ -n "$gap_5" ] && log_success "Created: $gap_5" || log_error "Failed to create ML-GAP-5"

    log_success "Phase 1B beads created (CRITICAL GATE)"
    log_warn "⚠️  Phase 2 cannot proceed until all Phase 1B beads are completed"
    echo "$gap_5"
}

# Main execution
main() {
    local phase=${1:-"all"}

    log_info "Mental Lattice Framework - Bead Creation"
    log_info "Dog-fooding the framework we just built"
    log_info ""

    case $phase in
        0)
            create_phase_0
            ;;
        1a)
            create_phase_1a "$2"
            ;;
        1b)
            create_phase_1b "$2"
            ;;
        all)
            schema_5=$(create_phase_0)
            create_phase_1a "$schema_5"
            create_phase_1b "$schema_5"
            log_info ""
            log_info "Remaining phases (2, 3, 4, 5) follow similar patterns"
            log_info "See schema/mental-lattice-beads.cue for full specification"
            ;;
        *)
            log_error "Unknown phase: $phase"
            echo "Usage: $0 [phase]"
            echo "Phases: 0, 1a, 1b, 2, 3, 4, 5, all"
            exit 1
            ;;
    esac

    log_success "Bead creation complete"
}

# Run main
main "$@"
