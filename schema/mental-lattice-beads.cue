// schema/mental-lattice-beads.cue
// Mental Lattice Framework Implementation - 29 Atomic Beads
//
// Complete specification of all beads needed to implement Mental Lattice
// integration into Intent CLI. Ready for conversion to .beads system.
//
// Structure: 5 phases, 29 beads total
// Dependencies: Follow the order specified, Phase 1B is a critical gate

package intent

// ============================================================================
// PHASE 0: SCHEMA EXTENSIONS (5 beads, 40 minutes)
// Foundation: Extend CUE schema to support KIRK metadata
// ============================================================================

beads_phase_0: {
	"ML-SCHEMA-1": {
		id:    "ML-SCHEMA-1"
		title: "[ML-SCHEMA-1] Add #BeadKirkMetadata to schema/beads.cue"
		what:  "THE SYSTEM SHALL add optional kirk?: #BeadKirkMetadata field to #Bead type"
		why:   "Enable beads to carry mental model validation metadata without breaking existing beads"

		test: {
			command: "cue vet schema/beads.cue"
			expect: {
				exit_code:   0
				stderr_empty: true
			}
		}

		done_when: [
			"kirk field added as optional to #Bead definition",
			"CUE schema compiles without errors",
			"Backwards compatibility preserved (existing beads without kirk field still valid)",
			"grep 'kirk?' schema/beads.cue returns match",
		]

		file:   "schema/beads.cue"
		effort: "5min"
		status: "pending"

		edge_cases: [
			"Beads without kirk field must validate against schema",
			"kirk field is completely optional (no default values)",
			"No impact on existing beads or code",
		]

		requires: []
	}

	"ML-SCHEMA-2": {
		id:    "ML-SCHEMA-2"
		title: "[ML-SCHEMA-2] Add EARS, Contract, Inversion, Quality types"
		what:  "THE SYSTEM SHALL add #BeadEARS, #BeadContract, #BeadInversion, #BeadQuality types to schema/beads.cue"
		why:   "Provide structured types for capturing mental model analysis on beads"

		test: {
			command: "cue eval -l '#BeadEARS | #BeadContract | #BeadInversion | #BeadQuality' schema/beads.cue"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"All 4 types defined with proper constraints",
			"Type relationships and dependencies are clear",
			"CUE schema compiles",
			"Examples show how to use each type",
		]

		file:   "schema/beads.cue"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"Clarity_score must be 0-100 (int)",
			"Risk severity must be one of: low, medium, high, critical",
			"Quality scores must not exceed 100 or be negative",
			"UniqueItems() constraint on preconditions/postconditions",
		]

		requires: ["ML-SCHEMA-1"]
	}

	"ML-SCHEMA-3": {
		id:    "ML-SCHEMA-3"
		title: "[ML-SCHEMA-3] Add RegenerationStrategy and BeadKirkAudit types"
		what:  "THE SYSTEM SHALL add #RegenerationStrategy and #BeadKirkAudit types"
		why:   "Track how/when beads were improved when they fail and capture audit trail"

		test: {
			command: "cue eval -l '#RegenerationStrategy | #BeadKirkAudit' schema/beads.cue"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Both types defined with clear enum values",
			"Audit trail captures analyzer, timestamp, schema version",
			"Strategy indicates how regeneration was done",
			"CUE schema compiles",
		]

		file:   "schema/beads.cue"
		effort: "5min"
		status: "pending"

		edge_cases: [
			"Timestamp must be ISO8601 format",
			"Schema version tracking for future migrations",
			"Strategy enum includes: inversion_driven, second_order_driven, premortem_driven, hybrid, human_guided, not_applied",
		]

		requires: ["ML-SCHEMA-2"]
	}

	"ML-SCHEMA-4": {
		id:    "ML-SCHEMA-4"
		title: "[ML-SCHEMA-4] Add MentalLatticeConfig type with strategic decisions"
		what:  "THE SYSTEM SHALL add #MentalLatticeConfig type capturing 5 strategic decisions"
		why:   "Provide single source of truth for how mental lattice tools should behave"

		test: {
			command: "cue eval -l '#MentalLatticeConfig' schema/beads.cue"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Config type has all 5 strategic decision fields",
			"Each field has correct enum values matching user choices",
			"Comments explain what each decision means",
			"CUE schema compiles",
		]

		file:   "schema/beads.cue"
		effort: "5min"
		status: "pending"

		edge_cases: [
			"All 5 decisions are required (no optional fields)",
			"quality_threshold is int 0-100",
			"regeneration_strategy must match implemented strategies",
		]

		requires: ["ML-SCHEMA-3"]
	}

	"ML-SCHEMA-5": {
		id:    "ML-SCHEMA-5"
		title: "[ML-SCHEMA-5] Create schema/mental-lattice-config.cue with Phase 1 analysis"
		what:  "THE SYSTEM SHALL create schema/mental-lattice-config.cue file with ml_config instance and all 8 Phase 1 bead KIRK metadata"
		why:   "Document the mental lattice analysis in machine-readable CUE format, ready for tools to consume"

		test: {
			command: "cue vet schema/mental-lattice-config.cue schema/beads.cue && cue eval schema/mental-lattice-config.cue | grep -c 'p1_t1'"
			expect: {
				exit_code:       0
				stdout_contains: "8"
			}
		}

		done_when: [
			"ml_config instance created with all 5 strategic decisions",
			"All 8 Phase 1 beads have complete KIRK metadata",
			"Each has EARS pattern, contract, inversion, quality, regeneration, audit fields",
			"File validates against schema (cue vet passes)",
			"File is valid CUE (cue export runs without errors)",
		]

		file:   "schema/mental-lattice-config.cue"
		effort: "15min"
		status: "pending"

		edge_cases: [
			"Each bead metadata must have analyzed_at timestamp (ISO8601)",
			"Quality scores must be consistent (completeness, testability, clarity)",
			"Edge cases must be specific and actionable",
			"Audit trail captures all analysis metadata",
		]

		requires: ["ML-SCHEMA-4"]
	}
}

// ============================================================================
// PHASE 1A: P1 BEAD ENHANCEMENT (8 beads, 75 minutes)
// Enhance: Update existing Phase 1 automation beads with KIRK metadata
// ============================================================================

beads_phase_1a: {
	"ML-P1A-1": {
		id:    "ML-P1A-1"
		title: "[ML-P1A-1] Enhance [P1-T1.1] answer_loader.gleam with KIRK"
		what:  "WHEN bead qnf (answer_loader) is analyzed THEN THE SYSTEM SHALL update it with complete KIRK metadata"
		why:   "Ensure bead meets 80+ quality threshold and has explicit contracts before execution"

		test: {
			command: "bd show intent-cli-qnf | jq -r '.kirk.quality.overall' | grep -E '^(9[0-9]|100)$'"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Original qnf bead updated with complete KIRK metadata from schema/mental-lattice-config.cue::p1_t1_1_kirk",
			"Edge cases expanded from 5-10 to 10+ specific cases",
			"Quality score calculated and documented (93/100)",
			"EARS pattern, contracts, inversion risks all populated",
			"File compiles and validates",
		]

		file:   ".beads/ (bead qnf)"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"File not found error message must be clear",
			"Invalid CUE parse errors must include line number",
			"Unknown answer keys should warn but not fail",
			"Empty strings should be rejected",
			"Large files (>1MB) should document bounds",
		]

		requires: ["ML-SCHEMA-5"]
	}

	"ML-P1A-2": {
		id:    "ML-P1A-2"
		title: "[ML-P1A-2] Enhance [P1-T1.2] ask_single_question with KIRK"
		what:  "WHEN bead f5y is analyzed THEN THE SYSTEM SHALL update with KIRK metadata documenting dict/prompt interaction"
		why:   "Ensure bead handles backwards compatibility and edge cases correctly"

		test: {
			command: "bd show intent-cli-f5y | jq -r '.kirk.quality.overall' | grep -E '^(8[0-9]|9[0-9]|100)$'"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Original f5y bead updated with KIRK metadata from schema/mental-lattice-config.cue::p1_t1_2_kirk",
			"Edge cases expanded to cover dict/prompt interaction",
			"Quality issues documented (strict mode semantics unclear)",
			"EARS pattern (optional) captured",
			"Compiles",
		]

		file:   ".beads/ (bead f5y)"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"Dict provided, answer missing, strict=false → must prompt",
			"Dict provided, answer missing, strict=true → must error",
			"Dict answer wrong type → validation must fail",
			"Dict with extra answers → must ignore gracefully",
			"Backwards compatibility: no dict → original behavior",
		]

		requires: ["ML-SCHEMA-5"]
	}

	"ML-P1A-3": {
		id:    "ML-P1A-3"
		title: "[ML-P1A-3] Enhance [P1-T1.3] intent.gleam CLI with KIRK"
		what:  "WHEN bead xk8 is analyzed THEN THE SYSTEM SHALL update with KIRK metadata documenting flag interaction"
		why:   "Ensure CLI flags work correctly and interactions are documented"

		test: {
			command: "bd show intent-cli-xk8 | jq '.kirk.quality.overall'"
			expect: {
				exit_code:       0
				stdout_contains: "83"
			}
		}

		done_when: [
			"Original xk8 bead updated with KIRK metadata from schema/mental-lattice-config.cue::p1_t1_3_kirk",
			"Edge cases include flag interactions (--strict without --answers)",
			"Quality issues documented (flag interaction needs docs)",
			"EARS pattern (event-driven) captured",
			"Compiles",
		]

		file:   ".beads/ (bead xk8)"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"--strict without --answers → error (not silent fail)",
			"Relative file paths → resolve from cwd",
			"~ expansion → handle home directory",
			"Empty answers file → load empty dict (not error)",
			"Both --answers and stdin → --answers takes precedence",
		]

		requires: ["ML-SCHEMA-5"]
	}

	"ML-P1A-4": {
		id:    "ML-P1A-4"
		title: "[ML-P1A-4] Enhance [P1-T1.4] bead_feedback.gleam with KIRK"
		what:  "WHEN bead 18a is analyzed THEN THE SYSTEM SHALL update with KIRK metadata flagging concurrent write risk"
		why:   "Ensure bead highlights critical file locking need before execution"

		test: {
			command: "bd show intent-cli-18a | jq '.kirk.quality.overall'"
			expect: {
				exit_code:       0
				stdout_contains: "87"
			}
		}

		done_when: [
			"Original 18a bead updated with KIRK metadata from schema/mental-lattice-config.cue::p1_t1_4_kirk",
			"Edge cases emphasize concurrent write protection (CRITICAL)",
			"Quality issue flags missing file locking specification",
			"EARS pattern (ubiquitous) captured",
			"Compiles",
		]

		file:   ".beads/ (bead 18a)"
		effort: "15min"
		status: "pending"

		edge_cases: [
			"Concurrent appends → atomic writes prevent corruption",
			"Disk full → graceful error, preserve file",
			"Session missing → clear error message",
			"Bead ID doesn't exist in session → warn but append anyway",
			"Very large feedback file → performance test with 1000+ entries",
		]

		requires: ["ML-SCHEMA-5"]
	}

	"ML-P1A-5": {
		id:    "ML-P1A-5"
		title: "[ML-P1A-5] Enhance [P1-T1.5] bead-status command with KIRK"
		what:  "WHEN bead bto is analyzed THEN THE SYSTEM SHALL update with KIRK metadata documenting validation needs"
		why:   "Ensure CLI command validates input and shows clear errors"

		test: {
			command: "bd show intent-cli-bto | jq '.kirk.quality.overall'"
			expect: {
				exit_code:       0
				stdout_contains: "87"
			}
		}

		done_when: [
			"Original bto bead updated with KIRK metadata from schema/mental-lattice-config.cue::p1_t1_5_kirk",
			"Edge cases include all validation scenarios",
			"Quality score documented",
			"EARS pattern (event-driven) captured",
			"Compiles",
		]

		file:   ".beads/ (bead bto)"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"Bead already marked completed → warn, update anyway",
			"Invalid status value → error with valid options",
			"--status blocked without --reason → error",
			"Session missing → clear error",
		]

		requires: ["ML-SCHEMA-5"]
	}

	"ML-P1A-6": {
		id:    "ML-P1A-6"
		title: "[ML-P1A-6] Enhance [P1-T1.6] beads-regenerate with KIRK"
		what:  "WHEN bead ozf is analyzed THEN THE SYSTEM SHALL update with KIRK metadata flagging low quality score"
		why:   "Highlight that this bead is below 80 quality threshold and needs gap fixes first"

		test: {
			command: "bd show intent-cli-ozf | jq '.kirk.quality.overall'"
			expect: {
				exit_code:       0
				stdout_contains: "67"
			}
		}

		done_when: [
			"Original ozf bead updated with KIRK metadata from schema/mental-lattice-config.cue::p1_t1_6_kirk",
			"Quality issues clearly documented (only 67, below 80 threshold)",
			"Suggested strategies for regeneration spelled out",
			"Flag: This bead FAILS quality gate (needs Phase 1B work first)",
			"EARS pattern (complex) captured",
			"Compiles",
		]

		file:   ".beads/ (bead ozf)"
		effort: "20min"
		status: "pending"

		edge_cases: [
			"No failed beads → error (document choice)",
			"Regeneration worse than original → compare, require approval",
			"New beads have circular deps → reject",
			"Backup of original beads → preserve for rollback",
		]

		requires: ["ML-SCHEMA-5"]
	}

	"ML-P1A-7": {
		id:    "ML-P1A-7"
		title: "[ML-P1A-7] Enhance [P1-T1.7] plan_mode.gleam with KIRK"
		what:  "WHEN bead 8sg is analyzed THEN THE SYSTEM SHALL update with KIRK metadata documenting high quality"
		why:   "Well-defined algorithmic task - should pass quality gate easily"

		test: {
			command: "bd show intent-cli-8sg | jq '.kirk.quality.overall'"
			expect: {
				exit_code:       0
				stdout_contains: "93"
			}
		}

		done_when: [
			"Original 8sg bead updated with KIRK metadata from schema/mental-lattice-config.cue::p1_t1_7_kirk",
			"Quality score 93 documented",
			"Edge cases include all algorithmic edge cases",
			"EARS pattern (ubiquitous) captured",
			"Compiles",
		]

		file:   ".beads/ (bead 8sg)"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"Circular dependencies (A → B → C → A) → error with cycle path",
			"100+ beads → efficient topological sort",
			"Missing bead ID in requires → validation error",
			"Empty session → empty plan (not error)",
		]

		requires: ["ML-SCHEMA-5"]
	}

	"ML-P1A-8": {
		id:    "ML-P1A-8"
		title: "[ML-P1A-8] Enhance [P1-T1.8] plan command with KIRK"
		what:  "WHEN bead woq is analyzed THEN THE SYSTEM SHALL update with KIRK metadata documenting high quality"
		why:   "Simple wrapper command - should pass quality gate"

		test: {
			command: "bd show intent-cli-woq | jq '.kirk.quality.overall'"
			expect: {
				exit_code:       0
				stdout_contains: "93"
			}
		}

		done_when: [
			"Original woq bead updated with KIRK metadata from schema/mental-lattice-config.cue::p1_t1_8_kirk",
			"Quality score 93 documented",
			"Edge cases documented",
			"EARS pattern (event-driven) captured",
			"Compiles",
		]

		file:   ".beads/ (bead woq)"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"Session doesn't exist → clear error with path",
			"Invalid format → show valid options",
			"Large plan (200+ beads) → pagination or summary",
			"Terminal colors detection for human format",
		]

		requires: ["ML-SCHEMA-5"]
	}
}

// ============================================================================
// PHASE 1B: CRITICAL GAP FIXES (5 beads, 85 minutes)
// GATE: Must complete before Phase 2
// ============================================================================

beads_phase_1b: {
	"ML-GAP-1": {
		id:    "ML-GAP-1"
		title: "[ML-GAP-1] CRITICAL: Specify file locking strategy for bead_feedback"
		what:  "THE SYSTEM SHALL design and document the file locking approach for concurrent append in bead_feedback.gleam"
		why:   "Concurrent appends cause file corruption - CRITICAL issue revealed by inversion analysis"

		test: {
			command: "test -f design/file-locking.md && grep -c -i 'lock\\|atomic' design/file-locking.md | grep -E '^[5-9]$|^[0-9]{2,}$'"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Design document created: design/file-locking.md",
			"Locking strategy chosen and justified (flock, LockFile, or library)",
			"OS-specific approaches documented (Unix vs Windows)",
			"Edge cases covered: timeout, stale locks, permissions",
			"Implementation guidance provided",
		]

		file:   "design/file-locking.md"
		effort: "15min"
		status: "pending"

		edge_cases: [
			"Lock acquisition timeout behavior",
			"Stale locks (process crashed while holding)",
			"Lock file permissions issues",
			"Cross-process lock coordination",
			"Recovery from lock failure",
		]

		requires: ["ML-P1A-4"]
	}

	"ML-GAP-2": {
		id:    "ML-GAP-2"
		title: "[ML-GAP-2] CRITICAL: Define regeneration strategies"
		what:  "THE SYSTEM SHALL design the 4 regeneration strategies (inversion, second-order, pre-mortem, hybrid) used by beads-regenerate"
		why:   "ozf bead says 'adjusted approach' but doesn't define what that means - strategy must be explicit"

		test: {
			command: "test -f design/regeneration-strategies.md && grep -c -i 'strategy\\|algorithm' design/regeneration-strategies.md | grep -E '^[0-9]{2,}$'"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Design document created: design/regeneration-strategies.md",
			"4 strategies defined with clear algorithms",
			"Inversion-driven: How to identify uncovered failure modes",
			"Second-order-driven: How to analyze consequences",
			"Pre-mortem-driven: How to prevent failure cascades",
			"Hybrid: How to combine all 3",
			"Approval workflow documented",
		]

		file:   "design/regeneration-strategies.md"
		effort: "20min"
		status: "pending"

		edge_cases: [
			"Multiple strategies generate conflicting beads → merge logic",
			"No failed beads → what does regenerate do?",
			"Regeneration produces worse beads → quality regression detection",
			"Circular dependencies generated → detection and error",
		]

		requires: ["ML-P1A-6"]
	}

	"ML-GAP-3": {
		id:    "ML-GAP-3"
		title: "[ML-GAP-3] Implement quality validation gates"
		what:  "THE SYSTEM SHALL implement validate_bead_kirk() function enforcing 80+ quality threshold before bead execution"
		why:   "Quality threshold (80+) must be enforced by tools, not just documented"

		test: {
			command: "gleam build && gleam test -- test_validate_bead_kirk"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Function validate_bead_kirk() exists in src/intent/bead_validator.gleam",
			"Checks: EARS pattern valid, preconditions present, quality_score >= 80",
			"Returns Result(Nil, List(ValidationIssue))",
			"Tests show validation passing for quality 80+, failing for <80",
			"All Phase 1A beads (except ozf) pass validation",
		]

		file:   "src/intent/bead_validator.gleam"
		effort: "20min"
		status: "pending"

		edge_cases: [
			"Bead without KIRK metadata → validation passes (optional)",
			"Bead with quality 79 → fails (below 80)",
			"Quality issue with severity='error' → fails validation",
		]

		requires: ["ML-P1A-8"]
	}

	"ML-GAP-4": {
		id:    "ML-GAP-4"
		title: "[ML-GAP-4] Integrate EARS parser into validation"
		what:  "THE SYSTEM SHALL integrate existing ears_parser module to validate bead 'what' field clarity"
		why:   "EARS requirement clarity must be enforced in validation gates"

		test: {
			command: "gleam test -- test_validate_bead_ears"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Function validate_bead_ears() uses ears_parser.parse()",
			"Checks EARS pattern of bead.what field",
			"Validates pattern matches test specification",
			"Extracts trigger/condition/state components",
			"Calculates clarity score (0-100)",
			"Tests pass",
		]

		file:   "src/intent/bead_validator.gleam"
		effort: "15min"
		status: "pending"

		edge_cases: [
			"Bead 'what' doesn't match any EARS pattern",
			"Pattern matches but clarity score too low (<75)",
			"Trigger/condition/state extraction fails",
		]

		requires: ["ML-GAP-3"]
	}

	"ML-GAP-5": {
		id:    "ML-GAP-5"
		title: "[ML-GAP-5] Integrate inversion coverage checking"
		what:  "THE SYSTEM SHALL use inversion_checker to validate bead edge_cases coverage"
		why:   "Inversion analysis results must be validated against actual edge cases in beads"

		test: {
			command: "gleam test -- test_analyze_bead_inversions"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Function analyze_bead_inversions() uses inversion_checker",
			"Identifies security/usability/integration risks",
			"Calculates edge_case_coverage % (identified risks vs tested)",
			"Returns suggested_edge_cases for improvement",
			"Tests pass",
		]

		file:   "src/intent/bead_validator.gleam"
		effort: "15min"
		status: "pending"

		edge_cases: [
			"Bead with no identified risks (coverage = 100%)",
			"Checker finds risks but no edge cases exist",
			"Multiple risks, only some are covered",
		]

		requires: ["ML-GAP-4"]
	}
}

// ============================================================================
// PHASE 2: BEAD GENERATION PIPELINE (6 beads, 90 minutes)
// Depends on: Phase 1B (gap fixes must complete first)
// ============================================================================

beads_phase_2: {
	"ML-GEN-1": {
		id:    "ML-GEN-1"
		title: "[ML-GEN-1] Create bead_generator.gleam module"
		what:  "THE SYSTEM SHALL implement behavior_to_bead() function converting CUE behaviors to beads"
		why:   "Automate bead creation from specifications"

		test: {
			command: "gleam test -- test_behavior_to_bead"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Module created: src/intent/bead_generator.gleam",
			"Function behavior_to_bead() exists",
			"Maps behavior.intent to EARS-formatted 'what'",
			"Generates done_when from behavior.checks",
			"Infers implementation file from feature name",
			"Tests pass on example specs",
		]

		file:   "src/intent/bead_generator.gleam"
		effort: "20min"
		status: "pending"

		edge_cases: [
			"Behavior with no checks → minimal done_when",
			"API behavior vs database vs validation",
			"Feature name to file inference ambiguity",
		]

		requires: ["ML-GAP-5"]
	}

	"ML-GEN-2": {
		id:    "ML-GEN-2"
		title: "[ML-GEN-2] Integrate EARS validation into generation"
		what:  "WHEN beads are generated THEN THE SYSTEM SHALL validate EARS pattern and clarity"
		why:   "Ensure generated beads have clear EARS patterns"

		test: {
			command: "gleam test -- test_ears_validation_in_generation"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Generation validates EARS pattern on generated 'what' field",
			"Clarity score calculated",
			"Low clarity (<75) triggers regeneration",
			"Tests pass",
		]

		file:   "src/intent/bead_generator.gleam"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"Generated 'what' fails EARS check → needs rewrite",
			"Pattern identified but clarity <75 → rewrite",
			"Multiple patterns possible → choose best",
		]

		requires: ["ML-GEN-1"]
	}

	"ML-GEN-3": {
		id:    "ML-GEN-3"
		title: "[ML-GEN-3] Integrate inversion analysis into generation"
		what:  "WHEN beads are generated THEN THE SYSTEM SHALL run inversion_checker and suggest edge cases"
		why:   "Identify missing edge cases automatically"

		test: {
			command: "gleam test -- test_inversion_in_generation"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Generation runs inversion_checker on each bead",
			"Automatically adds suggested edge cases to bead.edge_cases",
			"Coverage calculation integrated",
			"Tests pass",
		]

		file:   "src/intent/bead_generator.gleam"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"No identified inversions → coverage 100%",
			"Many inversions but few edge cases → coverage low",
			"Suggested edge case already exists → dedup",
		]

		requires: ["ML-GEN-2"]
	}

	"ML-GEN-4": {
		id:    "ML-GEN-4"
		title: "[ML-GEN-4] Implement quality scoring in generation"
		what:  "THE SYSTEM SHALL calculate quality scores (completeness, testability, clarity) for generated beads"
		why:   "Identify weak beads early and trigger regeneration"

		test: {
			command: "gleam test -- test_quality_scoring"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Quality module calculates completeness/testability/clarity",
			"Overall score is weighted average",
			"Issues identified and logged",
			"Tests pass",
		]

		file:   "src/intent/bead_generator.gleam"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"Perfect bead → quality 100",
			"Minimal bead → quality <80 (fails threshold)",
			"Missing preconditions → quality penalty",
		]

		requires: ["ML-GEN-3"]
	}

	"ML-GEN-5": {
		id:    "ML-GEN-5"
		title: "[ML-GEN-5] Implement iterative bead regeneration"
		what:  "WHEN generated bead quality is <80 THEN THE SYSTEM SHALL regenerate with improvements until quality >= 80"
		why:   "Ensure all generated beads meet quality threshold"

		test: {
			command: "gleam test -- test_regenerate_problem_beads"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Function regenerate_bead_from_issues() exists",
			"Rewrites 'what' with EARS patterns",
			"Expands done_when from test",
			"Adds missing edge cases from inversion",
			"Re-validates and recalculates quality",
			"Tests show iteration improves score",
		]

		file:   "src/intent/bead_generator.gleam"
		effort: "20min"
		status: "pending"

		edge_cases: [
			"Regeneration still <80 → needs human intervention",
			"Circular improvement (same issue regenerated) → detect and stop",
			"Max iterations reached → fail with message",
		]

		requires: ["ML-GEN-4"]
	}

	"ML-GEN-6": {
		id:    "ML-GEN-6"
		title: "[ML-GEN-6] Add 'beads-generate' CLI command"
		what:  "THE SYSTEM SHALL add CLI command: intent beads-generate <spec.cue> → generates beads"
		why:   "Enable automation of bead creation from specs"

		test: {
			command: "intent beads-generate examples/user-api.cue | jq '.beads | length'"
			expect: {
				exit_code:       0
				stdout_contains: "[0-9]"
			}
		}

		done_when: [
			"Command exists in src/intent.gleam",
			"Reads spec file",
			"Generates beads using full pipeline (generate → validate → refine)",
			"Outputs session with beads",
			"Tests cover error cases",
		]

		file:   "src/intent.gleam"
		effort: "15min"
		status: "pending"

		edge_cases: [
			"Spec file not found → clear error",
			"Spec invalid CUE → parse error",
			"Spec has no behaviors → empty beads list",
			"Very large spec → performance acceptable",
		]

		requires: ["ML-GEN-5"]
	}
}

// ============================================================================
// PHASE 3: VALIDATION & ANALYSIS (2 beads, 25 minutes)
// ============================================================================

beads_phase_3: {
	"ML-VAL-1": {
		id:    "ML-VAL-1"
		title: "[ML-VAL-1] Add 'analyze-beads' CLI command"
		what:  "THE SYSTEM SHALL add CLI command: intent analyze-beads <session_id> → shows Mental Lattice analysis"
		why:   "Display KIRK metadata, quality scores, and gaps to user"

		test: {
			command: "intent analyze-beads <session_id> --format human | grep -i quality"
			expect: {
				exit_code:       0
				stdout_contains: "quality"
			}
		}

		done_when: [
			"Command loads session",
			"Runs all validators (EARS, inversion, quality)",
			"Displays results in human/json/markdown format (--format flag)",
			"Shows gaps and improvement suggestions",
		]

		file:   "src/intent.gleam"
		effort: "15min"
		status: "pending"

		edge_cases: [
			"Session doesn't exist → clear error",
			"No beads in session → empty output",
			"Very large output → pagination",
			"--format invalid → error",
		]

		requires: ["ML-GEN-6"]
	}

	"ML-VAL-2": {
		id:    "ML-VAL-2"
		title: "[ML-VAL-2] Add Mental Lattice analysis to 'beads' command"
		what:  "WHEN user runs 'intent beads' THEN THE SYSTEM SHALL show KIRK metadata and quality scores by default"
		why:   "Make mental lattice output visible when displaying beads"

		test: {
			command: "intent beads <id> | grep -i quality"
			expect: {
				exit_code:       0
				stdout_contains: "quality"
			}
		}

		done_when: [
			"Default output shows basic KIRK metadata",
			"--detailed flag shows full analysis",
			"Quality score displayed for each bead",
			"Tests pass",
		]

		file:   "src/intent.gleam"
		effort: "10min"
		status: "pending"

		edge_cases: [
			"Beads without KIRK metadata → gracefully omitted",
			"Quality threshold not met → warning",
			"Very large analysis → truncate intelligently",
		]

		requires: ["ML-VAL-1"]
	}
}

// ============================================================================
// PHASE 4: ADVANCED MENTAL MODELS (2 beads, 40 minutes)
// ============================================================================

beads_phase_4: {
	"ML-2ND-1": {
		id:    "ML-2ND-1"
		title: "[ML-2ND-1] Create second_order_analyzer.gleam"
		what:  "THE SYSTEM SHALL implement second-order consequence analysis for beads"
		why:   "Identify cascading impacts and hidden dependencies"

		test: {
			command: "gleam test -- test_second_order_analysis"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Module created: src/intent/second_order_analyzer.gleam",
			"Analyzes bead consequences",
			"Identifies affected systems",
			"Suggests defensive beads",
			"Tests pass",
		]

		file:   "src/intent/second_order_analyzer.gleam"
		effort: "20min"
		status: "pending"

		edge_cases: []

		requires: ["ML-VAL-2"]
	}

	"ML-2ND-2": {
		id:    "ML-2ND-2"
		title: "[ML-2ND-2] Create premortem_analyzer.gleam"
		what:  "THE SYSTEM SHALL implement pre-mortem analysis (imagine failure, work backwards)"
		why:   "Prevent failures by identifying likely causes proactively"

		test: {
			command: "gleam test -- test_premortem_analysis"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Module created: src/intent/premortem_analyzer.gleam",
			"Performs pre-mortem analysis",
			"Identifies probable failure scenarios",
			"Suggests preventive beads",
			"Tests pass",
		]

		file:   "src/intent/premortem_analyzer.gleam"
		effort: "20min"
		status: "pending"

		edge_cases: []

		requires: ["ML-VAL-2"]
	}
}

// ============================================================================
// PHASE 5: SMART REGENERATION (1 bead, 25 minutes)
// ============================================================================

beads_phase_5: {
	"ML-REGEN-1": {
		id:    "ML-REGEN-1"
		title: "[ML-REGEN-1] Enhance beads-regenerate with hybrid mental models"
		what:  "WHEN a bead fails THEN THE SYSTEM SHALL use hybrid regeneration (inversion + second-order + pre-mortem) to improve it"
		why:   "Failed beads improve using multiple mental models, with quality comparison and approval"

		test: {
			command: "gleam test -- test_hybrid_regeneration"
			expect: {
				exit_code: 0
			}
		}

		done_when: [
			"Enhanced beads-regenerate command uses hybrid strategy",
			"Runs all three analyses on failed bead",
			"Generates replacement beads",
			"Quality comparison with original",
			"Requires approval before applying",
			"Tests pass",
		]

		file:   "src/intent.gleam"
		effort: "25min"
		status: "pending"

		edge_cases: [
			"No failed beads → no-op",
			"Regeneration worse than original → show diff, reject",
			"Circular dependencies generated → detect and error",
			"User rejects regeneration → rollback",
		]

		requires: ["ML-2ND-1", "ML-2ND-2"]
	}
}

// ============================================================================
// BEAD DEPENDENCY GRAPH (All 29 beads with requirements)
// ============================================================================

// Phase 0: Sequential (each depends on previous)
// ML-SCHEMA-1 → ML-SCHEMA-2 → ML-SCHEMA-3 → ML-SCHEMA-4 → ML-SCHEMA-5

// Phase 1A: All parallel (depend on ML-SCHEMA-5)
// ML-P1A-1..8 all depend on ML-SCHEMA-5

// Phase 1B: Sequential validation chain
// ML-P1A-8 → ML-GAP-3 → ML-GAP-4 → ML-GAP-5
// P1A-4 → ML-GAP-1, P1A-6 → ML-GAP-2 (parallel paths)

// Phase 2: Sequential generation pipeline
// ML-GEN-1 → ML-GEN-2 → ML-GEN-3 → ML-GEN-4 → ML-GEN-5 → ML-GEN-6

// Phase 3: Sequential analysis
// ML-GEN-6 → ML-VAL-1 → ML-VAL-2

// Phase 4: Parallel advanced models
// ML-VAL-2 → ML-2ND-1, ML-2ND-2 (can run in parallel)

// Phase 5: Final regeneration
// ML-2ND-1, ML-2ND-2 → ML-REGEN-1
