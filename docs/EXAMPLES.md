# Intent CLI - Usage Examples

This document provides practical examples and common workflows for using Intent CLI effectively.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Interview Workflow](#interview-workflow)
3. [Bead Generation](#bead-generation)
4. [Document Generation](#document-generation)
5. [Effects Analysis](#effects-analysis)
6. [Plan Management](#plan-management)
7. [Common Workflows](#common-workflows)
8. [Tips and Tricks](#tips-and-tricks)

## Getting Started

### Installation

```bash
# Clone the repository
git clone https://github.com/your-org/intent-cli.git
cd intent-cli

# Build the project
gleam build

# Verify installation
gleam run -- --version
```

### Quick Start

```bash
# 1. Start an interview for an API project
gleam run -- interview --profile api

# 2. View your session history
gleam run -- history

# 3. Generate beads from your session
gleam run -- beads --session <session-id> --format json

# 4. Analyze second-order effects
gleam run -- effects spec.cue
```

## Interview Workflow

### Starting a New Interview

```bash
# API project interview
gleam run -- interview --profile api

# CLI tool interview
gleam run -- interview --profile cli

# Event-driven system interview
gleam run -- interview --profile event

# Data pipeline interview
gleam run -- interview --profile data

# Workflow system interview
gleam run -- interview --profile workflow

# UI application interview
gleam run -- interview --profile ui
```

### Resuming a Session

```bash
# List all sessions to find the session ID
gleam run -- history

# Resume a specific session
gleam run -- interview --resume abc123def456

# Resume and continue from where you left off
gleam run -- interview --profile api --resume abc123def456
```

### Viewing Session Changes

```bash
# Show what changed in a session
gleam run -- diff --session abc123def456

# Compare sessions to understand evolution
gleam run -- history
gleam run -- diff --session abc123def456
```

### Listing and Filtering Sessions

```bash
# List all sessions with detailed info
gleam run -- sessions

# Filter by profile type
gleam run -- sessions --profile api
gleam run -- sessions --profile cli

# Quick chronological list
gleam run -- history
```

## Bead Generation

### Generating Beads

```bash
# Generate beads in JSON format (default)
gleam run -- beads --session abc123def456

# Generate beads in JSONL format
gleam run -- beads --session abc123def456 --format jsonl

# Generate beads in Markdown format
gleam run -- beads --session abc123def456 --format markdown

# Specify output directory
gleam run -- beads --session abc123def456 --out ./beads/
```

### Regenerating Beads

```bash
# Regenerate beads with updated templates
gleam run -- beads-regenerate --session abc123def456

# Useful after:
# - Updating bead templates
# - Modifying session answers
# - Fixing generation issues
```

### Emitting Beads to br

```bash
# Dry run - see what would be created (SAFE)
gleam run -- plan-emit-beads abc123def456

# Actually create beads
gleam run -- plan-emit-beads abc123def456 --execute

# Force recreate all beads (use with caution)
gleam run -- plan-emit-beads abc123def456 --execute --force

# Check bead status after creation
gleam run -- bead-status --bead-id intent-cli-113
```

### Working with Beads in br

```bash
# List all beads
br list

# Find ready beads
br ready

# Show bead details
br show --json intent-cli-113

# Update bead status
br update intent-cli-113 --status in_progress

# Complete a bead
br close intent-cli-113 --reason "Done"

# Find next bead to work on
br ready --json | head -20
```

## Document Generation

### Generating Vision Documents

```bash
# Generate vision document from spec
gleam run -- vision spec.cue

# Specify output directory
gleam run -- vision spec.cue --out docs/

# Example output structure
# docs/vision.md contains:
#   - System overview
#   - Goals and objectives
#   - Target audience
#   - Success criteria
#   - Feature summaries
```

### Generating Ready Documents

```bash
# Generate implementation-ready document
gleam run -- ready spec.cue

# Specify output directory
gleam run -- ready spec.cue --out docs/

# Example output structure
# docs/ready.md contains:
#   - Detailed behavior descriptions
#   - Preconditions and postconditions
#   - Verification criteria
#   - Test scenarios
#   - Implementation hints
```

### Combining Documents

```bash
# Generate both documents for a complete spec
gleam run -- vision spec.cue --out docs/
gleam run -- ready spec.cue --out docs/

# Documents created:
# docs/vision.md  - High-level vision (for stakeholders)
# docs/ready.md   - Implementation details (for developers)
```

## Effects Analysis

### Analyzing All Behaviors

```bash
# Analyze all behaviors in a spec
gleam run -- effects spec.cue

# Output includes:
#   - Behavior name
#   - Effect type (state_change, cascade, etc.)
#   - Severity level (high, medium, low)
#   - Description of the effect
#   - Mitigation suggestions
```

### Analyzing Specific Behaviors

```bash
# Analyze a single behavior
gleam run -- effects spec.cue --behavior create-user

# Useful for:
#   - Deep dive into specific feature
#   - Risk assessment
#   - Test planning
```

### JSON Output for Tools

```bash
# Generate JSON output for programmatic processing
gleam run -- effects spec.cue --json > effects.json

# Use with jq for filtering
gleam run -- effects spec.cue --json | jq '.behaviors.create-user'

# Generate report
gleam run -- effects spec.cue --json | jq '.behaviors | to_entries[] | select(.value.effects | length > 0)'
```

### Understanding Effect Types

```bash
# State changes
gleam run -- effects spec.cue --behavior update-database
# Shows: What state changes occur, what's affected

# Cascading effects
gleam run -- effects spec.cue --behavior delete-user
# Shows: What else is affected by this action

# Race conditions
gleam run -- effects spec.cue --behavior transfer-funds
# Shows: Potential concurrency issues

# Notifications
gleam run -- effects spec.cue --behavior place-order
# Shows: What events need to be broadcast

# Rollback scenarios
gleam run -- effects spec.cue --behavior process-payment
# Shows: What happens if this fails
```

## Plan Management

### Generating Plans

```bash
# Generate plan from current context
gleam run -- plan

# Add notes for context
gleam run -- plan --notes "Focus on authentication first"

# Plan includes:
#   - Total number of beads
#   - Estimated effort
#   - Risk level
#   - Phase breakdown
#   - Dependencies
```

### Getting Next Task Suggestions

```bash
# Suggest next task (default: page_rank strategy)
gleam run -- plan-next

# Use different strategies
gleam run -- plan-next --strategy page_rank       # Most influential
gleam run -- plan-next --strategy critical_path   # On critical path
gleam run -- plan-next --strategy shortest        # Quick wins
gleam run -- plan-next --strategy risk_first      # Highest risk

# Output includes:
#   - Task ID and title
#   - Rationale for selection
#   - Dependencies
#   - Estimated effort
```

### Approving Plans

```bash
# Approve a plan for execution
gleam run -- plan-approve plan-abc123

# Add approval notes
gleam run -- plan-approve plan-abc123 --notes "Reviewed with team, approved for sprint 3"

# This enables plan-emit-beads execution
```

## Common Workflows

### Workflow 1: New API Project

```bash
# 1. Start interview for API project
gleam run -- interview --profile api

# 2. Answer questions about your API
# (Interactive session)

# 3. Review your session
gleam run -- sessions --profile api

# 4. Generate specification documents
gleam run -- vision spec.cue --out docs/
gleam run -- ready spec.cue --out docs/

# 5. Analyze for potential issues
gleam run -- effects spec.cue

# 6. Generate beads
gleam run -- beads --session <session-id> --format json

# 7. Preview beads (dry run)
gleam run -- plan-emit-beads <session-id>

# 8. Emit beads to br
gleam run -- plan-emit-beads <session-id> --execute

# 9. Start working on beads
br ready
br update <bead-id> --status in_progress
```

### Workflow 2: Iterative Refinement

```bash
# 1. Start initial interview
gleam run -- interview --profile api

# 2. Review what you captured
gleam run -- diff --session <session-id>

# 3. Resume and add more details
gleam run -- interview --resume <session-id>

# 4. Generate updated beads
gleam run -- beads-regenerate --session <session-id>

# 5. Analyze again with new context
gleam run -- effects spec.cue --behavior new-feature

# 6. Update br with new understanding
gleam run -- plan-emit-beads <session-id> --execute
```

### Workflow 3: Documentation Generation

```bash
# 1. Create comprehensive specification
# (Write spec.cue manually or via interview)

# 2. Generate all documentation
gleam run -- vision spec.cue --out docs/
gleam run -- ready spec.cue --out docs/

# 3. Analyze for completeness
gleam run -- effects spec.cue --json > docs/effects-analysis.json

# 4. Review documentation
ls docs/
# vision.md          - High-level overview
# ready.md           - Implementation details
# effects-analysis.json - Potential issues

# 5. Share with team
git add docs/
git commit -m "Add project documentation"
```

### Workflow 4: Risk Assessment

```bash
# 1. Analyze entire specification for risks
gleam run -- effects spec.cue

# 2. Focus on high-severity effects
gleam run -- effects spec.cue --json | jq '.behaviors | to_entries[] | select(.value.effects[]?.severity == "high")'

# 3. Review specific risky behaviors
gleam run -- effects spec.cue --behavior delete-user
gleam run -- effects spec.cue --behavior process-payment

# 4. Generate mitigation plan
gleam run -- plan --notes "Focus on high-risk items first"

# 5. Prioritize by risk
gleam run -- plan-next --strategy risk_first

# 6. Document findings
gleam run -- effects spec.cue --json > risk-assessment.json
```

### Workflow 5: Test Planning

```bash
# 1. Generate ready document with test scenarios
gleam run -- ready spec.cue --out test-plan/

# 2. Analyze behaviors for edge cases
gleam run -- effects spec.cue --behavior create-user

# 3. Look for race conditions
gleam run -- effects spec.cue --json | jq '.behaviors | to_entries[] | select(.value.effects[]?.type == "race_condition")'

# 4. Identify state changes requiring verification
gleam run -- effects spec.cue --json | jq '.behaviors | to_entries[] | select(.value.effects[]?.type == "state_change")'

# 5. Create test cases based on verifications
# (Use ready.md as basis for test scenarios)
```

## Tips and Tricks

### Help and Discovery

```bash
# Get help for any command
gleam run -- <command> --help
gleam run -- interview --help
gleam run -- beads --help

# Use traditional help syntax
gleam run -- help interview
gleam run -- help beads

# List all available commands
gleam run -- --help
```

### Working with Sessions

```bash
# Quick session overview
gleam run -- history | head -20

# Detailed session info
gleam run -- sessions --profile api

# Find incomplete sessions
gleam run -- sessions | grep "gap"

# Resume most recent session
gleam run -- history | head -1 | awk '{print $1}' | xargs gleam run -- interview --resume
```

### Output Formatting

```bash
# JSON for programmatic processing
gleam run -- effects spec.cue --json | jq .

# Save output for later review
gleam run -- effects spec.cue > analysis.txt
gleam run -- beads --session abc123 > beads.json

# Combine with other tools
gleam run -- effects spec.cue --json | jq '.behaviors | length' | wc -l
```

### Safety First

```bash
# Always dry run first
gleam run -- plan-emit-beads <session-id>

# Check what would be created
# Review the output carefully

# Then execute
gleam run -- plan-emit-beads <session-id> --execute

# Verify after creation
br list | tail -10
```

### Productivity Tips

```bash
# Create aliases for common workflows
alias intent='gleam run --'
alias intent-interview-api='intent interview --profile api'
alias intent-effects='intent effects --json'
alias intent-vision='intent vision --out docs/'

# Use shell history
# Ctrl+R to search previous commands
# history | grep intent

# Batch operations
for spec in specs/*.cue; do
  intent effects "$spec" --json > "analysis/$(basename $spec .cue).json"
done
```

### Troubleshooting

```bash
# Check session state
gleam run -- sessions

# Verify spec file
cue export spec.cue

# Validate JSON output
gleam run -- beads --session abc123 --format json | jq .

# Check br status
br list
br show --json <bead-id>

# Enable verbose output (if available)
gleam run -- <command> --verbose
```

## Advanced Examples

### Multi-Project Setup

```bash
# Generate beads for multiple profiles
gleam run -- interview --profile api
SESSION_API=$(gleam run -- history | head -1 | awk '{print $1}')
gleam run -- beads --session $SESSION_API --out api/

gleam run -- interview --profile cli
SESSION_CLI=$(gleam run -- history | head -1 | awk '{print $1}')
gleam run -- beads --session $SESSION_CLI --out cli/
```

### Continuous Integration

```bash
#!/bin/bash
# ci-check.sh - Validate specification in CI

# 1. Export CUE to JSON
cue export spec.cue > /tmp/spec.json

# 2. Analyze for high-severity effects
gleam run -- effects spec.cue --json > /tmp/effects.json
HIGH_RISK=$(jq '[.behaviors[][] | select(.severity == "high")] | length' /tmp/effects.json)

if [ $HIGH_RISK -gt 0 ]; then
  echo "Found $HIGH_RISK high-risk effects"
  exit 1
fi

# 3. Generate documents
gleam run -- vision spec.cue --out docs/
gleam run -- ready spec.cue --out docs/

echo "CI checks passed"
```

### Batch Analysis

```bash
# Analyze all specs in a directory
for spec in examples/*.cue; do
  echo "Analyzing $spec"
  gleam run -- effects "$spec" --json > "reports/$(basename $spec .cue)-effects.json"
done

# Generate summary report
jq -s 'map({spec: .spec, behaviors: [.behaviors | length]})' reports/*-effects.json > summary.json
```

## Conclusion

These examples cover the most common use cases for Intent CLI. For more detailed information about each command, use the `--help` flag:

```bash
gleam run -- <command> --help
```

For specification format details, see [SPEC_FORMAT.md](docs/SPEC_FORMAT.md).
For user guide, see [USER_GUIDE.md](docs/USER_GUIDE.md).
