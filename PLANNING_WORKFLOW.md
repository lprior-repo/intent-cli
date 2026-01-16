# Intent CLI Planning Workflow

Complete guide to using Intent CLI as a pure planning system that transforms requirements into trackable work items.

## Overview

Intent CLI provides a systematic workflow from requirements capture to work item generation:

```
EARS Interview → Mental Lattice Analysis → KIRK Contract Generation →
Structure Planning → Bead Generation → Review Gates at Each Stage
```

Each stage validates and refines requirements, ending with concrete, dependency-ordered work items in the `bd` issue tracker.

## Pipeline Stages

### 1. EARS Interview

**Purpose**: Capture requirements using the 6 EARS (Easy Approach to Requirements Syntax) patterns.

**EARS Patterns**:
- **Ubiquitous**: `THE SYSTEM SHALL [behavior]` - Always-true behaviors
- **Event-Driven**: `WHEN [trigger] THE SYSTEM SHALL [behavior]` - Event responses
- **State-Driven**: `WHILE [state] THE SYSTEM SHALL [behavior]` - State-dependent behaviors
- **Optional**: `WHERE [condition] THE SYSTEM SHALL [behavior]` - Feature-flagged behaviors
- **Unwanted**: `IF [condition] THEN THE SYSTEM SHALL NOT [behavior]` - Security/safety constraints
- **Complex**: Combinations of patterns

**Commands**:
```bash
# Run interactive EARS interview
intent ears-interview

# Parse EARS requirements from file
intent ears requirements.txt
```

**Example Requirements File** (`requirements.txt`):
```
THE SYSTEM SHALL authenticate users before granting access

WHEN a user submits invalid credentials THE SYSTEM SHALL return a 401 error

WHILE processing a payment THE SYSTEM SHALL prevent duplicate submissions

WHERE the user has premium status THE SYSTEM SHALL enable advanced analytics

IF the request lacks authentication THE SYSTEM SHALL NOT expose sensitive data
```

**Review Stage**:
```bash
# Review requirements with validation questions
intent review-requirements requirements.txt

# Save checkpoint for later resumption
intent review-requirements requirements.txt --checkpoint
```

**Validation Questions**:
- Are all requirements clear and specific?
- Is the appropriate EARS pattern used for each?
- Are there missing requirements?
- Do requirements avoid vague terms?

### 2. Mental Lattice Analysis

**Purpose**: Apply 5 thinking models to validate and refine requirements.

**The 5 Lattice Models**:
1. **Inversion**: What could fail? What should we NOT do?
2. **Second-Order Effects**: What happens AFTER that? Long-term consequences?
3. **Pre-Mortem**: Imagine this failed. Why did it fail?
4. **Checklist**: What are we missing? What did we forget?
5. **Circle of Competence**: What's in scope? What's outside our expertise?

**Commands**:
```bash
# Analyze requirements with all 5 lattice models
intent lattice-analyze requirements.txt

# Apply specific model
intent lattice-analyze requirements.txt --model=inversion
```

**Output**: Insights, warnings, and questions for each model applied to each requirement.

### 3. KIRK Contract Generation

**Purpose**: Transform EARS requirements into Design-by-Contract specifications.

**Contract Elements**:
- **Preconditions**: What must be true before execution
  - Authentication requirements
  - Required input fields
  - Field-level constraints
- **Postconditions**: What must be true after execution
  - State changes that occurred
  - Response guarantees
- **Second-Order Effects**: What happens after this action
  - Cascading impacts
  - Triggered events
  - Side effects
- **Confidence Score**: How certain is this contract (0-1)

**Commands**:
```bash
# Generate contracts from requirements
intent generate-contract requirements.txt

# Output as JSON
intent generate-contract requirements.txt --json
```

**Review Stage**:
```bash
# Review contracts with validation questions
intent review-contracts requirements.txt

# Save checkpoint
intent review-contracts requirements.txt --checkpoint
```

**Validation Questions**:
- Do preconditions capture all necessary input requirements?
- Are postconditions complete and testable?
- Are second-order effects identified?
- Is contract confidence acceptable?

### 4. Structure Planning

**Purpose**: Organize KIRK contracts into epic/feature/task hierarchy with wave-based dependencies.

**Hierarchy**:
- **Epic**: High-level grouping of related features (e.g., "Authentication System")
- **Feature**: Cohesive set of behaviors (e.g., "User Login")
- **Task**: Individual implementable unit (e.g., "Validate credentials")

**Wave-Based Dependencies**:
- **Wave 1**: Tasks with no dependencies (can start immediately)
- **Wave 2**: Tasks that depend only on Wave 1
- **Wave N**: Tasks that depend on previous waves

Tasks within the same wave can execute in parallel.

**Domain Classification**:
- Authentication
- Authorization
- User Management
- Data Management
- Integration
- Validation
- Error Handling
- Generic

**Commands**:
```bash
# Plan structure from requirements
intent plan-structure requirements.txt

# Specify project name
intent plan-structure requirements.txt --project=MyProject

# Output as JSON
intent plan-structure requirements.txt --project=MyProject --json
```

**Review Stage**:
```bash
# Review structure with validation questions
intent review-structure requirements.txt --project=MyProject

# Save checkpoint
intent review-structure requirements.txt --project=MyProject --checkpoint
```

**Validation Questions**:
- Is the epic/feature/task hierarchy logical?
- Are tasks properly sized (not too large/small)?
- Is wave-based dependency order correct?
- Is parallelism score acceptable for your team?

**Metrics**:
- **Total Tasks**: Count of all tasks
- **Total Waves**: Maximum wave depth
- **Parallelism Score**: 0-1 scale (higher = more parallel work)

### 5. Bead Generation

**Purpose**: Create beads in `bd` database from project structure with proper hierarchy and dependencies.

**Bead Types**:
- **Epic**: High priority (1), groups related features
- **Feature**: Medium-high priority (2), groups related tasks
- **Task**: Wave-based priority (3=wave 1, 2=wave 2, 1=wave 3+)

**Task Bead Contents**:
- Full KIRK contract embedded in description
- Preconditions, postconditions, second-order effects
- Contract confidence score in AI hints
- Wave number in labels and hints
- Acceptance criteria derived from postconditions

**Commands**:
```bash
# Generate beads from requirements
intent generate-beads requirements.txt --project=MyProject

# Output as JSON
intent generate-beads requirements.txt --project=MyProject --json
```

**Review Stage**:
```bash
# View guidance for reviewing beads in bd
intent review-beads

# Review beads using bd commands
bd list --status=open
bd show <id>
bd ready
bd blocked
bd stats
```

**Validation Questions**:
- Were all expected beads created?
- Should failed beads be retried?
- Are bead relationships (epic → feature → task) correct?
- Are priorities and wave numbers appropriate?

## Complete Workflow Example

### Step 1: Write Requirements

Create `requirements.txt`:
```
THE SYSTEM SHALL authenticate users with email and password

WHEN a user submits valid credentials THE SYSTEM SHALL return a 200 OK with JWT token

WHEN a user submits invalid credentials THE SYSTEM SHALL return a 401 Unauthorized

WHILE a session is active THE SYSTEM SHALL authorize requests using JWT token

WHERE the user has admin role THE SYSTEM SHALL grant access to admin endpoints

IF the request lacks a valid JWT token THEN THE SYSTEM SHALL NOT process the request
```

### Step 2: Review Requirements

```bash
# Review with validation questions
intent review-requirements requirements.txt --checkpoint
```

Review output shows:
- Total requirements count
- Pattern breakdown for each requirement
- Validation questions

If issues found, edit `requirements.txt` and review again.

### Step 3: Generate and Review Contracts

```bash
# Generate contracts
intent generate-contract requirements.txt

# Review contracts
intent review-contracts requirements.txt --checkpoint
```

Review output shows:
- Contract count
- Confidence scores
- Preconditions, postconditions, second-order effects summary
- Validation questions

### Step 4: Plan and Review Structure

```bash
# Plan structure
intent plan-structure requirements.txt --project=AuthSystem

# Review structure
intent review-structure requirements.txt --project=AuthSystem --checkpoint
```

Review output shows:
- Project name
- Epic/feature/task hierarchy
- Total tasks and waves
- Parallelism score
- Validation questions

### Step 5: Generate Beads

```bash
# Generate beads in bd database
intent generate-beads requirements.txt --project=AuthSystem
```

Output shows:
- Total beads created
- Epic, feature, task counts
- Failed beads (if any)
- Next steps

### Step 6: Review Beads in bd

```bash
# View all open beads
bd list --status=open

# Show ready work (no blockers)
bd ready

# Show bead details
bd show <id>

# Check for blocked beads
bd blocked

# View project statistics
bd stats
```

### Step 7: Start Working

```bash
# Claim a task
bd update <id> --status=in_progress

# Complete work
# ... implement, test, commit ...

# Close task
bd close <id> --reason="Implemented and tested"

# Find next work
bd ready
```

## Checkpoint System

### Purpose

Checkpoints enable:
- Stop and resume pipeline at any stage
- Iterate on specific stages without re-running entire pipeline
- Track progress through multi-session work
- Collaborate with shared checkpoint files

### Checkpoint Files

Saved to `.intent/checkpoints/`:
- `requirements.json`: EARS requirements checkpoint
- `contracts.json`: KIRK contracts checkpoint
- `structure.json`: Project structure checkpoint
- `beads.json`: Generated beads checkpoint

### Usage

```bash
# Save checkpoint during review
intent review-requirements requirements.txt --checkpoint
intent review-contracts requirements.txt --checkpoint
intent review-structure requirements.txt --project=MyProject --checkpoint

# Resume from checkpoint (future feature)
# intent resume-from-checkpoint requirements
```

### Checkpoint Format

```json
{
  "checkpoint_type": "requirements",
  "timestamp": "2026-01-16T10:00:00Z",
  "artifact_count": 6,
  "notes": "Review checkpoint"
}
```

## Command Reference

### EARS Commands

| Command | Purpose | Options |
|---------|---------|---------|
| `intent ears <file>` | Parse EARS requirements | `--json` |
| `intent ears-interview` | Interactive interview | - |
| `intent review-requirements <file>` | Review requirements | `--checkpoint` |

### Analysis Commands

| Command | Purpose | Options |
|---------|---------|---------|
| `intent lattice-analyze <file>` | Apply 5 lattice models | `--model=<name>` |
| `intent quality <file>` | Quality analysis | - |

### Contract Commands

| Command | Purpose | Options |
|---------|---------|---------|
| `intent generate-contract <file>` | Generate KIRK contracts | `--json` |
| `intent review-contracts <file>` | Review contracts | `--checkpoint` |

### Structure Commands

| Command | Purpose | Options |
|---------|---------|---------|
| `intent plan-structure <file>` | Plan epic/feature/task structure | `--project=<name>`, `--json` |
| `intent review-structure <file>` | Review structure | `--project=<name>`, `--checkpoint` |

### Bead Commands

| Command | Purpose | Options |
|---------|---------|---------|
| `intent generate-beads <file>` | Generate bd beads | `--project=<name>`, `--json` |
| `intent review-beads` | Review bead guidance | - |

### bd Commands

| Command | Purpose | Options |
|---------|---------|---------|
| `bd list` | List beads | `--status=<open\|closed\|in_progress>` |
| `bd ready` | Show ready work | `--json` |
| `bd show <id>` | Show bead details | - |
| `bd update <id>` | Update bead | `--status=<status>`, `--assignee=<user>` |
| `bd close <id>` | Close bead | `--reason="..."` |
| `bd blocked` | Show blocked beads | - |
| `bd stats` | Project statistics | - |

## Best Practices

### 1. Start Small

Begin with 3-5 requirements to learn the workflow before tackling larger projects.

### 2. Iterate at Each Stage

Use review commands and checkpoints to refine at each stage rather than waiting until the end.

### 3. Save Checkpoints

Always use `--checkpoint` flag during reviews to enable resumption and collaboration.

### 4. Review Mental Lattice Output

The 5 thinking models often surface edge cases and failure modes you didn't consider.

### 5. Validate Confidence Scores

Low confidence scores (<0.6) indicate ambiguous requirements that need clarification.

### 6. Check Parallelism Score

Aim for >0.5 parallelism score. Lower scores may indicate over-serialization of work.

### 7. Use Wave Numbers

Schedule Wave 1 tasks first as they have no blockers. Delay Wave N tasks until dependencies complete.

### 8. Embed Contract Metadata

Task beads include full KIRK contracts - use them during implementation to ensure requirements are met.

### 9. Close Beads with Reasons

Always provide `--reason` when closing beads to document what was accomplished.

### 10. Monitor bd Stats

Regularly check `bd stats` to track progress and identify bottlenecks.

## Troubleshooting

### No Requirements Parsed

**Problem**: `intent ears requirements.txt` finds no requirements.

**Solutions**:
- Ensure requirements start with EARS keywords (THE SYSTEM SHALL, WHEN, WHILE, WHERE, IF)
- Check file exists and is readable
- Verify proper EARS syntax

### Low Contract Confidence

**Problem**: Contracts have confidence scores <0.5.

**Solutions**:
- Add more specific details to requirements
- Include HTTP status codes explicitly
- Specify authentication requirements clearly
- Mention specific field names and constraints

### High Wave Count

**Problem**: Structure has many waves (>5), reducing parallelism.

**Solutions**:
- Reduce explicit dependencies in requirements
- Break down large requirements into smaller, independent units
- Consider if dependencies are truly necessary

### Failed Bead Creation

**Problem**: Some beads fail to create in bd database.

**Solutions**:
- Check bd is installed and accessible
- Verify issue_type is valid (epic, feature, task)
- Ensure priority is 0-4
- Check bd database isn't corrupted

### Checkpoint Load Fails

**Problem**: Can't resume from checkpoint.

**Solutions**:
- Verify `.intent/checkpoints/` directory exists
- Check JSON files are valid (not corrupted)
- Ensure checkpoint was saved successfully

## Integration with Other Tools

### CI/CD Pipeline

```yaml
# .github/workflows/requirements.yml
name: Validate Requirements

on:
  pull_request:
    paths:
      - 'requirements/**'

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Install Intent CLI
        run: |
          # Install intent

      - name: Parse Requirements
        run: intent ears requirements/features.txt

      - name: Generate Contracts
        run: intent generate-contract requirements/features.txt --json > contracts.json

      - name: Plan Structure
        run: intent plan-structure requirements/features.txt --project=${{ github.event.repository.name }} --json > structure.json

      - name: Upload Artifacts
        uses: actions/upload-artifact@v2
        with:
          name: planning-artifacts
          path: |
            contracts.json
            structure.json
```

### Git Hooks

```bash
# .git/hooks/pre-commit
#!/bin/bash

# Validate requirements before commit
if git diff --cached --name-only | grep -q "requirements.txt"; then
  echo "Validating requirements..."
  intent ears requirements.txt || exit 1
  echo "✓ Requirements valid"
fi
```

### VS Code Tasks

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Review Requirements",
      "type": "shell",
      "command": "intent review-requirements ${file} --checkpoint",
      "problemMatcher": [],
      "group": "build"
    },
    {
      "label": "Generate Beads",
      "type": "shell",
      "command": "intent generate-beads ${file} --project=${workspaceFolderBasename}",
      "problemMatcher": [],
      "group": "build"
    }
  ]
}
```

## Advanced Topics

### Custom Domain Classification

Extend `structure_planner.gleam` to add custom domains:

```gleam
type Domain {
  // ... existing domains
  CustomDomain
}

fn classify_domain(contract: KirkContract) -> Domain {
  let behavior_lower = string.lowercase(contract.requirement.system_shall)

  case string.contains(behavior_lower, "custom_keyword") {
    True -> CustomDomain
    False -> // ... existing classification
  }
}
```

### Custom Review Questions

Extend `review_gates.gleam` to add custom validation questions:

```gleam
pub fn review_contracts_custom(
  contracts: List(KirkContract),
  custom_questions: List(String),
) -> String {
  // ... existing logic

  let custom_footer = [
    "Custom Questions:",
    ..list.map(custom_questions, fn(q) { "  • " <> q })
  ]

  // ... format output
}
```

### Batch Processing

Process multiple requirement files:

```bash
#!/bin/bash

for req_file in requirements/*.txt; do
  project_name=$(basename "$req_file" .txt)

  echo "Processing $project_name..."

  intent review-requirements "$req_file" --checkpoint
  intent generate-contract "$req_file" > "contracts/$project_name.json"
  intent plan-structure "$req_file" --project="$project_name" > "structures/$project_name.json"
  intent generate-beads "$req_file" --project="$project_name"

  echo "✓ $project_name complete"
done

echo "All projects processed. Review in bd:"
bd list --status=open
```

## Future Enhancements

Planned features for future releases:

- [ ] Resume from checkpoint (load saved state)
- [ ] Interactive editing at review gates
- [ ] Automatic dependency inference from contracts
- [ ] Multi-project planning support
- [ ] Export to other formats (Jira, GitHub Issues, Linear)
- [ ] AI-assisted requirement refinement
- [ ] Contract test generation
- [ ] Parallel bead creation
- [ ] Rollback capabilities
- [ ] Planning analytics and insights

## References

### EARS Specification

- [EARS: Easy Approach to Requirements Syntax](https://ieeexplore.ieee.org/document/5328509)
- Mavin, A., et al. (2009). "Easy Approach to Requirements Syntax (EARS)"

### Design by Contract

- Meyer, B. (1992). "Applying 'Design by Contract'"
- [DbC Methodology](https://en.wikipedia.org/wiki/Design_by_contract)

### Mental Models

- Munger, C. (1994). "A Lesson on Elementary, Worldly Wisdom"
- [Farnam Street Mental Models](https://fs.blog/mental-models/)

### bd Issue Tracker

- [bd GitHub Repository](https://github.com/steveyegge/beads)
- [bd Documentation](https://github.com/steveyegge/beads/blob/main/README.md)

## Support

For issues, questions, or contributions:

- GitHub Issues: [intent-cli/issues](https://github.com/your-org/intent-cli/issues)
- Documentation: [intent-cli/docs](https://github.com/your-org/intent-cli/tree/main/docs)
- Discord: [Join our community](https://discord.gg/intent-cli)

## License

Intent CLI is licensed under the MIT License. See LICENSE file for details.

---

**Last Updated**: 2026-01-16
**Version**: 1.0.0
**Maintainer**: Intent CLI Team
