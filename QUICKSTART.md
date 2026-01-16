# Intent CLI Quick Start

Get started with the Intent planning workflow in 5 minutes.

## Prerequisites

- [Gleam](https://gleam.run/) installed
- [bd (beads)](https://github.com/steveyegge/beads) issue tracker installed
- Git repository initialized

## Installation

```bash
# Clone repository
git clone https://github.com/your-org/intent-cli.git
cd intent-cli

# Build
gleam build

# Add to PATH (optional)
export PATH="$PWD/build/dev/erlang/intent/ebin:$PATH"
```

## Your First Planning Session

### 1. Create Requirements File

Create `example-requirements.txt`:

```
THE SYSTEM SHALL authenticate users with email and password

WHEN a user submits valid credentials THE SYSTEM SHALL return a 200 OK with JWT token

WHEN a user submits invalid credentials THE SYSTEM SHALL return a 401 Unauthorized

IF the request lacks a valid JWT token THEN THE SYSTEM SHALL NOT process the request
```

### 2. Review Requirements

```bash
gleam run -- review-requirements example-requirements.txt --checkpoint
```

**Output**: Requirements with validation questions.

**Review**: Check if patterns are appropriate and requirements are clear.

### 3. Generate Contracts

```bash
gleam run -- generate-contract example-requirements.txt
```

**Output**: KIRK contracts with preconditions, postconditions, and second-order effects.

### 4. Review Contracts

```bash
gleam run -- review-contracts example-requirements.txt --checkpoint
```

**Review**: Verify preconditions capture all inputs, postconditions are testable.

### 5. Plan Structure

```bash
gleam run -- plan-structure example-requirements.txt --project=AuthDemo
```

**Output**: Epic/feature/task hierarchy with wave-based dependencies.

### 6. Review Structure

```bash
gleam run -- review-structure example-requirements.txt --project=AuthDemo --checkpoint
```

**Review**: Check if hierarchy is logical and wave ordering is correct.

### 7. Generate Beads

```bash
gleam run -- generate-beads example-requirements.txt --project=AuthDemo
```

**Output**: Beads created in bd database with proper hierarchy.

### 8. View Beads

```bash
# List all open beads
bd list --status=open

# Show ready work
bd ready

# View bead details
bd show <id>
```

### 9. Start Working

```bash
# Claim a task
bd update <id> --status=in_progress

# After completing work
bd close <id> --reason="Implemented authentication endpoint"

# Find next work
bd ready
```

## What You Just Did

1. **Captured requirements** using EARS syntax (5 patterns)
2. **Generated contracts** with Design-by-Contract methodology
3. **Planned structure** with epic/feature/task hierarchy
4. **Created beads** in bd database with dependencies
5. **Reviewed at each stage** with validation questions
6. **Saved checkpoints** for resumption and iteration

## Next Steps

### Learn More

- Read [PLANNING_WORKFLOW.md](PLANNING_WORKFLOW.md) for complete guide
- Explore [examples/](examples/) directory for more requirement files
- Try Mental Lattice analysis: `gleam run -- lattice-analyze example-requirements.txt`

### Try Advanced Features

```bash
# Apply specific mental model
gleam run -- lattice-analyze example-requirements.txt --model=inversion

# Output as JSON
gleam run -- generate-contract example-requirements.txt --json

# Batch processing
for file in requirements/*.txt; do
  gleam run -- generate-beads "$file" --project=$(basename "$file" .txt)
done
```

### Customize Your Workflow

- Add custom validation questions to `src/intent/review_gates.gleam`
- Extend domain classification in `src/intent/structure_planner.gleam`
- Create VS Code tasks for common commands
- Set up Git hooks for requirement validation

## Common Commands

| Task | Command |
|------|---------|
| Review requirements | `gleam run -- review-requirements <file> --checkpoint` |
| Generate contracts | `gleam run -- generate-contract <file>` |
| Plan structure | `gleam run -- plan-structure <file> --project=<name>` |
| Generate beads | `gleam run -- generate-beads <file> --project=<name>` |
| View ready work | `bd ready` |
| Show bead | `bd show <id>` |
| Update bead | `bd update <id> --status=in_progress` |
| Close bead | `bd close <id> --reason="..."` |

## Tips

1. **Start small**: Try 3-5 requirements first
2. **Use checkpoints**: Always add `--checkpoint` flag during reviews
3. **Check confidence**: Low scores indicate ambiguous requirements
4. **Monitor parallelism**: Aim for >0.5 score for efficient work distribution
5. **Read contracts**: Task beads embed full KIRK contracts - use them!

## Troubleshooting

### "No requirements found"
- Check EARS syntax starts with proper keywords
- Verify file exists and is readable

### "Bead creation failed"
- Ensure bd is installed: `which bd`
- Check bd database: `bd stats`

### "Low confidence scores"
- Add more specific details to requirements
- Include HTTP codes and field names explicitly

## Get Help

- Full documentation: [PLANNING_WORKFLOW.md](PLANNING_WORKFLOW.md)
- GitHub Issues: [intent-cli/issues](https://github.com/your-org/intent-cli/issues)
- Examples: [examples/](examples/) directory

---

**Ready to plan your project?** Start with `example-requirements.txt` and follow the steps above!
