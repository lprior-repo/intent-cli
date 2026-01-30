# Intent CLI - AI Agent Instructions

**Documentation:** See `.beads/AGENTS.jsonl` for complete reference (JSONL format for token efficiency).

**Tagline:** Human-writes, AI-verifies, AI-implements.

**Quick start:**
```bash
bd ready                    # Find unblocked work
bv --robot-triage          # AI triage (everything in one call)
bv --robot-next            # Single top pick

# Workspace isolation
zjj add <bead-id>          # Create isolated workspace
zjj done <workspace>       # Complete and merge

# Session completion (MANDATORY)
git pull --rebase && bd sync && git push
```

**Tech Stack:** Gleam (compiles to Erlang/OTP), CUE for specifications
