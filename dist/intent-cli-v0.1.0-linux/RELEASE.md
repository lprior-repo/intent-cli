# Intent CLI v0.1.0 - Linux Release

Contract-driven API testing and requirements engineering CLI tool.

## System Requirements

- **Erlang/OTP**: Version 24 or later (recommended: 28)
- **Linux**: Any modern distribution (Ubuntu 20.04+, Fedora 35+, Arch, etc.)
- **Architecture**: x86_64 (amd64)
- **Disk Space**: ~3 MB

## What's Included

- `intent` - Self-contained executable escript (2.2 MB)
- `install.sh` - Automated installation script
- `README.md` - Project documentation
- `LICENSE` - Apache 2.0 license

## Installation

### Option 1: Quick Install (Recommended)

```bash
./install.sh
```

This will:
1. Check for Erlang/OTP
2. Install `intent` to `~/.local/bin/`
3. Set executable permissions

### Option 2: Manual Install

```bash
# Copy binary to any directory in your PATH
cp intent ~/.local/bin/intent
chmod +x ~/.local/bin/intent
```

### Option 3: System-Wide Install (requires sudo)

```bash
sudo cp intent /usr/local/bin/intent
sudo chmod +x /usr/local/bin/intent
```

### Verify Installation

```bash
intent --help
```

## Installing Erlang/OTP

If you don't have Erlang installed:

### Ubuntu/Debian
```bash
sudo apt update
sudo apt install erlang-nox  # Minimal Erlang without GUI dependencies
# Or full Erlang:
sudo apt install erlang
```

### Fedora/RHEL/CentOS
```bash
sudo dnf install erlang
```

### Arch Linux
```bash
sudo pacman -S erlang
```

### Alpine Linux
```bash
apk add erlang
```

### Using ASDF (recommended for developers)
```bash
asdf plugin add erlang
asdf install erlang latest
asdf global erlang latest
```

### Using Docker (no local Erlang needed)
```bash
docker run -it --rm -v "$(pwd):/work" -w /work erlang:28 ./intent --help
```

## Quick Start Guide

### 1. Interactive API Specification Interview
```bash
intent interview --profile api
```

This starts a structured interview that guides you through creating a complete API specification using EARS patterns.

### 2. Validate an Existing Spec
```bash
intent validate examples/user-api.cue
```

### 3. Run API Tests Against Live Endpoint
```bash
intent check spec.cue --target http://localhost:8080
```

### 4. Analyze Specification Quality
```bash
# Multi-dimensional quality scoring
intent quality spec.cue

# Find missing requirements
intent gaps spec.cue

# Discover failure modes
intent invert spec.cue

# Check HTTP coverage
intent coverage spec.cue
```

### 5. Generate Work Items (Beads)
```bash
# After completing an interview
intent beads <session-id>
```

## Core Features

### KIRK Analysis Framework
- **Quality Scoring**: Multi-dimensional spec quality analysis
- **Gap Detection**: Mental lattice-based requirement discovery
- **Inversion Thinking**: "What could fail?" analysis
- **Coverage Analysis**: HTTP method/status code coverage + OWASP Top 10

### Interview-Driven Development
- Structured Q&A using EARS patterns
- Profile-based interviews (API, CLI, Event, Data, Workflow, UI)
- Automatic CUE schema generation
- Session resumability

### Contract-Driven Testing
- Behavior-level API testing
- JSONPath assertions
- Dependency-aware test execution
- Human-readable and JSON output

### Bead Generation
- Atomic, executable work items
- Dependency tracking
- Integration with bd (beads tracker)

## EARS Patterns

Intent uses EARS (Easy Approach to Requirements Syntax):

```
THE SYSTEM SHALL authenticate users via JWT tokens
WHEN user submits login, THE SYSTEM SHALL return access token
WHILE session is active, THE SYSTEM SHALL authorize requests
IF rate limit exceeded, THEN THE SYSTEM SHALL return 429 status
WHERE admin role enabled, THE SYSTEM SHALL allow user management
```

## Example Workflow

```bash
# 1. Create spec via interview
intent interview --profile api --export user-api.cue

# 2. Analyze quality
intent quality user-api.cue
intent gaps user-api.cue

# 3. Run tests
intent check user-api.cue --target http://localhost:3000

# 4. Generate implementation work items
intent beads <session-id>
```

## Exit Codes

- **0**: Success
- **1**: Test failures
- **2**: Blocked behaviors (dependencies failed)
- **3**: Invalid specification
- **4**: General error (file not found, network error, etc.)

## File Locations

Intent uses these directories:

- `.interview/` - Interview sessions and generated specs
- `.beads/` - Generated work items for bd tracker
- `examples/` - Sample specifications

## Troubleshooting

### "escript: command not found"
Install Erlang/OTP using the instructions above.

### "Bad magic number" error
Your Erlang version may be incompatible. This binary was built with Erlang/OTP 28.
Try upgrading to Erlang 24 or later.

### Binary doesn't run on older Linux distributions
The escript requires a modern Linux kernel. Tested on:
- Ubuntu 20.04+
- Fedora 35+
- Arch Linux (current)
- Alpine 3.16+

## Documentation

Full documentation:
- Repository: https://github.com/lprior-repo/intent-cli
- EARS Guide: `docs/EARS_KIRK_WORKFLOW.md`
- AI Protocol: `docs/AI_PROTOCOL_QUICKSTART.md`
- API Reference: `docs/API_REFERENCE.md`

## License

Apache 2.0 License - See LICENSE file

## Support

For issues and questions:
- GitHub Issues: https://github.com/lprior-repo/intent-cli/issues

---

**Release Info:**
- Version: 0.1.0
- Built with: Gleam (targeting Erlang)
- Runtime: Erlang/OTP 28
- Platform: Linux x86_64
- Binary Type: Escript (self-contained Erlang bytecode)
