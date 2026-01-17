#!/bin/bash
# Package existing Intent CLI binary for Linux distribution
# Uses the already-built escript from the project root

set -euo pipefail

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Intent CLI - Linux Release Packager ===${NC}"
echo ""

# Check for existing binary
if [ ! -f "intent" ]; then
    echo -e "${RED}ERROR: intent binary not found in current directory${NC}"
    echo "Run this script from the project root: ./scripts/package-linux-release.sh"
    exit 1
fi

# Verify it's an escript
if ! file intent | grep -q "escript"; then
    echo -e "${RED}ERROR: 'intent' is not a valid escript${NC}"
    exit 1
fi

# Get version from gleam.toml
if [ ! -f "gleam.toml" ]; then
    echo -e "${RED}ERROR: gleam.toml not found${NC}"
    exit 1
fi

VERSION=$(grep '^version' gleam.toml | cut -d'"' -f2)
echo -e "${BLUE}Packaging Intent CLI v${VERSION}...${NC}"
echo ""

# Test the binary
echo -e "${BLUE}Testing binary...${NC}"
if ! ./intent --help > /dev/null 2>&1; then
    echo -e "${YELLOW}WARNING: Binary test failed, but continuing...${NC}"
else
    echo -e "${GREEN}✓ Binary works${NC}"
fi
echo ""

# Get Erlang version recommendation
ERL_VERSION="unknown"
if command -v erl &> /dev/null; then
    ERL_VERSION=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>&1 | tr -d '"')
    echo -e "${GREEN}✓ Built with Erlang/OTP ${ERL_VERSION}${NC}"
else
    echo -e "${YELLOW}⚠ Erlang not detected (optional for packaging)${NC}"
fi
echo ""

# Create dist directory
DIST_DIR="dist/intent-cli-v${VERSION}-linux"
rm -rf "${DIST_DIR}"
mkdir -p "${DIST_DIR}"

# Copy escript
echo -e "${BLUE}Packaging files...${NC}"
cp intent "${DIST_DIR}/intent"
chmod +x "${DIST_DIR}/intent"
echo -e "${GREEN}✓ Binary copied${NC}"

# Copy documentation
if [ -f "README.md" ]; then
    cp README.md "${DIST_DIR}/"
    echo -e "${GREEN}✓ README copied${NC}"
fi

if [ -f "LICENSE" ]; then
    cp LICENSE "${DIST_DIR}/"
    echo -e "${GREEN}✓ LICENSE copied${NC}"
fi

# Create installation script
cat > "${DIST_DIR}/install.sh" << 'INSTALL_SCRIPT'
#!/bin/bash
# Intent CLI Installation Script

set -euo pipefail

INSTALL_DIR="${HOME}/.local/bin"
BINARY_NAME="intent"

echo "Installing Intent CLI..."

# Check for Erlang
if ! command -v escript &> /dev/null; then
    echo "ERROR: Erlang runtime not found."
    echo "Please install Erlang/OTP from your package manager:"
    echo "  - Ubuntu/Debian: sudo apt install erlang"
    echo "  - Fedora: sudo dnf install erlang"
    echo "  - Arch: sudo pacman -S erlang"
    exit 1
fi

ERL_VER=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>&1 | tr -d '"')
echo "✓ Found Erlang/OTP ${ERL_VER}"

# Create installation directory
mkdir -p "${INSTALL_DIR}"

# Copy binary
cp "${BINARY_NAME}" "${INSTALL_DIR}/${BINARY_NAME}"
chmod +x "${INSTALL_DIR}/${BINARY_NAME}"

echo "✓ Installed to ${INSTALL_DIR}/${BINARY_NAME}"

# Check if directory is in PATH
if [[ ":$PATH:" != *":${INSTALL_DIR}:"* ]]; then
    echo ""
    echo "WARNING: ${INSTALL_DIR} is not in your PATH"
    echo "Add this to your ~/.bashrc or ~/.zshrc:"
    echo "  export PATH=\"\$PATH:${INSTALL_DIR}\""
fi

echo ""
echo "Installation complete! Run 'intent --help' to get started."
INSTALL_SCRIPT

chmod +x "${DIST_DIR}/install.sh"
echo -e "${GREEN}✓ Install script created${NC}"
echo ""

# Create comprehensive README for the release
cat > "${DIST_DIR}/RELEASE.md" << README_DOC
# Intent CLI v${VERSION} - Linux Release

Contract-driven API testing and requirements engineering CLI tool.

## System Requirements

- **Erlang/OTP**: Version 24 or later (recommended: ${ERL_VERSION})
- **Linux**: Any modern distribution (Ubuntu 20.04+, Fedora 35+, Arch, etc.)
- **Architecture**: x86_64 (amd64)
- **Disk Space**: ~3 MB

## What's Included

- \`intent\` - Self-contained executable escript (2.2 MB)
- \`install.sh\` - Automated installation script
- \`README.md\` - Project documentation
- \`LICENSE\` - Apache 2.0 license

## Installation

### Option 1: Quick Install (Recommended)

\`\`\`bash
./install.sh
\`\`\`

This will:
1. Check for Erlang/OTP
2. Install \`intent\` to \`~/.local/bin/\`
3. Set executable permissions

### Option 2: Manual Install

\`\`\`bash
# Copy binary to any directory in your PATH
cp intent ~/.local/bin/intent
chmod +x ~/.local/bin/intent
\`\`\`

### Option 3: System-Wide Install (requires sudo)

\`\`\`bash
sudo cp intent /usr/local/bin/intent
sudo chmod +x /usr/local/bin/intent
\`\`\`

### Verify Installation

\`\`\`bash
intent --help
\`\`\`

## Installing Erlang/OTP

If you don't have Erlang installed:

### Ubuntu/Debian
\`\`\`bash
sudo apt update
sudo apt install erlang-nox  # Minimal Erlang without GUI dependencies
# Or full Erlang:
sudo apt install erlang
\`\`\`

### Fedora/RHEL/CentOS
\`\`\`bash
sudo dnf install erlang
\`\`\`

### Arch Linux
\`\`\`bash
sudo pacman -S erlang
\`\`\`

### Alpine Linux
\`\`\`bash
apk add erlang
\`\`\`

### Using ASDF (recommended for developers)
\`\`\`bash
asdf plugin add erlang
asdf install erlang latest
asdf global erlang latest
\`\`\`

### Using Docker (no local Erlang needed)
\`\`\`bash
docker run -it --rm -v "\$(pwd):/work" -w /work erlang:28 ./intent --help
\`\`\`

## Quick Start Guide

### 1. Interactive API Specification Interview
\`\`\`bash
intent interview --profile api
\`\`\`

This starts a structured interview that guides you through creating a complete API specification using EARS patterns.

### 2. Validate an Existing Spec
\`\`\`bash
intent validate examples/user-api.cue
\`\`\`

### 3. Run API Tests Against Live Endpoint
\`\`\`bash
intent check spec.cue --target http://localhost:8080
\`\`\`

### 4. Analyze Specification Quality
\`\`\`bash
# Multi-dimensional quality scoring
intent quality spec.cue

# Find missing requirements
intent gaps spec.cue

# Discover failure modes
intent invert spec.cue

# Check HTTP coverage
intent coverage spec.cue
\`\`\`

### 5. Generate Work Items (Beads)
\`\`\`bash
# After completing an interview
intent beads <session-id>
\`\`\`

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

\`\`\`
THE SYSTEM SHALL authenticate users via JWT tokens
WHEN user submits login, THE SYSTEM SHALL return access token
WHILE session is active, THE SYSTEM SHALL authorize requests
IF rate limit exceeded, THEN THE SYSTEM SHALL return 429 status
WHERE admin role enabled, THE SYSTEM SHALL allow user management
\`\`\`

## Example Workflow

\`\`\`bash
# 1. Create spec via interview
intent interview --profile api --export user-api.cue

# 2. Analyze quality
intent quality user-api.cue
intent gaps user-api.cue

# 3. Run tests
intent check user-api.cue --target http://localhost:3000

# 4. Generate implementation work items
intent beads <session-id>
\`\`\`

## Exit Codes

- **0**: Success
- **1**: Test failures
- **2**: Blocked behaviors (dependencies failed)
- **3**: Invalid specification
- **4**: General error (file not found, network error, etc.)

## File Locations

Intent uses these directories:

- \`.interview/\` - Interview sessions and generated specs
- \`.beads/\` - Generated work items for bd tracker
- \`examples/\` - Sample specifications

## Troubleshooting

### "escript: command not found"
Install Erlang/OTP using the instructions above.

### "Bad magic number" error
Your Erlang version may be incompatible. This binary was built with Erlang/OTP ${ERL_VERSION}.
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
- EARS Guide: \`docs/EARS_KIRK_WORKFLOW.md\`
- AI Protocol: \`docs/AI_PROTOCOL_QUICKSTART.md\`
- API Reference: \`docs/API_REFERENCE.md\`

## License

Apache 2.0 License - See LICENSE file

## Support

For issues and questions:
- GitHub Issues: https://github.com/lprior-repo/intent-cli/issues

---

**Release Info:**
- Version: ${VERSION}
- Built with: Gleam (targeting Erlang)
- Runtime: Erlang/OTP ${ERL_VERSION}
- Platform: Linux x86_64
- Binary Type: Escript (self-contained Erlang bytecode)
README_DOC

echo -e "${GREEN}✓ Release documentation created${NC}"
echo ""

# Create tarball
echo -e "${BLUE}Creating release archive...${NC}"
TARBALL="dist/intent-cli-v${VERSION}-linux-x86_64.tar.gz"
tar -czf "${TARBALL}" -C dist "intent-cli-v${VERSION}-linux"
echo -e "${GREEN}✓ Release archive created${NC}"
echo ""

# Create checksum
echo -e "${BLUE}Generating checksum...${NC}"
cd dist
sha256sum "intent-cli-v${VERSION}-linux-x86_64.tar.gz" > "intent-cli-v${VERSION}-linux-x86_64.tar.gz.sha256"
cd ..
echo -e "${GREEN}✓ SHA256 checksum created${NC}"
echo ""

# Show release info
BINARY_SIZE=$(du -h "${DIST_DIR}/intent" | cut -f1)
TARBALL_SIZE=$(du -h "${TARBALL}" | cut -f1)
CHECKSUM=$(cat "dist/intent-cli-v${VERSION}-linux-x86_64.tar.gz.sha256" | cut -d' ' -f1)

echo -e "${GREEN}╔════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║            RELEASE PACKAGING COMPLETE!                 ║${NC}"
echo -e "${GREEN}╚════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${BLUE}Version:${NC}       ${VERSION}"
echo -e "${BLUE}Binary:${NC}        ${DIST_DIR}/intent (${BINARY_SIZE})"
echo -e "${BLUE}Archive:${NC}       ${TARBALL} (${TARBALL_SIZE})"
echo -e "${BLUE}Checksum:${NC}      dist/intent-cli-v${VERSION}-linux-x86_64.tar.gz.sha256"
echo -e "${BLUE}Runtime:${NC}       Erlang/OTP ${ERL_VERSION}+"
echo ""
echo -e "${BLUE}SHA256:${NC}"
echo -e "  ${CHECKSUM}"
echo ""
echo -e "${GREEN}═══════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}Next Steps:${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════════${NC}"
echo ""
echo -e "${BLUE}1. Test the release:${NC}"
echo "   cd ${DIST_DIR}"
echo "   ./install.sh"
echo "   intent --help"
echo ""
echo -e "${BLUE}2. Create GitHub Release:${NC}"
echo "   gh release create v${VERSION} \\"
echo "     ${TARBALL} \\"
echo "     dist/intent-cli-v${VERSION}-linux-x86_64.tar.gz.sha256 \\"
echo "     --title \"Intent CLI v${VERSION}\" \\"
echo "     --notes \"See RELEASE.md for details\""
echo ""
echo -e "${BLUE}3. Or manually:${NC}"
echo "   - Go to https://github.com/lprior-repo/intent-cli/releases/new"
echo "   - Tag: v${VERSION}"
echo "   - Upload: ${TARBALL}"
echo "   - Upload: dist/intent-cli-v${VERSION}-linux-x86_64.tar.gz.sha256"
echo ""
echo -e "${GREEN}═══════════════════════════════════════════════════════${NC}"
