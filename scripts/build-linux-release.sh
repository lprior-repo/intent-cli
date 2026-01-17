#!/bin/bash
# Build Linux release binary for Intent CLI
# This script creates a self-contained escript that can run on any Linux system with Erlang installed

set -euo pipefail

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Intent CLI - Linux Release Builder ===${NC}"
echo ""

# Check for required tools
echo -e "${BLUE}Checking dependencies...${NC}"

if ! command -v gleam &> /dev/null; then
    echo -e "${RED}ERROR: gleam not found. Install from https://gleam.run${NC}"
    exit 1
fi

if ! command -v erl &> /dev/null; then
    echo -e "${RED}ERROR: Erlang runtime not found. Install Erlang/OTP${NC}"
    exit 1
fi

GLEAM_VERSION=$(gleam --version | cut -d' ' -f2)
ERL_VERSION=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>&1 | tr -d '"')

echo -e "${GREEN}✓ Gleam ${GLEAM_VERSION}${NC}"
echo -e "${GREEN}✓ Erlang/OTP ${ERL_VERSION}${NC}"
echo ""

# Get version from gleam.toml
VERSION=$(grep '^version' gleam.toml | cut -d'"' -f2)
echo -e "${BLUE}Building Intent CLI v${VERSION}...${NC}"
echo ""

# Build escript
echo -e "${BLUE}Step 1: Compiling Gleam code...${NC}"
if ! gleam build --target erlang; then
    echo -e "${RED}ERROR: Gleam build failed${NC}"
    exit 1
fi
echo -e "${GREEN}✓ Compilation successful${NC}"
echo ""

echo -e "${BLUE}Step 2: Generating escript...${NC}"
if ! gleam run -m gleescript; then
    echo -e "${RED}ERROR: Escript generation failed${NC}"
    exit 1
fi
echo -e "${GREEN}✓ Escript generated${NC}"
echo ""

# Create dist directory
DIST_DIR="dist/intent-cli-v${VERSION}-linux"
mkdir -p "${DIST_DIR}"

# Copy escript
cp intent "${DIST_DIR}/intent"
chmod +x "${DIST_DIR}/intent"

# Copy documentation
cp README.md "${DIST_DIR}/" 2>/dev/null || echo "# Intent CLI v${VERSION}" > "${DIST_DIR}/README.md"
cp LICENSE "${DIST_DIR}/" 2>/dev/null || true

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

# Create README for the release
cat > "${DIST_DIR}/README.md" << README_DOC
# Intent CLI v${VERSION} - Linux Release

Contract-driven API testing and requirements engineering CLI tool.

## System Requirements

- **Erlang/OTP**: Version 24 or later (recommended: 28+)
- **Linux**: Any modern distribution (Ubuntu, Fedora, Arch, etc.)
- **Architecture**: x86_64 (amd64)

## Installation

### Quick Install

\`\`\`bash
./install.sh
\`\`\`

This will install the \`intent\` binary to \`~/.local/bin/intent\`.

### Manual Install

\`\`\`bash
# Copy binary to any directory in your PATH
cp intent ~/.local/bin/intent
chmod +x ~/.local/bin/intent
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
sudo apt install erlang
\`\`\`

### Fedora
\`\`\`bash
sudo dnf install erlang
\`\`\`

### Arch Linux
\`\`\`bash
sudo pacman -S erlang
\`\`\`

### Using ASDF (recommended for developers)
\`\`\`bash
asdf plugin add erlang
asdf install erlang latest
asdf global erlang latest
\`\`\`

## Quick Start

### 1. Start an Interactive Interview
\`\`\`bash
intent interview --profile api
\`\`\`

### 2. Validate a Spec
\`\`\`bash
intent validate examples/user-api.cue
\`\`\`

### 3. Run API Tests
\`\`\`bash
intent check spec.cue --target http://localhost:8080
\`\`\`

### 4. Analyze Spec Quality
\`\`\`bash
intent quality spec.cue
intent gaps spec.cue
intent invert spec.cue
\`\`\`

## What is Intent CLI?

Intent transforms vague requirements into deterministic, testable specifications:

1. **Interview Mode**: Structured Q&A using EARS patterns (THE SYSTEM SHALL...)
2. **KIRK Analysis**: Quality scoring, gap detection, inversion thinking
3. **API Testing**: Contract-driven behavior verification
4. **Bead Generation**: Atomic work items for AI or human execution

## Documentation

Full documentation available at:
- Repository: https://github.com/lprior-repo/intent-cli
- EARS Guide: \`docs/EARS_KIRK_WORKFLOW.md\`
- AI Protocol: \`docs/AI_PROTOCOL_QUICKSTART.md\`

## License

Apache 2.0 License - See LICENSE file

## Support

For issues and questions:
- GitHub Issues: https://github.com/lprior-repo/intent-cli/issues

---

Built with Gleam v${GLEAM_VERSION} | Erlang/OTP ${ERL_VERSION}
README_DOC

# Create tarball
echo -e "${BLUE}Step 3: Creating release archive...${NC}"
TARBALL="dist/intent-cli-v${VERSION}-linux-x86_64.tar.gz"
tar -czf "${TARBALL}" -C dist "intent-cli-v${VERSION}-linux"
echo -e "${GREEN}✓ Release archive created${NC}"
echo ""

# Show release info
BINARY_SIZE=$(du -h "${DIST_DIR}/intent" | cut -f1)
TARBALL_SIZE=$(du -h "${TARBALL}" | cut -f1)

echo -e "${GREEN}=== Release Complete ===${NC}"
echo ""
echo "  Version: ${VERSION}"
echo "  Binary:  ${DIST_DIR}/intent (${BINARY_SIZE})"
echo "  Archive: ${TARBALL} (${TARBALL_SIZE})"
echo ""
echo -e "${BLUE}To test the release:${NC}"
echo "  cd ${DIST_DIR}"
echo "  ./install.sh"
echo "  intent --help"
echo ""
echo -e "${BLUE}To distribute:${NC}"
echo "  Upload ${TARBALL} to GitHub Releases"
echo ""
