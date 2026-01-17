#!/usr/bin/env bash
#
# Build release binary for all platforms (macOS, Linux, Windows)
#
# The Intent CLI uses Erlang escripts which are cross-platform.
# The same binary works on macOS (Intel + Apple Silicon), Linux, and Windows
# as long as Erlang/OTP is installed.
#
# Usage:
#   ./scripts/build-release.sh [version]
#
# Example:
#   ./scripts/build-release.sh 0.1.0

set -euo pipefail

VERSION="${1:-dev}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
DIST_DIR="$PROJECT_ROOT/dist"
RELEASE_DIR="$DIST_DIR/release/$VERSION"

echo "Building Intent CLI release v$VERSION"
echo "======================================"
echo ""

# Clean previous dist builds only
echo "Cleaning previous release builds..."
rm -rf "$DIST_DIR/release"

# Build or use existing escript
echo "Preparing escript binary..."
cd "$PROJECT_ROOT"

# If intent binary doesn't exist in project root, try to build or copy it
if [ ! -f "$PROJECT_ROOT/intent" ]; then
    echo "No existing binary found, attempting to build..."
    if gleam run -m gleescript 2>&1 | grep -q "Generated"; then
        echo "Built escript successfully"
    elif [ -f "$HOME/.local/bin/intent" ]; then
        echo "Build failed, using installed binary from ~/.local/bin/intent"
        cp "$HOME/.local/bin/intent" "$PROJECT_ROOT/intent"
    else
        echo "Error: Cannot build or find intent binary"
        echo "Please run 'gleam build' first or install a working binary"
        exit 1
    fi
else
    echo "Using existing intent binary"
fi

# Verify the binary exists
if [ ! -f "$PROJECT_ROOT/intent" ]; then
    echo "Error: intent binary not found"
    exit 1
fi

# Verify the binary works
echo "Verifying binary..."
if ! "$PROJECT_ROOT/intent" --help > /dev/null 2>&1; then
    echo "Error: Binary verification failed - does not execute correctly"
    echo "Try removing ./intent and running this script again to rebuild"
    exit 1
fi

# Create release directory structure
echo "Creating release directory..."
mkdir -p "$RELEASE_DIR"

# Copy binary to release directory
cp "$PROJECT_ROOT/intent" "$RELEASE_DIR/intent"
chmod +x "$RELEASE_DIR/intent"

# Create platform-specific packages
echo "Creating platform packages..."

# macOS (universal - works on both Intel and Apple Silicon)
mkdir -p "$RELEASE_DIR/macos"
cp "$RELEASE_DIR/intent" "$RELEASE_DIR/macos/intent"
cat > "$RELEASE_DIR/macos/README.txt" << 'EOF'
Intent CLI for macOS
====================

This binary works on both Intel and Apple Silicon Macs.

Prerequisites:
- Erlang/OTP 27 or later must be installed

Installation via Homebrew:
    brew install erlang

Installation via mise/asdf:
    mise install erlang@27
    # or
    asdf install erlang 27.0

Quick Install:
    chmod +x intent
    sudo mv intent /usr/local/bin/
    intent --help

Or add to PATH:
    export PATH="$PATH:$(pwd)"
    intent --help

Verification:
    erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell
    # Should show "27" or higher

Usage:
    intent --help
EOF

# Linux
mkdir -p "$RELEASE_DIR/linux"
cp "$RELEASE_DIR/intent" "$RELEASE_DIR/linux/intent"
cat > "$RELEASE_DIR/linux/README.txt" << 'EOF'
Intent CLI for Linux
====================

Prerequisites:
- Erlang/OTP 27 or later must be installed

Installation (Ubuntu/Debian):
    sudo apt-get update
    sudo apt-get install erlang

Installation (Fedora/RHEL):
    sudo dnf install erlang

Installation via mise/asdf:
    mise install erlang@27
    # or
    asdf install erlang 27.0

Quick Install:
    chmod +x intent
    sudo mv intent /usr/local/bin/
    intent --help

Or add to PATH:
    export PATH="$PATH:$(pwd)"
    intent --help

Verification:
    erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell
    # Should show "27" or higher

Usage:
    intent --help
EOF

# Windows
mkdir -p "$RELEASE_DIR/windows"
cp "$RELEASE_DIR/intent" "$RELEASE_DIR/windows/intent"
cat > "$RELEASE_DIR/windows/README.txt" << 'EOF'
Intent CLI for Windows
======================

Prerequisites:
- Erlang/OTP 27 or later must be installed
- Download from: https://www.erlang.org/downloads

Installation:
1. Install Erlang from https://www.erlang.org/downloads
2. Add Erlang bin directory to PATH (usually C:\Program Files\erl-27.0\bin)
3. Run: escript intent --help

Usage:
    escript intent --help

Note: On Windows, you must prefix the command with 'escript':
    escript intent check spec.cue --target http://localhost:8080
EOF

# Create checksums
echo "Generating checksums..."
cd "$RELEASE_DIR"
shasum -a 256 macos/intent > macos/SHA256SUM
shasum -a 256 linux/intent > linux/SHA256SUM
shasum -a 256 windows/intent > windows/SHA256SUM

# Create tarballs
echo "Creating archives..."
cd "$RELEASE_DIR"
tar czf "intent-$VERSION-macos.tar.gz" macos/
tar czf "intent-$VERSION-linux.tar.gz" linux/
tar czf "intent-$VERSION-windows.tar.gz" windows/

# Generate release checksums
shasum -a 256 *.tar.gz > SHA256SUMS

echo ""
echo "Build complete!"
echo "==============="
echo ""
echo "Release artifacts in: $RELEASE_DIR"
echo ""
echo "Files created:"
ls -lh "$RELEASE_DIR"/*.tar.gz
echo ""
echo "Checksums:"
cat "$RELEASE_DIR/SHA256SUMS"
echo ""
echo "Next steps:"
echo "1. Test the binary on each platform"
echo "2. Create GitHub release with these artifacts"
echo "3. Update documentation with download links"
