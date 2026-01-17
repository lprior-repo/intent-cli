#!/usr/bin/env bash
# Build Windows-compatible binaries for Intent CLI
# This script creates both escript and erlang-shipment packages for Windows

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DIST_DIR="$PROJECT_ROOT/dist/windows"

echo "Building Intent CLI for Windows..."

# Clean previous builds
echo "Cleaning previous Windows builds..."
rm -rf "$DIST_DIR"
mkdir -p "$DIST_DIR/escript"
mkdir -p "$DIST_DIR/shipment"

# Check if we should build or use existing binary
if [ "${USE_EXISTING_BINARY:-0}" = "1" ]; then
    echo "Using existing binary from ~/.local/bin/intent or dist/intent/intent..."
    # Try to find existing binary
    if [ -f "$HOME/.local/bin/intent" ]; then
        ESCRIPT_PATH="$HOME/.local/bin/intent"
        echo "Found binary at: $ESCRIPT_PATH"
    elif [ -f "$PROJECT_ROOT/dist/intent/intent" ]; then
        ESCRIPT_PATH="$PROJECT_ROOT/dist/intent/intent"
        echo "Found binary at: $ESCRIPT_PATH"
    else
        echo "Error: No existing binary found. Please build first or set USE_EXISTING_BINARY=0"
        exit 1
    fi
else
    # Build the project
    echo "Compiling Gleam project..."
    cd "$PROJECT_ROOT"
    gleam build --target erlang

    # Create escript binary
    echo "Creating escript binary..."
    gleam run -m gleescript

    ESCRIPT_PATH="$PROJECT_ROOT/dist/intent/intent"
fi

# Copy escript and create Windows wrapper
if [ -f "$ESCRIPT_PATH" ]; then
    echo "Copying escript to Windows distribution..."
    cp "$ESCRIPT_PATH" "$DIST_DIR/escript/intent"

    # Create Windows batch wrapper
    cat > "$DIST_DIR/escript/intent.cmd" << 'EOF'
@echo off
setlocal
set intentscript=%~dp0intent
escript.exe "%intentscript%" %*
EOF

    # Create PowerShell wrapper
    cat > "$DIST_DIR/escript/intent.ps1" << 'EOF'
#!/usr/bin/env pwsh
# Intent CLI PowerShell wrapper for Windows
$scriptPath = Join-Path $PSScriptRoot "intent"
& escript.exe $scriptPath $args
EOF

    chmod +x "$DIST_DIR/escript/intent.cmd"
    chmod +x "$DIST_DIR/escript/intent.ps1"

    echo "Escript created successfully!"
else
    echo "Error: escript binary not found at dist/intent/intent"
    exit 1
fi

# Create erlang-shipment package
echo "Creating erlang-shipment package..."
gleam export erlang-shipment

if [ -d "$PROJECT_ROOT/build/erlang-shipment" ]; then
    echo "Copying erlang-shipment to Windows distribution..."
    cp -r "$PROJECT_ROOT/build/erlang-shipment" "$DIST_DIR/shipment/"

    # Create PowerShell entrypoint wrapper (GitHub issue #2380)
    cat > "$DIST_DIR/shipment/erlang-shipment/entrypoint.ps1" << 'EOF'
#!/usr/bin/env pwsh
# Intent CLI Erlang Shipment PowerShell Entrypoint for Windows
# Based on: https://github.com/gleam-lang/gleam/issues/2380

param(
    [Parameter(ValueFromRemainingArguments)]
    [string[]]$Arguments
)

$ErrorActionPreference = "Stop"
$shipmentDir = $PSScriptRoot

# Set up Erlang runtime paths
$env:BINDIR = Join-Path $shipmentDir "erts/bin"
$env:EMU = "beam"
$env:PROGNAME = "intent"

# Run the application
$erlExe = Join-Path $env:BINDIR "erl.exe"
& $erlExe -boot_var RELEASE_LIB "$shipmentDir/lib" -boot "$shipmentDir/releases/start" -noshell -s intent -extra @Arguments
EOF

    chmod +x "$DIST_DIR/shipment/erlang-shipment/entrypoint.ps1"

    echo "Erlang-shipment created successfully!"
else
    echo "Warning: erlang-shipment directory not found"
fi

# Create README for Windows users
cat > "$DIST_DIR/README.txt" << 'EOF'
Intent CLI - Windows Installation
==================================

This directory contains two options for running Intent on Windows:

OPTION 1: Escript (Recommended for most users)
-----------------------------------------------
Location: escript/

Requirements:
- Erlang/OTP 27+ must be installed on your system
- escript.exe must be in your PATH

Usage:
1. Ensure Erlang is installed: https://www.erlang.org/downloads
2. Add the escript directory to your PATH, or
3. Run directly:
   - Command Prompt: escript\intent.cmd check myspec.cue
   - PowerShell: escript\intent.ps1 check myspec.cue
   - With escript: escript escript\intent check myspec.cue

OPTION 2: Erlang Shipment (Self-contained)
-------------------------------------------
Location: shipment/erlang-shipment/

Requirements:
- None! Includes its own Erlang runtime

Usage:
1. Run directly:
   - PowerShell: shipment\erlang-shipment\entrypoint.ps1 check myspec.cue
   - Bash/WSL: shipment/erlang-shipment/entrypoint.sh check myspec.cue

Known Issues:
-------------
- OTP 28 on Windows has a known issue with spawning escripts as child processes
  See: https://github.com/erlang/otp/issues/9872
- If you encounter issues, try OTP 27 instead

Installation to PATH:
--------------------
To make 'intent' available system-wide:

1. Copy intent.cmd to a directory in your PATH, such as:
   C:\Program Files\Intent\intent.cmd
   C:\Users\YourName\bin\intent.cmd

2. Or add the escript directory to your PATH:
   - Windows 11/10: Settings > System > About > Advanced system settings
   - Add the full path to the escript folder

Support:
--------
- Documentation: https://github.com/lprior-repo/intent-cli
- Issues: https://github.com/lprior-repo/intent-cli/issues

EOF

# Create a manifest file
cat > "$DIST_DIR/manifest.json" << EOF
{
  "version": "0.1.0",
  "build_date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "platform": "windows",
  "erlang_version_required": "27.0+",
  "packages": {
    "escript": {
      "path": "escript/",
      "files": ["intent", "intent.cmd", "intent.ps1"],
      "requires_erlang": true
    },
    "erlang_shipment": {
      "path": "shipment/erlang-shipment/",
      "files": ["entrypoint.ps1", "entrypoint.sh"],
      "requires_erlang": false,
      "self_contained": true
    }
  },
  "notes": [
    "OTP 28 has known issues on Windows with escript spawning",
    "Recommend using OTP 27 for maximum compatibility",
    "See: https://github.com/erlang/otp/issues/9872"
  ]
}
EOF

echo ""
echo "========================================"
echo "Windows build completed successfully!"
echo "========================================"
echo ""
echo "Distribution location: $DIST_DIR"
echo ""
echo "Contents:"
echo "  - escript/intent       - Escript binary (cross-platform)"
echo "  - escript/intent.cmd   - Windows Command Prompt wrapper"
echo "  - escript/intent.ps1   - PowerShell wrapper"
echo "  - shipment/            - Self-contained Erlang shipment"
echo "  - README.txt           - Installation instructions"
echo "  - manifest.json        - Build metadata"
echo ""
echo "To create a release archive:"
echo "  cd $DIST_DIR && zip -r intent-windows-v0.1.0.zip ."
echo ""
