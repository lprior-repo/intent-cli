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
