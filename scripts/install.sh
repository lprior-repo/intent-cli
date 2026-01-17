#!/usr/bin/env bash
# Install intent CLI to local system

set -e

echo "Building intent binary..."
moon run :escript

echo "Installing to ~/.local/bin..."
mkdir -p ~/.local/bin
cp dist/intent/intent ~/.local/bin/intent
chmod +x ~/.local/bin/intent

echo "Checking PATH configuration..."
if ! echo "$PATH" | grep -q "$HOME/.local/bin"; then
    if [ -n "$BASH_VERSION" ]; then
        if ! grep -q 'export PATH="$HOME/.local/bin:$PATH"' ~/.bashrc 2>/dev/null; then
            echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
            echo "Added to ~/.bashrc"
        fi
    fi

    if [ -n "$ZSH_VERSION" ] || [ -f ~/.zshrc ]; then
        if ! grep -q 'export PATH="$HOME/.local/bin:$PATH"' ~/.zshrc 2>/dev/null; then
            echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
            echo "Added to ~/.zshrc"
        fi
    fi

    echo ""
    echo "⚠️  Please reload your shell or run:"
    echo "   source ~/.bashrc  # or source ~/.zshrc"
fi

echo ""
echo "✅ Installation complete!"
echo ""
echo "Test with: ~/.local/bin/intent --help"
echo "After reloading shell: intent --help"
