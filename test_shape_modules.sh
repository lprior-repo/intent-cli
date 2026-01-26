#!/usr/bin/env bash
# Quick test script for shape modules

echo "Testing shape_session and shape_storage modules..."

# Try to build - this will verify compilation
gleam build

if [ $? -eq 0 ]; then
    echo "✓ Modules compiled successfully"
else
    echo "✗ Compilation failed"
    exit 1
fi

# Format the files
gleam format src/intent/shape_session.gleam src/intent/shape_storage.gleam

echo "✓ All checks passed"
