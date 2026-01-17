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
