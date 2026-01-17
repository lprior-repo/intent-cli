Intent CLI - Windows Installation
==================================

Version: 0.1.0
Build Date: 2026-01-17

QUICK START
-----------

1. Install Erlang/OTP 27+: https://www.erlang.org/downloads
2. Extract this ZIP file
3. Run: escript\intent.cmd --help

REQUIREMENTS
------------

- Windows 10 or later
- Erlang/OTP 27.0 or later (DO NOT use OTP 28 - has known Windows issues)
- PATH configured to include Erlang's bin directory

USAGE
-----

Command Prompt:
  escript\intent.cmd check myspec.cue --target http://localhost:8080

PowerShell:
  escript\intent.ps1 check myspec.cue --target http://localhost:8080

Direct escript (if Erlang in PATH):
  escript escript\intent check myspec.cue

INSTALLATION TO PATH
--------------------

Option 1: Copy wrapper to system directory
  copy escript\intent.cmd C:\Windows\System32\intent.cmd

Option 2: Add escript directory to PATH
  1. Right-click This PC > Properties > Advanced system settings
  2. Environment Variables > System variables > Path > Edit
  3. Add full path to escript folder

VERIFICATION
------------

After installation:
  intent.cmd --help

Should display Intent CLI help information.

KNOWN ISSUES
------------

OTP 28 Compatibility:
  - OTP 28 has a known issue with spawning escript processes on Windows
  - Use OTP 27 instead
  - See: https://github.com/erlang/otp/issues/9872

SUPPORT
-------

- Documentation: https://github.com/lprior-repo/intent-cli
- Issues: https://github.com/lprior-repo/intent-cli/issues
