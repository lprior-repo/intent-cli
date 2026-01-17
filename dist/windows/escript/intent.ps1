#!/usr/bin/env pwsh
# Intent CLI PowerShell wrapper for Windows
$scriptPath = Join-Path $PSScriptRoot "intent"
& escript.exe $scriptPath $args
