@echo off
setlocal
set intentscript=%~dp0intent
escript.exe "%intentscript%" %*
