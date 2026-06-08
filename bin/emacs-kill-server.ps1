#!/usr/bin/env pwsh

$ErrorActionPreference = 'SilentlyContinue'
emacsclient --suppress-output --eval '(let (kill-emacs-hook) (kill-emacs))' 2>$null