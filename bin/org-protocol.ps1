#!/usr/bin/env pwsh
# Decides whether to open a new frame for org-protocol.
#
# Usage: org-protocol [URI]

param(
    [string]$uri
)

# Run emacs
$args = @(
    if ($uri -like "org-protocol://store-link*") { "-r" }
    $uri
)
emacsc @args
