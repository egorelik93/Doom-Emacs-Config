#!/usr/bin/env pwsh
# Decides whether to open a new frame for org-protocol.
#
# Usage: org-protocol [URI]

param(
    [string]$uri
)

$uri = [regex]::Replace($uri, '[^\x00-\x7F]+', {
                            param($m)
                            -join ([Text.Encoding]::UTF8.GetBytes($m.Value) | ForEach-Object { '%{0:X2}' -f $_ })
                        })

# Run emacs
$args = @(
    if ($uri -like "org-protocol://store-link*") { "-r" }
    $uri
)
emacsc @args
