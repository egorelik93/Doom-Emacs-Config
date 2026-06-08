#!/usr/bin/env pwsh
# Open an org-capture popup frame from the shell. This opens a temporary emacs
# daemon if emacs isn't already running.
#
# Usage: org-capture [-k KEY] [MESSAGE]
# Examples:
#   org-capture -k n "To the mind that is still, the whole universe surrenders."

param(
    [string]$k,
    [Parameter(ValueFromRemainingArguments)]
    [string[]]$Message
)

# Build the key argument
$key = if ($k) { "\`"$k\`"" } else { 'nil' }

# Join remaining args into a single string, or read from stdin if "-"
$str = $Message -join ' '
if ($str -eq '-') {
    $str = $input | Out-String
    $str = $str.TrimEnd("`r`n")
}

# Run emacs
$eval = "(+org-capture/open-frame-ms \`"$str\`" $key)"
$args = @(
    "-e", "`"$eval`""
)
emacsc @args

# Original script, ported from doom emacs.
# Most of this has been moved to emacsc script.
#
#$ErrorActionPreference = 'Stop'
#
#function Invoke-Cleanup {
#    emacsclientw --eval '(let (kill-emacs-hook) (kill-emacs))'
#}
#
#try {
#    # If emacs isn't running, start a temporary daemon solely for this window
#    $ErrorActionPreference = 'SilentlyContinue'
#    $null = emacsclient --suppress-output --eval 'nil' 2>$null
#    $ErrorActionPreference = 'Stop'
#    if ($LASTEXITCODE -ne 0) {
#        Write-Host "No Emacs daemon/server is available! Starting one..."
#        runemacs --daemon
#        # Register cleanup on script exit
#        $cleanup = $true
#    }
#
#    # Build the key argument
#    $key = if ($k) { "\`"$k\`"" } else { 'nil' }
#
#    # Join remaining args into a single string, or read from stdin if "-"
#    $str = $Message -join ' '
#    if ($str -eq '-') {
#        $str = $input | Out-String
#        $str = $str.TrimEnd("`r`n")
#    }
#
#    # Fix incompatible terminals
#    if ($env:TERM -eq 'alacritty') {
#        $env:TERM = 'xterm-256color'
#    }
#
#    # Open the org-capture frame
#    $eval = "(+org-capture/open-frame-ms \`"$str\`" $key)"
#    $args = @(
#        "-c"
#        "-a", "`"`"",
#        "-e", "`"$eval`""
#    )
#    Start-Process emacsclientw -Wait -ArgumentList $args
#} finally {
#    if ($cleanup) {
#        Invoke-Cleanup
#    }
#}
#
