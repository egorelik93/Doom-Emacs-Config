#!/usr/bin/env pwsh
# Open an emacsclientw frame from the shell. This opens a temporary emacs
# daemon if emacs isn't already running.

function Invoke-Cleanup {
    $ErrorActionPreference = 'SilentlyContinue'
    emacsclient --eval '(let (kill-emacs-hook) (kill-emacs))' 2>$null
}

function Is-ServerRunning {
    $errorAction = $ErrorActionPreference
    $ErrorActionPreference = 'SilentlyContinue'
    $null = emacsclient --suppress-output --eval 'nil' 2>$null
    $ErrorActionPreference = $errorAction
    return $LASTEXITCODE -eq 0
}

try {
    $ErrorActionPreference = 'Stop'

    $indexA = $args.IndexOf('-a')
    if ($indexA -ne -1 -and ($indexA + 1) -lt $args.Count) {
        $a = $args[$indexA + 1]

        if (@($null, "", '""', "''") -contains $a) {
            $a = $null
            $args[$indexA + 1] = '""'
        }
    }

    # If emacs isn't running, start a temporary daemon solely for this window
    if (($a -eq $null) -and !(Is-ServerRunning)) {
        Write-Host "No Emacs daemon/server is available! Starting one..."
        runemacs --daemon

        $deadline = (Get-Date).AddSeconds(30)
        $newServer = $false
        while ((Get-Date) -lt $deadline) {
            if (Is-ServerRunning) {
                $newServer = $true
                break 
            }
            Start-Sleep -Milliseconds 200
        }
        if (!($newServer)) {
            Write-Error "Emacs daemon failed to start"
            exit 1
        }

        # Register cleanup on script exit
        if ($args -notcontains "-a") {
            $cleanup = $true
        }
    }

    # Fix incompatible terminals
    if ($env:TERM -eq 'alacritty') {
        $env:TERM = 'xterm-256color'
    }

    # Run emacs
    $emacsargs = @(
        if ($args -notcontains "-r") { "-c" }
        # "-a", "`"`""
    ) + $args
    Start-Process -NoNewWindow emacsclient -Wait:$cleanup -ArgumentList $emacsargs
} finally {
    if ($cleanup) {
        Invoke-Cleanup
    }
}
