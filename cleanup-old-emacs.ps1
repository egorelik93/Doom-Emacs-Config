#!/usr/bin/env pwsh
<#
.SYNOPSIS
    Finds Emacs entries under the Windows Uninstall registry keys and removes
    any whose install/uninstaller no longer exists on disk (leftover entries
    from an Emacs version that was already removed).

.PARAMETER Delete
    Actually remove orphaned entries. Without this switch the script only
    reports what it finds (dry run).

.EXAMPLE
    ./bin/cleanup-old-emacs.ps1
    Lists all Emacs uninstall entries and flags any that are orphaned.

.EXAMPLE
    ./bin/cleanup-old-emacs.ps1 -Delete
    Same as above, and removes the orphaned entries (prompts via -Confirm
    unless -Confirm:$false is also passed; supports -WhatIf).
#>
[CmdletBinding(SupportsShouldProcess, ConfirmImpact = 'High')]
param(
    [switch]$Delete
)

$uninstallPaths = @(
    'HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\*',
    'HKLM:\SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\*',
    'HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\*'
)

function Get-UninstallEntryPath {
    param($Entry)

    if ($Entry.InstallLocation) {
        return $Entry.InstallLocation
    }

    foreach ($raw in @($Entry.UninstallString, $Entry.DisplayIcon)) {
        if (-not $raw) { continue }

        $candidate = $raw -replace ',\s*-?\d+\s*$', ''  # strip trailing icon index

        if ($candidate -match '^\s*"([^"]+)"') {
            return $Matches[1]
        }
        if ($candidate -match '^\s*(.*?\.exe)\b') {
            return $Matches[1]
        }
        return $candidate.Trim()
    }

    return $null
}

$entries = Get-ItemProperty -Path $uninstallPaths -ErrorAction SilentlyContinue |
    Where-Object { $_.DisplayName -match 'emacs' }

if (-not $entries) {
    Write-Host "No Emacs entries found under the Uninstall registry keys."
    return
}

$orphans = @()

foreach ($entry in $entries) {
    $checkPath = Get-UninstallEntryPath -Entry $entry
    $exists = [bool]($checkPath -and (Test-Path -LiteralPath $checkPath))
    $regKey = $entry.PSPath -replace '^Microsoft\.PowerShell\.Core\\Registry::', ''

    Write-Host ""
    Write-Host "DisplayName : $($entry.DisplayName)"
    Write-Host "RegistryKey : $regKey"
    Write-Host "CheckedPath : $checkPath"
    Write-Host "Exists      : $exists"

    if ($exists) { continue }

    if ($Delete) {
        if ($PSCmdlet.ShouldProcess($regKey, 'Remove orphaned uninstall registry entry')) {
            Remove-Item -Path $entry.PSPath -Recurse -Force
            Write-Host "Removed." -ForegroundColor Yellow
        }
    } else {
        Write-Host "Orphaned - would be deleted with -Delete." -ForegroundColor Yellow
        $orphans += [PSCustomObject]@{ DisplayName = $entry.DisplayName; RegistryKey = $regKey }
    }
}

if (-not $Delete -and $orphans.Count -gt 0) {
    Write-Host ""
    Write-Host "$($orphans.Count) orphaned entr$(if ($orphans.Count -eq 1) { 'y' } else { 'ies' }) found:" -ForegroundColor Cyan
    $orphans | ForEach-Object { Write-Host "  - $($_.DisplayName)  [$($_.RegistryKey)]" -ForegroundColor Cyan }
    Write-Host ""
    Write-Host "Rerun with -Delete to remove the entries listed above." -ForegroundColor Cyan
}
