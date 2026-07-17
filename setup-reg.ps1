# This script has replaced org-protocol.reg;
# use this instead.
#
# Make sure that PATH includes at least:
#   %USERPROFILE%\bin
#   %USERPROFILE%\AppData\Local\Microsoft\WindowsApps
#   ~\.doom.d\bin
#   ~\.emacs.d\bin
#   %LocalAppData%\Microsoft\WinGet\Links
#   C:\msys64\usr\bin
#   C:\msys64\mingw64\bin
#   %EMACS_PATH%\bin
# Relatively in that order.
#
# Make sure HOME is set appropriately.
#
# Make sure DICPATH is set to hunspell location,
# probably '%LocalAppData%\hunspell'.


$runemacs = '%EMACS_PATH%\bin\runemacs.exe'
$icon = "$runemacs"
# With -a, my script is basically the same thing as emacsclient
# If emacs isn't running and server isn't running, will open directly in emacs
# If emacs is running with server, opens using emacsclientw (C-# when done)
# $emacsc = 'wscript.exe %HOME%\.doom.d\bin\emacsc.vbs -a ""'
$emacsc = "%EMACS_PATH%\bin\emacsclientw.exe -n -a ""$runemacs"""
$orgprotocol = 'wscript.exe %HOME%\.doom.d\bin\org-protocol.vbs'
# Where we cannot use my emacsc script, this is the next best thing.
$emacsclient = "%EMACS_PATH%\bin\emacsclientw.exe -n -a ""$runemacs"""

function Set-RegistryKey {
    param($Path)

    if ($Path -match '^(HKCU:|HKCU|HKEY_CURRENT_USER)([\\/]|$)?') {
        $Path = $Path -replace '^(HKCU:|HKCU|HKEY_CURRENT_USER)[\\/]?', ''

        $BaseKey = [Microsoft.Win32.Registry]::CurrentUser
        $Key = $BaseKey.CreateSubKey($Path)

        if ($Key) {
            $Key.Close()
            Write-Host "Verified/Created safely: HKCU\$Path" -ForegroundColor Green
        }
    } else {
        Throw "Validation Error: The registry path '$Path' must start with 'HKCU', 'HKCU:', or 'HKEY_CURRENT_USER'. Execution halted."
    }
}

function Set-RegistryEntry {
    param($Path, $Name, $Value, $Type)

    $TypeParam = @{}
    if ($Type) {
        $TypeParam['Type'] = $Type
    }

    echo "Setting [$PATH] $NAME"
    Set-ItemProperty -LiteralPath "Registry::$Path" -Name $Name -Value $Value @TypeParam
}

# Update EMACS_PATH

# Find the latest Emacs version folder under Program Files
$emacsRoot = "$env:ProgramFiles\Emacs"

$latestVersionDir = Get-ChildItem -Path $emacsRoot -Directory -ErrorAction SilentlyContinue |
    Where-Object { $_.Name -match '^emacs-[\d.]+$' } |
    Sort-Object { [version]($_.Name -replace '^emacs-','') } -Descending |
    Select-Object -First 1

if ($latestVersionDir) {
    $target = $latestVersionDir.FullName
    Write-Host "Latest Emacs version dir: $target"

    # User-level environment variable
    [Environment]::SetEnvironmentVariable("EMACS_PATH", $target, "User")

    Write-Host "EMACS_PATH set to: $target"
} else {
    Write-Warning "No versioned Emacs folder found under $emacsRoot"
}


# org-protocol

Set-RegistryKey -Path "HKCU\Software\Classes\org-protocol"
Set-RegistryEntry -Path "HKCU\Software\Classes\org-protocol" -Name "(default)" -Value "URL:org-protocol"
Set-RegistryEntry -Path "HKCU\Software\Classes\org-protocol" -Name "URL Protocol" -Value ""

Set-RegistryKey -Path "HKCU\Software\Classes\org-protocol\shell"

Set-RegistryKey -Path "HKCU\Software\Classes\org-protocol\shell\open"

Set-RegistryKey -Path "HKCU\Software\Classes\org-protocol\shell\open\command"
Set-RegistryEntry -Path "HKCU\Software\Classes\org-protocol\shell\open\command" -Name "(default)" -Value "$orgprotocol ""%1""" -Type ExpandString


# ProgIds

Set-RegistryKey -Path "HKCU\Software\Classes\Emacs.File"

Set-RegistryKey -Path "HKCU\Software\Classes\Emacs.File\shell"
Set-RegistryKey -Path "HKCU\Software\Classes\Emacs.File\shell\open"
Set-RegistryKey -Path "HKCU\Software\Classes\Emacs.File\shell\open\command"
# Has to be emacsclient, or the icon and description will be wrong.
Set-RegistryEntry -Path "HKCU\Software\Classes\Emacs.File\shell\open\command" -Name "(default)" -Value "$emacsclient ""%1""" -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\Emacs.File\DefaultIcon"
Set-RegistryEntry -Path "HKCU\Software\Classes\Emacs.File\DefaultIcon" -Name "(default)" -Value "$icon" -Type ExpandString

# Open file in existing frame
Set-RegistryKey -Path "HKCU\Software\Classes\Emacs.File\shell\emacsopen1sameframe"
Set-RegistryEntry -Path "HKCU\Software\Classes\Emacs.File\shell\emacsopen1sameframe" -Name "(default)" -Value "&Emacs: Edit in existing window"
Set-RegistryEntry -Path "HKCU\Software\Classes\Emacs.File\shell\emacsopen1sameframe" -Name "icon" -Value $icon -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\Emacs.File\shell\emacsopen1sameframe\command"
Set-RegistryEntry -Path "HKCU\Software\Classes\Emacs.File\shell\emacsopen1sameframe\command" -Name "(default)" -Value "$emacsc ""%1""" -Type ExpandString

# Open file in new frame
Set-RegistryKey -Path "HKCU\Software\Classes\Emacs.File\shell\emacsopen2newframe"
Set-RegistryEntry -Path "HKCU\Software\Classes\Emacs.File\shell\emacsopen2newframe" -Name "(default)" -Value "&Emacs: Edit in new window"
Set-RegistryEntry -Path "HKCU\Software\Classes\Emacs.File\shell\emacsopen2newframe" -Name "icon" -Value $icon -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\Emacs.File\shell\emacsopen2newframe\command"
Set-RegistryEntry -Path "HKCU\Software\Classes\Emacs.File\shell\emacsopen2newframe\command" -Name "(default)" -Value "$runemacs ""%1""" -Type ExpandString

# Applications

Set-RegistryKey -Path "HKCU\Software\Classes\Applications\runemacs.exe"
Set-RegistryKey -Path "HKCU\Software\Classes\Applications\runemacs.exe\shell"
Set-RegistryKey -Path "HKCU\Software\Classes\Applications\runemacs.exe\shell\open"
Set-RegistryKey -Path "HKCU\Software\Classes\Applications\runemacs.exe\shell\open\command"
Set-RegistryEntry -Path "HKCU\Software\Classes\Applications\runemacs.exe\shell\open\command" -Name "(default)" -Value "$runemacs ""%1""" -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\Applications\emacsclientw.exe"
Set-RegistryKey -Path "HKCU\Software\Classes\Applications\emacsclientw.exe\shell"
Set-RegistryKey -Path "HKCU\Software\Classes\Applications\emacsclientw.exe\shell\open"
Set-RegistryKey -Path "HKCU\Software\Classes\Applications\emacsclientw.exe\shell\open\command"
Set-RegistryEntry -Path "HKCU\Software\Classes\Applications\emacsclientw.exe\shell\open\command" -Name "(default)" -Value "$emacsc ""%1""" -Type ExpandString
Set-RegistryKey -Path "HKCU\Software\Classes\Applications\emacsclientw.exe\DefaultIcon"
Set-RegistryEntry -Path "HKCU\Software\Classes\Applications\emacsclientw.exe\DefaultIcon" -Name "(default)" -Value "$icon" -Type ExpandString


# Context Menu
#
# Modified from https://www.emacswiki.org/emacs/MsWindowsGlobalContextMenu

# Open file in existing frame
Set-RegistryKey -Path "HKCU\Software\Classes\*\shell\emacsopen1sameframe"
Set-RegistryEntry -Path "HKCU\Software\Classes\*\shell\emacsopen1sameframe" -Name "(default)" -Value "&Emacs: Edit in existing window"
Set-RegistryEntry -Path "HKCU\Software\Classes\*\shell\emacsopen1sameframe" -Name "icon" -Value $icon -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\*\shell\emacsopen1sameframe\command"
Set-RegistryEntry -Path "HKCU\Software\Classes\*\shell\emacsopen1sameframe\command" -Name "(default)" -Value "$emacsc ""%1""" -Type ExpandString

# Open file in new frame
Set-RegistryKey -Path "HKCU\Software\Classes\*\shell\emacsopen2newframe"
Set-RegistryEntry -Path "HKCU\Software\Classes\*\shell\emacsopen2newframe" -Name "(default)" -Value "&Emacs: Edit in new window"
Set-RegistryEntry -Path "HKCU\Software\Classes\*\shell\emacsopen2newframe" -Name "icon" -Value $icon -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\*\shell\emacsopen2newframe\command"
Set-RegistryEntry -Path "HKCU\Software\Classes\*\shell\emacsopen2newframe\command" -Name "(default)" -Value "$runemacs ""%1""" -Type ExpandString

# Dired for desktop background
Set-RegistryKey -Path "HKCU\Software\Classes\DesktopBackground\shell\emacsopensameframe"
Set-RegistryEntry -Path "HKCU\Software\Classes\DesktopBackground\shell\emacsopensameframe" -Name "(default)" -Value "&Emacs: Open in dired"
Set-RegistryEntry -Path "HKCU\Software\Classes\DesktopBackground\shell\emacsopensameframe" -Name "icon" -Value $icon -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\DesktopBackground\shell\emacsopensameframe\command"
Set-RegistryEntry -Path "HKCU\Software\Classes\DesktopBackground\shell\emacsopensameframe\command" -Name "(default)" -Value "$emacsc ""%v""" -Type ExpandString

# Dired for directory
Set-RegistryKey -Path "HKCU\Software\Classes\Directory\shell\emacsopensameframe"
Set-RegistryEntry -Path "HKCU\Software\Classes\Directory\shell\emacsopensameframe" -Name "(default)" -Value "&Emacs: Open in dired"
Set-RegistryEntry -Path "HKCU\Software\Classes\Directory\shell\emacsopensameframe" -Name "icon" -Value $icon -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\Directory\shell\emacsopensameframe\command"
Set-RegistryEntry -Path "HKCU\Software\Classes\Directory\shell\emacsopensameframe\command" -Name "(default)" -Value "$emacsc ""%V""" -Type ExpandString

# Dired for directory background
Set-RegistryKey -Path "HKCU\Software\Classes\Directory\Background\shell\emacsopensameframe"
Set-RegistryEntry -Path "HKCU\Software\Classes\Directory\Background\shell\emacsopensameframe" -Name "(default)" -Value "&Emacs: Open in dired"
Set-RegistryEntry -Path "HKCU\Software\Classes\Directory\Background\shell\emacsopensameframe" -Name "icon" -Value $icon -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\Directory\Background\shell\emacsopensameframe\command"
Set-RegistryEntry -Path "HKCU\Software\Classes\Directory\Background\shell\emacsopensameframe\command" -Name "(default)" -Value "$emacsc ""%V""" -Type ExpandString

# Dired for drive background
Set-RegistryKey -Path "HKCU\Software\Classes\Drive\Background\"
Set-RegistryKey -Path "HKCU\Software\Classes\Drive\Background\shell\EmacsOpenDirSameFrame"
Set-RegistryEntry -Path "HKCU\Software\Classes\Drive\Background\shell\EmacsOpenDirSameFrame" -Name "(default)" -Value "&Emacs: Open in dired"
Set-RegistryEntry -Path "HKCU\Software\Classes\Drive\Background\shell\EmacsOpenDirSameFrame" -Name "icon" -Value $icon -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\Drive\Background\shell\EmacsOpenDirSameFrame\command"
# as drives haven't space inside their name don't use quoting
Set-RegistryEntry -Path "HKCU\Software\Classes\Drive\Background\shell\EmacsOpenDirSameFrame\command" -Name "(default)" -Value "$emacsc %L" -Type ExpandString

# Dired for drive
Set-RegistryKey -Path "HKCU\Software\Classes\Drive\shell\EmacsOpenDirSameFrame"
Set-RegistryEntry -Path "HKCU\Software\Classes\Drive\shell\EmacsOpenDirSameFrame" -Name "(default)" -Value "&Emacs: Open in dired"
Set-RegistryEntry -Path "HKCU\Software\Classes\Drive\shell\EmacsOpenDirSameFrame" -Name "icon" -Value $icon -Type ExpandString

Set-RegistryKey -Path "HKCU\Software\Classes\Drive\shell\EmacsOpenDirSameFrame\command"
# as drives haven't space inside their name don't use quoting
Set-RegistryEntry -Path "HKCU\Software\Classes\Drive\shell\EmacsOpenDirSameFrame\command" -Name "(default)" -Value "$emacsc %L" -Type ExpandString


# Add Emacs to Open With.
# Not sure if it works.
Set-RegistryKey -Path "HKCU\Software\Classes\*\OpenWithProgids"
Set-RegistryEntry -Path "HKCU\Software\Classes\*\OpenWithProgids" -Name "Emacs.File" -Value ""
