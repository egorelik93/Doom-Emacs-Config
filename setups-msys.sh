#!/usr/bin/env bash
#
# Run in msys to install needed packages for emacs

# Created by Claude Code

set -euo pipefail

# --- Usage -------------------------------------------------------------------
#
#   setup-msys.sh [--copy-to-emacs <dir>]
#
#   --copy-to-emacs <dir>   Copy FILES from SRC_DIR into <dir>. Skipped if omitted.

# --- Argument parsing --------------------------------------------------------

COPY_TO_EMACS=0
DST_DIR=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --copy-to-emacs)
            COPY_TO_EMACS=1
            DST_DIR="${2:?--copy-to-emacs requires a target directory}"
            shift 2
            ;;
        *)
            echo "Unknown option: $1" >&2
            echo "Usage: $0 [--copy-to-emacs <dir>]" >&2
            exit 1
            ;;
    esac
done

# --- Configuration -----------------------------------------------------------

# Specify which version version was used to compile emacs
TARGET_GCC_VERSION="14.2.0-1"   # full version with epoch, as used in package filenames

# Adjust the package name for your MSYS2 subsystem:
#   MINGW64/UCRT64:  mingw-w64-x86_64-gcc
#   CLANG64:         mingw-w64-clang-x86_64-gcc
#   MSYS (native):   gcc
PKG_PREFIX="mingw-w64-x86_64"

# Check SRC_DIR for what this should be
BIN_PREFIX="x86_64-w64-mingw32"

SRC_DIR="/mingw64/bin"

# List of files to copy. Paths are relative to SRC_DIR.
FILES=(
"printf_ngettext.exe"
"libcharset-*.dll"
"addr2line.exe"
"ar.exe"
"as.exe"
"c++filt.exe"
"dlltool.exe"
"dllwrap.exe"
"elfedit.exe"
"gprof.exe"
"ld.bfd.exe"
"ld.exe"
"nm.exe"
"objcopy.exe"
"objdump.exe"
"ranlib.exe"
"readelf.exe"
"size.exe"
"strings.exe"
"strip.exe"
"windmc.exe"
"windres.exe"
"envsubst.exe"
"gettext.exe"
"gettext.sh"
"libasprintf-*.dll"
"ngettext.exe"
"printf_gettext.exe"
"${BIN_PREFIX}-gcc-*.exe"
"${BIN_PREFIX}-gcc-ar.exe"
"${BIN_PREFIX}-gcc-nm.exe"
"${BIN_PREFIX}-gcc-ranlib.exe"
"libgmpxx-*.dll"
"pzstd.exe"
"libdatrie-*.dll"
"libexpat-*.dll"
"libffi-*.dll"
"libfreetype-*.dll"
"libgif-*.dll"
"libgio-*.dll"
"libglib-*.dll"
"libgmodule-*.dll"
"libgnutls-*.dll"
"libhogweed-*.dll"
"libidn2-*.dll"
"libjpeg-*.dll"
"libmpc-*.dll"
"libnettle-*.dll"
"libpangoft2-*.dll"
"libpangowin32-*.dll"
"libpcre2-*.dll"
"librsvg-*.dll"
"libstdc++-*.dll"
"libtasn1-*.dll"
"libthai-*.dll"
"libunistring-*.dll"
"libwinpthread-*.dll"
"libXpm-*.dll"
"zlib1.dll"
"libbrotlicommon.dll"
"libbrotlidec.dll"
"libbrotlienc.dll"
"libbz2-*.dll"
"libcairo-*.dll"
"libcairo-gobject-*.dll"
"libfontconfig-*.dll"
"libfribidi-*.dll"
"libgcc_s_seh-*.dll"
"libgdk_pixbuf-*.dll"
"libgmp-*.dll"
"libgobject-*.dll"
"libgraphite2.dll"
"libharfbuzz-*.dll"
"libiconv-*.dll"
"libintl-*.dll"
"libisl-*.dll"
"liblcms2-*.dll"
"liblzma-*.dll"
"libmpfr-*.dll"
"libp11-kit-*.dll"
"libpango-*.dll"
"libpangocairo-*.dll"
"libpixman-*.dll"
"libpng16-*.dll"
"libsharpyuv-*.dll"
"libsqlite3-*.dll"
"libtree-sitter.dll"
"libwebp-*.dll"
"libwebpdemux-*.dll"
"libxml2-*.dll"
"libzstd.dll"
"c++.exe"
"cc.exe"
"cpp.exe"
"g++.exe"
"gcc.exe"
"gcc-ar.exe"
"gcc-nm.exe"
"gcc-ranlib.exe"
"gcov.exe"
"gcov-tool.exe"
"libatomic-*.dll"
"libgccjit-*.dll"
"libgomp-*.dll"
"libquadmath-*.dll"
"${BIN_PREFIX}-c++.exe"
"${BIN_PREFIX}-cc.exe"
"${BIN_PREFIX}-g++.exe"
"${BIN_PREFIX}-gcc.exe"
)


# Set up native-comp

# --- GCC install/check -------------------------------------------------------

GCC_PACKAGES=(
    "${PKG_PREFIX}-gcc"
    "${PKG_PREFIX}-gcc-libs"
    "${PKG_PREFIX}-libgccjit"
)

# Version check is done against the first package; all three track together.
GCC_VERSION_PKG="${GCC_PACKAGES[0]}"

pkg_installed_version() {
    pacman -Q "$1" 2>/dev/null | awk '{print $2}' | sed 's/-.*//'
}

available_version() {
    pacman -Si "$GCC_VERSION_PKG" 2>/dev/null | awk '/^Version/{print $3}' | sed 's/-.*//'
}

available_version_full() {
    pacman -Si "$GCC_VERSION_PKG" 2>/dev/null | awk '/^Version/{print $3}'
}

install_from_repo() {
    pacman -S --needed --noconfirm "${GCC_PACKAGES[@]}"
}

install_from_downloads() {
    # Install Deps

    pacman -S --needed --noconfirm --asdeps "${PKG_PREFIX}-libiconv"
    pacman -Sdd --needed --noconfirm --asdeps "${PKG_PREFIX}-gettext-runtime"

    # Install packages

    local pkgfiles=()
    for pkg in "${GCC_PACKAGES[@]}"; do
        local pkgfile="${pkg}-${TARGET_GCC_VERSION}-any.pkg.tar.zst"
        local dest="$HOME/Downloads/$pkgfile"
        if [[ -f "$dest" ]]; then
            echo "    Found cached: $pkgfile"
        else
            local url="https://mirror.msys2.org/mingw/mingw64/$pkgfile"
            echo "    Downloading $pkgfile to ~/Downloads..."
            curl -L --fail --no-clobber --output "$dest" "$url"
        fi
        pkgfiles+=("$dest")
    done
    pacman -U --noconfirm "${pkgfiles[@]}"
}

install_to_target() {
    local avail
    avail="$(available_version)"
    if [[ "$avail" == "${TARGET_GCC_VERSION%%-*}" ]]; then
        echo "    Target version is available in pacman repos. Installing..."
        install_from_repo
    else
        echo "    Target version $TARGET_GCC_VERSION is NOT in pacman repos (latest available: $avail)."
        install_from_downloads
    fi
}

# Returns the common installed version if all packages are present and in sync, empty string otherwise.
check_installed_versions() {
    local first="" ver="" in_sync=1
    for pkg in "${GCC_PACKAGES[@]}"; do
        ver="$(pkg_installed_version "$pkg")"
        if [[ -z "$first" ]]; then
            first="$ver"
        elif [[ "$ver" != "$first" ]]; then
            in_sync=0
        fi
        echo "    $pkg: ${ver:-not installed}" >&2
    done
    [[ "$in_sync" -eq 0 ]] && echo "    WARNING: packages are out of sync." >&2
    [[ "$in_sync" -eq 1 ]] && echo "$first" || echo ""
}

echo "==> Checking GCC versions..."

current="$(check_installed_versions)"

if [[ "$current" == "${TARGET_GCC_VERSION%%-*}" ]]; then
    echo "    All packages at target version $TARGET_GCC_VERSION. Nothing to do."
else
    [[ -n "$current" ]] && echo "    Installed version ($current) differs from target ($TARGET_GCC_VERSION)."
    install_to_target
fi


# --- File copy ---------------------------------------------------------------

if [[ "$COPY_TO_EMACS" -eq 1 ]]; then
    echo ""
    echo "==> Copying files from $SRC_DIR to $DST_DIR..."

    if [[ ${#FILES[@]} -eq 0 ]]; then
        echo "    FILES array is empty — nothing to copy."
    else
        mkdir -p "$DST_DIR"
        shopt -s nullglob
        for pattern in "${FILES[@]}"; do
            matched=0
            for src in "$SRC_DIR"/$pattern; do
                [[ -f "$src" ]] || continue
                rel="${src#$SRC_DIR/}"
                dst="$DST_DIR/$rel"
                mkdir -p "$(dirname "$dst")"
                if [[ -e "$dst" ]]; then
                    echo "    Skipped (already exists): $rel"
                    continue
                fi
                cp "$src" "$dst"
                echo "    Copied: $rel"
                matched=1
            done
            [[ "$matched" -eq 0 ]] && echo "    WARNING: no files matched, skipping: $pattern"
        done
        shopt -u nullglob
    fi
else
    echo ""
    echo "==> Skipping file copy (pass --copy-to-emacs <dir> to enable)."
fi


# Set up spell-checking

echo "Setting up spell checkers"

echo "Installing hunspell"
pacman -S --needed --noconfirm "${PKG_PREFIX}-hunspell" --assume-installed "${PKG_PREFIX}-gcc-libs=$(available_version_full)"
pacman -S --needed --noconfirm "${PKG_PREFIX}-hunspell-en"

echo "Installing enchant"
pacman -Sdd --needed --noconfirm "${PKG_PREFIX}-enchant"
pacman -S --needed --noconfirm "${PKG_PREFIX}-pkgconf"

# The msys glib package has unnecessary python dependencies
pacman -S --needed --noconfirm --asdeps "${PKG_PREFIX}-pcre2"
pacman -Sdd --needed --noconfirm --asdeps "${PKG_PREFIX}-glib2"

# Libenchant unfortunately produces annoying warnings if its optional dependencies
# are not installed
pacman -Sdd --needed --noconfirm --asdeps "${PKG_PREFIX}-aspell" "${PKG_PREFIX}-nuspell" "${PKG_PREFIX}-icu" "${PKG_PREFIX}-libvoikko"


echo ""
echo "==> Done."
