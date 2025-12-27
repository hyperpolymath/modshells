# SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
# os/linux.sh - Linux-specific configuration

# Only load on Linux
[ "$(uname -s)" = "Linux" ] || return 0

# Package manager aliases (adjust for your distro)
if command -v apt >/dev/null 2>&1; then
    alias apt-update='sudo apt update && sudo apt upgrade'
    alias apt-search='apt search'
    alias apt-install='sudo apt install'
elif command -v dnf >/dev/null 2>&1; then
    alias dnf-update='sudo dnf upgrade'
    alias dnf-search='dnf search'
    alias dnf-install='sudo dnf install'
elif command -v pacman >/dev/null 2>&1; then
    alias pac-update='sudo pacman -Syu'
    alias pac-search='pacman -Ss'
    alias pac-install='sudo pacman -S'
fi

# Systemd shortcuts
if command -v systemctl >/dev/null 2>&1; then
    alias sc='systemctl'
    alias scs='systemctl status'
    alias scr='sudo systemctl restart'
    alias sce='sudo systemctl enable'
    alias scd='sudo systemctl disable'
fi

# Open file manager
alias open='xdg-open'
