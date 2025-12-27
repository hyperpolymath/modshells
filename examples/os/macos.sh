# SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
# os/macos.sh - macOS-specific configuration

# Only load on macOS
[ "$(uname -s)" = "Darwin" ] || return 0

# Homebrew
if [ -x /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -x /usr/local/bin/brew ]; then
    eval "$(/usr/local/bin/brew shellenv)"
fi

# Homebrew aliases
alias brew-update='brew update && brew upgrade'
alias brew-cleanup='brew cleanup -s && brew autoremove'

# macOS-specific aliases
alias showfiles='defaults write com.apple.finder AppleShowAllFiles YES && killall Finder'
alias hidefiles='defaults write com.apple.finder AppleShowAllFiles NO && killall Finder'

# Flush DNS cache
alias flushdns='sudo dscacheutil -flushcache && sudo killall -HUP mDNSResponder'

# Open current directory in Finder
alias finder='open -a Finder .'

# Quick Look preview
alias ql='qlmanage -p'

# Copy to clipboard
alias copy='pbcopy'
alias paste='pbpaste'
