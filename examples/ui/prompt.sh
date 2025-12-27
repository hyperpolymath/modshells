# SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
# ui/prompt.sh - Custom prompt (if not using starship)
#
# Skip if starship is being used
command -v starship >/dev/null 2>&1 && return 0

# Colour codes
RED='\[\e[0;31m\]'
GREEN='\[\e[0;32m\]'
YELLOW='\[\e[0;33m\]'
BLUE='\[\e[0;34m\]'
PURPLE='\[\e[0;35m\]'
CYAN='\[\e[0;36m\]'
RESET='\[\e[0m\]'

# Git branch function
__git_branch() {
    git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

# Build prompt
# user@host:directory (git-branch)$
PS1="${GREEN}\u${RESET}@${BLUE}\h${RESET}:${CYAN}\w${YELLOW}\$(__git_branch)${RESET}\$ "

# Set terminal title
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;\u@\h: \w\a\]$PS1"
        ;;
esac
