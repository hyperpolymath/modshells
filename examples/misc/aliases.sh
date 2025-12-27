# SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
# misc/aliases.sh - General purpose aliases

# Navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# List files
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Grep with colour
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Safety nets
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# Disk usage
alias df='df -h'
alias du='du -h'

# Process management
alias psg='ps aux | grep -v grep | grep -i'

# Quick edit
alias bashrc='$EDITOR ~/.bashrc'
alias zshrc='$EDITOR ~/.zshrc'
