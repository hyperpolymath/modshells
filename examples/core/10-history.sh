# SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
# core/10-history.sh - Shell history configuration

# Increase history size
export HISTSIZE=10000
export HISTFILESIZE=20000

# Ignore duplicates and commands starting with space
export HISTCONTROL=ignoreboth:erasedups

# Append to history file, don't overwrite
shopt -s histappend 2>/dev/null || true

# Timestamp history entries
export HISTTIMEFORMAT="%Y-%m-%d %H:%M:%S  "

# Ignore common commands
export HISTIGNORE="ls:ll:la:cd:pwd:exit:clear:history"
