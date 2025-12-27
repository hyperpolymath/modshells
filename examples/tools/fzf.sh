# SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
# tools/fzf.sh - Fuzzy finder configuration

# Check if fzf is installed
command -v fzf >/dev/null 2>&1 || return 0

# Default options
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'

# Use fd if available (faster than find)
if command -v fd >/dev/null 2>&1; then
    export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'
fi

# Ctrl+R for history search with preview
export FZF_CTRL_R_OPTS='--preview "echo {}" --preview-window=down:3:wrap'

# fzf key bindings (if available)
[ -f /usr/share/fzf/key-bindings.bash ] && . /usr/share/fzf/key-bindings.bash
[ -f /usr/share/fzf/completion.bash ] && . /usr/share/fzf/completion.bash
