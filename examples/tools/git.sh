# SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
# tools/git.sh - Git aliases and configuration

# Short aliases
alias g='git'
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git pull'
alias gd='git diff'
alias gco='git checkout'
alias gb='git branch'
alias glog='git log --oneline --graph --decorate -20'

# Useful shortcuts
alias gca='git commit --amend'
alias gcm='git commit -m'
alias gaa='git add --all'
alias gst='git stash'
alias gstp='git stash pop'

# Show diff before commit
alias gcv='git commit -v'

# Undo last commit (keep changes)
alias gundo='git reset --soft HEAD~1'
