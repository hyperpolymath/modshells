# SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
# core/20-options.sh - Shell options and settings

# Check window size after each command
shopt -s checkwinsize 2>/dev/null || true

# Enable extended globbing
shopt -s extglob 2>/dev/null || true

# Correct minor spelling errors in cd
shopt -s cdspell 2>/dev/null || true

# Enable recursive globbing with **
shopt -s globstar 2>/dev/null || true

# Case-insensitive globbing
shopt -s nocaseglob 2>/dev/null || true

# Default editor
export EDITOR="${EDITOR:-vim}"
export VISUAL="${VISUAL:-$EDITOR}"

# Locale
export LANG="${LANG:-en_US.UTF-8}"
export LC_ALL="${LC_ALL:-en_US.UTF-8}"
