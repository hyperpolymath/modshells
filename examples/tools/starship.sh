# SPDX-License-Identifier: PMPL-1.0-or-later
# tools/starship.sh - Starship prompt initialisation

# Check if starship is installed
command -v starship >/dev/null 2>&1 || return 0

# Initialise starship prompt
eval "$(starship init bash)"
