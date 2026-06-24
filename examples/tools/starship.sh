# SPDX-License-Identifier: MPL-2.0 OR MIT
# tools/starship.sh - Starship prompt initialisation

# Check if starship is installed
command -v starship >/dev/null 2>&1 || return 0

# Initialise starship prompt
eval "$(starship init bash)"
