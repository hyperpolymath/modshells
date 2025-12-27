# SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
# core/00-path.sh - PATH modifications (loaded first)
#
# Add custom directories to PATH. Use numeric prefix to control load order.

# Local binaries
export PATH="$HOME/.local/bin:$PATH"

# Cargo (Rust)
[ -d "$HOME/.cargo/bin" ] && export PATH="$HOME/.cargo/bin:$PATH"

# Go
[ -d "$HOME/go/bin" ] && export PATH="$HOME/go/bin:$PATH"

# Deno
[ -d "$HOME/.deno/bin" ] && export PATH="$HOME/.deno/bin:$PATH"
