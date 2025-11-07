#!/usr/bin/env nu

# Define standard permissions for source/text files (read/write by owner)
let rw_perms = "u+rw,go-w"
# Define executable permissions (read/write/execute by owner)
let rwx_perms = "u+rwx,go-w,go+r"

print "Setting default read/write permissions and removing execute flags..."
# ... (Your full Nushell logic for chmod goes here) ...
# Ensure the final script is executable itself
^chmod +x scripts/safer-perms.nu

print "Permissions dynamic enforcement complete."
