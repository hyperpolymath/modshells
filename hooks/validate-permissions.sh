#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Pre-commit hook: Validate workflow permissions declarations
set -euo pipefail
ERRORS=0
for workflow in .github/workflows/*.yml .github/workflows/*.yaml; do
    [ -f "$workflow" ] || continue
    if ! grep -qE '^permissions:' "$workflow"; then
        echo "ERROR: Missing top-level permissions in $workflow"
        ERRORS=$((ERRORS + 1))
    fi
done
[ $ERRORS -gt 0 ] && exit 1
exit 0
