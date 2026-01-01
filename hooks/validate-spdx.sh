#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Pre-commit hook: Validate SPDX headers in workflow files

set -euo pipefail

ERRORS=0
SPDX_PATTERN="^# SPDX-License-Identifier:"

for workflow in .github/workflows/*.yml .github/workflows/*.yaml; do
    [ -f "$workflow" ] || continue
    
    first_line=$(head -n1 "$workflow")
    if ! echo "$first_line" | grep -qE "$SPDX_PATTERN"; then
        echo "ERROR: Missing SPDX header in $workflow"
        echo "  First line should be: # SPDX-License-Identifier: AGPL-3.0-or-later"
        ERRORS=$((ERRORS + 1))
    fi
done

if [ $ERRORS -gt 0 ]; then
    exit 1
fi

exit 0
