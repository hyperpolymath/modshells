#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Pre-commit hook: Validate GitHub Actions are SHA-pinned

set -euo pipefail

ERRORS=0

for workflow in .github/workflows/*.yml .github/workflows/*.yaml; do
    [ -f "$workflow" ] || continue
    
    # Find uses: lines that aren't SHA-pinned
    while IFS= read -r line; do
        if [[ "$line" =~ uses:.*@ ]]; then
            # Check if it has a SHA (40 hex chars)
            if ! echo "$line" | grep -qE '@[a-f0-9]{40}'; then
                echo "ERROR: Unpinned action in $workflow"
                echo "  $line"
                echo "  Actions must use SHA pins: uses: action/name@SHA # version"
                ERRORS=$((ERRORS + 1))
            fi
        fi
    done < "$workflow"
done

if [ $ERRORS -gt 0 ]; then
    echo ""
    echo "Found $ERRORS unpinned actions. Please SHA-pin all GitHub Actions."
    echo "Use: gh api repos/OWNER/REPO/git/matching-refs/tags/VERSION to find SHAs"
    exit 1
fi

exit 0
