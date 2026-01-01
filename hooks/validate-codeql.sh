#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Pre-commit hook: Validate CodeQL language matrix matches repo
set -euo pipefail

CODEQL_FILE=".github/workflows/codeql.yml"
[ -f "$CODEQL_FILE" ] || exit 0

# Detect languages in repo
HAS_JS=$(find . -name "*.js" -o -name "*.ts" -o -name "*.jsx" -o -name "*.tsx" 2>/dev/null | grep -v node_modules | head -1)
HAS_PY=$(find . -name "*.py" 2>/dev/null | grep -v __pycache__ | head -1)
HAS_GO=$(find . -name "*.go" 2>/dev/null | head -1)
HAS_RS=$(find . -name "*.rs" 2>/dev/null | head -1)

# Check if matrix includes unsupported languages
if grep -q "language:.*python" "$CODEQL_FILE" && [ -z "$HAS_PY" ]; then
    echo "WARNING: CodeQL configured for Python but no .py files found"
fi
if grep -q "language:.*go" "$CODEQL_FILE" && [ -z "$HAS_GO" ]; then
    echo "WARNING: CodeQL configured for Go but no .go files found"
fi
if grep -q "language:.*javascript" "$CODEQL_FILE" && [ -z "$HAS_JS" ]; then
    echo "WARNING: CodeQL configured for JavaScript but no JS/TS files found"
fi

# Rust/OCaml are not supported - should use 'actions' only
if [ -n "$HAS_RS" ]; then
    if grep -q "language:.*rust" "$CODEQL_FILE"; then
        echo "ERROR: CodeQL does not support Rust - use ['actions'] instead"
        exit 1
    fi
fi

exit 0
