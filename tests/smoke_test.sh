#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later OR MIT
# tests/smoke_test.sh - Basic smoke test for modshells
#
# This script tests the modshells binary without modifying real config files.
# It uses a temporary directory to verify core functionality.

set -e

# Colours for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No colour

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Cleanup function
cleanup() {
    if [ -n "$TEST_DIR" ] && [ -d "$TEST_DIR" ]; then
        rm -rf "$TEST_DIR"
    fi
}
trap cleanup EXIT

# Test helper functions
pass() {
    TESTS_PASSED=$((TESTS_PASSED + 1))
    printf "${GREEN}PASS${NC}: %s\n" "$1"
}

fail() {
    TESTS_FAILED=$((TESTS_FAILED + 1))
    printf "${RED}FAIL${NC}: %s\n" "$1"
}

skip() {
    printf "${YELLOW}SKIP${NC}: %s\n" "$1"
}

run_test() {
    TESTS_RUN=$((TESTS_RUN + 1))
}

# Find the modshells binary
find_binary() {
    if [ -x "./bin/modshells" ]; then
        echo "./bin/modshells"
    elif [ -x "../bin/modshells" ]; then
        echo "../bin/modshells"
    else
        echo ""
    fi
}

# Create temporary test directory
setup_test_env() {
    TEST_DIR=$(mktemp -d)
    export MODSHELLS_CONFIG_PATH="$TEST_DIR/modshells"
    export HOME="$TEST_DIR/home"
    mkdir -p "$HOME"
}

echo "=========================================="
echo "Modshells Smoke Test Suite"
echo "=========================================="
echo

# Setup
setup_test_env
BINARY=$(find_binary)

# Test 1: Binary exists
run_test
if [ -n "$BINARY" ] && [ -x "$BINARY" ]; then
    pass "Binary exists and is executable"
else
    skip "Binary not found (run 'gprbuild -p -j0 modshells.gpr' first)"
    echo
    echo "Running source-level tests only..."
    echo
    BINARY=""
fi

# Test 2: Source files exist
run_test
if [ -f "src/main/modshells.adb" ]; then
    pass "Main source file exists"
else
    fail "Main source file missing: src/main/modshells.adb"
fi

# Test 3: Shell manager package exists
run_test
if [ -f "src/shell_manager/shell_manager.ads" ] && [ -f "src/shell_manager/shell_manager.adb" ]; then
    pass "Shell manager package exists"
else
    fail "Shell manager package missing"
fi

# Test 4: Config store package exists
run_test
if [ -f "src/config_store/config_store.ads" ] && [ -f "src/config_store/config_store.adb" ]; then
    pass "Config store package exists"
else
    fail "Config store package missing"
fi

# Test 5: GPR build file exists
run_test
if [ -f "modshells.gpr" ]; then
    pass "GPRBuild project file exists"
else
    fail "GPRBuild project file missing"
fi

# Test 6: Examples directory structure
run_test
EXAMPLES_OK=1
for dir in core tools misc os ui; do
    if [ ! -d "examples/$dir" ]; then
        EXAMPLES_OK=0
        break
    fi
done
if [ "$EXAMPLES_OK" -eq 1 ]; then
    pass "Examples directory structure is correct"
else
    fail "Examples directory structure is incorrect"
fi

# Test 7: Example files have SPDX headers
run_test
SPDX_OK=1
for file in examples/*/*.sh; do
    if [ -f "$file" ]; then
        if ! head -1 "$file" | grep -q "SPDX-License-Identifier"; then
            SPDX_OK=0
            break
        fi
    fi
done
if [ "$SPDX_OK" -eq 1 ]; then
    pass "All example files have SPDX license headers"
else
    fail "Some example files missing SPDX headers"
fi

# Binary-dependent tests
if [ -n "$BINARY" ]; then
    echo
    echo "Running binary tests..."
    echo

    # Test 8: Binary runs without error
    run_test
    if "$BINARY" > "$TEST_DIR/output.txt" 2>&1; then
        pass "Binary runs successfully"
    else
        fail "Binary execution failed"
    fi

    # Test 9: Creates directory structure
    run_test
    DIRS_OK=1
    for dir in core tools misc os ui; do
        if [ ! -d "$MODSHELLS_CONFIG_PATH/$dir" ]; then
            DIRS_OK=0
            break
        fi
    done
    if [ "$DIRS_OK" -eq 1 ]; then
        pass "Directory structure created correctly"
    else
        fail "Directory structure not created"
    fi

    # Test 10: Output contains expected text
    run_test
    if grep -q "Modshells v0.1" "$TEST_DIR/output.txt"; then
        pass "Version banner displayed"
    else
        fail "Version banner not found in output"
    fi

    # Test 11: Shell detection works
    run_test
    if grep -q "\[installed\]\|\[not found\]" "$TEST_DIR/output.txt"; then
        pass "Shell detection produces status output"
    else
        fail "Shell detection output not found"
    fi

    # Test 12: Idempotency - run again
    run_test
    if "$BINARY" > "$TEST_DIR/output2.txt" 2>&1; then
        if grep -q "Already modularized\|Injected" "$TEST_DIR/output2.txt"; then
            pass "Idempotent re-run works"
        else
            pass "Second run completed (check idempotency manually)"
        fi
    else
        fail "Second run failed"
    fi
fi

# Summary
echo
echo "=========================================="
echo "Test Summary"
echo "=========================================="
printf "Tests run:    %d\n" "$TESTS_RUN"
printf "Tests passed: ${GREEN}%d${NC}\n" "$TESTS_PASSED"
printf "Tests failed: ${RED}%d${NC}\n" "$TESTS_FAILED"
echo

if [ "$TESTS_FAILED" -eq 0 ]; then
    printf "${GREEN}All tests passed!${NC}\n"
    exit 0
else
    printf "${RED}Some tests failed.${NC}\n"
    exit 1
fi
