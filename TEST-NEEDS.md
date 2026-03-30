# TEST-NEEDS.md — modshells

> Generated 2026-03-29 by punishing audit.

## Current State

| Category     | Count | Notes |
|-------------|-------|-------|
| Unit tests   | 1     | tests/test_shell_manager.adb |
| Integration  | 1     | tests/smoke_test.sh |
| E2E          | 0     | None |
| Benchmarks   | 0     | None |

**Source modules:** 7 Ada source files: config_store (adb+ads), modshells main (adb), shell_manager (adb+ads), shell_validator (adb+ads).

## What's Missing

### P2P (Property-Based) Tests
- [ ] Config store: property tests for config read/write roundtrip
- [ ] Shell validator: arbitrary shell path/config validation
- [ ] Shell manager: property tests for shell registration/deregistration invariants

### E2E Tests
- [ ] Full lifecycle: register shell -> configure -> validate -> switch -> use -> deregister
- [ ] Multi-shell: register multiple shells, switch between them, verify isolation
- [ ] Config persistence: save config -> restart -> verify restored

### Aspect Tests
- **Security:** No tests for shell path injection, config file tampering, privilege escalation through shell switching
- **Performance:** No shell switching latency benchmarks
- **Concurrency:** No tests for concurrent shell operations
- **Error handling:** No tests for invalid shell paths, corrupted config, missing shell binaries

### Build & Execution
- [ ] GNAT compilation
- [ ] Ada test runner (AUnit or similar)
- [ ] Smoke test execution

### Benchmarks Needed
- [ ] Shell switching time
- [ ] Config load/save time
- [ ] Shell validation throughput

### Self-Tests
- [ ] Shell binary existence verification
- [ ] Config file integrity check

## Priority

**HIGH.** 7 source files with 1 unit test and 1 smoke test. Shell management is security-sensitive (shell injection, privilege). The shell_validator and config_store both lack any tests. The smoke test covers only the happy path.

## FAKE-FUZZ ALERT

- `tests/fuzz/placeholder.txt` is a scorecard placeholder inherited from rsr-template-repo — it does NOT provide real fuzz testing
- Replace with an actual fuzz harness (see rsr-template-repo/tests/fuzz/README.adoc) or remove the file
- Priority: P2 — creates false impression of fuzz coverage
