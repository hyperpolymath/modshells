# PROOF-NEEDS.md — modshells

## Current State

- **src/abi/*.idr**: NO
- **Dangerous patterns**: 0
- **LOC**: ~2,900
- **ABI layer**: Missing

## What Needs Proving

| Component | What | Why |
|-----------|------|-----|
| Shell config validation | Validator rejects all malicious shell configurations | Shell configs can contain arbitrary code execution |
| Config store integrity | Stored configs are not corrupted or tampered | Corrupted shell config breaks user's terminal |
| Shell manager isolation | Module loading does not leak between shell contexts | Cross-contamination between shell environments |

## Recommended Prover

**Idris2** — Create `src/abi/` with config validation types. Shell config parsing is security-sensitive (arbitrary code in shell configs).

## Priority

**LOW** — Shell configuration manager. Config validation preventing code injection is the only security-relevant proof target. Small blast radius.
