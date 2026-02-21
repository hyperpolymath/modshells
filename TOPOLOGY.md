<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

# Modshells — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              SHELL USER                 │
                        │        (Bash, Zsh, Fish, Nushell, etc.) │
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           MODSHELLS CLI (ADA)           │
                        │    (Idempotent Configuration Manager)    │
                        └──────────┬───────────────────┬──────────┘
                                   │                   │
                                   ▼                   ▼
                        ┌───────────────────────┐  ┌────────────────────────────────┐
                        │ SHELL MANAGER         │  │ CONFIG STORE                   │
                        │ - Shell Detection     │  │ - Path Resolution              │
                        │ - Sourcing Injection  │  │ - Idempotency Checks           │
                        │ - Config Backups      │  │ - Directory Scaffolding        │
                        └──────────┬────────────┘  └──────────┬─────────────────────┘
                                   │                          │
                                   └────────────┬─────────────┘
                                                ▼
                        ┌─────────────────────────────────────────┐
                        │           MODULAR CONFIG HUB            │
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ core/     │  │ tools/    │  │ misc/ ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        │  ┌───────────┐  ┌───────────┐           │
                        │  │ os/       │  │ ui/       │           │
                        │  └───────────┘  └───────────┘           │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  GPRBuild / Ada     .machine_readable/  │
                        │  Smoke Tests (Sh)   0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE MANAGER (ADA)
  Shell Detection                   ██████████ 100%    10+ shells supported
  Directory Scaffolding             ██████████ 100%    Idempotent creation stable
  Source Injection                  ██████████ 100%    Shell-specific syntax verified
  Config Backup (Timestamped)       ██████████ 100%    Verification before edit stable

INTERFACES & CONFIG
  CLI Interface (modshells)         ██████████ 100%    v0.1 core functional
  Environment Path Resolution       ██████████ 100%    MODSHELLS_CONFIG_PATH active
  Idempotency (Signature-based)     ██████████ 100%    Duplicate prevention verified

REPO INFRASTRUCTURE
  GPRBuild Automation               ██████████ 100%    Standard build tasks
  .machine_readable/                ██████████ 100%    STATE tracking active
  Smoke & Unit Tests                ██████████ 100%    Full core logic coverage

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ██████████ 100%    v0.1 Core complete & stable
```

## Key Dependencies

```
Shell Binary ──────► Detection Logic ───► Backup Config ──► Sourcing Injected
     │                   │                   │                 │
     ▼                   ▼                   ▼                 ▼
Config Path ───────► Scaffolding ──────► Module Hub ──────► Shell Env
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
