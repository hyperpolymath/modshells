<!--
SPDX-License-Identifier: CC-BY-SA-4.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# Proof Requirements

<!-- Created 2026-05-18 by estate proof-debt audit; reconciled 2026-05-19. -->

## Current state (reconciled 2026-05-19)

The original 2026-05-18 audit entry claimed "Rust only, no SPARK present".
That was **stale**. Ground truth (direct census 2026-05-19):

- Source is **Ada** (9 units: `config_store`, `shell_validator`,
  `shell_manager`, `modshells` main, tests), **not** "Rust only".
- **Zero `SPARK_Mode` markers** anywhere. There is therefore **no SPARK
  proof theatre to demote**, and nothing for the estate SPARK Theatre Gate
  (hyperpolymath/standards#135) to flag — modshells passes it vacuously.
- A Rust/SPARK rewrite is in flight on branch `wip/rust-spark-migration`
  (bounded consolidation into one verified binary; tracked in the
  migration plan outside this repo).

## SPARK proof stance — DOCUMENTED NO-OP / EXEMPTION

**No SPARK proof action is owed for modshells in the current audit cycle.**
This is an explicit, deliberate exemption, not unresolved drift:

1. The Ada units are *migration source*, scheduled for replacement by the
   `wip/rust-spark-migration` Rust/SPARK consolidation. Adding SPARK
   contracts to soon-to-be-deleted Ada would be wasted effort.
2. Because there are zero `SPARK_Mode` markers, modshells makes **no proof
   claim** — it cannot be "SPARK theatre" (theatre requires a hollow
   claim). It is honest by absence.
3. Estate audits and the SPARK Theatre Gate should treat modshells as
   **resolved-by-exemption** and not re-triage it until the Rust/SPARK
   migration lands, at which point the *Rust/SPARK* tier (seam + stance)
   is assessed against the new binary, not the Ada source.

Re-open trigger: merge of `wip/rust-spark-migration` to `main` (assess the
Rust/SPARK seam then), or any new `SPARK_Mode` marker appearing in the Ada
source before then (would reintroduce theatre risk).

## Recommended prover

- **Idris2** if a contract surface is warranted post-migration; otherwise
  N/A under the exemption above.

## Priority

**RESOLVED-BY-EXEMPTION** (2026-05-19). Was LOW/policy-relevant; the
seam-or-exemption decision is now made: documented exemption pending the
`wip/rust-spark-migration` rewrite. No SPARK work owed this cycle.
