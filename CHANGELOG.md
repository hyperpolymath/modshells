<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Changelog

All notable changes to `modshells` will be documented in this file.

This file is generated from conventional commits by the
[`changelog-reusable.yml`](https://github.com/hyperpolymath/standards/blob/main/.github/workflows/changelog-reusable.yml)
workflow (`hyperpolymath/standards#206`). Adopt the workflow in this repo's CI to keep this file in sync automatically — see
[`templates/cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml)
for the canonical config.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- feat(validator): add comprehensive shell validation package
- feat: Switch mirror workflow to SSH
- feat(init): Rhodium Standard project structure (v0.0) with Ada, LMDB stubs, and GitLab CI.

### Fixed

- fix(ci): sync hypatia-scan.yml to canonical (#46)
- fix(ci): point CodeQL at `actions` so SAST runs every commit (#72) (#41)
- fix(ci): adopt canonical hypatia-scan.yml (#39)
- fix(security): update editorconfig SHA and CodeQL language
- fix: correct author name in CITATION.cff
- fix(ci): update quality.yml workflow
- fix: align CodeQL with repo languages (robot-cleaner)

### Changed

- refactor: Rename project to 'modshells' for conciseness and update GNAT project file (Rhodium Standard).

### Documentation

- docs(proof): modshells SPARK no-op exemption (reconcile stale audit) (#49)

### CI

- ci(spark): adopt estate SPARK Theatre Gate (#135) (#50)

## Pre-history

Prior commits to this file's introduction are recorded in git history but not formally classified into Keep-a-Changelog sections. To backfill, run `git cliff -o CHANGELOG.md` locally using the canonical [`cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml) — this is one-shot mechanical work.

---

<!-- This file was seeded by the 2026-05-26 estate tech-debt audit follow-up (Row-2 Phase 3); see [`hyperpolymath/standards/docs/audits/2026-05-26-estate-documentation-debt.md`](https://github.com/hyperpolymath/standards/blob/main/docs/audits/2026-05-26-estate-documentation-debt.md). -->
