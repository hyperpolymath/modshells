;; SPDX-License-Identifier: MPL-2.0-or-later
;; STATE.scm - Project state for modshells
;; Media-Type: application/vnd.state+scm

(state
  (metadata
    (version "0.1.0")
    (schema-version "1.0")
    (created "2024-01-01")
    (updated "2026-01-04")
    (project "modshells")
    (repo "github.com/hyperpolymath/modshells"))

  (project-context
    (name "modshells")
    (tagline "Modular Shell Configuration Manager")
    (tech-stack
      ("Ada" "primary-language" "safety-critical-reliability")
      ("Nushell" "automation-scripts" "modern-shell")
      ("POSIX-Shell" "git-hooks" "minimal-use")
      ("GPRBuild" "build-system" "gnat-project-builder")))

  (current-position
    (phase "v0.1-complete")
    (overall-completion 25)
    (components
      (modshells-main
        (status "complete")
        (file "src/main/modshells.adb")
        (description "Entry point with initialization flow"))
      (config-store
        (status "complete")
        (files "src/config_store/config_store.ads" "src/config_store/config_store.adb")
        (description "Path resolution from env vars and home directory"))
      (shell-manager
        (status "complete")
        (files "src/shell_manager/shell_manager.ads" "src/shell_manager/shell_manager.adb")
        (description "Shell detection, directory creation, modularisation"))
      (shell-validator
        (status "in-progress")
        (files "src/shell_validator/shell_validator.ads" "src/shell_validator/shell_validator.adb")
        (description "Comprehensive validation, syntax checking, security analysis"))
      (examples
        (status "complete")
        (directory "examples/")
        (description "Ready-to-use configuration snippets for all categories")))
    (working-features
      ("Directory creation - idempotent core/tools/misc/os/ui structure")
      ("Shell detection - checks /usr/bin /bin /usr/local/bin /opt/homebrew/bin")
      ("Config backup - timestamped .modshells-backup-YYYYMMDD-HHMMSS")
      ("Source injection - shell-specific syntax for 10 shells")
      ("Idempotency - signature-based detection prevents duplicates")
      ("Multi-shell support - Bash Zsh Fish Nushell Ion Oils Tcsh Ksh Dash PowerShell")))

  (route-to-mvp
    (milestones
      (milestone-1
        (name "Core Functionality")
        (version "v0.1")
        (status "complete")
        (items
          ("Shell detection via binary path checking" "complete")
          ("Configuration file path mapping" "complete")
          ("Timestamped backup creation" "complete")
          ("Shell-specific source injection" "complete")
          ("Signature-based idempotency" "complete")))
      (milestone-2
        (name "Multi-Shell Orchestration")
        (version "v0.5")
        (status "planned")
        (items
          ("Command-line arguments --shell=bash,zsh --dry-run" "pending")
          ("Shell-agnostic .modshells config format" "pending")
          ("Configuration drift detection" "pending")
          ("Import existing configs into modular structure" "pending")))
      (milestone-3
        (name "Configuration Management")
        (version "v1.0")
        (status "planned")
        (items
          ("Snippet management commands add/list/enable/disable" "pending")
          ("Template system for common configurations" "pending")
          ("Syntax validation before injection" "pending")
          ("Security audit for exposed secrets" "pending")))
      (milestone-4
        (name "Distribution and Integration")
        (version "v1.5")
        (status "planned")
        (items
          ("Guix package definition guix.scm" "pending")
          ("Nix flake flake.nix" "pending")
          ("Starship direnv asdf integration" "pending")
          ("Migration tools from oh-my-zsh prezto bash-it" "pending")))
      (milestone-5
        (name "Comprehensive Validation")
        (version "v2.0")
        (status "in-progress")
        (items
          ("Syntax validation via shell -n and shellcheck" "in-progress")
          ("Command sequence validation" "pending")
          ("Permission and ownership validation" "pending")
          ("Duplicate alias/function detection" "pending")
          ("POSIX compliance checking" "pending")
          ("Security validation for secrets and dangerous commands" "pending")
          ("Performance profiling for shell startup" "pending")))))

  (blockers-and-issues
    (critical)
    (high
      ("Shell validator implementation needs completion"))
    (medium
      ("Justfile recipes are stubs - need actual build/test/clean commands")
      ("No unit test suite yet - AUnit integration pending")
      ("Man page and shell completion scripts not created"))
    (low
      ("PowerShell config path handling needs platform detection")
      ("Cross-compilation for macOS and Windows not configured")))

  (critical-next-actions
    (immediate
      ("Complete shell_validator.adb implementation")
      ("Add actual GPRBuild commands to justfile"))
    (this-week
      ("Create AUnit test suite for shell_manager")
      ("Add --dry-run command-line argument parsing"))
    (this-month
      ("Implement configuration drift detection")
      ("Create Guix package definition")
      ("Write man page modshells.1")))

  (session-history
    (session
      (date "2026-01-04")
      (focus "SCM files population")
      (accomplishments
        ("Populated comprehensive STATE.scm with project status")
        ("Created META.scm with architecture decisions")
        ("Defined ECOSYSTEM.scm with related projects")
        ("Configured AGENTIC.scm for AI patterns")
        ("Set up NEUROSYM.scm for validation approach")
        ("Documented PLAYBOOK.scm with operational procedures")))))