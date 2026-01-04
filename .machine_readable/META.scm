;; SPDX-License-Identifier: AGPL-3.0-or-later
;; META.scm - Meta-level information for modshells
;; Media-Type: application/meta+scheme

(meta
  (architecture-decisions
    (adr-001
      (status "accepted")
      (date "2024-01-01")
      (title "Use Ada for core implementation")
      (context "Need a language with strong static typing and safety guarantees for a tool that modifies critical shell configuration files")
      (decision "Implement core logic in Ada using GNAT compiler and GPRBuild")
      (consequences
        ("Strong compile-time error detection")
        ("Deterministic memory management")
        ("Verbose but explicit code")
        ("Smaller contributor pool than mainstream languages")))
    (adr-002
      (status "accepted")
      (date "2024-01-01")
      (title "Signature-based idempotency")
      (context "Tool must be safe to run multiple times without duplicating configuration")
      (decision "Use MODSHELLS_START/END comment markers to detect existing modularisation")
      (consequences
        ("Simple grep-based detection")
        ("Works across all shell types")
        ("User must not remove markers")))
    (adr-003
      (status "accepted")
      (date "2024-01-01")
      (title "Nushell as primary target shell")
      (context "Need a modern shell to demonstrate modular configuration benefits")
      (decision "Default config path is ~/.config/nushell/modshells but support all 10 shells")
      (consequences
        ("Nushell users get first-class experience")
        ("POSIX shells remain fully supported")
        ("Documentation emphasizes cross-shell compatibility")))
    (adr-004
      (status "accepted")
      (date "2024-06-01")
      (title "Shell Validator as separate package")
      (context "Validation logic is complex enough to warrant separation from Shell_Manager")
      (decision "Create Shell_Validator package with comprehensive validation capabilities")
      (consequences
        ("Clear separation of concerns")
        ("Validator can evolve independently")
        ("Larger codebase but better maintainability"))))

  (development-practices
    (code-style
      (language "Ada")
      (naming "CamelCase for types, Underscore_Separated for procedures and variables")
      (documentation "Ada comments with -- for inline, package headers for API")
      (line-length 100))
    (security
      (principle "Defense in depth")
      (backup-before-modify "Always create timestamped backup")
      (signature-verification "Check for existing markers before injection")
      (no-eval "Never execute user-provided code"))
    (testing
      (unit "AUnit for Ada unit tests")
      (integration "Shell-based smoke tests")
      (coverage "Target 80% for core packages"))
    (versioning "SemVer")
    (documentation "AsciiDoc for user docs, Ada comments for code")
    (branching "main for stable, feature branches for development"))

  (design-rationale
    (why-ada "Ada provides memory safety and strong typing without runtime overhead, critical for a tool modifying shell configs")
    (why-modular-dirs "Five categories (core/tools/misc/os/ui) provide natural separation without excessive fragmentation")
    (why-alphabetical-ordering "Numeric prefixes (00-path.sh, 10-history.sh) give explicit control while maintaining simplicity")
    (why-shell-agnostic-goal "Users often work across multiple shells; unified config reduces duplication")
    (why-backup-always "Shell misconfiguration can lock users out; timestamped backups enable recovery")
    (why-10-shells "Coverage of major POSIX shells plus modern alternatives (Fish, Nushell, Ion, PowerShell)")))