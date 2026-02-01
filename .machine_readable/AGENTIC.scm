;; SPDX-License-Identifier: MPL-2.0-or-later
;; AGENTIC.scm - AI agent interaction patterns for modshells

(define agentic-config
  `((version . "1.0.0")
    (claude-code
      ((model . "claude-opus-4-5-20251101")
       (tools . ("read" "edit" "bash" "grep" "glob"))
       (permissions . "read-all")))
    (patterns
      ((code-review
         (style . "thorough")
         (focus . ("Ada idioms" "safety patterns" "SPARK-ready code")))
       (refactoring
         (style . "conservative")
         (preserve . ("idempotency" "signature markers" "backup logic")))
       (testing
         (style . "comprehensive")
         (coverage . ("shell detection" "config backup" "injection safety")))))
    (constraints
      ((languages
         (allowed . ("ada" "nushell" "posix-shell"))
         (ada-usage . "core implementation")
         (nushell-usage . "automation scripts")
         (shell-usage . "git hooks only"))
       (banned . ("typescript" "go" "python" "makefile" "javascript"))))
    (interaction-modes
      ((implementation
         (approach . "safety-first")
         (verify . ("backup creation" "signature detection" "idempotency")))
       (review
         (check . ("SPDX headers" "Ada naming conventions" "exception handling")))
       (documentation
         (format . "asciidoc")
         (examples . "include runnable examples"))))
    (project-specific
      ((shell-types . ("Bash" "Zsh" "Fish" "Nushell" "Ion" "Oils" "Tcsh" "Ksh" "Dash" "PowerShell"))
       (config-categories . ("core" "tools" "misc" "os" "ui"))
       (signature-markers . ("MODSHELLS_START" "MODSHELLS_END"))
       (backup-format . ".modshells-backup-YYYYMMDD-HHMMSS")))))