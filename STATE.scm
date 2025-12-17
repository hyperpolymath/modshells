;;; STATE.scm - Project Checkpoint
;;; modshells
;;; Format: Guile Scheme S-expressions
;;; Purpose: Preserve AI conversation context across sessions
;;; Reference: https://github.com/hyperpolymath/state.scm

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

;;;============================================================================
;;; METADATA
;;;============================================================================

(define metadata
  '((version . "0.1.0")
    (schema-version . "1.0")
    (created . "2025-12-15")
    (updated . "2025-12-17")
    (project . "modshells")
    (repo . "github.com/hyperpolymath/modshells")))

;;;============================================================================
;;; PROJECT CONTEXT
;;;============================================================================

(define project-context
  '((name . "modshells")
    (tagline . "Declarative shell configuration manager for modular environments")
    (version . "0.1.0")
    (license . "AGPL-3.0-or-later")
    (rsr-compliance . "gold-target")

    (tech-stack
     ((primary . "Ada/GNAT")
      (build . "gprbuild + GNAT project files")
      (package-manager . "Guix (primary), Nix (fallback)")
      (ci-cd . "GitHub Actions + GitLab CI")
      (security . "OSSF Scorecard + TruffleHog + Custom workflow linter")))))

;;;============================================================================
;;; CURRENT POSITION
;;;============================================================================

(define current-position
  '((phase . "v0.1 - Initial Setup and RSR Compliance")
    (overall-completion . 30)

    (components
     ((rsr-compliance
       ((status . "complete")
        (completion . 100)
        (notes . "SHA-pinned actions, SPDX headers, multi-platform CI")))

      (documentation
       ((status . "foundation")
        (completion . 40)
        (notes . "README, META/ECOSYSTEM/STATE.scm, CITATIONS complete")))

      (testing
       ((status . "minimal")
        (completion . 10)
        (notes . "CI/CD scaffolding exists, AUnit framework ready")))

      (core-functionality
       ((status . "in-progress")
        (completion . 30)
        (notes . "Shell detection and config path resolution implemented")))

      (security
       ((status . "complete")
        (completion . 100)
        (notes . "All workflow actions SHA-pinned, security.txt valid")))))

    (working-features
     ("RSR-compliant CI/CD pipeline"
      "Multi-platform mirroring (GitHub, GitLab)"
      "SPDX license headers on all files"
      "SHA-pinned GitHub Actions (all workflows)"
      "Security.txt with valid expiry date"
      "Guix package with GNAT build system"
      "Shell detection (Bash, Zsh, Fish, Nushell, etc.)"
      "Config path resolution with env fallback"))))

;;;============================================================================
;;; ROUTE TO MVP (ROADMAP)
;;;============================================================================

(define route-to-mvp
  '((target-version . "1.0.0")
    (definition . "Production-ready shell configuration manager")

    (milestones
     ((v0.1
       ((name . "Initial Setup and RSR Compliance")
        (status . "complete")
        (target-date . "2025-12")
        (items
         ("RSR Gold compliance" . "done")
         ("Multi-platform CI/CD" . "done")
         ("Security hardening" . "done")
         ("Guix package definition" . "done")
         ("Basic Ada structure" . "done"))))

      (v0.2
       ((name . "Core Shell Management")
        (status . "in-progress")
        (target-date . "2025-Q1")
        (items
         ("Complete Shell_Manager implementation" . "pending")
         ("Modular directory creation" . "partial")
         ("Shell config injection" . "pending")
         ("Idempotency checks" . "pending")
         ("Unit tests with AUnit" . "pending"))))

      (v0.3
       ((name . "Configuration Store")
        (status . "pending")
        (target-date . "2025-Q1")
        (items
         ("Config file parsing" . "pending")
         ("TOML/YAML support" . "pending")
         ("Environment variable expansion" . "pending")
         ("Config validation" . "pending"))))

      (v0.5
       ((name . "Feature Complete")
        (status . "pending")
        (target-date . "2025-Q2")
        (items
         ("All shell backends supported" . "pending")
         ("Cross-platform paths" . "pending")
         ("Test coverage > 70%" . "pending")
         ("API stability" . "pending")
         ("Integration tests" . "pending"))))

      (v0.8
       ((name . "Polish and Documentation")
        (status . "pending")
        (target-date . "2025-Q2")
        (items
         ("User documentation" . "pending")
         ("Man pages" . "pending")
         ("Example configurations" . "pending")
         ("Migration guides" . "pending"))))

      (v1.0
       ((name . "Production Release")
        (status . "pending")
        (target-date . "2025-Q3")
        (items
         ("Security audit" . "pending")
         ("Performance optimization" . "pending")
         ("Packaging for distros" . "pending")
         ("Guix channel publication" . "pending"))))))))

;;;============================================================================
;;; BLOCKERS & ISSUES
;;;============================================================================

(define blockers-and-issues
  '((critical
     ())  ;; No critical blockers

    (high-priority
     ())  ;; No high-priority blockers

    (medium-priority
     ((test-coverage
       ((description . "Limited test infrastructure")
        (impact . "Risk of regressions")
        (needed . "AUnit test suites for all packages")))))

    (low-priority
     ((documentation-gaps
       ((description . "Some documentation areas incomplete")
        (impact . "Harder for new contributors")
        (needed . "Expand API documentation")))))))

;;;============================================================================
;;; CRITICAL NEXT ACTIONS
;;;============================================================================

(define critical-next-actions
  '((immediate
     (("Complete Shell_Manager.Create_Modshell_Directories" . high)
      ("Implement Shell_Manager.Modularise_Config" . high)
      ("Add AUnit test framework setup" . high)))

    (this-week
     (("Implement shell detection for all supported shells" . high)
      ("Add idempotency checks" . medium)
      ("Write tests for Config_Store" . medium)))

    (this-month
     (("Reach v0.2 milestone" . high)
      ("Complete Shell_Manager package" . high)
      ("Begin Config_Store implementation" . medium)))))

;;;============================================================================
;;; SESSION HISTORY
;;;============================================================================

(define session-history
  '((snapshots
     ((date . "2025-12-17")
      (session . "scm-security-audit")
      (accomplishments
       ("Fixed security.txt expiry placeholder with valid ISO 8601 date"
        "SHA-pinned editorconfig-checker action in quality.yml"
        "Fixed modshells.gpr main file reference"
        "Enhanced guix.scm with GNAT build system and inputs"
        "Added SPDX headers to guix.scm"
        "Updated STATE.scm with comprehensive roadmap"))
      (notes . "Security audit and SCM file improvements"))

     ((date . "2025-12-15")
      (session . "initial-state-creation")
      (accomplishments
       ("Added META.scm, ECOSYSTEM.scm, STATE.scm"
        "Established RSR compliance"
        "Created initial project checkpoint"))
      (notes . "First STATE.scm checkpoint created via automated script")))))

;;;============================================================================
;;; HELPER FUNCTIONS (for Guile evaluation)
;;;============================================================================

(define (get-completion-percentage component)
  "Get completion percentage for a component"
  (let ((comp (assoc component (cdr (assoc 'components current-position)))))
    (if comp
        (cdr (assoc 'completion (cdr comp)))
        #f)))

(define (get-blockers priority)
  "Get blockers by priority level"
  (cdr (assoc priority blockers-and-issues)))

(define (get-milestone version)
  "Get milestone details by version"
  (assoc version (cdr (assoc 'milestones route-to-mvp))))

;;;============================================================================
;;; EXPORT SUMMARY
;;;============================================================================

(define state-summary
  '((project . "modshells")
    (version . "0.1.0")
    (overall-completion . 30)
    (current-milestone . "v0.1 - Initial Setup (complete)")
    (next-milestone . "v0.2 - Core Shell Management")
    (critical-blockers . 0)
    (high-priority-issues . 0)
    (updated . "2025-12-17")))

;;; End of STATE.scm
