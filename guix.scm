;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;
;; modshells - Guix Package Definition
;; Run: guix shell -D -f guix.scm

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix build-system gnat)
             ((guix licenses) #:prefix license:)
             (gnu packages base)
             (gnu packages ada))

(define-public modshells
  (package
    (name "modshells")
    (version "0.1.0")
    (source (local-file "." "modshells-checkout"
                        #:recursive? #t
                        #:select? (git-predicate ".")))
    (build-system gnat-build-system)
    (arguments
     '(#:gpr-file "modshells.gpr"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'create-output-dirs
           (lambda _
             (mkdir-p "obj")
             (mkdir-p "bin")
             #t)))))
    (native-inputs
     (list gprbuild gnat))
    (synopsis "Declarative shell configuration manager")
    (description
     "Modshells is a declarative configuration manager for shell environments.
It establishes modular shell configuration directories and provides idempotent
initialization of shell-agnostic configurations. Part of the RSR ecosystem.")
    (home-page "https://github.com/hyperpolymath/modshells")
    (license license:agpl3+)))

;; Return package for guix shell
modshells
