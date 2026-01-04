;; SPDX-License-Identifier: AGPL-3.0-or-later
;; PLAYBOOK.scm - Operational runbook for modshells

(define playbook
  `((version . "1.0.0")
    (procedures
      ((build
         ((prerequisites . ("GNAT compiler" "GPRBuild"))
          (commands
            ((step-1 . "gprbuild -p -j0 modshells.gpr")
             (step-2 . "ls bin/modshells")))
          (verification . "./bin/modshells --version")))
       (test
         ((unit-tests
            ((build . "gprbuild -p -j0 -P tests/tests.gpr")
             (run . "./bin/test_shell_manager")))
          (smoke-tests
            ((run . "./tests/smoke_test.sh")))
          (integration
            ((setup . "Create test user with multiple shells")
             (run . "./bin/modshells")
             (verify . "Check ~/.bashrc, ~/.zshrc for signatures")))))
       (release
         ((pre-release
            ((update-version . "Edit modshells.adb version string")
             (run-tests . "just test")
             (update-changelog . "Document changes in CHANGELOG.adoc")))
          (release
            ((tag . "git tag -s vX.Y.Z -m 'Release vX.Y.Z'")
             (push . "git push origin main --tags")
             (github-release . "gh release create vX.Y.Z --generate-notes")))
          (post-release
            ((update-guix . "Submit package to guix-patches")
             (update-nix . "Update flake.nix with new version")))))
       (deploy
         ((local-install
            ((build . "gprbuild -p -j0 modshells.gpr")
             (install . "cp bin/modshells ~/.local/bin/")))
          (guix-install . "guix install modshells")
          (nix-install . "nix profile install github:hyperpolymath/modshells")))
       (rollback
         ((shell-config
            ((identify . "Find latest .modshells-backup-* file")
             (restore . "cp ~/.bashrc.modshells-backup-TIMESTAMP ~/.bashrc")
             (verify . "source ~/.bashrc")))
          (binary
            ((identify . "Check previous version in git tags")
             (checkout . "git checkout vX.Y.Z")
             (rebuild . "gprbuild -p -j0 modshells.gpr")))))
       (debug
         ((shell-detection
            ((check-paths . "ls /usr/bin/bash /bin/zsh /usr/local/bin/fish")
             (trace . "Run modshells with GNAT traceback")
             (verbose . "Future: modshells --verbose")))
          (config-injection
            ((check-signature . "grep MODSHELLS_START ~/.bashrc")
             (check-backup . "ls -la ~/.bashrc.modshells-backup-*")
             (manual-test . "source ~/.bashrc")))
          (validation
            ((shellcheck . "shellcheck ~/.config/nushell/modshells/**/*.sh")
             (syntax . "bash -n ~/.config/nushell/modshells/**/*.sh")))))))
    (alerts
      ((backup-failure
         (severity . "critical")
         (description . "Failed to create backup before config modification")
         (response . "Abort operation, check disk space and permissions"))
       (signature-mismatch
         (severity . "warning")
         (description . "MODSHELLS_START found but MODSHELLS_END missing")
         (response . "Manual inspection required, may need to clean up config"))
       (shell-not-found
         (severity . "info")
         (description . "Configured shell not found in binary paths")
         (response . "Normal for shells not installed on system"))
       (validation-failure
         (severity . "warning")
         (description . "Syntax validation failed for config snippet")
         (response . "Review snippet, run shellcheck manually"))))
    (contacts
      ((maintainer
         (name . "hyperpolymath")
         (github . "@hyperpolymath")
         (role . "Primary maintainer"))
       (security
         (email . "security@hyperpolymath.dev")
         (pgp . "See SECURITY.md for key")
         (response-time . "48 hours"))))))