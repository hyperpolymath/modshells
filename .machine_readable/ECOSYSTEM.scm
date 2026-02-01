;; SPDX-License-Identifier: MPL-2.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for modshells
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "modshells")
  (type "cli-tool")
  (purpose "Modular shell configuration management with safety-first design")

  (position-in-ecosystem
    (category "developer-tools")
    (subcategory "shell-configuration")
    (unique-value
      ("Written in Ada for safety-critical reliability")
      ("Supports 10 shells including modern alternatives")
      ("Idempotent operations with signature-based detection")
      ("Automatic backup before any modification")))

  (related-projects
    (oh-my-zsh
      (relationship "alternative-to")
      (description "Popular Zsh framework with plugins and themes")
      (differentiation "modshells is shell-agnostic, simpler, safety-focused"))
    (prezto
      (relationship "alternative-to")
      (description "Zsh configuration framework")
      (differentiation "modshells supports 10 shells, not just Zsh"))
    (bash-it
      (relationship "alternative-to")
      (description "Bash community framework")
      (differentiation "modshells is cross-shell and uses Ada"))
    (fisher
      (relationship "complementary")
      (description "Fish plugin manager")
      (differentiation "modshells manages config structure, fisher manages plugins"))
    (starship
      (relationship "integration-target")
      (description "Cross-shell prompt")
      (integration "modshells provides starship.sh example in tools/"))
    (direnv
      (relationship "integration-target")
      (description "Environment switcher")
      (integration "Compatible with modular sourcing approach"))
    (asdf
      (relationship "integration-target")
      (description "Version manager")
      (integration "Example configs for asdf in tools/"))
    (chezmoi
      (relationship "complementary")
      (description "Dotfiles manager")
      (differentiation "chezmoi manages files, modshells manages structure"))
    (yadm
      (relationship "complementary")
      (description "Yet Another Dotfiles Manager")
      (differentiation "Different focus - yadm is git-based, modshells is structure-based"))
    (nushell
      (relationship "primary-target")
      (description "Modern structured data shell")
      (integration "Default config path targets Nushell"))
    (carapace
      (relationship "integration-target")
      (description "Multi-shell completion engine")
      (integration "Can be configured in tools/ directory")))

  (what-this-is
    ("A CLI tool for modularising shell configuration files")
    ("A safety-first approach to dotfiles management")
    ("Cross-shell configuration structure")
    ("Idempotent configuration injector")
    ("Backup-aware shell config modifier"))

  (what-this-is-not
    ("A shell implementation")
    ("A plugin manager like oh-my-zsh or fisher")
    ("A dotfiles synchronisation tool like chezmoi")
    ("A shell prompt like starship")
    ("A version manager like asdf or mise")))