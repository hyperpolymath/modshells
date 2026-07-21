# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Development environment for modshells.
#
# Estate policy is Guix primary / Nix fallback (hyperpolymath/standards).
# This is the Nix fallback tier. It is a dev shell, not a package build:
# it declares the toolchain needed to work on this repo, pinned to an
# exact nixpkgs revision per the estate SHA-pinning rule.
#
# Packages mirror the build tooling actually present in this repo
# (just) — not a generic estate default.
#
#   nix develop      # enter the shell
#   nix flake check  # verify this file evaluates (run before committing)
{
  description = "modshells development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/b134951a4c9f3c995fd7be05f3243f8ecd65d798";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [ just ];
        };
      });
    };
}
