// SPDX-License-Identifier: PMPL-1.0-or-later
// tests/e2e/shell_lifecycle_test.ts - End-to-end shell lifecycle tests
//
// Tests full workflows: initialization, configuration, backup/restore, multi-shell management.

import {
  assertEquals,
  assert,
  assertStringIncludes,
} from "https://deno.land/std@0.208.0/assert/mod.ts";

// ============================================================================
// Mock Shell Manager
// ============================================================================

interface ShellConfig {
  name: string;
  path: string;
  configFile: string;
  isActive: boolean;
}

/**
 * Mock shell manager for E2E testing.
 */
class ShellManager {
  private shells: Map<string, ShellConfig> = new Map();
  private activeShell: string | null = null;
  private backups: Map<string, string> = new Map();

  /**
   * Initialize the shell manager with default shells.
   */
  initialize(): void {
    this.shells.set("bash", {
      name: "bash",
      path: "/bin/bash",
      configFile: "~/.bashrc",
      isActive: false,
    });
    this.shells.set("zsh", {
      name: "zsh",
      path: "/usr/bin/zsh",
      configFile: "~/.zshrc",
      isActive: false,
    });
    this.shells.set("fish", {
      name: "fish",
      path: "/usr/bin/fish",
      configFile: "~/.config/fish/config.fish",
      isActive: false,
    });
  }

  /**
   * Add a shell configuration.
   */
  addShell(name: string, path: string, configFile: string): void {
    this.shells.set(name, {
      name,
      path,
      configFile,
      isActive: false,
    });
  }

  /**
   * Get a shell by name.
   */
  getShell(name: string): ShellConfig | undefined {
    return this.shells.get(name);
  }

  /**
   * List all shells.
   */
  listShells(): ShellConfig[] {
    return Array.from(this.shells.values());
  }

  /**
   * Switch to a shell.
   */
  switchShell(name: string): boolean {
    if (!this.shells.has(name)) return false;

    // Deactivate current shell
    if (this.activeShell) {
      const current = this.shells.get(this.activeShell);
      if (current) current.isActive = false;
    }

    // Activate new shell
    const newShell = this.shells.get(name);
    if (newShell) {
      newShell.isActive = true;
      this.activeShell = name;
      return true;
    }

    return false;
  }

  /**
   * Get the active shell.
   */
  getActiveShell(): string | null {
    return this.activeShell;
  }

  /**
   * Backup a shell's config.
   */
  backupConfig(name: string): string {
    if (!this.shells.has(name)) throw new Error(`Shell ${name} not found`);

    const shell = this.shells.get(name)!;
    const backupKey = `${name}-backup-${Date.now()}`;
    this.backups.set(backupKey, JSON.stringify(shell));

    return backupKey;
  }

  /**
   * Restore a shell's config from backup.
   */
  restoreConfig(backupKey: string): boolean {
    if (!this.backups.has(backupKey)) return false;

    const backupData = this.backups.get(backupKey)!;
    const restored = JSON.parse(backupData) as ShellConfig;

    this.shells.set(restored.name, restored);
    return true;
  }

  /**
   * Verify a shell is configured correctly.
   */
  verifyShell(name: string): boolean {
    const shell = this.shells.get(name);
    if (!shell) return false;

    // Check basic contract: name, path, configFile must be set
    return (
      shell.name.length > 0 &&
      shell.path.length > 0 &&
      shell.configFile.length > 0
    );
  }
}

// ============================================================================
// E2E Tests
// ============================================================================

Deno.test("E2E: Initialize config and add shells", () => {
  const manager = new ShellManager();
  manager.initialize();

  const shells = manager.listShells();
  assertEquals(shells.length, 3);
  assertEquals(shells[0].name, "bash");
  assertEquals(shells[1].name, "zsh");
  assertEquals(shells[2].name, "fish");
});

Deno.test("E2E: Add custom shells", () => {
  const manager = new ShellManager();
  manager.initialize();

  manager.addShell("dash", "/bin/dash", "~/.dashrc");
  manager.addShell("ksh", "/bin/ksh", "~/.kshrc");

  const shells = manager.listShells();
  assertEquals(shells.length, 5);

  const dash = manager.getShell("dash");
  assertEquals(dash?.name, "dash");
  assertEquals(dash?.path, "/bin/dash");
});

Deno.test("E2E: Switch between shells", () => {
  const manager = new ShellManager();
  manager.initialize();

  assertEquals(manager.getActiveShell(), null);

  // Switch to bash
  const success1 = manager.switchShell("bash");
  assertEquals(success1, true);
  assertEquals(manager.getActiveShell(), "bash");

  const bash = manager.getShell("bash");
  assert(bash?.isActive);

  // Switch to zsh
  const success2 = manager.switchShell("zsh");
  assertEquals(success2, true);
  assertEquals(manager.getActiveShell(), "zsh");

  const zsh = manager.getShell("zsh");
  assert(zsh?.isActive);

  // bash should no longer be active
  const bashAfter = manager.getShell("bash");
  assertEquals(bashAfter?.isActive, false);
});

Deno.test("E2E: Verify shell after configuration", () => {
  const manager = new ShellManager();
  manager.initialize();

  const bashValid = manager.verifyShell("bash");
  assertEquals(bashValid, true);

  const nonexistent = manager.verifyShell("nonexistent");
  assertEquals(nonexistent, false);
});

Deno.test("E2E: List all configured shells", () => {
  const manager = new ShellManager();
  manager.initialize();

  manager.addShell("dash", "/bin/dash", "~/.dashrc");
  manager.addShell("ksh", "/bin/ksh", "~/.kshrc");
  manager.addShell("tcsh", "/bin/tcsh", "~/.tcshrc");

  const shells = manager.listShells();
  assertEquals(shells.length, 6);

  const names = shells.map((s) => s.name).sort();
  assertEquals(names.includes("bash"), true);
  assertEquals(names.includes("zsh"), true);
  assertEquals(names.includes("fish"), true);
  assertEquals(names.includes("dash"), true);
  assertEquals(names.includes("ksh"), true);
  assertEquals(names.includes("tcsh"), true);
});

Deno.test("E2E: Backup and restore shell configuration", () => {
  const manager = new ShellManager();
  manager.initialize();

  // Get original bash config
  const original = manager.getShell("bash");
  assertEquals(original?.name, "bash");

  // Backup
  const backupKey = manager.backupConfig("bash");
  assertStringIncludes(backupKey, "bash-backup");

  // Modify bash (simulate change)
  manager.addShell("bash", "/usr/local/bin/bash", "~/.bashrc_custom");

  const modified = manager.getShell("bash");
  assertEquals(modified?.path, "/usr/local/bin/bash");

  // Restore
  const restored = manager.restoreConfig(backupKey);
  assertEquals(restored, true);

  const afterRestore = manager.getShell("bash");
  assertEquals(afterRestore?.path, "/bin/bash");
  assertEquals(afterRestore?.configFile, "~/.bashrc");
});

Deno.test("E2E: Multi-shell workflow", () => {
  const manager = new ShellManager();
  manager.initialize();

  // 1. Configure 3 shells
  manager.addShell("custom1", "/opt/shells/custom1", "~/.custom1");
  manager.addShell("custom2", "/opt/shells/custom2", "~/.custom2");

  let shells = manager.listShells();
  assertEquals(shells.length, 5);

  // 2. Switch to first shell
  manager.switchShell("bash");
  assertEquals(manager.getActiveShell(), "bash");

  // 3. Verify all shells
  for (const shell of shells) {
    const valid = manager.verifyShell(shell.name);
    assertEquals(valid, true, `Shell ${shell.name} should be valid`);
  }

  // 4. Switch to another shell
  manager.switchShell("zsh");
  assertEquals(manager.getActiveShell(), "zsh");

  // 5. Check active status
  const bash = manager.getShell("bash");
  const zsh = manager.getShell("zsh");
  assertEquals(bash?.isActive, false);
  assertEquals(zsh?.isActive, true);

  // 6. Backup current config
  const backup = manager.backupConfig("zsh");
  assertStringIncludes(backup, "zsh-backup");

  // 7. List all shells again
  shells = manager.listShells();
  assertEquals(shells.length, 5);
});

Deno.test("E2E: Get shell returns correct configuration", () => {
  const manager = new ShellManager();
  manager.initialize();

  const bash = manager.getShell("bash");
  assert(bash);
  assertEquals(bash.name, "bash");
  assertEquals(bash.path, "/bin/bash");
  assertEquals(bash.configFile, "~/.bashrc");

  const zsh = manager.getShell("zsh");
  assert(zsh);
  assertEquals(zsh.name, "zsh");
  assertEquals(zsh.path, "/usr/bin/zsh");
  assertEquals(zsh.configFile, "~/.zshrc");
});

Deno.test("E2E: Error handling for nonexistent shells", () => {
  const manager = new ShellManager();
  manager.initialize();

  const result = manager.switchShell("nonexistent");
  assertEquals(result, false);

  const none = manager.getShell("nonexistent");
  assertEquals(none, undefined);
});
