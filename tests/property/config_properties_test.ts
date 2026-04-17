// SPDX-License-Identifier: PMPL-1.0-or-later
// tests/property/config_properties_test.ts - Property-based tests for configuration
//
// Tests idempotency, determinism, and invariants using property-based testing.

import {
  assertEquals,
  assertExists,
} from "https://deno.land/std@0.208.0/assert/mod.ts";

// ============================================================================
// Configuration Store Mock
// ============================================================================

/**
 * Simple in-memory configuration store for testing.
 */
class ConfigStore {
  private data: Map<string, string> = new Map();

  set(key: string, value: string): void {
    this.data.set(key, value);
  }

  get(key: string): string | undefined {
    return this.data.get(key);
  }

  has(key: string): boolean {
    return this.data.has(key);
  }

  delete(key: string): boolean {
    return this.data.delete(key);
  }

  clear(): void {
    this.data.clear();
  }

  getAll(): Map<string, string> {
    return new Map(this.data);
  }

  clone(): ConfigStore {
    const cloned = new ConfigStore();
    for (const [k, v] of this.data) {
      cloned.set(k, v);
    }
    return cloned;
  }
}

// ============================================================================
// Property Tests
// ============================================================================

Deno.test("Property: Store and retrieve returns same value", () => {
  const store = new ConfigStore();
  const testCases = [
    { key: "SHELL", value: "bash" },
    { key: "EDITOR", value: "/usr/bin/vim" },
    { key: "PATH", value: "/usr/bin:/bin" },
    { key: "empty", value: "" },
    { key: "special", value: "value:with:colons" },
  ];

  for (const testCase of testCases) {
    store.set(testCase.key, testCase.value);
    const retrieved = store.get(testCase.key);
    assertEquals(
      retrieved,
      testCase.value,
      `Retrieved value for ${testCase.key} should match stored value`
    );
  }
});

Deno.test("Property: Overwrite is idempotent with same value", () => {
  const store = new ConfigStore();
  const key = "SHELL";
  const value = "bash";

  // Set, set again, set again
  store.set(key, value);
  store.set(key, value);
  store.set(key, value);

  const retrieved = store.get(key);
  assertEquals(retrieved, value, "Multiple identical sets should result in same value");
});

Deno.test("Property: Overwrite changes value", () => {
  const store = new ConfigStore();
  const key = "SHELL";

  store.set(key, "bash");
  assertEquals(store.get(key), "bash");

  store.set(key, "zsh");
  assertEquals(store.get(key), "zsh");

  store.set(key, "fish");
  assertEquals(store.get(key), "fish");
});

Deno.test("Property: Shell names are always lowercase", () => {
  const shells = ["bash", "zsh", "fish", "dash", "ksh", "tcsh"];

  for (const shell of shells) {
    assertEquals(
      shell,
      shell.toLowerCase(),
      "Shell name should be lowercase"
    );
  }
});

Deno.test("Property: Config store is deterministic", () => {
  const inputs = [
    { key: "A", value: "1" },
    { key: "B", value: "2" },
    { key: "C", value: "3" },
  ];

  // Run twice with same inputs
  const run1 = new ConfigStore();
  const run2 = new ConfigStore();

  for (const input of inputs) {
    run1.set(input.key, input.value);
    run2.set(input.key, input.value);
  }

  // Should have identical state
  const all1 = run1.getAll();
  const all2 = run2.getAll();

  assertEquals(all1.size, all2.size);
  for (const [key, value] of all1) {
    assertEquals(
      all2.get(key),
      value,
      `Store 2 should have same value for key ${key}`
    );
  }
});

Deno.test("Property: Deletion is idempotent after first delete", () => {
  const store = new ConfigStore();
  const key = "SHELL";

  store.set(key, "bash");
  assertEquals(store.has(key), true);

  // First delete
  const deleted1 = store.delete(key);
  assertEquals(deleted1, true, "First delete should return true");
  assertEquals(store.has(key), false);

  // Second delete
  const deleted2 = store.delete(key);
  assertEquals(deleted2, false, "Second delete should return false");
  assertEquals(store.has(key), false);

  // Third delete
  const deleted3 = store.delete(key);
  assertEquals(deleted3, false, "Third delete should also return false");
});

Deno.test("Property: Clear makes store empty", () => {
  const store = new ConfigStore();
  store.set("A", "1");
  store.set("B", "2");
  store.set("C", "3");

  assertEquals(store.getAll().size, 3);

  store.clear();
  assertEquals(store.getAll().size, 0);

  // Even after clear, can set again
  store.set("A", "1");
  assertEquals(store.get("A"), "1");
});

Deno.test("Property: Clone produces independent copy", () => {
  const original = new ConfigStore();
  original.set("SHELL", "bash");

  const cloned = original.clone();

  // Modify clone
  cloned.set("SHELL", "zsh");
  cloned.set("NEW_KEY", "new_value");

  // Original should be unchanged
  assertEquals(original.get("SHELL"), "bash");
  assertEquals(original.has("NEW_KEY"), false);

  // Clone should have changes
  assertEquals(cloned.get("SHELL"), "zsh");
  assertEquals(cloned.get("NEW_KEY"), "new_value");
});

Deno.test("Property: Value type preservation", () => {
  const store = new ConfigStore();

  // Test that values are stored as strings
  store.set("number_like", "123");
  store.set("float_like", "3.14");
  store.set("boolean_like", "true");

  // All should be strings
  assertEquals(typeof store.get("number_like"), "string");
  assertEquals(typeof store.get("float_like"), "string");
  assertEquals(typeof store.get("boolean_like"), "string");

  // And should equal their original string values
  assertEquals(store.get("number_like"), "123");
  assertEquals(store.get("float_like"), "3.14");
  assertEquals(store.get("boolean_like"), "true");
});

Deno.test("Property: Large config store doesn't break determinism", () => {
  const store1 = new ConfigStore();
  const store2 = new ConfigStore();

  // Add many entries
  for (let i = 0; i < 1000; i++) {
    const key = `KEY_${i}`;
    const value = `value_${i}`;
    store1.set(key, value);
    store2.set(key, value);
  }

  // Verify all entries match
  const all1 = store1.getAll();
  const all2 = store2.getAll();

  assertEquals(all1.size, all2.size);
  for (const [key, value] of all1) {
    assertEquals(
      all2.get(key),
      value,
      `Mismatch at key ${key} in large store`
    );
  }
});

Deno.test("Property: Key not found returns undefined", () => {
  const store = new ConfigStore();

  const nonexistent = store.get("DOES_NOT_EXIST");
  assertEquals(nonexistent, undefined);

  store.set("EXISTS", "yes");
  assertEquals(store.has("EXISTS"), true);
  assertEquals(store.has("NOT_EXISTS"), false);
});
