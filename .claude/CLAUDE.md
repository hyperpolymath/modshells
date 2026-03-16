## Machine-Readable Artefacts

The following files in `.machine_readable/` contain structured project metadata:

- `.machine_readable/6a2/STATE.a2ml` - Current project state and progress
- `.machine_readable/6a2/META.a2ml` - Architecture decisions and development practices
- `.machine_readable/6a2/ECOSYSTEM.a2ml` - Position in the ecosystem and related projects
- `.machine_readable/6a2/AGENTIC.a2ml` - AI agent interaction patterns
- `.machine_readable/6a2/NEUROSYM.a2ml` - Neurosymbolic integration config
- `.machine_readable/6a2/PLAYBOOK.a2ml` - Operational runbook

---

# CLAUDE.md - AI Assistant Instructions

## Language Policy (Hyperpolymath Standard)

### ALLOWED Languages & Tools

| Language/Tool | Use Case | Notes |
|---------------|----------|-------|
| **ReScript** | Primary application code | Compiles to JS, type-safe |
| **Deno** | Runtime & package management | Replaces Node/npm/bun |
| **Rust** | Performance-critical, systems, WASM | Preferred for CLI tools |
| **Tauri 2.0+** | Mobile apps (iOS/Android) | Rust backend + web UI |
| **Dioxus** | Mobile apps (native UI) | Pure Rust, React-like |
| **Gleam** | Backend services | Runs on BEAM or compiles to JS |
| **Bash/POSIX Shell** | Scripts, automation | Keep minimal |
| **JavaScript** | Only where ReScript cannot | MCP protocol glue, Deno APIs |
| **Nickel** | Configuration language | For complex configs |
| **Guile Scheme** | State/meta files | .machine_readable/6a2/STATE.a2ml, .machine_readable/6a2/META.a2ml, .machine_readable/6a2/ECOSYSTEM.a2ml |
| **Julia** | Batch scripts, data processing | Per RSR |
| **OCaml** | AffineScript compiler | Language-specific |
| **Ada** | Safety-critical systems | Where required |

### BANNED - Do Not Use

| Banned | Replacement |
|--------|-------------|
| TypeScript | ReScript |
| Node.js | Deno |
| npm | Deno |
| Bun | Deno |
| pnpm/yarn | Deno |
| Go | Rust |
| Python | Julia/Rust/ReScript |
| Java/Kotlin | Rust/Tauri/Dioxus |
| Swift | Tauri/Dioxus |
| React Native | Tauri/Dioxus |
| Flutter/Dart | Tauri/Dioxus |

### Mobile Development

**No exceptions for Kotlin/Swift** - use Rust-first approach:

1. **Tauri 2.0+** - Web UI (ReScript) + Rust backend, MIT/Apache-2.0
2. **Dioxus** - Pure Rust native UI, MIT/Apache-2.0

Both are FOSS with independent governance (no Big Tech).

### Enforcement Rules

1. **No new TypeScript files** - Convert existing TS to ReScript
2. **No package.json for runtime deps** - Use deno.json imports
3. **No node_modules in production** - Deno caches deps automatically
4. **No Go code** - Use Rust instead
5. **No Python anywhere** - Use Julia for data/batch, Rust for systems, ReScript for apps
6. **No Kotlin/Swift for mobile** - Use Tauri 2.0+ or Dioxus

### Package Management

- **Primary**: Guix (guix.scm)
- **Fallback**: Nix (flake.nix)
- **JS deps**: Deno (deno.json imports)

### Security Requirements

- No MD5/SHA1 for security (use SHA256+)
- HTTPS only (no HTTP URLs)
- No hardcoded secrets
- SHA-pinned dependencies
- SPDX license headers on all files

