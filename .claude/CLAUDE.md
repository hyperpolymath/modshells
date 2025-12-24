# CLAUDE.md - AI Assistant Instructions

## Language Policy (Hyperpolymath Standard)

### ALLOWED Languages & Tools

| Language/Tool | Use Case | Notes |
|---------------|----------|-------|
| **ReScript** | Primary application code | Compiles to JS, type-safe |
| **Deno** | Runtime & package management | Replaces Node/npm/bun |
| **Rust** | Performance-critical, systems, WASM | Preferred for CLI tools |
| **Bash/POSIX Shell** | Scripts, automation | Keep minimal |
| **JavaScript** | Only where ReScript cannot | MCP protocol glue, Deno APIs |
| **Python** | SaltStack only | No other Python permitted |
| **Nickel** | Configuration language | For complex configs |
| **Guile Scheme** | State/meta files | STATE.scm, META.scm, ECOSYSTEM.scm |

### BANNED - Do Not Use

| Banned | Replacement |
|--------|-------------|
| TypeScript | ReScript |
| Node.js | Deno |
| npm | Deno |
| Bun | Deno |
| pnpm/yarn | Deno |
| Go | Rust |
| Python (general) | ReScript/Rust |
| Java/Kotlin | Rust |

### Enforcement Rules

1. **No new TypeScript files** - Convert existing TS to ReScript
2. **No package.json for runtime deps** - Use deno.json imports
3. **No node_modules in production** - Deno caches deps automatically
4. **No Go code** - Use Rust instead
5. **Python only for SaltStack** - All other Python must be rewritten

### ReScript Conventions

- Output format: ES6 modules (`"module": "es6"` in rescript.json)
- File extension: `.res` (compiled to `.res.js`)
- Use `@rescript/core` for stdlib
- Bindings in `src/bindings/` directory

### Deno Conventions

- Import maps in `deno.json`
- Permissions explicitly declared
- Use `Deno.Command` not shell execution
- Format with `deno fmt`
- Lint with `deno lint`

### Build Commands

```bash
# ReScript build
deno task res:build   # or: npx rescript build

# Run server
deno task start

# Development
deno task dev
```

### Migration Priority

When encountering banned languages:
1. **Immediate**: Block new code in banned languages
2. **Short-term**: Convert TypeScript to ReScript
3. **Medium-term**: Replace Node/npm with Deno
4. **Long-term**: Rewrite Go/Python in Rust

## Code Quality

- SPDX license headers on all files
- SHA-pinned dependencies
- No shell metacharacters in commands
- Whitelist approach for CLI subcommands
