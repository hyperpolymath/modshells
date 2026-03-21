# modshells - Development Tasks
set shell := ["bash", "-uc"]
set dotenv-load := true

project := "modshells"

# Show all recipes
default:
    @just --list --unsorted

# Build
build:
    @echo "TODO: Add build command"

# Test
test:
    @echo "TODO: Add test command"

# Clean
clean:
    @echo "TODO: Add clean command"

# Format
fmt:
    @echo "TODO: Add format command"

# Lint
lint:
    @echo "TODO: Add lint command"

# [AUTO-GENERATED] Multi-arch / RISC-V target
build-riscv:
	@echo "Building for RISC-V..."
	cross build --target riscv64gc-unknown-linux-gnu

# Run panic-attacker pre-commit scan
assail:
    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"
