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
