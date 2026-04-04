# Copilot Instructions for Feline

## Project Overview

Feline is a **Lean 4 functional programming example** demonstrating core functional programming concepts. It's a simple library with an executable that includes a file reader (`cat`-like utility).

- **Language**: Lean 4 (v4.28.0)
- **Build System**: Lake (Lean's package manager)
- **Key Dependencies**: mathlib4 (v4.28.0), LeanCopilot (v4.28.0) for tactic suggestions

## Build, Test & Run Commands

### Build the project
```bash
lake build
```

### Run the main executable (cat-like utility)
```bash
# Print stdin to stdout
lake run feline -

# Print file contents
lake run feline <filename>

# Read multiple files
lake run feline file1 file2 -
```

### Update dependencies
```bash
lake update
```

### Clean build artifacts
```bash
lake clean
```

### Check for compilation errors without building
```bash
lake check Feline
```

## Architecture & Core Concepts

### Library Structure (`Feline/`)

- **`Feline.lean`** (root module): Imports all library modules and demonstrates `LeanCopilot`'s `suggest_tactics` tactic
- **`Basic.lean`**: Simple example definitions (currently contains `hello := "world"`)
- **`Pos.lean`**: Custom inductive type `Pos` (positive integers without 0)
  - Implements arithmetic: `Add`, heterogeneous addition (`HAdd` for Nat/Pos mixing)
  - Implements `Hashable`, `ToString`, `OfNat` instances
  - Recursive arithmetic operations use `let rec` with pattern matching

### Executable (`Main.lean`)

A simple file reader utility (like `cat`) that:
- Reads from stdin if `-` is passed
- Reads multiple files sequentially
- Returns exit code 1 if any file is not found, 0 otherwise
- Uses partial functions and `IO` monad for file operations

## Key Conventions

### Lean 4 Style in This Codebase

1. **Recursive Definitions with `let rec`**
   - Arithmetic and string operations use nested `let rec` instead of standalone definitions
   - Pattern matching directly in the recursive lambda

2. **Instance Definitions**
   - Custom instances for `Pos` (Add, HAdd, Hashable, ToString, etc.) are grouped together
   - Heterogeneous addition (`HAdd`) allows mixing `Nat + Pos` and `Pos + Nat`

3. **No External Testing Framework**
   - Verification is done via `#eval` in source files (see `Pos.lean` end)
   - CI only runs `lake build` via GitHub Actions

4. **LeanCopilot Integration**
   - The library imports `LeanCopilot` for tactic suggestions
   - Use `suggest_tactics` in proof goals to get AI-assisted tactic recommendations
   - Requires offline model setup (see README for binary download)

### Inductive Type Patterns

Custom recursive types should follow the `Pos` pattern:
- Define inductive with constructors
- Implement typeclass instances in order: basic arithmetic, then conversion utilities, then display

## Dependencies

- **mathlib4**: Standard library for Lean 4 math constructs and tactics
- **LeanCopilot**: Offline AI tactic suggestion (requires `.lake/build/lib/libctranslate2` binary)

Both are pinned to v4.28.0 in `lakefile.toml`.

## CI/CD

GitHub Actions workflow (`.github/workflows/lean_action_ci.yml`):
- Runs on push, PR, and manual trigger (`workflow_dispatch`)
- Uses `leanprover/lean-action@v1` to build and check for compilation errors
- No linting or test running beyond `lake build`
