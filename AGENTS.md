# Repository Guidelines

## Project Structure & Module Organization

rlox is a single Cargo package targeting Rust 2024. The CLI entry point lives
in `src/main.rs` and drives the REPL. Shared logic is exported through
`src/lib.rs`, which wires the tokenizer (`tokenizer.rs` and `tokens.rs`),
parser (`parser.rs`, `syntax.rs`), and interpreter (`interpreter.rs`).
Integration tests reside under `tests/`, currently `tests/test_main.rs`, and
spawn `target/debug/rlox` to validate prompt interactions. Cargo keeps build
artifacts under `target/`; do not edit checked-in files there.

## Build, Test, and Development Commands

Use `cargo build` for incremental compilation, and `cargo run` to launch the
REPL locally. `cargo fmt` applies the repository formatting rules before you
commit; add `--check` in CI-friendly scripts. `cargo test` executes the
googletest-powered integration suite; run it after touching parser or
interpreter code. When debugging a single test, target it with `cargo test
test_simple_calculation`.

## Coding Style & Naming Conventions

Adhere to Rust's default four-space indentation and 80-character width enforced
by `rustfmt.toml`. Prefer `snake_case` for modules, functions, and locals;
stick to `UpperCamelCase` for types and enums. Public APIs should live in
`src/lib.rs` so `use rlox::…` remains consistent. Keep prompt strings and
user-visible messages matching the expectations in the tests.

## Testing Guidelines

All tests use the `googletest` crate via `#[gtest]` macros in
`tests/test_main.rs`. Mirror the existing pattern—spawn the binary, write to
stdin, assert on `stdout`. Name new tests after the scenario
(`test_<behavior>`), and favor deterministic inputs over random ones. If you
add new language features, extend the suite with both success and error cases,
and confirm `cargo test` passes before pushing.

## Developer workflow

When you are asked to add new functionality, first state your plan. You must
have a discussion with the user, and wait until the user agrees to your plan,
until making any changes whatsoever.

Once the user confirms that you can start, proceed according to the following
order. The principle is test driven development.

1. Implement tests for the new functionality. At first they may not even
   compile, that is fine.
2. Make any API changes required to make the tests compile (but not yet pass).
3. Verify the tests compile but don't pass.
4. Implement the functionality.
5. Verify the tests pass.
6. Present the result to the user.
7. If the user confirms, run the formatter again and then create a git commit.

## Commit & Pull Request Guidelines

Follow the existing history: short, imperative commits such as "Simplify parser
return values"; scoped prefixes like `feat:` are acceptable when helpful. Each
pull request should summarize the behavior change, list any follow-up TODOs,
and note the commands you ran (e.g., `cargo fmt --check`, `cargo test`). Link
issues when applicable and include screenshots only when surface-level output
changes.
