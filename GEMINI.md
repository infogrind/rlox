# Overview

This directory contains `rlox`, an implementation of a virtual machine
implementation of the "Lox" language in Rust. This is based on the book
"Crafting Interpreters" by Robert Nystrom, but I want to make some of my own
changes to the syntax from the book, to make the language more functional.

The source of truth for the language syntax is src/syntax.rs.

## Code Structure

- src/main.rs: Binary crate, contains only the minimal code to present a
  REPL loop to the user in order to test the language.
- tests/test_main.rs: A minimal set of end-to-end tests to make sure the
  binary works.
- src/lib.rs: Root of the library crate, only use to export modules and make
  them available to the other modules.
- src/syntax.rs: Syntax definition (in comments) and implementation of the
  AST as recursive enums.
- src/tokens.rs: Token definition.
- src/tokenizer.rs: Tokenizer (also known as scanner or lexer).
- src/parser.rs: Parser, reads tokens and creates the abstract syntax tree.

## Tools

1. To compile the code: `cargo build`.
2. To run tests: `cargo test`.
3. To run the formatter: `cargo fmt --all`.
4. To add a dependency using cargo: `cargo add <package>`.
5. To add a dependency that is only used in tests: `cargo add <package> --dev`.

## Development Workflow

When you are asked to add new functionality, first state your plan. You must have a discussion with the user, and wait until the user agrees to your plan, until making any changes whatsoever.

Once the user confirms that you can start, proceed according to the following order. The principle is test driven development.

1. Implement tests for the new functionality. At first they may not even compile, that is fine.
2. Make any API changes required to make the tests compile (but not yet pass).
3. Verify the tests compile but don't pass.
4. Implement the functionality.
5. Verify the tests pass.
6. Present the result to the user.
7. If the user confirms, run the formatter again and then create a git commit.

## Additional Rules

- If you still don't succeed to implement something after three attempts, give
  up and ask the user for help. Do not continue trying.
- NEVER run `git push` yourself.

## Git Repo

The main branch for this project is called `main`.
