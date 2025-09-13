# rlox

Rust implementation of the VM-based compiler for the Lox language from the book
[Crafting Interpreters](https://craftinginterpreters.com/).

## Usage

Currently, the application is limited to an interactive REPL loop for simple arithmetic expressions (addition, subtraction, multiplication, division). The only type supported are literal integers.

```shell
cargo run
```

## Todos for a More Comprehensive Application

Besides completing the syntax and implementing the VM and byte code generation:

- Proper error propagation (maybe using a custom error type)
- Support for processing input from files
