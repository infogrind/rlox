#[derive(Debug, PartialEq)]
pub enum Token {
    // Special name to avoid clashing with theh Number AST node type.
    NumberToken(i32),
    Plus,
    Minus,
    Times,
    Slash,
    // TODO: Support additional tokens.
    // Lparen,
    // Rparen,
}
