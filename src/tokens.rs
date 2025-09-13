#[derive(Debug, PartialEq)]
pub enum Token {
    // Special name to avoid clashing with theh Number AST node type.
    // We need to disable the lint warning because the variant name ends in the enum name.
    #[allow(clippy::enum_variant_names)]
    NumberToken(i32),
    Plus,
    Minus,
    Times,
    Slash,
    // TODO: Support additional tokens.
    // Lparen,
    // Rparen,
}
