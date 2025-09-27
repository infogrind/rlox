use std::fmt;

#[derive(Debug, PartialEq)]
#[allow(clippy::enum_variant_names)]
pub enum Token {
    // Special name to avoid clashing with theh Number AST node type.
    // We need to disable the lint warning because the variant name ends in the enum name.
    NumberToken(i32), // E.g. '123'
    Plus,             // '+'
    Minus,            // '-'
    Times,            // '*'
    Slash,            // '-'
    GtToken,          // '>'
    GeToken,          // '>='
    LtToken,          // '<'
    LeToken,          // '<='
    Lparen,           // '('
    Rparen,           // ')'
                      // TODO: Support additional tokens.
}

use Token::*;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NumberToken(i) => write!(f, "{}", i),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Times => write!(f, "*"),
            Slash => write!(f, "/"),
            GtToken => write!(f, ">"),
            GeToken => write!(f, ">="),
            LtToken => write!(f, "<"),
            LeToken => write!(f, "<="),
            Lparen => write!(f, "("),
            Rparen => write!(f, ")"),
        }
    }
}
