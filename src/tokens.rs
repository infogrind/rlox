#[derive(Debug, PartialEq)]
pub enum Token {
    Number(i32),
    Plus,
    Minus,
    Times,
    Div,
    Lparen,
    Rparen,
}
