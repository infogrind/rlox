use crate::parser;
use crate::tokenizer;

pub fn interpret(input: &str) -> String {
    let mut tokenizer = tokenizer::Tokenizer::from(input.chars());
    if let Some(expr) = parser::parse_expression(&mut tokenizer) {
        format!("{}", expr)
    } else {
        String::from("No input.")
    }
}
