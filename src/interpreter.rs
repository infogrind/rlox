use crate::parser;
use crate::tokenizer;

pub fn interpret(input: &str) -> String {
    let mut tokenizer = tokenizer::Tokenizer::from(input.chars());
    match parser::parse_expression(&mut tokenizer) {
        Ok(r) => match r {
            Some(expr) => format!("{expr}"),
            None => String::from("No input."),
        },
        Err(e) => format!("Error: {e}"),
    }
}
