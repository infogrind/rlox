use crate::parser;
use crate::tokenizer;

pub fn interpret(input: &str) -> String {
    let mut p = tokenizer::Tokenizer::from(input.chars()).peekable();
    match parser::parse_program(&mut p) {
        Ok(r) => match r {
            Some(expr) => format!("{expr}"),
            None => String::from("No input."),
        },
        Err(e) => format!("Error: {e}"),
    }
}
