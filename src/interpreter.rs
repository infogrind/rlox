use crate::parser;
use crate::syntax::Expression;
use crate::tokenizer;

pub fn interpret(input: &str) -> Result<Option<Expression>, String> {
    let mut tokens = tokenizer::Tokenizer::from(input.chars()).peekable();
    parser::parse_program(&mut tokens)
}
