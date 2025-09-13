use crate::syntax::Expression;
use crate::tokens::Token;

pub fn parse_expression<I>(tokens: &mut I) -> Option<Expression>
where
    I: Iterator<Item = Token>,
{
    let mut p = tokens.peekable();
    let c = p.peek()?;
    match c {
        Token::Number(i) => Some(Expression::Number(*i)),
        t => unimplemented!("Cannot parse token {:?} yet.", t),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::Tokenizer;
    use googletest::prelude::*;

    fn p(s: &str) -> Option<Expression> {
        let mut tokenizer = Tokenizer::from(s.chars());
        parse_expression(&mut tokenizer)
    }

    #[gtest]
    fn parse_number() {
        expect_that!(p("123"), eq(&Some(Expression::Number(123))));
    }
}
