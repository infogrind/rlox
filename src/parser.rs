use crate::syntax::Expression::{self, *};
use crate::tokens::Token::{self, *};
use std::iter::Peekable;

pub fn parse_expression<I>(tokens: &mut I) -> Option<Expression>
where
    I: Iterator<Item = Token>,
{
    let mut p = tokens.peekable();
    let result = match p.peek() {
        Some(_) => Some(parse_term(&mut p)),
        None => None,
    };
    // Make sure no tokens follow.
    match p.peek() {
        Some(t) => panic!("Unexpected token {:?} after term.", t),
        None => result,
    }
}

fn parse_term<I>(p: &mut Peekable<I>) -> Expression
where
    I: Iterator<Item = Token>,
{
    let mut lhs = parse_factor(p);
    loop {
        match p.peek() {
            // End of input
            Some(Plus) => {
                p.next();
                lhs = Add(Box::new(lhs), Box::new(parse_factor(p)));
            }
            Some(Minus) => {
                p.next();
                lhs = Sub(Box::new(lhs), Box::new(parse_factor(p)));
            }
            // End of production, just return what we have.
            Some(_) => break lhs,
            // End of input, just return what we have.
            None => break lhs,
        }
    }
}

fn parse_factor<I>(p: &mut Peekable<I>) -> Expression
where
    I: Iterator<Item = Token>,
{
    let mut lhs = parse_primary(p);
    loop {
        match p.peek() {
            // End of input
            Some(Times) => {
                p.next();
                lhs = Mult(Box::new(lhs), Box::new(parse_primary(p)));
            }
            Some(Slash) => {
                p.next();
                lhs = Div(Box::new(lhs), Box::new(parse_primary(p)));
            }
            // End of production, just return what we have.
            Some(_) => break lhs,
            // End of input, just return what we have.
            None => break lhs,
        }
    }
}

fn parse_primary<I>(p: &mut Peekable<I>) -> Expression
where
    I: Iterator<Item = Token>,
{
    match p.next() {
        Some(NumberToken(i)) => Number(i),
        None => panic!("Unexpected end of expression"),
        Some(t) => panic!("Unexpected token while parsing primary: {:?}", t),
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
        expect_that!(p("123"), eq(&Some(Number(123))));
    }

    #[gtest]
    fn parse_addition() {
        expect_that!(
            p("1+2"),
            eq(&Some(Add(Box::new(Number(1)), Box::new(Number(2)))))
        )
    }

    #[gtest]
    fn parse_addition_three_terms() {
        expect_that!(
            p("1+2+3"),
            eq(&Some(Add(
                Box::new(Add(Box::new(Number(1)), Box::new(Number(2)))),
                Box::new(Number(3))
            )))
        )
    }

    #[gtest]
    fn parse_subtraction_three_terms() {
        expect_that!(
            p("1-2-3"),
            eq(&Some(Sub(
                Box::new(Sub(Box::new(Number(1)), Box::new(Number(2)))),
                Box::new(Number(3))
            )))
        )
    }

    #[gtest]
    fn parse_multiplication() {
        expect_that!(
            p("2*3"),
            eq(&Some(Mult(Box::new(Number(2)), Box::new(Number(3)))))
        )
    }

    #[gtest]
    fn parse_multiplication_three_terms() {
        expect_that!(
            p("2*3*4"),
            eq(&Some(Mult(
                Box::new(Mult(Box::new(Number(2)), Box::new(Number(3)))),
                Box::new(Number(4))
            )))
        )
    }

    #[gtest]
    fn parse_division() {
        expect_that!(
            p("2/3"),
            eq(&Some(Div(Box::new(Number(2)), Box::new(Number(3)))))
        )
    }

    #[gtest]
    fn parse_division_three_terms() {
        expect_that!(
            p("2/3/4"),
            eq(&Some(Div(
                Box::new(Div(Box::new(Number(2)), Box::new(Number(3)))),
                Box::new(Number(4))
            )))
        )
    }

    #[gtest]
    fn multiplication_addition_priority() {
        expect_that!(
            p("2+3*5"),
            eq(&Some(Add(
                Box::new(Number(2)),
                Box::new(Mult(Box::new(Number(3)), Box::new(Number(5))))
            )))
        )
    }
}
