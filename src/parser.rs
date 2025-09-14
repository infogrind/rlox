use crate::syntax::Expression::{self, *};
use crate::tokens::Token::{self, *};
use std::iter::Peekable;

/// Parses a sequence of tokens into an expression.
///
/// Returns `None` if and only if the given token stream is empty.
pub fn parse_expression<I>(tokens: &mut I) -> Result<Option<Expression>, String>
where
    I: Iterator<Item = Token>,
{
    let mut p = tokens.peekable();
    // We can't use `map()` on the Option returned by peek() here, as this would lead to a double
    // mutable borrow, because we'd have to use p inside the lambda while it's borrowed immutably
    // by `peek()`.
    #[allow(clippy::manual_map)]
    let result = match p.peek() {
        Some(_) => Some(parse_term(&mut p)),
        None => None,
    };
    // Make sure no tokens follow.
    match p.peek() {
        Some(t) => Err(format!(
            "Unexpected token {:?} after expression. Can evaluate only a single expression at a time.",
            t
        )),
        None => Ok(result),
    }
}

/// Parses a term expression.
///
/// Syntax definition:
///
/// ```ignore
/// term → factor ( ( "-" | "+" ) factor )* ;
/// ```
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

/// Parses a factor expression.
///
/// Syntax definition:
///
/// ```ignore
/// term → primary ( ( "*" | "/" ) primary )* ;
/// ```
fn parse_factor<I>(p: &mut Peekable<I>) -> Expression
where
    I: Iterator<Item = Token>,
{
    // Note that the implementation has exactly the same structure as `parse_term` above, only the
    // tokens are different and the types of sub-expressions.
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

/// Parses a primary expression.
fn parse_primary<I>(p: &mut Peekable<I>) -> Expression
where
    I: Iterator<Item = Token>,
{
    match p.next() {
        Some(NumberToken(i)) => Number(i),
        // TODO: Support other primaries besides numbers.
        // TODO: Use Result to return a proper error here.
        None => panic!("Unexpected end of expression"),
        Some(t) => panic!("Unexpected token while parsing primary: {:?}", t),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::Tokenizer;
    use googletest::prelude::*;

    fn p(s: &str) -> std::result::Result<Option<Expression>, String> {
        let mut tokenizer = Tokenizer::from(s.chars());
        parse_expression(&mut tokenizer)
    }

    #[gtest]
    fn parse_number() {
        expect_that!(p("123"), ok(eq(&Some(Number(123)))));
    }

    #[gtest]
    fn parse_addition() {
        expect_that!(
            p("1+2"),
            ok(eq(&Some(Add(Box::new(Number(1)), Box::new(Number(2))))))
        )
    }

    #[gtest]
    fn parse_addition_three_terms() {
        expect_that!(
            p("1+2+3"),
            ok(eq(&Some(Add(
                Box::new(Add(Box::new(Number(1)), Box::new(Number(2)))),
                Box::new(Number(3))
            ))))
        )
    }

    #[gtest]
    fn parse_subtraction_three_terms() {
        expect_that!(
            p("1-2-3"),
            ok(eq(&Some(Sub(
                Box::new(Sub(Box::new(Number(1)), Box::new(Number(2)))),
                Box::new(Number(3))
            ))))
        )
    }

    #[gtest]
    fn parse_multiplication() {
        expect_that!(
            p("2*3"),
            ok(eq(&Some(Mult(Box::new(Number(2)), Box::new(Number(3))))))
        )
    }

    #[gtest]
    fn parse_multiplication_three_terms() {
        expect_that!(
            p("2*3*4"),
            ok(eq(&Some(Mult(
                Box::new(Mult(Box::new(Number(2)), Box::new(Number(3)))),
                Box::new(Number(4))
            ))))
        )
    }

    #[gtest]
    fn parse_division() {
        expect_that!(
            p("2/3"),
            ok(eq(&Some(Div(Box::new(Number(2)), Box::new(Number(3))))))
        )
    }

    #[gtest]
    fn parse_division_three_terms() {
        expect_that!(
            p("2/3/4"),
            ok(eq(&Some(Div(
                Box::new(Div(Box::new(Number(2)), Box::new(Number(3)))),
                Box::new(Number(4))
            ))))
        )
    }

    #[gtest]
    fn multiplication_addition_priority() {
        expect_that!(
            p("2+3*5"),
            ok(eq(&Some(Add(
                Box::new(Number(2)),
                Box::new(Mult(Box::new(Number(3)), Box::new(Number(5))))
            ))))
        )
    }

    #[gtest]
    fn invalid_token_after_expression() {
        expect_that!(p("2 3"), err(contains_substring("Unexpected token")))
    }
}
