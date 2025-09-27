use crate::syntax::Expression::{self, *};
use crate::tokens::Token::{self, *};
use std::iter::Peekable;

/// Parses a sequence of tokens into an expression.
///
/// Returns `None` if and only if the given token stream is empty.
pub fn parse_expression<I>(tokens: &mut I) -> Result<Option<Expression>, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    let mut p = tokens.peekable();
    // We can't use `map()` on the Option returned by peek() here, as this would lead to a double
    // mutable borrow, because we'd have to use p inside the lambda while it's borrowed immutably
    // by `peek()`.
    #[allow(clippy::manual_map)]
    let result = match p.peek() {
        Some(_) => Some(parse_comparison(&mut p)?),
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

/// Parses a comparison expression.
///
/// Syntax definition:
///
/// ```ignore
/// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// ```
fn parse_comparison<I>(p: &mut Peekable<I>) -> Result<Expression, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    let mut lhs = parse_term(p)?;
    loop {
        match p.peek().map(|r| r.as_ref()).transpose()? {
            Some(GtToken) => {
                p.next();
                lhs = Gt(Box::new(lhs), Box::new(parse_term(p)?));
            }
            Some(GeToken) => {
                p.next();
                lhs = Ge(Box::new(lhs), Box::new(parse_term(p)?));
            }
            Some(LtToken) => {
                p.next();
                lhs = Lt(Box::new(lhs), Box::new(parse_term(p)?));
            }
            Some(LeToken) => {
                p.next();
                lhs = Le(Box::new(lhs), Box::new(parse_term(p)?));
            }
            Some(_) => break Ok(lhs),
            None => break Ok(lhs),
        }
    }
}

/// Parses a term expression.
///
/// Syntax definition:
///
/// ```ignore
/// term → factor ( ( "-" | "+" ) factor )* ;
/// ```
fn parse_term<I>(p: &mut Peekable<I>) -> Result<Expression, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    let mut lhs = parse_factor(p)?;
    loop {
        // We need to map a &Result<Token, String> to a Result<&Token, &String>, then
        // transpose() will give us a Result<Option<&Token>, String>, and we can use the ?
        // operator early on.
        match p.peek().map(|r| r.as_ref()).transpose()? {
            // End of input
            Some(Plus) => {
                p.next();
                lhs = Add(Box::new(lhs), Box::new(parse_factor(p)?));
            }
            Some(Minus) => {
                p.next();
                lhs = Sub(Box::new(lhs), Box::new(parse_factor(p)?));
            }
            // End of production, just return what we have.
            Some(_) => break Ok(lhs),
            // End of input, just return what we have.
            None => break Ok(lhs),
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
fn parse_factor<I>(p: &mut Peekable<I>) -> Result<Expression, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    // Note that the implementation has exactly the same structure as `parse_term` above, only the
    // tokens are different and the types of sub-expressions.
    let mut lhs = parse_primary(p)?;
    loop {
        match p.peek().map(|r| r.as_ref()).transpose()? {
            // End of input
            Some(Times) => {
                p.next();
                lhs = Mult(Box::new(lhs), Box::new(parse_primary(p)?));
            }
            Some(Slash) => {
                p.next();
                lhs = Div(Box::new(lhs), Box::new(parse_primary(p)?));
            }
            // End of production, just return what we have.
            Some(_) => break Ok(lhs),
            // End of input, just return what we have.
            None => break Ok(lhs),
        }
    }
}

/// Parses a primary expression.
fn parse_primary<I>(p: &mut Peekable<I>) -> Result<Expression, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    match p.next().transpose()? {
        Some(NumberToken(i)) => Ok(Number(i)),
        // TODO: Support other primaries besides numbers.
        Some(t) => {
            Err(format!("Unexpected token while parsing primary: {:?}", t))
        }
        None => {
            Err(String::from("Unexpected end of input, expected: primary."))
        }
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
    fn parse_greater_than() {
        expect_that!(
            p("2>3"),
            ok(eq(&Some(Gt(Box::new(Number(2)), Box::new(Number(3))))))
        )
    }

    #[gtest]
    fn parse_greater_equal() {
        expect_that!(
            p("2>=3"),
            ok(eq(&Some(Ge(Box::new(Number(2)), Box::new(Number(3))))))
        )
    }

    #[gtest]
    fn parse_less_than() {
        expect_that!(
            p("2<3"),
            ok(eq(&Some(Lt(Box::new(Number(2)), Box::new(Number(3))))))
        )
    }

    #[gtest]
    fn parse_less_equal() {
        expect_that!(
            p("2<=3"),
            ok(eq(&Some(Le(Box::new(Number(2)), Box::new(Number(3))))))
        )
    }

    #[gtest]
    fn comparison_has_lower_priority_than_addition() {
        expect_that!(
            p("2+3>4+5"),
            ok(eq(&Some(Gt(
                Box::new(Add(Box::new(Number(2)), Box::new(Number(3)))),
                Box::new(Add(Box::new(Number(4)), Box::new(Number(5))))
            ))))
        )
    }

    #[gtest]
    fn comparison_has_lower_priority_than_multiplication() {
        expect_that!(
            p("2*3>4*5"),
            ok(eq(&Some(Gt(
                Box::new(Mult(Box::new(Number(2)), Box::new(Number(3)))),
                Box::new(Mult(Box::new(Number(4)), Box::new(Number(5))))
            ))))
        )
    }

    #[gtest]
    fn chained_comparison() {
        expect_that!(
            p("1<2>3"),
            ok(eq(&Some(Gt(
                Box::new(Lt(Box::new(Number(1)), Box::new(Number(2)))),
                Box::new(Number(3))
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

    #[gtest]
    fn incomplete_addition() {
        expect_that!(
            p("2+"),
            err(contains_substring("Unexpected end of input"))
        )
    }

    #[gtest]
    fn incomplete_multiplication() {
        expect_that!(
            p("2*"),
            err(contains_substring("Unexpected end of input"))
        )
    }

    #[gtest]
    fn invalid_addition() {
        expect_that!(
            p("2++"),
            err(contains_substring("Unexpected token while parsing primary"))
        )
    }

    #[gtest]
    fn invalid_multiplication() {
        expect_that!(
            p("2*+"),
            err(contains_substring("Unexpected token while parsing primary"))
        )
    }
}
