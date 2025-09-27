use crate::syntax::Expression::{self, *};
use crate::tokens::Token::{self, *};
use std::iter::Peekable;

pub fn parse_program<I>(
    p: &mut Peekable<I>,
) -> Result<Option<Expression>, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    let program = match p.peek().map(|r| r.as_ref()).transpose()? {
        Some(_) => Some(parse_expression(p)?),
        None => None,
    };

    // No other tokens after expression are allowed.
    match p.peek().map(|r| r.as_ref()).transpose()? {
        Some(t) => Err(format!(
            "Unexpected token: {} after single allowed expression.",
            t
        )),
        None => Ok(program),
    }
}

/// Parses a sequence of tokens into an expression.
fn parse_expression<I>(p: &mut Peekable<I>) -> Result<Expression, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    parse_comparison(p)
}

fn eat<I>(p: &mut Peekable<I>, t: Token) -> Result<(), String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    match p.next().transpose()? {
        Some(tt) => {
            if t != tt {
                Err(format!("Unexpected token: {}, expected: {}", tt, t))
            } else {
                Ok(())
            }
        }
        None => Err(format!("Unexpected end of input, expected: {}", t)),
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
        Some(Lparen) => {
            let expr = parse_expression(p)?;
            eat(p, Rparen)?;
            Ok(expr)
        }
        // TODO: Support other primaries.
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
        let tokenizer = Tokenizer::from(s.chars());
        parse_program(&mut tokenizer.peekable())
    }

    #[gtest]
    fn test_empty_program() {
        expect_that!(p(""), ok(eq(&None)))
    }

    #[gtest]
    fn test_fail_with_two_expressions() {
        expect_that!(p("2 3"), err(contains_substring("Unexpected token: 3")))
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

    #[gtest]
    fn test_simple_group() {
        expect_that!(p("(3)"), ok(eq(&Some(Number(3)))))
    }

    #[gtest]
    fn test_group_precedence() {
        expect_that!(
            p("2 * (3 + 5)"),
            ok(eq(&Some(Mult(
                Box::new(Number(2)),
                Box::new(Add(Box::new(Number(3)), Box::new(Number(5))))
            ))))
        )
    }
}
