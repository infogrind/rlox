use crate::syntax::Expression::{self, *};
use crate::tokens::Token::{self, *};
use std::iter::Peekable;

// Helper type alias so we can store a simple function pointer for building binary expressions.
type BinaryBuilder = fn(Expression, Expression) -> Expression;

/// Peek at the upcoming token without consuming it, converting any tokenizer error into an owned
/// `String` so the parser does not borrow from the iterator's error value.
fn peek_token<I>(p: &mut Peekable<I>) -> Result<Option<&Token>, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    p.peek()
        .map(|r| r.as_ref())
        .transpose()
        .map_err(|e| e.clone())
}

/// Advance the iterator by one step and return the token if available.
fn next_token<I>(p: &mut Peekable<I>) -> Result<Option<Token>, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    p.next().transpose()
}

/// Advance the iterator and yield a token, returning an error when the stream unexpectedly ends.
fn advance_token<I>(p: &mut Peekable<I>) -> Result<Token, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    next_token(p)?.ok_or_else(|| {
        String::from("Unexpected end of input while advancing token stream.")
    })
}

/// Parse a left-associative expression layer by repeatedly matching operators and combining
/// operands, keeping the grammar-specific pieces (operand parser and operator matcher) pluggable.
fn parse_left_associative<I, F>(
    p: &mut Peekable<I>,
    mut parse_operand: F,
    match_op: fn(&Token) -> Option<BinaryBuilder>,
) -> Result<Expression, String>
where
    I: Iterator<Item = Result<Token, String>>,
    F: FnMut(&mut Peekable<I>) -> Result<Expression, String>,
{
    let mut lhs = parse_operand(p)?;
    while let Some(token) = peek_token(p)? {
        let Some(builder) = match_op(token) else {
            break;
        };
        // Consume the operator we just matched.
        advance_token(p)?;
        let rhs = parse_operand(p)?;
        lhs = builder(lhs, rhs);
    }
    Ok(lhs)
}

fn match_comparison_op(token: &Token) -> Option<BinaryBuilder> {
    match token {
        GtToken => Some(|lhs, rhs| Gt(Box::new(lhs), Box::new(rhs))),
        GeToken => Some(|lhs, rhs| Ge(Box::new(lhs), Box::new(rhs))),
        LtToken => Some(|lhs, rhs| Lt(Box::new(lhs), Box::new(rhs))),
        LeToken => Some(|lhs, rhs| Le(Box::new(lhs), Box::new(rhs))),
        _ => None,
    }
}

fn match_term_op(token: &Token) -> Option<BinaryBuilder> {
    match token {
        Plus => Some(|lhs, rhs| Add(Box::new(lhs), Box::new(rhs))),
        Minus => Some(|lhs, rhs| Sub(Box::new(lhs), Box::new(rhs))),
        _ => None,
    }
}

fn match_factor_op(token: &Token) -> Option<BinaryBuilder> {
    match token {
        Times => Some(|lhs, rhs| Mult(Box::new(lhs), Box::new(rhs))),
        Slash => Some(|lhs, rhs| Div(Box::new(lhs), Box::new(rhs))),
        _ => None,
    }
}

pub fn parse_program<I>(
    p: &mut Peekable<I>,
) -> Result<Option<Expression>, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    let program = match peek_token(p)? {
        Some(_) => Some(parse_expression(p)?),
        None => None,
    };

    // No other tokens after expression are allowed.
    match peek_token(p)? {
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
    match next_token(p)? {
        Some(tt) if tt == t => Ok(()),
        Some(tt) => Err(format!("Unexpected token: {}, expected: {}", tt, t)),
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
    parse_left_associative(p, |stream| parse_term(stream), match_comparison_op)
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
    parse_left_associative(p, |stream| parse_factor(stream), match_term_op)
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
    parse_left_associative(p, |stream| parse_primary(stream), match_factor_op)
}

/// Parses a primary expression.
fn parse_primary<I>(p: &mut Peekable<I>) -> Result<Expression, String>
where
    I: Iterator<Item = Result<Token, String>>,
{
    match next_token(p)? {
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
