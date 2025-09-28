use crate::tokens::Token::{self, *};
use std::{iter::Peekable, num::IntErrorKind};

/// A tokenizer allows to iterate over the tokens produced from a stream of characters.
pub struct Tokenizer<I: Iterator<Item = char>> {
    // We need to keep the char iterator as state, so that we can repeatedly access it within a
    // single `next()` call. We wrap it in `Peekable` so that we can `peek()` to the next
    // character.
    chars: Peekable<I>,
}

impl<I: Iterator<Item = char>> Tokenizer<I> {
    /// Create a new tokenizer from a character stream.
    pub fn from(iter: I) -> Self {
        Tokenizer {
            chars: iter.peekable(),
        }
    }

    /// Moves forward in the character stream until all whitespace has been skipped.
    ///
    /// This guarantees that the next character returned is NOT whitespace (but it is possible that
    /// `next()` will return None, if we've reached the end of the character stream).
    fn skip_whitespace(&mut self) {
        // The matches! macro is a nice way to use a complex match expression in place of a boolean
        // expression.
        while self.chars.next_if(|c| c.is_whitespace()).is_some() {}
    }

    /// Scans a number. Must only be called if it has previously been detected that the next
    /// character is a digit.
    fn scan_number(&mut self) -> Result<Token, String> {
        let first = match self.chars.next() {
            Some(c) if c.is_ascii_digit() => c,
            Some(c) => {
                return Err(format!(
                    "Internal lexer error: expected digit, found '{}' while scanning number.",
                    c
                ));
            }
            None => {
                return Err(String::from(
                    "Internal lexer error: reached end of input while scanning number.",
                ));
            }
        };

        let mut buf = String::new();
        buf.push(first);

        while let Some(c) = self.chars.next_if(|c| c.is_ascii_digit()) {
            buf.push(c);
        }

        match buf.parse::<i32>() {
            Ok(i) => Ok(Token::NumberToken(i)),
            Err(e) => match e.kind() {
                IntErrorKind::PosOverflow => Err(format!(
                    "Number {} is too large, can only parse number up to {}.",
                    buf,
                    i32::MAX
                )),
                _ => Err(format!(
                    "Number parsing error {} while parsing string '{}'.",
                    e, buf,
                )),
            },
        }
    }
}

/// Iterator trait implementation for Tokenizer.
impl<I: Iterator<Item = char>> Iterator for Tokenizer<I> {
    type Item = Result<Token, String>;
    fn next(&mut self) -> Option<Result<Token, String>> {
        self.skip_whitespace();
        // Here we can use copied() at no extra cost, since the single character
        // type implements Copy, this is better than passing around a pointer/reference.
        let c = self.chars.peek().copied()?;
        if c.is_ascii_digit() {
            return Some(self.scan_number());
            // TODO: Handle other multi-character tokens here.
        }
        // Single character tokens are handled easily.
        match self.chars.next()? {
            '+' => Some(Ok(Plus)),
            '-' => Some(Ok(Minus)),
            '*' => Some(Ok(Times)),
            '/' => Some(Ok(Slash)),
            '(' => Some(Ok(Lparen)),
            ')' => Some(Ok(Rparen)),
            '<' => match self.chars.next_if_eq(&'=') {
                Some(_) => Some(Ok(LeToken)),
                None => Some(Ok(LtToken)),
            },
            '>' => match self.chars.next_if_eq(&'=') {
                Some(_) => Some(Ok(GeToken)),
                None => Some(Ok(GtToken)),
            },
            // TODO: Handle other single character tokens here.
            c => Some(Err(format!("Invalid token: {}", c))),
        }
    }
}

#[cfg(test)]
mod tests {
    // Standard practice: make everything from above accessible.
    use super::*;
    use googletest::prelude::*;

    fn tokenize(s: &str) -> std::result::Result<Vec<Token>, String> {
        let t = Tokenizer::from(s.chars());
        t.collect()
    }

    #[gtest]
    fn test_too_large_number() {
        expect_that!(
            tokenize("1234567890123"),
            err(eq(
                "Number 1234567890123 is too large, can only parse number up to 2147483647."
            ))
        )
    }

    #[gtest]
    fn test_invalid_token() {
        expect_that!(tokenize("?"), err(eq("Invalid token: ?")))
    }

    #[gtest]
    fn test_scan_number() {
        expect_that!(tokenize("123"), ok(elements_are!(&NumberToken(123))));
    }

    #[gtest]
    fn test_leading_whitespace_trimmed() {
        expect_that!(tokenize("  12"), ok(elements_are!(&NumberToken(12))));
    }

    #[gtest]
    fn test_trailing_whitespace_trimmed() {
        expect_that!(tokenize("34  "), ok(elements_are!(&NumberToken(34))));
    }

    #[gtest]
    fn test_scan_plus() {
        expect_that!(tokenize("+"), ok(elements_are!(&Plus)));
    }

    #[gtest]
    fn test_scan_two_numbers() {
        expect_that!(
            tokenize(" 123  456 "),
            ok(elements_are!(&NumberToken(123), &NumberToken(456)))
        );
    }

    #[gtest]
    fn test_scan_number_plus_number() {
        expect_that!(
            tokenize("1+2"),
            ok(elements_are!(&NumberToken(1), &Plus, &NumberToken(2)))
        )
    }

    #[gtest]
    fn test_scan_minus() {
        expect_that!(tokenize("-"), ok(elements_are!(&Minus)))
    }

    #[gtest]
    fn test_scan_times() {
        expect_that!(tokenize("*"), ok(elements_are!(&Times)))
    }

    #[gtest]
    fn test_scan_slash() {
        expect_that!(tokenize("/"), ok(elements_are!(&Slash)))
    }

    #[gtest]
    fn test_scan_gt() {
        expect_that!(tokenize(">"), ok(elements_are!(&GtToken)))
    }

    #[gtest]
    fn test_scan_ge() {
        expect_that!(tokenize(">="), ok(elements_are!(&GeToken)))
    }

    #[gtest]
    fn test_scan_lt() {
        expect_that!(tokenize("<"), ok(elements_are!(&LtToken)))
    }

    #[gtest]
    fn test_scan_le() {
        expect_that!(tokenize("<="), ok(elements_are!(&LeToken)))
    }

    #[gtest]
    fn test_scan_lparen() {
        expect_that!(tokenize("("), ok(elements_are!(&Lparen)))
    }

    #[gtest]
    fn test_scan_rparen() {
        expect_that!(tokenize(")"), ok(elements_are!(&Rparen)))
    }

    #[gtest]
    fn test_scan_complex_sequence() {
        expect_that!(
            tokenize(" 1/34 > 9+9<=1-**/02+ 2+< 3 "),
            ok(elements_are!(
                &NumberToken(1),
                &Slash,
                &NumberToken(34),
                &GtToken,
                &NumberToken(9),
                &Plus,
                &NumberToken(9),
                &LeToken,
                &NumberToken(1),
                &Minus,
                &Times,
                &Times,
                &Slash,
                &NumberToken(2),
                &Plus,
                &NumberToken(2),
                &Plus,
                &LtToken,
                &NumberToken(3)
            ))
        )
    }
}
