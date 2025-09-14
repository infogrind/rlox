use crate::tokens::Token::{self, *};
use std::iter::Peekable;

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
        while matches!(self.chars.peek(), Some(c) if c.is_whitespace()) {
            self.chars.next();
        }
    }

    /// Scans a number. Must only be called if it has previously been detected that the next
    /// character is a digit.
    fn scan_number(&mut self) -> Token {
        // FIXME: Protect against too large numbers. E.g. if the character stream contains
        // 1000000000000 then parse() below will fail.
        assert!(self.chars.peek().is_some_and(|c| c.is_ascii_digit()));
        let mut buf = String::from(self.chars.next().expect(
            "There should always be a next character at the start of scan_number()."
        ));
        loop {
            match self.chars.peek() {
                None => break, // End of input
                Some(c) => {
                    if c.is_ascii_digit() {
                        buf.push(self.chars.next().expect(
                            "Peek indicated character, but next() returned None."));
                    } else {
                        break;
                    }
                }
            }
        }
        // It's safe to use expect() here, as the above code ensures we are only adding digits to
        // the buffer.
        Token::NumberToken(
            buf.parse()
                .expect("Parsing error, even though we only added digits."),
        )
    }
}

/// Iterator trait implementation for Tokenizer.
impl<I: Iterator<Item = char>> Iterator for Tokenizer<I> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let c = self.chars.peek()?;
        if c.is_ascii_digit() {
            Some(self.scan_number())
            // TODO: Handle other multi-character tokens here.
        } else {
            // Single character tokens are handled easily.
            match self.chars.next()? {
                '+' => Some(Plus),
                '-' => Some(Minus),
                '*' => Some(Times),
                '/' => Some(Slash),
                // TODO: Handle other single character tokens here.
                // TODO: Change this code to return a Result, so that we dont' have to panic here.
                c => panic!("Invalid character: '{}'", c),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    // Standard practice: make everything from above accessible.
    use super::*;
    use googletest::prelude::*;

    fn tokenize(s: &str) -> Vec<Token> {
        let t = Tokenizer::from(s.chars());
        t.collect()
    }

    #[gtest]
    fn test_scan_number() {
        expect_that!(tokenize("123"), elements_are!(&NumberToken(123)));
    }

    #[gtest]
    fn test_leading_whitespace_trimmed() {
        expect_that!(tokenize("  12"), elements_are!(&NumberToken(12)));
    }

    #[gtest]
    fn test_trailing_whitespace_trimmed() {
        expect_that!(tokenize("34  "), elements_are!(&NumberToken(34)));
    }

    #[gtest]
    fn test_scan_plus() {
        expect_that!(tokenize("+"), elements_are!(&Plus));
    }

    #[gtest]
    fn test_scan_two_numbers() {
        expect_that!(
            tokenize(" 123  456 "),
            elements_are!(&NumberToken(123), &NumberToken(456))
        );
    }

    #[gtest]
    fn test_scan_number_plus_number() {
        expect_that!(
            tokenize("1+2"),
            elements_are!(&NumberToken(1), &Plus, &NumberToken(2))
        )
    }

    #[gtest]
    fn test_scan_minus() {
        expect_that!(tokenize("-"), elements_are!(&Minus))
    }

    #[gtest]
    fn test_scan_times() {
        expect_that!(tokenize("*"), elements_are!(&Times))
    }

    #[gtest]
    fn test_scan_slash() {
        expect_that!(tokenize("/"), elements_are!(&Slash))
    }

    #[gtest]
    fn test_scan_complex_sequence() {
        expect_that!(
            tokenize(" 1/34 9+9/1-**/02+ 2+ 3 "),
            elements_are!(
                &NumberToken(1),
                &Slash,
                &NumberToken(34),
                &NumberToken(9),
                &Plus,
                &NumberToken(9),
                &Slash,
                &NumberToken(1),
                &Minus,
                &Times,
                &Times,
                &Slash,
                &NumberToken(2),
                &Plus,
                &NumberToken(2),
                &Plus,
                &NumberToken(3)
            )
        )
    }
}
