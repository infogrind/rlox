use crate::tokens::Token::{self, *};
use std::iter::Peekable;

pub struct Tokenizer<I: Iterator<Item = char>> {
    chars: Peekable<I>,
}

impl<I: Iterator<Item = char>> Tokenizer<I> {
    pub fn from(iter: I) -> Self {
        Tokenizer {
            chars: iter.peekable(),
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.chars.peek(), Some(c) if c.is_whitespace()) {
            self.chars.next();
        }
    }

    fn scan_number(&mut self) -> Token {
        // Invariant: next character is invariably a char.
        let mut buf = String::from(self.chars.next().unwrap());
        loop {
            match self.chars.peek() {
                None => break, // End of input
                Some(c) => {
                    if c.is_ascii_digit() {
                        buf.push(self.chars.next().unwrap())
                    } else {
                        break;
                    }
                }
            }
        }
        Token::Number(buf.parse().unwrap())
    }
}

impl<I: Iterator<Item = char>> Iterator for Tokenizer<I> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let c = self.chars.peek()?;
        if c.is_ascii_digit() {
            Some(self.scan_number())
        } else {
            match self.chars.next()? {
                '+' => Some(Plus),
                '-' => Some(Minus),
                '*' => Some(Times),
                '/' => Some(Slash),
                // TODO: Handle other token types here.
                c => panic!("Invalid character: '{}'", c),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use googletest::prelude::*;

    fn tokenize(s: &str) -> Vec<Token> {
        let t = Tokenizer::from(s.chars());
        t.collect()
    }

    #[gtest]
    fn test_scan_number() {
        expect_that!(tokenize("123"), elements_are!(&Number(123)));
    }

    #[gtest]
    fn test_leading_whitespace_trimmed() {
        expect_that!(tokenize("  12"), elements_are!(&Number(12)));
    }

    #[gtest]
    fn test_trailing_whitespace_trimmed() {
        expect_that!(tokenize("34  "), elements_are!(&Number(34)));
    }

    #[gtest]
    fn test_scan_plus() {
        expect_that!(tokenize("+"), elements_are!(&Plus));
    }

    #[gtest]
    fn test_scan_two_numbers() {
        expect_that!(
            tokenize(" 123  456 "),
            elements_are!(&Number(123), &Number(456))
        );
    }

    #[gtest]
    fn test_scan_number_plus_number() {
        expect_that!(
            tokenize("1+2"),
            elements_are!(&Number(1), &Plus, &Number(2))
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
                &Number(1),
                &Slash,
                &Number(34),
                &Number(9),
                &Plus,
                &Number(9),
                &Slash,
                &Number(1),
                &Minus,
                &Times,
                &Times,
                &Slash,
                &Number(2),
                &Plus,
                &Number(2),
                &Plus,
                &Number(3)
            )
        )
    }
}
