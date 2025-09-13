use crate::tokens::Token;
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
        let c = self.chars.peek()?;
        if c.is_ascii_digit() {
            Some(self.scan_number())
        } else if c == &'+' {
            self.chars.next();
            Some(Token::Plus)
        } else {
            // TODO: Parse other token types, skip whitespace.
            // TODO: Proper error handling using Result<> instead of panic.
            panic!("Invalid character: {}", c)
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
    fn test_parse_number() {
        expect_that!(tokenize("123"), elements_are!(&Token::Number(123)));
    }

    #[gtest]
    fn test_parse_plus() {
        expect_that!(tokenize("+"), elements_are!(&Token::Plus));
    }
}
