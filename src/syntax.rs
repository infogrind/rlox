use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mult(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Number(i32), // TODO: Support floats
    Str(String),
    Identifier(String),
    True,
    False,
    Nil,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Add(lhs, rhs) => write!(f, "{} + {}", lhs, rhs),
            Expression::Number(i) => write!(f, "{i}"),
            _ => unimplemented!(),
        }
    }
}
