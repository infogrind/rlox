use std::fmt;

// Syntax definition. Token names are capitalized here.
//
// Program       -> Expression?
// Expression    -> Comparison
// Comparison    -> Term ( ( GT | GE | LT | LE ) Term )*
// Term          -> Factor ( ( PLUS | MINUS ) Factor )*
// Factor        -> Primary ( ( TIMES | SLASH ) Primary )*
// Primary       -> Number
//                  | Group
// Group         -> LPAREN Expression RPAREN

#[derive(Debug, PartialEq)]
pub enum Expression {
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mult(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),
    Ge(Box<Expression>, Box<Expression>),
    Lt(Box<Expression>, Box<Expression>),
    Le(Box<Expression>, Box<Expression>),
    // TODO: Support floats.
    Number(i32),
    // TODO: Add support for additional expression types.
    // Str(String),
    // Identifier(String),
    // True,
    // False,
    // Nil,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            Expression::Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            Expression::Mult(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            Expression::Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
            Expression::Gt(lhs, rhs) => write!(f, "({} > {})", lhs, rhs),
            Expression::Ge(lhs, rhs) => write!(f, "({} >= {})", lhs, rhs),
            Expression::Lt(lhs, rhs) => write!(f, "({} < {})", lhs, rhs),
            Expression::Le(lhs, rhs) => write!(f, "({} <= {})", lhs, rhs),
            Expression::Number(i) => write!(f, "{i}"),
        }
    }
}
