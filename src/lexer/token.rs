use std::fmt;
use std::hash::Hash;
use rust_decimal::Decimal;
use rust_decimal::prelude::{FromPrimitive, ToPrimitive};

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone)]
pub struct Hf64(pub Decimal);

impl From<f64> for Hf64 {
    fn from(value: f64) -> Self {
        Hf64(Decimal::from_f64(value).unwrap())
    }
}

impl From<Hf64> for f64 {
    fn from(val: Hf64) -> Self {
        val.0.to_f64().unwrap()
    }
}

impl From<&Hf64> for f64 {
    fn from(val: &Hf64) -> Self {
        val.0.to_f64().unwrap()
    }
}

impl fmt::Display for Hf64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Literal {
    Str(String),
    Num(Hf64),
    True,
    False,
    Nil,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Literal,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: Literal, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let literal = match &self.literal {
            Literal::Str(str) => str.clone(),
            Literal::Num(num) => num.to_string(),
            Literal::Nil => "".to_string(),
            Literal::True => "true".to_string(),
            Literal::False => "false".to_string(),
        };
        write!(
            f,
            "line {}: {:?} {} {}",
            self.line, self.token_type, self.lexeme, literal
        )
    }
}