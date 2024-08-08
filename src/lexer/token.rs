use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
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

    // One or two caharacter tokens.
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

#[derive(Debug, Copy, Clone)]
pub struct Hf64(f64);

impl PartialEq for Hf64 {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 || (self.0.is_nan() && other.0.is_nan())
    }
}

impl Eq for Hf64 {}

impl Hash for Hf64 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if self.0.is_nan() {
            state.write_u8(0);
        } else {
            state.write_u64(self.0.to_bits());
        }
    }
}

impl From<f64> for Hf64 {
    fn from(value: f64) -> Self {
        Hf64(value)
    }
}

impl Into<f64> for Hf64 {
    fn into(self) -> f64 {
        self.0
    }
}

impl fmt::Display for Hf64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Str(String),
    Num(Hf64),
    True,
    False,
    Nil,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
