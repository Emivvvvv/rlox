use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
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

    Error,
    #[default]
    Eof,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // We display enums as integer values as it were a C-like enum with their rust enum names.
        write!(f, "({}){:?}", *self as i32, self)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Token<'b> {
    pub typ: TokenType,
    pub src: &'b str,
    pub line: i32,
}

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: i32,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    //Return the character at the given position or '\0' if we are out of bound.
    fn char_at(&self, i: usize) -> char {
        self.source[i..].chars().next().unwrap_or('\0')
    }

    fn char(&self) -> char {
        self.char_at(self.current)
    }

    fn is_alpha(&self, c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_ascii_digit()
    }

    fn is_at_end(&self) -> bool {
        self.char_at(self.current) == '\0'
    }

    fn advance(&mut self) -> char {
        self.current += 1;

        self.char_at(self.current - 1)
    }

    fn peek(&self) -> char {
        self.char()
    }

    fn peek_next(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.char_at(self.current + 1)
        }
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.char() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn make_token(&self, typ: TokenType) -> Token<'a> {
        Token {
            typ,
            src: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn error_token(&self, message: &'static str) -> Token<'a> {
        Token {
            typ: TokenType::Error,
            src: message,
            line: self.line,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c: char = self.peek();
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        // A comment goes until the end of the line.
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn check_keyword(&self, start: usize, length: usize, rest: &str, typ: TokenType) -> TokenType {
        let left = &self.source[(self.start + start)..(self.start + start + length)];
        if self.current - self.start == start + length && left == rest {
            typ
        } else {
            TokenType::Identifier
        }
    }

    fn identifier_type(&mut self) -> TokenType {
        match self.char_at(self.start) {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'f' => {
                if self.current - self.start > 1 {
                    match self.char_at(self.start + 1) {
                        'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                        'o' => self.check_keyword(2, 1, "r", TokenType::For),
                        'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            't' => {
                if self.current - self.start > 1 {
                    match self.char_at(self.start + 1) {
                        'h' => self.check_keyword(2, 2, "is", TokenType::This),
                        'r' => self.check_keyword(2, 2, "ue", TokenType::True),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        while self.is_alpha(self.peek()) || self.is_digit(self.peek()) {
            self.advance();
        }

        let typ = self.identifier_type();
        self.make_token(typ)
    }

    fn number(&mut self) -> Token<'a> {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        // Look for a fractional part.
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            // Consume the "."
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn string(&mut self) -> Token<'a> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }

        // The closing quote.
        self.advance();
        self.make_token(TokenType::String)
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c: char = self.advance();
        if self.is_alpha(c) {
            return self.identifier();
        }
        if self.is_digit(c) {
            return self.number();
        }

        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                let typ = if self.matches('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.make_token(typ)
            }
            '=' => {
                let typ = if self.matches('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.make_token(typ)
            }
            '<' => {
                let typ = if self.matches('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.make_token(typ)
            }
            '>' => {
                let typ = if self.matches('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.make_token(typ)
            }
            '"' => self.string(),
            _ => self.error_token("Unexpected character."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_token_type() {
        let mut scanner = Scanner::new(
            "and class else false\r fun \nfor if nil or print return super this true var while\0",
        );

        assert_eq!(scanner.scan_token().typ, TokenType::And);
        assert_eq!(scanner.scan_token().typ, TokenType::Class);
        assert_eq!(scanner.scan_token().typ, TokenType::Else);
        assert_eq!(scanner.scan_token().typ, TokenType::False);
        assert_eq!(scanner.scan_token().typ, TokenType::Fun);
        assert_eq!(scanner.scan_token().typ, TokenType::For);
        assert_eq!(scanner.scan_token().typ, TokenType::If);
        assert_eq!(scanner.scan_token().typ, TokenType::Nil);
        assert_eq!(scanner.scan_token().typ, TokenType::Or);
        assert_eq!(scanner.scan_token().typ, TokenType::Print);
        assert_eq!(scanner.scan_token().typ, TokenType::Return);
        assert_eq!(scanner.scan_token().typ, TokenType::Super);
        assert_eq!(scanner.scan_token().typ, TokenType::This);
        assert_eq!(scanner.scan_token().typ, TokenType::True);
        assert_eq!(scanner.scan_token().typ, TokenType::Var);
        assert_eq!(scanner.scan_token().typ, TokenType::While);
    }
}
