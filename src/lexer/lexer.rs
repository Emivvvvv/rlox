use crate::lexer::token::{Token, TokenType};
use crate::lox::report;

use super::token::Literal;

pub struct Lexer {
    source: String,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        let tokens: Vec<Token> = Vec::new();
        Self {
            source,
            tokens,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            let _ = &self.scan_token();
        }

        let _ = &self.tokens.push(Token::new(
            TokenType::Eof,
            "".to_string(),
            Literal::Nil,
            self.line,
        ));
    }

    fn is_at_end(&self) -> bool {
        &self.current >= &self.source.len()
    }

    fn scan_token(&mut self) {
        match self.advance() {
            '(' => self.add_token(TokenType::LeftParen, Literal::Nil),
            ')' => self.add_token(TokenType::RightParen, Literal::Nil),
            '{' => self.add_token(TokenType::LeftBrace, Literal::Nil),
            '}' => self.add_token(TokenType::RightBrace, Literal::Nil),
            ',' => self.add_token(TokenType::Comma, Literal::Nil),
            '.' => self.add_token(TokenType::Dot, Literal::Nil),
            '-' => self.add_token(TokenType::Minus, Literal::Nil),
            '+' => self.add_token(TokenType::Plus, Literal::Nil),
            ';' => self.add_token(TokenType::Semicolon, Literal::Nil),
            '*' => self.add_token(TokenType::Star, Literal::Nil),
            '!' => {
                let token_type = if self.match_operators('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(token_type, Literal::Nil);
            }
            '=' => {
                let token_type = if self.match_operators('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token_type, Literal::Nil);
            }
            '<' => {
                let token_type = if self.match_operators('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token_type, Literal::Nil);
            }
            '>' => {
                let token_type = if self.match_operators('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type, Literal::Nil);
            }
            '/' => {
                if self.match_operators('/') {
                    // A comment goes until the end of the line.
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_operators('*') {
                    self.block_comment();
                } else {
                    self.add_token(TokenType::Slash, Literal::Nil);
                }
            }
            ' ' | '\r' | '\t' => return,
            'n' => self.line += 1,
            '"' => self.string(),
            '0'..='9' => self.number(),
            c => {
                if c.is_alphabetic() {
                    self.identifier();
                } else {
                    report(
                        self.line,
                        &self.source[self.start..self.current],
                        "Unexpected character.",
                    );
                }
            }
        }
    }

    fn advance(&mut self) -> char {
        let char = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        char
    }

    fn add_token(&mut self, token_type: TokenType, literal: Literal) {
        let text = self.source[self.start..self.current].to_string();
        self.tokens
            .push(Token::new(token_type, text, literal, self.line))
    }

    fn match_operators(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.chars().nth(self.current).unwrap()
    }

    fn block_comment(&mut self) {
        while self.peek() != '*' && self.peek_next() != '/' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            report(self.line, "", "Unterminated block comment.");
            return;
        }

        self.advance(); // Consume '*'
        self.advance(); // Consume '/'
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            report(self.line, "", "Unterminated string.");
            return;
        }

        self.advance();

        let string_value = self.source[self.start + 1..self.current - 1].to_string();
        self.add_token(TokenType::String, Literal::Str(string_value));
    }

    fn number(&mut self) {
        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        let num = self.source[self.start..self.current].to_string();
        self.add_token(TokenType::Number, Literal::Num(num.parse::<f64>().unwrap()));
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        };
        self.source.chars().nth(self.current).unwrap()
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let token_type: TokenType = match text {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };

        self.add_token(token_type, Literal::Nil);
    }
}
