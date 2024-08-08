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
        self.current >= self.source.len()
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
            ' ' | '\r' | '\t' => (),
            '\n' => self.line += 1,
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
        let mut nesting = 1;

        while nesting > 0 && !self.is_at_end() {
            if self.peek() == '/' && self.peek_next() == '*' {
                // Detect opening of a nested comment
                self.advance(); // Consume '/'
                self.advance(); // Consume '*'
                nesting += 1; // Increase nesting level
            } else if self.peek() == '*' && self.peek_next() == '/' {
                // Detect closing of a comment
                self.advance(); // Consume '*'
                self.advance(); // Consume '/'
                nesting -= 1; // Decrease nesting level
            } else {
                if self.peek() == '\n' {
                    self.line += 1;
                }
                self.advance();
            }
        }

        if nesting > 0 {
            report(self.line, "", "Unterminated block comment.");
        }
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
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Check for a fractional part
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // Advance over the '.'
            self.advance();

            // Continue with the fractional part
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let num_str = self.source[self.start..self.current].to_string();
        match num_str.parse::<f64>() {
            Ok(num) => self.add_token(TokenType::Number, Literal::Num(num.into())),
            Err(e) => report(
                self.line,
                &num_str,
                &format!("Failed to parse number: {}", e),
            ),
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        };
        self.source.chars().nth(self.current + 1).unwrap()
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

#[cfg(test)]
mod tests {
    use super::*; // Import everything from the parent module

    #[test]
    fn test_basic_tokens() {
        let source = "( ) { } , . - + ; * ! = < > /";
        let mut lexer = Lexer::new(source.to_string());
        lexer.scan_tokens();
        let expected_types = vec![
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::Comma,
            TokenType::Dot,
            TokenType::Minus,
            TokenType::Plus,
            TokenType::Semicolon,
            TokenType::Star,
            TokenType::Bang,
            TokenType::Equal,
            TokenType::Less,
            TokenType::Greater,
            TokenType::Slash,
            TokenType::Eof,
        ];

        assert_eq!(lexer.tokens.len(), expected_types.len());
        for (token, expected_type) in lexer.tokens.iter().zip(expected_types.iter()) {
            assert_eq!(token.token_type, *expected_type);
        }
    }

    #[test]
    fn test_string_literal() {
        let source = "\"hello world\"";
        let mut lexer = Lexer::new(source.to_string());
        lexer.scan_tokens();
        assert_eq!(lexer.tokens.len(), 2); // String token + EOF
        assert_eq!(lexer.tokens[0].token_type, TokenType::String);
        assert_eq!(lexer.tokens[0].lexeme, "\"hello world\"");
    }

    #[test]
    fn test_numbers() {
        let source = "123 456.789";
        let mut lexer = Lexer::new(source.to_string());
        lexer.scan_tokens();
        println!("{:?}", lexer.tokens);
        assert_eq!(
            lexer.tokens.len(),
            3,
            "Expected 3 tokens, found {}",
            lexer.tokens.len()
        ); // Improved assertion message
        assert_eq!(lexer.tokens[0].token_type, TokenType::Number);
        assert_eq!(lexer.tokens[0].lexeme, "123");
        assert_eq!(lexer.tokens[1].token_type, TokenType::Number);
        assert_eq!(lexer.tokens[1].lexeme, "456.789");
    }

    #[test]
    fn test_comments() {
        let source = "// This is a comment\n123";
        let mut lexer = Lexer::new(source.to_string());
        lexer.scan_tokens();
        // Only the number and EOF should be tokenized, not the comment
        assert_eq!(lexer.tokens.len(), 2);
        assert_eq!(lexer.tokens[0].token_type, TokenType::Number);
        assert_eq!(lexer.tokens[0].lexeme, "123");
    }

    #[test]
    fn test_block_comments() {
        let source = "/* This is a block comment */ 456";
        let mut lexer = Lexer::new(source.to_string());
        lexer.scan_tokens();
        // Only the number and EOF should be tokenized, not the block comment
        assert_eq!(lexer.tokens.len(), 2);
        assert_eq!(lexer.tokens[0].token_type, TokenType::Number);
        assert_eq!(lexer.tokens[0].lexeme, "456");
    }

    #[test]
    fn test_nested_block_comments() {
        let source = "/* This is a /* nested */ block comment */ 789";
        let mut lexer = Lexer::new(source.to_string());
        lexer.scan_tokens();
        // Only the number and EOF should be tokenized, not the nested block comment
        assert_eq!(lexer.tokens.len(), 2);
        assert_eq!(lexer.tokens[0].token_type, TokenType::Number);
        assert_eq!(lexer.tokens[0].lexeme, "789");
    }

    #[test]
    fn test_unterminated_string() {
        let source = "\"This string does not close";
        let mut lexer = Lexer::new(source.to_string());
        lexer.scan_tokens();
        // Expecting a report of an unterminated string
        assert!(lexer
            .tokens
            .iter()
            .any(|t| t.token_type == TokenType::Eof && t.lexeme.is_empty()));
    }

    #[test]
    fn test_keywords() {
        let source = "class var if else";
        let mut lexer = Lexer::new(source.to_string());
        lexer.scan_tokens();
        let expected_types = vec![
            TokenType::Class,
            TokenType::Var,
            TokenType::If,
            TokenType::Else,
            TokenType::Eof,
        ];
        assert_eq!(lexer.tokens.len(), expected_types.len());
        for (token, expected_type) in lexer.tokens.iter().zip(expected_types.iter()) {
            assert_eq!(token.token_type, *expected_type);
        }
    }

    #[test]
    fn test_identifiers() {
        let source = "foo bar baz";
        let mut lexer = Lexer::new(source.to_string());
        lexer.scan_tokens();
        let expected_types = vec![
            TokenType::Identifier,
            TokenType::Identifier,
            TokenType::Identifier,
            TokenType::Eof,
        ];
        assert_eq!(lexer.tokens.len(), expected_types.len());
        for (token, expected_type) in lexer.tokens.iter().zip(expected_types.iter()) {
            assert_eq!(token.token_type, *expected_type);
            assert!(matches!(token.literal, Literal::Nil));
        }
    }

    #[test]
    fn test_mixed_input() {
        let source = "var x = 100; // variable declaration\nfunc(y)";
        let mut lexer = Lexer::new(source.to_string());
        lexer.scan_tokens();
        let expected_types = vec![
            TokenType::Var,
            TokenType::Identifier,
            TokenType::Equal,
            TokenType::Number,
            TokenType::Semicolon,
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::Identifier,
            TokenType::RightParen,
            TokenType::Eof,
        ];
        assert_eq!(lexer.tokens.len(), expected_types.len());
        for (token, expected_type) in lexer.tokens.iter().zip(expected_types.iter()) {
            assert_eq!(token.token_type, *expected_type);
        }
    }
}
