use crate::expr::Expr;
use crate::lexer::token::{Literal, Token, TokenType};
use crate::lox;

#[derive(Debug, Clone)]
pub struct ParseError;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison();

        while self.match_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison();
            expr = Ok(Expr::Binary {
                left: Box::new(expr?),
                operator,
                right: Box::new(right?),
            })
        }

        expr
    }

    fn match_types(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().token_type == token_type
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term();

        while self.match_types(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = self.term();
            expr = Ok(Expr::Binary {
                left: Box::new(expr?),
                operator,
                right: Box::new(right?),
            })
        }

        expr
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor();

        while self.match_types(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.factor();
            expr = Ok(Expr::Binary {
                left: Box::new(expr?),
                operator,
                right: Box::new(right?),
            })
        }

        expr
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary();

        while self.match_types(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary();
            expr = Ok(Expr::Binary {
                left: Box::new(expr?),
                operator,
                right: Box::new(right?),
            })
        }

        expr
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_types(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary();
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right?),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_types(&[TokenType::False]) {
            return Ok(Expr::Literal {
                value: Literal::False,
            });
        }

        if self.match_types(&[TokenType::True]) {
            return Ok(Expr::Literal {
                value: Literal::True,
            });
        }

        if self.match_types(&[TokenType::Nil]) {
            return Ok(Expr::Literal {
                value: Literal::Nil,
            });
        }

        if self.match_types(&[TokenType::Number, TokenType::String]) {
            return Ok(Expr::Literal {
                value: self.previous().literal,
            });
        }

        if self.match_types(&[TokenType::LeftParen]) {
            let expr_ = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping {
                expression: Box::new(expr_),
            });
        }

        Err(self.error(self.peek(), "Expect expression."))
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<Token, ParseError> {
        if self.check(&token_type) {
            return Ok(self.advance());
        }

        Err(self.error(self.peek(), message))
    }

    fn error(&self, token: Token, message: &str) -> ParseError {
        lox::error(token, message);
        return ParseError;
    }

    #[allow(dead_code)]
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => self.advance(),
            };
        }
    }
}
