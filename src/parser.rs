use crate::expr::Expr;
use crate::lexer::token::{Literal, Token, TokenType};
use crate::lox;
use crate::stmt::Stmt;

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

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?)
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        // Attempt to match and process a variable declaration
        if self.match_types(&[TokenType::Var]) {
            let var_decl_result = self.var_declaration();
            if var_decl_result.is_err() {
                self.synchronize(); // Synchronize after encountering a parsing error
                return var_decl_result; // Return the error
            }
            return var_decl_result; // Return the successful variable declaration
        }

        // If not a variable declaration, process other statements
        let stmt_result = self.statement();
        if stmt_result.is_err() {
            self.synchronize(); // Synchronize after encountering a parsing error
        }
        stmt_result // Return the result of the statement (either Ok or Err)
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Identifier, "Expect variable name.")?;

        let mut initializer = None;
        if self.match_types(&[TokenType::Equal]) {
            initializer = Some(self.expression()?);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        return Ok(Stmt::Var { name, initializer });
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_types(&[TokenType::Print]) {
            return self.print_stmt();
        };

        if self.match_types(&[TokenType::LeftBrace]) {
            let statements = self.block()?;
            return Ok(Stmt::Block { statements });
        };

        self.expression_stmt()
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;

        Ok(Stmt::Print { expression: expr })
    }

    fn expression_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;

        Ok(Stmt::Expression { expression: expr })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::<Stmt>::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.equality()?;

        if self.match_types(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;

            match expr {
                Expr::Variable { name } => {
                    return Ok(Expr::Assign {
                        name,
                        value: Box::new(value),
                    })
                }
                _ => lox::error(equals, "Invalid assignment target."),
            }
        }

        Ok(expr)
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

        if self.match_types(&[TokenType::Identifier]) {
            return Ok(Expr::Variable {
                name: self.previous(),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_expression() {
        let tokens = vec![
            Token::new(TokenType::Number, "123".to_string(), Literal::Num(123.0), 1),
            Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "456".to_string(), Literal::Num(456.0), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 1); // Should contain one expression statement
    }

    #[test]
    fn test_error_handling_missing_semicolon() {
        let tokens = vec![
            Token::new(TokenType::Number, "123".to_string(), Literal::Num(123.0), 1),
            Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "456".to_string(), Literal::Num(456.0), 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1), // No semicolon before EOF
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_err()); // Should detect the missing semicolon error
    }

    #[test]
    fn test_unary_expression() {
        let tokens = vec![
            Token::new(TokenType::Minus, "-".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "123".to_string(), Literal::Num(123.0), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok());
        let binding = &result.unwrap()[0];
        let expr = match binding {
            Stmt::Expression { expression } => expression,
            _ => panic!("Expected expression statement"),
        };
        matches!(expr, Expr::Unary { .. });
    }

    #[test]
    fn test_grouping_expression() {
        let tokens = vec![
            Token::new(TokenType::LeftParen, "(".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "123".to_string(), Literal::Num(123.0), 1),
            Token::new(TokenType::RightParen, ")".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok());
        let binding = &result.unwrap()[0];
        let expr = match binding {
            Stmt::Expression { expression } => expression,
            _ => panic!("Expected expression statement"),
        };
        matches!(expr, Expr::Grouping { .. });
    }

    #[test]
    fn test_unexpected_token_error() {
        let tokens = vec![
            Token::new(TokenType::Number, "123".to_string(), Literal::Num(123.0), 1),
            Token::new(TokenType::Number, "456".to_string(), Literal::Num(456.0), 1), // Unexpected token
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_err());
    }

    #[test]
    fn test_binary_expression_precedence() {
        let tokens = vec![
            Token::new(TokenType::Number, "1".to_string(), Literal::Num(1.0), 1),
            Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "2".to_string(), Literal::Num(2.0), 1),
            Token::new(TokenType::Star, "*".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "3".to_string(), Literal::Num(3.0), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok());
        let stmt = &result.unwrap()[0];

        match stmt {
            Stmt::Expression { expression } => {
                if let Expr::Binary {
                    operator, right, ..
                } = expression
                {
                    assert_eq!(operator.lexeme, "+");
                    if let Expr::Binary { operator, .. } = right.as_ref() {
                        assert_eq!(operator.lexeme, "*");
                    } else {
                        panic!("Right hand expression is not a binary expression");
                    }
                } else {
                    panic!("Expected binary expression");
                }
            }
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_print_statement() {
        let tokens = vec![
            Token::new(TokenType::Print, "print".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::String,
                "\"Hello, world!\"".to_string(),
                Literal::Str("Hello, world!".to_string()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok());
        matches!(result.unwrap()[0], Stmt::Print { .. });
    }

    #[test]
    fn test_empty_block() {
        let tokens = vec![
            Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 1); // Should contain one block statement
    }

    #[test]
    fn test_nested_blocks() {
        let tokens = vec![
            Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
            Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1); // Should contain one block statement
        matches!(stmts[0], Stmt::Block { .. });
    }

    #[test]
    fn test_block_with_statements() {
        let tokens = vec![
            Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Print, "print".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::String,
                "\"Hello\"".to_string(),
                Literal::Str("Hello".to_string()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 1); // Should contain one block statement
        matches!(&stmts[0], Stmt::Block { statements } if statements.len() == 1);
    }

    #[test]
    fn test_missing_closing_brace() {
        let tokens = vec![
            Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Print, "print".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::String,
                "\"Hello\"".to_string(),
                Literal::Str("Hello".to_string()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1), // Missing closing brace
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_err());
    }
}
