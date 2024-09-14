use crate::expr::{Expr, ExprIdx, ExprPool};
use crate::lexer::token::{ErrorToken, Literal, Token, TokenType};
use crate::lox;
use crate::stmt::Stmt;
use crate::symbol::SymbolTable;

#[derive(Debug, Clone)]
pub struct ParseError;

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    expr_pool: ExprPool,
    symbol_table: &'a SymbolTable
}

impl<'a> Parser<'a> {
    pub fn new(symbol_table: &'a SymbolTable, tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            expr_pool: ExprPool { exprs: Vec::with_capacity(9999) },
            symbol_table
        }
    }

    pub fn parse(mut self) -> Result<(Vec<Stmt>, ExprPool), ParseError> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok((statements, self.expr_pool))
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.match_types(&[TokenType::Class]) {
            return self.class_declaration();
        }

        if self.match_types(&[TokenType::Fun]) {
            return self.function("function");
        }

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

    fn class_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Identifier, "Expect class name.")?;

        let mut superclass: Option<ExprIdx> = None;
        if self.match_types(&[TokenType::Less]) {
            let superclass_name = self.consume(TokenType::Identifier, "Expect superclass name.")?;
            let superclass_expr = Expr::Variable {
                name: superclass_name,
            };
            let idx = self.expr_pool.add_expr(superclass_expr);
            superclass = Some(idx);
        }

        self.consume(TokenType::LeftBrace, "Expect '{' before class body.")?;

        let mut methods = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function("method")?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.")?;

        Ok(Stmt::Class {
            name,
            superclass,
            methods,
        })
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Identifier, &format!("Expect {} name.", kind))?;

        self.consume(
            TokenType::LeftParen,
            &format!("Expect '(' after {} name.", kind),
        )?;

        let mut parameters = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if parameters.len() >= 255 {
                    self.error(self.peek(), "Can't have more than 255 parameters.");
                }

                parameters.push(self.consume(TokenType::Identifier, "Expect parameter name.")?);

                if !self.match_types(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;

        self.consume(
            TokenType::LeftBrace,
            &format!("Expect '{{' after {} body.", kind),
        )?;

        let body = self.block()?;

        Ok(Stmt::Function {
            name,
            params: parameters,
            body,
        })
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

        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_types(&[TokenType::Print]) {
            return self.print_stmt();
        };

        if self.match_types(&[TokenType::Return]) {
            return self.return_stmt();
        };

        if self.match_types(&[TokenType::While]) {
            return self.while_stmt();
        }

        if self.match_types(&[TokenType::LeftBrace]) {
            let statements = self.block()?;
            return Ok(Stmt::Block { statements });
        };

        if self.match_types(&[TokenType::For]) {
            return self.for_stmt();
        };

        if self.match_types(&[TokenType::If]) {
            return self.if_stmt();
        };

        self.expression_stmt()
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;

        Ok(Stmt::Print { expression: expr })
    }

    fn return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let keyword = self.previous();
        let mut value = None;

        if !self.check(&TokenType::Semicolon) {
            value = Some(self.expression()?);
        }
        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;

        Ok(Stmt::Return { keyword, value })
    }

    fn while_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;
        let body = self.statement()?;

        Ok(Stmt::While {
            condition,
            body: Box::new(body),
        })
    }

    fn expression_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;

        Ok(Stmt::Expression { expression: expr })
    }

    fn for_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_types(&[TokenType::Semicolon]) {
            None
        } else if self.match_types(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_stmt()?)
        };

        let mut condition = None;
        if !self.check(&TokenType::Semicolon) {
            condition = Some(self.expression()?);
        }
        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let mut increment = None;
        if !self.check(&TokenType::RightParen) {
            increment = Some(self.expression()?);
        }
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(increment_expr_idx) = increment {
            body = Stmt::Block {
                statements: vec![
                    body,
                    Stmt::Expression {
                        expression: increment_expr_idx,
                    },
                ],
            };
        }

        if let Some(condition_expr_idx) = condition {
            body = Stmt::While {
                condition: condition_expr_idx,
                body: Box::new(body),
            };
        }

        if let Some(initializer_stmt) = initializer {
            body = Stmt::Block {
                statements: vec![initializer_stmt, body],
            };
        }

        Ok(body)
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = Box::new(self.statement()?);
        let mut else_branch = None;
        if self.match_types(&[TokenType::Else]) {
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::<Stmt>::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;

        Ok(statements)
    }

    fn expression(&mut self) -> Result<ExprIdx, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<ExprIdx, ParseError> {
        let expr_idx = self.or()?;

        if self.match_types(&[TokenType::Equal]) {
            let equals = self.previous();
            let value_idx = self.assignment()?;

            let target_expr = self.expr_pool.get_expr(expr_idx);

            match target_expr {
                Expr::Variable { name } => {
                    let assign_expr = Expr::Assign {
                        name: name.clone(),
                        value: value_idx,
                    };
                    let idx = self.expr_pool.add_expr(assign_expr);
                    return Ok(idx);
                }
                Expr::Get { object, name } => {
                    let set_expr = Expr::Set {
                        object: *object,
                        name: name.clone(),
                        value: value_idx,
                    };
                    let idx = self.expr_pool.add_expr(set_expr);
                    return Ok(idx);
                }
                _ => {
                    lox::error(&ErrorToken::new(&equals, self.symbol_table), "Invalid assignment target.");
                }
            }
        }

        Ok(expr_idx)
    }

    fn or(&mut self) -> Result<ExprIdx, ParseError> {
        let mut expr_idx = self.and()?;

        while self.match_types(&[TokenType::Or]) {
            let operator = self.previous();
            let right_idx = self.and()?;
            let logical_expr = Expr::Logical {
                left: expr_idx,
                operator,
                right: right_idx,
            };
            expr_idx = self.expr_pool.add_expr(logical_expr);
        }

        Ok(expr_idx)
    }

    fn and(&mut self) -> Result<ExprIdx, ParseError> {
        let mut expr_idx = self.equality()?;

        while self.match_types(&[TokenType::And]) {
            let operator = self.previous();
            let right_idx = self.equality()?;
            let logical_expr = Expr::Logical {
                left: expr_idx,
                operator,
                right: right_idx,
            };
            expr_idx = self.expr_pool.add_expr(logical_expr);
        }

        Ok(expr_idx)
    }

    fn equality(&mut self) -> Result<ExprIdx, ParseError> {
        let mut expr_idx = self.comparison()?;

        while self.match_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right_idx = self.comparison()?;
            let binary_expr = Expr::Binary {
                left: expr_idx,
                operator,
                right: right_idx,
            };
            expr_idx = self.expr_pool.add_expr(binary_expr);
        }

        Ok(expr_idx)
    }

    fn comparison(&mut self) -> Result<ExprIdx, ParseError> {
        let mut expr_idx = self.term()?;

        while self.match_types(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right_idx = self.term()?;
            let binary_expr = Expr::Binary {
                left: expr_idx,
                operator,
                right: right_idx,
            };
            expr_idx = self.expr_pool.add_expr(binary_expr);
        }

        Ok(expr_idx)
    }

    fn term(&mut self) -> Result<ExprIdx, ParseError> {
        let mut expr_idx = self.factor()?;

        while self.match_types(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right_idx = self.factor()?;
            let binary_expr = Expr::Binary {
                left: expr_idx,
                operator,
                right: right_idx,
            };
            expr_idx = self.expr_pool.add_expr(binary_expr);
        }

        Ok(expr_idx)
    }

    fn factor(&mut self) -> Result<ExprIdx, ParseError> {
        let mut expr_idx = self.unary()?;

        while self.match_types(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right_idx = self.unary()?;
            let binary_expr = Expr::Binary {
                left: expr_idx,
                operator,
                right: right_idx,
            };
            expr_idx = self.expr_pool.add_expr(binary_expr);
        }

        Ok(expr_idx)
    }

    fn unary(&mut self) -> Result<ExprIdx, ParseError> {
        if self.match_types(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right_idx = self.unary()?;
            let unary_expr = Expr::Unary {
                operator,
                right: right_idx,
            };
            let idx = self.expr_pool.add_expr(unary_expr);
            return Ok(idx);
        }

        self.call()
    }

    fn call(&mut self) -> Result<ExprIdx, ParseError> {
        let mut expr_idx = self.primary()?;

        loop {
            if self.match_types(&[TokenType::LeftParen]) {
                expr_idx = self.finish_call(expr_idx)?;
            } else if self.match_types(&[TokenType::Dot]) {
                let name =
                    self.consume(TokenType::Identifier, "Expect property name after '.'.")?;
                let get_expr = Expr::Get {
                    object: expr_idx,
                    name,
                };
                expr_idx = self.expr_pool.add_expr(get_expr);
            } else {
                break;
            }
        }

        Ok(expr_idx)
    }

    fn finish_call(&mut self, callee_idx: ExprIdx) -> Result<ExprIdx, ParseError> {
        let mut arguments: Vec<ExprIdx> = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    self.error(self.peek(), "Can't have more than 255 arguments.");
                }
                arguments.push(self.expression()?);
                if !self.match_types(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        let call_expr = Expr::Call {
            callee: callee_idx,
            paren,
            arguments,
        };
        let idx = self.expr_pool.add_expr(call_expr);
        Ok(idx)
    }

    fn primary(&mut self) -> Result<ExprIdx, ParseError> {
        if self.match_types(&[TokenType::False]) {
            let literal_expr = Expr::Literal {
                value: Literal::False,
            };
            let idx = self.expr_pool.add_expr(literal_expr);
            return Ok(idx);
        }

        if self.match_types(&[TokenType::True]) {
            let literal_expr = Expr::Literal {
                value: Literal::True,
            };
            let idx = self.expr_pool.add_expr(literal_expr);
            return Ok(idx);
        }

        if self.match_types(&[TokenType::Nil]) {
            let literal_expr = Expr::Literal {
                value: Literal::Nil,
            };
            let idx = self.expr_pool.add_expr(literal_expr);
            return Ok(idx);
        }

        if self.match_types(&[TokenType::Number, TokenType::String]) {
            let literal_expr = Expr::Literal {
                value: self.previous().literal,
            };
            let idx = self.expr_pool.add_expr(literal_expr);
            return Ok(idx);
        }

        if self.match_types(&[TokenType::LeftParen]) {
            let expr_idx = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
            let grouping_expr = Expr::Grouping {
                expression: expr_idx,
            };
            let idx = self.expr_pool.add_expr(grouping_expr);
            return Ok(idx);
        }

        if self.match_types(&[TokenType::Super]) {
            let keyword = self.previous();
            self.consume(TokenType::Dot, "Expect '.' after 'super'.")?;
            let method = self.consume(TokenType::Identifier, "Expect superclass method name.")?;
            let super_expr = Expr::Super { keyword, method };
            let idx = self.expr_pool.add_expr(super_expr);
            return Ok(idx);
        }

        if self.match_types(&[TokenType::This]) {
            let keyword = self.previous();
            let this_expr = Expr::This { keyword };
            let idx = self.expr_pool.add_expr(this_expr);
            return Ok(idx);
        }

        if self.match_types(&[TokenType::Identifier]) {
            let name = self.previous();
            let variable_expr = Expr::Variable { name };
            let idx = self.expr_pool.add_expr(variable_expr);
            return Ok(idx);
        }

        Err(self.error(self.peek(), "Expect expression."))
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

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<Token, ParseError> {
        if self.check(&token_type) {
            return Ok(self.advance());
        }

        Err(self.error(self.peek(), message))
    }

    fn error(&self, token: Token, message: &str) -> ParseError {
        lox::error(&ErrorToken::new(&token, self.symbol_table), message);

        ParseError
    }

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
                _ => {
                    self.advance();
                }
            };
        }
    }
}

//
// #[cfg(test)]
// mod tests {
//     use crate::lexer::token::Hf64;
//     use super::*;
//
//     #[test]
//     fn test_basic_expression() {
//         let tokens = vec![
//             Token::new(TokenType::Number, "123".to_string(), Literal::Num(Hf64::from(123.0)), 1),
//             Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Number, "456".to_string(), Literal::Num(Hf64::from(456.0)), 1),
//             Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_ok());
//         assert_eq!(result.unwrap().len(), 1); // Should contain one expression statement
//     }
//
//     #[test]
//     fn test_error_handling_missing_semicolon() {
//         let tokens = vec![
//             Token::new(TokenType::Number, "123".to_string(), Literal::Num(Hf64::from(123.0)), 1),
//             Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Number, "456".to_string(), Literal::Num(Hf64::from(456.0)), 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1), // No semicolon before EOF
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_err()); // Should detect the missing semicolon error
//     }
//
//     #[test]
//     fn test_unary_expression() {
//         let tokens = vec![
//             Token::new(TokenType::Minus, "-".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Number, "123".to_string(), Literal::Num(Hf64::from(123.0)), 1),
//             Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_ok());
//         let binding = &result.unwrap()[0];
//         let expr = match binding {
//             Stmt::Expression { expression } => expression,
//             _ => panic!("Expected expression statement"),
//         };
//         matches!(expr, Expr::Unary { .. });
//     }
//
//     #[test]
//     fn test_grouping_expression() {
//         let tokens = vec![
//             Token::new(TokenType::LeftParen, "(".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Number, "123".to_string(), Literal::Num(Hf64::from(123.0)), 1),
//             Token::new(TokenType::RightParen, ")".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_ok());
//         let binding = &result.unwrap()[0];
//         let expr = match binding {
//             Stmt::Expression { expression } => expression,
//             _ => panic!("Expected expression statement"),
//         };
//         matches!(expr, Expr::Grouping { .. });
//     }
//
//     #[test]
//     fn test_unexpected_token_error() {
//         let tokens = vec![
//             Token::new(TokenType::Number, "123".to_string(), Literal::Num(Hf64::from(123.0)), 1),
//             Token::new(TokenType::Number, "456".to_string(), Literal::Num(Hf64::from(456.0)), 1), // Unexpected token
//             Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_err());
//     }
//
//     #[test]
//     fn test_binary_expression_precedence() {
//         let tokens = vec![
//             Token::new(TokenType::Number, "1".to_string(), Literal::Num(Hf64::from(1.0)), 1),
//             Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Number, "2".to_string(), Literal::Num(Hf64::from(2.0)), 1),
//             Token::new(TokenType::Star, "*".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Number, "3".to_string(), Literal::Num(Hf64::from(3.0)), 1),
//             Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_ok());
//         let stmt = &result.unwrap()[0];
//
//         match stmt {
//             Stmt::Expression { expression } => {
//                 if let Expr::Binary {
//                     operator, right, ..
//                 } = expression
//                 {
//                     assert_eq!(operator.lexeme, "+");
//                     if let Expr::Binary { operator, .. } = right.as_ref() {
//                         assert_eq!(operator.lexeme, "*");
//                     } else {
//                         panic!("Right hand expression is not a binary expression");
//                     }
//                 } else {
//                     panic!("Expected binary expression");
//                 }
//             }
//             _ => panic!("Expected expression statement"),
//         }
//     }
//
//     #[test]
//     fn test_print_statement() {
//         let tokens = vec![
//             Token::new(TokenType::Print, "print".to_string(), Literal::Nil, 1),
//             Token::new(
//                 TokenType::String,
//                 "\"Hello, world!\"".to_string(),
//                 Literal::Str("Hello, world!".to_string()),
//                 1,
//             ),
//             Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_ok());
//         matches!(result.unwrap()[0], Stmt::Print { .. });
//     }
//
//     #[test]
//     fn test_empty_block() {
//         let tokens = vec![
//             Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_ok());
//         assert_eq!(result.unwrap().len(), 1); // Should contain one block statement
//     }
//
//     #[test]
//     fn test_nested_blocks() {
//         let tokens = vec![
//             Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_ok());
//         let stmts = result.unwrap();
//         assert_eq!(stmts.len(), 1); // Should contain one block statement
//         matches!(stmts[0], Stmt::Block { .. });
//     }
//
//     #[test]
//     fn test_block_with_statements() {
//         let tokens = vec![
//             Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Print, "print".to_string(), Literal::Nil, 1),
//             Token::new(
//                 TokenType::String,
//                 "\"Hello\"".to_string(),
//                 Literal::Str("Hello".to_string()),
//                 1,
//             ),
//             Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_ok());
//         let stmts = result.unwrap();
//         assert_eq!(stmts.len(), 1); // Should contain one block statement
//         matches!(&stmts[0], Stmt::Block { statements } if statements.len() == 1);
//     }
//
//     #[test]
//     fn test_missing_closing_brace() {
//         let tokens = vec![
//             Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Print, "print".to_string(), Literal::Nil, 1),
//             Token::new(
//                 TokenType::String,
//                 "\"Hello\"".to_string(),
//                 Literal::Str("Hello".to_string()),
//                 1,
//             ),
//             Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
//             Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1), // Missing closing brace
//         ];
//         let mut parser = Parser::new(tokens);
//         let result = parser.parse();
//         assert!(result.is_err());
//     }
// }