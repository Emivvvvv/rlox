use crate::environment::{Environment, EnvironmentError};
use crate::expr::Expr;
use crate::lexer::token::Literal;
use crate::lexer::token::Token;
use crate::lexer::token::TokenType;
use crate::lox;
use crate::stmt::Stmt;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub enum RuntimeError {
    IncorrectOperand(Token, LoxValueError),
    InterpreterPanic(Token, String),
    DivideByZero(Token, LoxValueError),
    UndefinedVariable(Token, EnvironmentError),
}

impl RuntimeError {
    pub fn get_info(self) -> (Token, String) {
        match self {
            RuntimeError::IncorrectOperand(token, lox_value_err)
            | RuntimeError::DivideByZero(token, lox_value_err) => {
                (token, lox_value_err.get_string())
            }
            RuntimeError::UndefinedVariable(token, environment_err) => {
                (token, environment_err.get_string())
            }
            RuntimeError::InterpreterPanic(token, err_str) => (token, err_str),
        }
    }
}

pub enum LoxValueError {
    IncorrectOperand(String),
    DivideByZero(String),
}

impl LoxValueError {
    fn get_string(self) -> String {
        match self {
            LoxValueError::IncorrectOperand(err_str) | LoxValueError::DivideByZero(err_str) => {
                err_str
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LoxValue {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl LoxValue {
    fn negate_if_num(self) -> Result<Self, LoxValueError> {
        match self {
            LoxValue::Number(num) => Ok(LoxValue::Number(-num)),
            _ => Err(LoxValueError::IncorrectOperand(
                "Operand must be a number.".to_string(),
            )),
        }
    }

    fn bang_if_bool(self) -> Result<Self, LoxValueError> {
        match self {
            LoxValue::Boolean(bool) => Ok(LoxValue::Boolean(!bool)),
            _ => Err(LoxValueError::IncorrectOperand(
                "Operand must be a bool.".to_string(),
            )),
        }
    }

    fn is_truthy(&self) -> Self {
        if self == &LoxValue::Nil {
            return LoxValue::Boolean(false);
        }
        if let &LoxValue::Boolean(bool) = self {
            return LoxValue::Boolean(bool);
        }

        return LoxValue::Boolean(true);
    }

    fn math_if_num(self, other: Self, opeator: TokenType) -> Result<Self, LoxValueError> {
        match (self, other) {
            (LoxValue::Number(left_num), LoxValue::Number(right_num)) => {
                let result = match opeator {
                    TokenType::Plus => left_num + right_num,
                    TokenType::Minus => left_num - right_num,
                    TokenType::Slash => {
                        if right_num == 0. {
                            return Err(LoxValueError::DivideByZero("Divide by zero".to_string()));
                        }
                        left_num / right_num
                    }
                    TokenType::Star => left_num * right_num,
                    _ => {
                        return Err(LoxValueError::IncorrectOperand(
                            "Undefined operator on numbers.".to_string(),
                        ))
                    }
                };

                Ok(LoxValue::Number(result))
            }
            _ => Err(LoxValueError::IncorrectOperand(
                "Operands must be numbers.".to_string(),
            )),
        }
    }

    fn compare_if_num(self, other: Self, operator: TokenType) -> Result<Self, LoxValueError> {
        match (self, other) {
            (LoxValue::Number(left_num), LoxValue::Number(right_num)) => {
                let bool_value = match operator {
                    TokenType::Greater => left_num > right_num,
                    TokenType::GreaterEqual => left_num >= right_num,
                    TokenType::Less => left_num < right_num,
                    TokenType::LessEqual => left_num <= right_num,
                    _ => {
                        return Err(LoxValueError::IncorrectOperand(
                            "Undefined comparator on numbers.".to_string(),
                        ))
                    }
                };

                Ok(LoxValue::Boolean(bool_value))
            }
            _ => Err(LoxValueError::IncorrectOperand(
                "Operands must be numbers.".to_string(),
            )),
        }
    }

    fn is_equal(self, other: Self) -> Self {
        let bool_value = match (self, other) {
            (LoxValue::Boolean(left_bool), LoxValue::Boolean(right_bool)) => {
                left_bool == right_bool
            }
            (LoxValue::String(left_str), LoxValue::String(right_str)) => left_str == right_str,
            (LoxValue::Number(left_num), LoxValue::Number(right_num)) => left_num == right_num,
            (LoxValue::Nil, LoxValue::Nil) => true,
            _ => false,
        };

        LoxValue::Boolean(bool_value)
    }
}

impl From<&Literal> for LoxValue {
    fn from(literal: &Literal) -> Self {
        match literal {
            Literal::Str(str) => Self::String(str.clone()),
            Literal::Num(num) => Self::Number(num.clone()),
            Literal::Nil => Self::Nil,
            Literal::True => Self::Boolean(true),
            Literal::False => Self::Boolean(false),
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text = match self {
            LoxValue::String(str) => str.clone(),
            LoxValue::Number(num) => {
                let mut str_num = num.to_string();

                if str_num.ends_with(".0") {
                    str_num.pop();
                    str_num.pop();
                }

                str_num
            }
            LoxValue::Boolean(bool) => bool.to_string(),
            LoxValue::Nil => "nil".to_string(),
        };
        write!(f, "{text}",)
    }
}

pub trait Evaluable {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<LoxValue, RuntimeError>;
}

impl Evaluable for Expr {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<LoxValue, RuntimeError> {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => interpreter.evaluate_binary(left, operator, right),
            Expr::Unary { operator, right } => interpreter.evaluate_unary(operator, right),
            Expr::Literal { value } => Ok(Interpreter::evaluate_literal(value)),
            Expr::Grouping { expression } => interpreter.evaluate(expression),
            Expr::Variable { name } => interpreter.evaluate_variable(name),
            Expr::Assign { name, value } => interpreter.evaluate_assign(name, value),
        }
    }
}

impl Evaluable for Box<Expr> {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<LoxValue, RuntimeError> {
        // Dereference the Box to access the inner Expr, and then call evaluate on it
        self.as_ref().evaluate(interpreter)
    }
}

impl Evaluable for Stmt {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<LoxValue, RuntimeError> {
        match self {
            Stmt::Expression { expression } => interpreter.evaluate_expression_stmt(expression),
            Stmt::Print { expression } => interpreter.evaluate_print_stmt(expression),
            Stmt::Var { name, initializer } => interpreter.evaluate_var_stmt(name, initializer),
            Stmt::Block { statements } => interpreter.evaluate_block_stmt(statements),
        }
    }
}

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new(environment: Rc<RefCell<Environment>>) -> Self {
        Self {
            environment: environment,
        }
    }

    #[cfg(test)]
    pub fn interpret_test(&mut self, statements: Vec<Stmt>) -> LoxValue {
        for statement in statements {
            match self.evaluate(&statement) {
                Ok(value) => return value,
                Err(e) => lox::runtime_error(e),
            }
        }

        todo!()
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) {
        for statement in statements {
            match self.evaluate(&statement) {
                Ok(_) => continue, // println!("debug print: {value}")
                Err(e) => lox::runtime_error(e),
            }
        }
    }

    fn evaluate(&mut self, evaluable: &dyn Evaluable) -> Result<LoxValue, RuntimeError> {
        evaluable.evaluate(self)
    }

    fn evaluate_literal(value: &Literal) -> LoxValue {
        LoxValue::from(value)
    }

    fn evaluate_binary(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<LoxValue, RuntimeError> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        match operator.token_type {
            TokenType::Minus | TokenType::Slash | TokenType::Star => left
                .math_if_num(right, operator.token_type.clone())
                .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e)),
            TokenType::Plus => match (&left, &right) {
                (LoxValue::String(left_str), LoxValue::String(right_str)) => {
                    return Ok(LoxValue::String(left_str.clone() + right_str));
                }
                (LoxValue::Number(_), LoxValue::Number(_)) => {
                    return left
                        .math_if_num(right, TokenType::Plus)
                        .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e));
                }
                _ => Ok(LoxValue::String(format!("{left}{right}"))),
            },
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => left
                .compare_if_num(right, operator.token_type.clone())
                .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e)),
            TokenType::BangEqual => left
                .is_equal(right)
                .bang_if_bool()
                .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e)),
            TokenType::EqualEqual => Ok(left.is_equal(right)),
            _ => Err(RuntimeError::InterpreterPanic(
                operator.clone(),
                "Invalid token type for evaluating binarys.".to_string(),
            )),
        }
    }

    fn evaluate_unary(&mut self, operator: &Token, right: &Expr) -> Result<LoxValue, RuntimeError> {
        let right = self.evaluate(right)?;

        match operator.token_type {
            TokenType::Bang => return Ok(right.is_truthy()),
            TokenType::Minus => {
                return right
                    .negate_if_num()
                    .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e))
            }
            _ => Err(RuntimeError::InterpreterPanic(
                operator.clone(),
                "Invalid token type for evaluating unarys.".to_string(),
            )),
        }
    }

    fn evaluate_variable(&mut self, name: &Token) -> Result<LoxValue, RuntimeError> {
        self.environment
            .borrow_mut()
            .get(name)
            .map_err(|e| RuntimeError::UndefinedVariable(name.clone(), e))
    }

    fn evaluate_assign(&mut self, name: &Token, expr: &Expr) -> Result<LoxValue, RuntimeError> {
        let value = self.evaluate(expr)?;
        self.environment
            .borrow_mut()
            .assign(name, value.clone())
            .map_err(|e| RuntimeError::UndefinedVariable(name.clone(), e))?;

        Ok(value)
    }

    fn evaluate_expression_stmt(&mut self, expr: &Expr) -> Result<LoxValue, RuntimeError> {
        self.evaluate(expr)
    }

    fn evaluate_print_stmt(&mut self, expr: &Expr) -> Result<LoxValue, RuntimeError> {
        let value = self.evaluate(expr)?;
        println!("{value}");

        Ok(LoxValue::String(value.to_string()))
    }

    fn evaluate_var_stmt(
        &mut self,
        name: &Token,
        initializer: &Option<Expr>,
    ) -> Result<LoxValue, RuntimeError> {
        let mut value = None;
        if let Some(expr) = initializer {
            value = Some(self.evaluate(expr)?);
        }

        let final_value = match value {
            Some(lox_value) => lox_value,
            None => LoxValue::Nil,
        };

        self.environment
            .borrow_mut()
            .define(name.lexeme.clone(), final_value);

        Ok(LoxValue::Nil)
    }

    pub fn evaluate_block_stmt(&mut self, statements: &[Stmt]) -> Result<LoxValue, RuntimeError> {
        // Create a new environment with the current environment as enclosing
        let new_environment = Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(
            &self.environment,
        ))));

        // Create a new interpreter instance with the new environment
        let mut block_interpreter = Interpreter::new(Rc::clone(&new_environment));

        // Execute each statement in the block using the new interpreter
        for statement in statements {
            block_interpreter.evaluate(statement)?;
        }

        Ok(LoxValue::Nil)
    }
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_arithmetic_expression() {
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
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
        let output = interpreter.interpret_test(ast);
        assert_eq!(output, LoxValue::Number(7.0)); // 1 + (2 * 3) = 7
    }

    #[test]
    #[should_panic]
    fn test_unary_negation() {
        let tokens = vec![
            Token::new(TokenType::Minus, "-".to_string(), Literal::Nil, 1),
            Token::new(TokenType::True, "true".to_string(), Literal::True, 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
        let _ = interpreter.interpret_test(ast);
    }

    #[test]
    #[should_panic]
    fn test_division_by_zero() {
        let tokens = vec![
            Token::new(TokenType::Number, "10".to_string(), Literal::Num(10.0), 1),
            Token::new(TokenType::Slash, "/".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "0".to_string(), Literal::Num(0.0), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
        let _ = interpreter.interpret_test(ast);
    }

    #[test]
    fn test_string_concatenation() {
        let tokens = vec![
            Token::new(
                TokenType::String,
                "\"Hello, \"".to_string(),
                Literal::Str("Hello, ".to_string()),
                1,
            ),
            Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::String,
                "\"world!\"".to_string(),
                Literal::Str("world!".to_string()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
        let output = interpreter.interpret_test(ast);
        assert_eq!(output, LoxValue::String("Hello, world!".to_string()));
    }

    #[test]
    fn test_grouping_and_precedence() {
        let tokens = vec![
            Token::new(TokenType::LeftParen, "(".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "1".to_string(), Literal::Num(1.0), 1),
            Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "2".to_string(), Literal::Num(2.0), 1),
            Token::new(TokenType::RightParen, ")".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Star, "*".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "3".to_string(), Literal::Num(3.0), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
        let output = interpreter.interpret_test(ast);
        assert_eq!(output, LoxValue::Number(9.0)); // (1 + 2) * 3 = 9
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
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
        let output = interpreter.interpret_test(ast);
        assert_eq!(output, LoxValue::String("Hello, world!".to_string()));
    }

    #[test]
    fn test_math_and_print() {
        // Tokens for the expression: print (2 + 3) * 4;
        let tokens = vec![
            Token::new(TokenType::Print, "print".to_string(), Literal::Nil, 1),
            Token::new(TokenType::LeftParen, "(".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "2".to_string(), Literal::Num(2.0), 1),
            Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "3".to_string(), Literal::Num(3.0), 1),
            Token::new(TokenType::RightParen, ")".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Star, "*".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "4".to_string(), Literal::Num(4.0), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
        let output = interpreter.interpret_test(ast);

        assert_eq!(output, LoxValue::String("20".to_string()));
    }

    #[test]
    fn test_single_statement_block() {
        let tokens = vec![
            Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Var, "var".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Identifier, "x".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Equal, "=".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "10".to_string(), Literal::Num(10.0), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
        let output = interpreter.interpret_test(ast);
        assert_eq!(output, LoxValue::Nil);
    }

    #[test]
    fn test_nested_block() {
        let tokens = vec![
            Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Var, "var".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Identifier, "x".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Equal, "=".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "10".to_string(), Literal::Num(10.0), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Var, "var".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Identifier, "y".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Equal, "=".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "5".to_string(), Literal::Num(5.0), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
        let output = interpreter.interpret_test(ast);
        assert_eq!(output, LoxValue::Nil);
    }

    #[test]
    fn test_block_with_multiple_statements() {
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
            Token::new(TokenType::Var, "var".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Identifier, "number".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Equal, "=".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Number, "42".to_string(), Literal::Num(42.0), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new(Rc::new(RefCell::new(Environment::new())));
        let output = interpreter.interpret_test(ast);
        assert_eq!(output, LoxValue::Nil); // Output from print is visible during test but not captured by assert
    }
}
