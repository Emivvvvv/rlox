use crate::environment::{Environment, EnvironmentError};
use crate::expr::Expr;
use crate::globals::define_globals;
use crate::lexer::token::Literal;
use crate::lexer::token::Token;
use crate::lexer::token::TokenType;
use crate::lox;
use crate::lox_callable::LoxCallable;
use crate::lox_function::LoxFunction;
use crate::stmt::Stmt;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub enum RuntimeError {
    IncorrectOperand(Token, LoxValueError),
    InterpreterPanic(Token, String),
    DivideByZero(Token, LoxValueError),
    UndefinedVariable(Token, EnvironmentError),
    AssignVariableError(Token, EnvironmentError),
    InputError(String),
    Return(LoxValue),
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
            _ => panic!("Should not reach here!"),
        }
    }
}

#[derive(Debug)]
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

#[derive(Clone)]
pub enum LoxValue {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Rc<dyn LoxCallable>),
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
            return LoxValue::Boolean(false)
        }
        if let &LoxValue::Boolean(bool) = self {
            return LoxValue::Boolean(bool)
        }

        LoxValue::Boolean(true)
    }

    fn math_if_num(self, other: Self, operator: TokenType) -> Result<Self, LoxValueError> {
        match (self, other) {
            (LoxValue::Number(left_num), LoxValue::Number(right_num)) => {
                let result = match operator {
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
            Literal::Num(num) => Self::Number(f64::from(num)),
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
            LoxValue::Callable(callable) => callable.to_string(),
        };
        write!(f, "{text}",)
    }
}

impl fmt::Debug for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "Nil"),
            LoxValue::Boolean(b) => write!(f, "Boolean({:?})", b),
            LoxValue::Number(n) => write!(f, "Number({:?})", n),
            LoxValue::String(s) => write!(f, "String({:?})", s),
            LoxValue::Callable(_) => write!(f, "Callable(<function>)"),
        }
    }
}

impl PartialEq for LoxValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoxValue::Nil, LoxValue::Nil) => true,
            (LoxValue::Boolean(a), LoxValue::Boolean(b)) => a == b,
            (LoxValue::Number(a), LoxValue::Number(b)) => a == b,
            (LoxValue::String(a), LoxValue::String(b)) => a == b,
            // Comparing callables directly is usually not meaningful
            (LoxValue::Callable(_), LoxValue::Callable(_)) => false,
            _ => false,
        }
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
            Expr::Logical {
                left,
                operator,
                right,
            } => interpreter.evaluate_logical(left, operator, right),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => interpreter.evaluate_call(callee, paren, arguments),
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
            Stmt::Block { statements } => interpreter.evaluate_block_stmt(statements, None),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => interpreter.evaluate_if_stmt(condition, *then_branch.clone(), else_branch),
            Stmt::While { condition, body } => {
                interpreter.evaluate_while_stmt(condition, *body.clone())
            }
            Stmt::Function { name, params, body } => {
                interpreter.interpret_function_stmt(name, params, body)
            }
            Stmt::Return { keyword, value } => interpreter.interpret_return_stmt(keyword, value),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,
    locals: HashMap<Expr, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        // Create a new global environment
        let globals = Environment::new();
        define_globals(&globals);

        // Initially, the environment is the global environment
        let environment = Rc::clone(&globals);

        Self {
            globals,
            environment,
            locals: HashMap::new(),
        }
    }

    pub fn get_globals(&self) -> Rc<RefCell<Environment>> {
        Rc::clone(&self.globals)
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
                Ok(_) => continue,
                Err(e) => lox::runtime_error(e),
            }
        }
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        self.locals.insert(expr.clone(), depth);
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
                    Ok(LoxValue::String(left_str.clone() + right_str))
                }
                (LoxValue::Number(_), LoxValue::Number(_)) => {
                    left
                        .math_if_num(right, TokenType::Plus)
                        .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e))
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
                "Invalid token type for evaluating binary's.".to_string(),
            )),
        }
    }

    fn evaluate_unary(&mut self, operator: &Token, right: &Expr) -> Result<LoxValue, RuntimeError> {
        let right = self.evaluate(right)?;

        match operator.token_type {
            TokenType::Bang => Ok(right.is_truthy()),
            TokenType::Minus => {
                right
                    .negate_if_num()
                    .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e))
            }
            _ => Err(RuntimeError::InterpreterPanic(
                operator.clone(),
                "Invalid token type for evaluating unary.".to_string(),
            )),
        }
    }

    fn evaluate_variable(&mut self, name: &Token) -> Result<LoxValue, RuntimeError> {
        self.look_up_variable(name, Expr::Variable { name: name.clone() })
    }

    fn look_up_variable(&mut self, name: &Token, expr: Expr) -> Result<LoxValue, RuntimeError> {
        match self.locals.get(&expr) {
            Some(distance) => {
                // Call get_at with the environment wrapped in Rc<RefCell<Environment>>
                Environment::get_at(Rc::clone(&self.environment), *distance, &name.lexeme)
                    .map_err(|e| RuntimeError::UndefinedVariable(name.clone(), e))
            },
            None => {
                // Call get with the globals environment wrapped in Rc<RefCell<Environment>>
                self.globals.borrow_mut().get(name)
                    .map_err(|e| RuntimeError::UndefinedVariable(name.clone(), e))
            },
        }
    }

    fn evaluate_assign(&mut self, name: &Token, expr: &Expr) -> Result<LoxValue, RuntimeError> {
        let value = self.evaluate(expr)?;

        match self.locals.get(&expr) {
            Some(distance) => {
                // Call assign_at with the environment wrapped in Rc<RefCell<Environment>>
                Environment::assign_at(Rc::clone(&self.environment), *distance, name, value.clone())
                    .map_err(|e| RuntimeError::AssignVariableError(name.clone(), e))
            },
            None => {
                // Call assign with the globals environment wrapped in Rc<RefCell<Environment>>
                self.globals.borrow_mut().assign(name, value.clone())
                    .map_err(|e| RuntimeError::AssignVariableError(name.clone(), e))
            },
        }
    }

    pub fn evaluate_logical(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<LoxValue, RuntimeError> {
        let left = self.evaluate(left)?;
        let is_truthy = left.is_truthy() == LoxValue::Boolean(true);

        if operator.token_type == TokenType::Or && is_truthy
            || operator.token_type != TokenType::Or && !is_truthy
        {
            return Ok(left);
        }

        self.evaluate(right)
    }

    pub fn evaluate_call(
        &mut self,
        callee: &Expr,
        paren: &Token,
        arguments: &Vec<Expr>,
    ) -> Result<LoxValue, RuntimeError> {
        let callee = self.evaluate(callee)?;

        let mut evaluated_arguments = Vec::new();
        for argument in arguments {
            let value = self.evaluate(argument)?;
            evaluated_arguments.push(value);
        }

        match callee {
            LoxValue::Callable(function) => {
                if arguments.len() != function.arity() {
                    return Err(RuntimeError::InterpreterPanic(
                        paren.clone(),
                        format!(
                            "Expected {} arguments but got {}.",
                            function.arity(),
                            arguments.len()
                        ),
                    ));
                }
                match function.call(self, evaluated_arguments) {
                    Ok(value) => Ok(value), // Normal function return
                    Err(RuntimeError::Return(return_value)) => Ok(return_value), // Handle the return exception
                    Err(err) => Err(err), // Propagate other errors
                }
            }
            _ => Err(RuntimeError::InterpreterPanic(
                paren.clone(),
                "Can only call functions and classes.".to_string(),
            )),
        }
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

    pub fn evaluate_block_stmt(
        &mut self,
        statements: &[Stmt],
        environment: Option<Rc<RefCell<Environment>>>,
    ) -> Result<LoxValue, RuntimeError> {
        let previous_environment = Rc::clone(&self.environment);

        let new_environment = match environment {
            Some(env) => env,
            None => Environment::with_enclosing(Rc::clone(
                &self.environment,
            )),
        };

        self.environment = Rc::clone(&new_environment);

        let result = statements
            .iter()
            .try_for_each(|statement| self.evaluate(statement).map(|_| ()));

        self.environment = previous_environment;

        result.map(|_| LoxValue::Nil)
    }

    pub fn evaluate_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<LoxValue, RuntimeError> {
        if self.evaluate(condition)?.is_truthy() == LoxValue::Boolean(true) {
            self.evaluate(&then_branch)?;
        } else if let Some(else_stmt) = else_branch {
            self.evaluate(else_stmt.as_ref())?;
        }

        Ok(LoxValue::Nil)
    }

    pub fn evaluate_while_stmt(
        &mut self,
        condition: &Expr,
        body: Stmt,
    ) -> Result<LoxValue, RuntimeError> {
        while self.evaluate(condition)?.is_truthy() == LoxValue::Boolean(true) {
            self.evaluate(&body)?;
        }

        Ok(LoxValue::Nil)
    }

    pub fn interpret_function_stmt(
        &mut self,
        name: &Token,
        params: &Vec<Token>,
        body: &Vec<Stmt>,
    ) -> Result<LoxValue, RuntimeError> {
        let function = LoxFunction::new(
            name.clone(),
            params.clone(),
            body.clone(),
            Rc::clone(&self.environment),
        );

        let callable: Rc<dyn LoxCallable> = Rc::new(function);

        self.environment
            .borrow_mut()
            .define(name.lexeme.clone(), LoxValue::Callable(callable));

        Ok(LoxValue::Nil)
    }

    pub fn interpret_return_stmt(
        &mut self,
        _keyword: &Token,
        value: &Option<Expr>,
    ) -> Result<LoxValue, RuntimeError> {
        let value = match value {
            Some(expr) => self.evaluate(expr)?,
            None => LoxValue::Nil,
        };
        Err(RuntimeError::Return(value))
    }
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_arithmetic_expression() {
        let tokens = vec![
            Token::new(
                TokenType::Number,
                "1".to_string(),
                Literal::Num(1.0.into()),
                1,
            ),
            Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::Number,
                "2".to_string(),
                Literal::Num(2.0.into()),
                1,
            ),
            Token::new(TokenType::Star, "*".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::Number,
                "3".to_string(),
                Literal::Num(3.0.into()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new();
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
        let mut interpreter = Interpreter::new();
        let _ = interpreter.interpret_test(ast);
    }

    #[test]
    #[should_panic]
    fn test_division_by_zero() {
        let tokens = vec![
            Token::new(
                TokenType::Number,
                "10".to_string(),
                Literal::Num(10.0.into()),
                1,
            ),
            Token::new(TokenType::Slash, "/".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::Number,
                "0".to_string(),
                Literal::Num(0.0.into()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new();
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
        let mut interpreter = Interpreter::new();
        let output = interpreter.interpret_test(ast);
        assert_eq!(output, LoxValue::String("Hello, world!".to_string()));
    }

    #[test]
    fn test_grouping_and_precedence() {
        let tokens = vec![
            Token::new(TokenType::LeftParen, "(".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::Number,
                "1".to_string(),
                Literal::Num(1.0.into()),
                1,
            ),
            Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::Number,
                "2".to_string(),
                Literal::Num(2.0.into()),
                1,
            ),
            Token::new(TokenType::RightParen, ")".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Star, "*".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::Number,
                "3".to_string(),
                Literal::Num(3.0.into()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new();
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
        let mut interpreter = Interpreter::new();
        let output = interpreter.interpret_test(ast);
        assert_eq!(output, LoxValue::String("Hello, world!".to_string()));
    }

    #[test]
    fn test_math_and_print() {
        // Tokens for the expression: print (2 + 3) * 4;
        let tokens = vec![
            Token::new(TokenType::Print, "print".to_string(), Literal::Nil, 1),
            Token::new(TokenType::LeftParen, "(".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::Number,
                "2".to_string(),
                Literal::Num(2.0.into()),
                1,
            ),
            Token::new(TokenType::Plus, "+".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::Number,
                "3".to_string(),
                Literal::Num(3.0.into()),
                1,
            ),
            Token::new(TokenType::RightParen, ")".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Star, "*".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::Number,
                "4".to_string(),
                Literal::Num(4.0.into()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new();
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
            Token::new(
                TokenType::Number,
                "10".to_string(),
                Literal::Num(10.0.into()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new();
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
            Token::new(
                TokenType::Number,
                "10".to_string(),
                Literal::Num(10.0.into()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::LeftBrace, "{".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Var, "var".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Identifier, "y".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Equal, "=".to_string(), Literal::Nil, 1),
            Token::new(
                TokenType::Number,
                "5".to_string(),
                Literal::Num(5.0.into()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new();
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
            Token::new(
                TokenType::Number,
                "42".to_string(),
                Literal::Num(42.0.into()),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightBrace, "}".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut interpreter = Interpreter::new();
        let output = interpreter.interpret_test(ast);
        assert_eq!(output, LoxValue::Nil); // Output from print is visible during test but not captured by assert
    }

    #[test]
    fn test_clock_function() {
        let tokens = vec![
            Token::new(TokenType::Identifier, "clock".to_string(), Literal::Nil, 1),
            Token::new(TokenType::LeftParen, "(".to_string(), Literal::Nil, 1),
            Token::new(TokenType::RightParen, ")".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Semicolon, ";".to_string(), Literal::Nil, 1),
            Token::new(TokenType::Eof, "".to_string(), Literal::Nil, 1),
        ];

        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();

        let mut interpreter = Interpreter::new();
        let output = interpreter.interpret_test(ast);

        if let LoxValue::Number(time) = output {
            assert!(time > 0.0);
        } else {
            panic!("Expected a number from clock function");
        }
    }

    #[test]
    fn test_evaluate_block_stmt_environment_reversion() {
        let mut interpreter = Interpreter::new();
        let global_env = interpreter.get_globals();

        // Define a variable in the global scope
        global_env
            .borrow_mut()
            .define("x".to_string(), LoxValue::Number(10.0));

        // Block that defines a new variable and should revert to the global environment
        let block_statements = vec![Stmt::Var {
            name: Token::new(TokenType::Identifier, "y".to_string(), Literal::Nil, 1),
            initializer: Some(Expr::Literal {
                value: Literal::Num(20.0.into()),
            }),
        }];

        interpreter
            .evaluate_block_stmt(&block_statements, None)
            .unwrap();

        // Ensure that 'x' still exists in the global environment after block execution
        assert_eq!(
            global_env.borrow().get(&Token::new(
                TokenType::Identifier,
                "x".to_string(),
                Literal::Nil,
                1
            )),
            Ok(LoxValue::Number(10.0))
        );

        // Ensure that 'y' is not accessible outside the block (since it's local to the block)
        assert!(global_env
            .borrow()
            .get(&Token::new(
                TokenType::Identifier,
                "y".to_string(),
                Literal::Nil,
                1
            ))
            .is_err());
    }

    #[test]
    fn test_evaluate_assign_correct_environment() {
        let mut interpreter = Interpreter::new();
        let global_env = interpreter.get_globals();

        // Define variable 'a' in global scope
        global_env
            .borrow_mut()
            .define("a".to_string(), LoxValue::String("first".to_string()));

        // Assign new value to 'a'
        let name = Token::new(TokenType::Identifier, "a".to_string(), Literal::Nil, 1);
        let value = Box::new(Expr::Literal {
            value: Literal::Str("second".to_string()),
        });

        interpreter.evaluate_assign(&name, &value).unwrap();

        // Ensure 'a' was updated in the global scope
        assert_eq!(
            global_env.borrow().get(&name),
            Ok(LoxValue::String("second".to_string()))
        );
    }

    #[test]
    fn test_scope_management() {
        let mut interpreter = Interpreter::new();

        // Begin a new scope
        interpreter
            .evaluate_block_stmt(
                &[Stmt::Var {
                    name: Token::new(TokenType::Identifier, "x".to_string(), Literal::Nil, 1),
                    initializer: Some(Expr::Literal {
                        value: Literal::Num(10.0.into()),
                    }),
                }],
                None,
            )
            .unwrap();

        // Ensure 'x' is not accessible after the scope ends
        assert!(interpreter
            .get_globals()
            .borrow()
            .get(&Token::new(
                TokenType::Identifier,
                "x".to_string(),
                Literal::Nil,
                1
            ))
            .is_err());
    }
}