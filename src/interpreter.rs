use crate::expr::Expr;
use crate::lexer::token::Literal;
use crate::lexer::token::Token;
use crate::lexer::token::TokenType;
use crate::lox;
use std::fmt;

pub enum RuntimeError {
    IncorrectOperand(Token, LoxValueError),
    InterpreterPanic(Token, String),
    DivideByZero(Token, LoxValueError),
}

impl RuntimeError {
    pub fn get_info(self) -> (Token, String) {
        match self {
            RuntimeError::IncorrectOperand(token, lox_value_err)
            | RuntimeError::DivideByZero(token, lox_value_err) => {
                (token, lox_value_err.get_string())
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

#[derive(PartialEq)]
enum LoxValue {
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
                    TokenType::Minus => left_num - right_num,
                    TokenType::Slash => left_num / right_num,
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

pub struct Interpreter;

impl Interpreter {
    pub fn interpret(expression: Expr) {
        match Self::evaluate(&expression) {
            Ok(value) => println!("{value}"),
            Err(e) => lox::runtime_error(e),
        }
    }

    fn evaluate(expr: &Expr) -> Result<LoxValue, RuntimeError> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => Self::evaluate_binary(left, operator, right),
            Expr::Unary { operator, right } => Self::evaluate_unary(operator, right),
            Expr::Literal { value } => Ok(Self::evaluate_literal(value)),
            Expr::Grouping { expression } => Self::evaluate(expression),
        }
    }

    fn evaluate_literal(value: &Literal) -> LoxValue {
        LoxValue::from(value)
    }

    fn evaluate_binary(
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<LoxValue, RuntimeError> {
        let left = Self::evaluate(left)?;
        let right = Self::evaluate(right)?;

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

    fn evaluate_unary(operator: &Token, right: &Expr) -> Result<LoxValue, RuntimeError> {
        let right = Self::evaluate(right)?;

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
}
