use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::globals::{ClockFunction, InputFunction};
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lexer::token::{Literal, TokenType};
use crate::lox_callable::lox_class::LoxClass;
use crate::lox_callable::lox_function::LoxFunction;
use crate::lox_callable::lox_instance::LoxInstance;
use crate::lox_callable::callable::Callable;
use crate::symbol::SymbolTable;

#[derive(Debug)]
pub enum LoxValueError {
    IncorrectOperand(String),
    DivideByZero(String),
}

impl LoxValueError {
    pub(crate) fn get_string(self) -> String {
        match self {
            LoxValueError::IncorrectOperand(err_str) | LoxValueError::DivideByZero(err_str) => {
                err_str
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NativeFunctions {
    ClockFunction(Rc<ClockFunction>),
    InputFunction(Rc<InputFunction>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LoxCallable {
    Function(Rc<LoxFunction>),
    Class(Rc<LoxClass>),
    Instance(Rc<RefCell<LoxInstance>>),
    NativeFunction(NativeFunctions),
}

#[derive(Clone, Debug)]
pub enum LoxValue {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(LoxCallable),
}

impl LoxValue {
    pub(crate) fn negate_if_num(self) -> Result<Self, LoxValueError> {
        match self {
            LoxValue::Number(num) => Ok(LoxValue::Number(-num)),
            _ => Err(LoxValueError::IncorrectOperand(
                "Operand must be a number.".to_string(),
            )),
        }
    }

    pub(crate) fn bang_if_bool(self) -> Result<Self, LoxValueError> {
        match self {
            LoxValue::Boolean(bool) => Ok(LoxValue::Boolean(!bool)),
            _ => Err(LoxValueError::IncorrectOperand(
                "Operand must be a bool.".to_string(),
            )),
        }
    }

    pub(crate) fn is_truthy(&self) -> Self {
        if self == &LoxValue::Nil {
            return LoxValue::Boolean(false)
        }
        if let &LoxValue::Boolean(bool) = self {
            return LoxValue::Boolean(bool)
        }

        LoxValue::Boolean(true)
    }

    pub(crate) fn math_if_num(self, other: Self, operator: &TokenType) -> Result<Self, LoxValueError> {
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

    pub(crate) fn compare_if_num(self, other: Self, operator: TokenType) -> Result<Self, LoxValueError> {
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

    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn is_equal(self, other: Self) -> Self {
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

impl Callable for LoxCallable {
    fn arity(&self, symbol_table: &mut SymbolTable) -> usize {
        match self {
            LoxCallable::Function(function) => function.arity(symbol_table),
            LoxCallable::Class(class) => class.arity(symbol_table),
            LoxCallable::Instance(instance) => instance.borrow().arity(symbol_table),
            LoxCallable::NativeFunction(native_function) => match native_function {
                NativeFunctions::ClockFunction(clock) => clock.arity(symbol_table),
                NativeFunctions::InputFunction(input) => input.arity(symbol_table),
            },
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        match self {
            LoxCallable::Function(function) => function.as_ref().call(interpreter, arguments),
            LoxCallable::Class(class) => class.as_ref().call(interpreter, arguments),
            LoxCallable::Instance(instance) => instance.borrow().call(interpreter, arguments),
            LoxCallable::NativeFunction(native_function) => match native_function {
                NativeFunctions::ClockFunction(clock) => clock.as_ref().call(interpreter, arguments),
                NativeFunctions::InputFunction(input) => input.as_ref().call(interpreter, arguments),
            },
        }
    }

    fn get_name(&self) -> String {
        match self {
            LoxCallable::Function(function) => function.get_name(),
            LoxCallable::Class(class) => class.get_name(),
            LoxCallable::Instance(instance) => instance.borrow().get_name(),
            LoxCallable::NativeFunction(native_function) => match native_function {
                NativeFunctions::ClockFunction(clock) => clock.get_name(),
                NativeFunctions::InputFunction(input) => input.get_name(),
            },
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
            LoxValue::Callable(callable) => {
                let name = match callable {
                    LoxCallable::Function(function) => {
                        let func = Rc::clone(function);
                        func.get_name().to_string()
                    }
                    LoxCallable::Class(class) => {
                        let class_ref = Rc::clone(class);
                        class_ref.get_name().to_string()
                    }
                    LoxCallable::Instance(instance) => {
                        let instance_ref = Rc::clone(instance);
                        let x = instance_ref.borrow().get_name().to_string();
                        x
                    }
                    LoxCallable::NativeFunction(native_function) => {
                        match native_function {
                            NativeFunctions::ClockFunction(clock) => {
                                let clock_ref = Rc::clone(clock);
                                clock_ref.get_name().to_string()
                            }
                            NativeFunctions::InputFunction(input) => {
                                let input_ref = Rc::clone(input);
                                input_ref.get_name().to_string()
                            }
                        }
                    }
                };
                name // Now `name` is a `String`, so it owns the data
            },
        };
        write!(f, "{}", text)
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