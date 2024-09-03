use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_value::{LoxCallable, LoxValue};
use crate::lexer::token::Token;
use crate::lox_callable::lox_instance::LoxInstance;
use crate::stmt::Stmt;

pub struct LoxFunction {
    display_name: String,
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
    is_initializer: bool
}

impl LoxFunction {
    pub fn new(
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
        closure: Rc<RefCell<Environment>>,
        is_initializer: bool
    ) -> Self {
        Self {
            display_name: format!("fn {}", name.lexeme),
            name,
            params,
            body,
            closure,
            is_initializer
        }
    }
}

impl LoxFunction {

    pub(crate) fn arity(&self) -> usize {
        self.params.len()
    }

    pub(crate) fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        let environment = Environment::with_enclosing(Rc::clone(&self.closure));

        // Bind parameters
        for (param, argument) in self.params.iter().zip(arguments.iter()) {
            environment.borrow_mut().define(param.lexeme.clone(), argument.clone());
        }

        // Execute the function body
        match interpreter.evaluate_block_stmt(&self.body, Some(environment)) {
            Err(RuntimeError::Return(value)) => self.handle_return(value),
            Err(err) => Err(err), // Propagate other errors
            Ok(_) => self.handle_return(LoxValue::Nil),
        }
    }


    pub(crate) fn get_name(&self) -> &str {
        &self.display_name
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl LoxFunction {
    fn handle_return(&self, value: LoxValue) -> Result<LoxValue, RuntimeError> {
        if self.is_initializer {
            Environment::get_at(Rc::clone(&self.closure), 0, &"this".to_string())
                .map_err(|_| {
                    RuntimeError::CustomError("initializer function couldn't find `this`".to_string())
                })
        } else {
            Ok(value)
        }
    }

    pub fn get_raw_name(&self) -> &str {
        &self.name.lexeme
    }

    pub fn bind(&self, rc_instance: Rc<LoxInstance>) -> LoxFunction {
        let environment = Environment::with_enclosing(Rc::clone(&self.closure));
        environment.borrow_mut().define("this".to_string(), LoxValue::Callable(LoxCallable::Instance(rc_instance)));

        LoxFunction {
            display_name: self.display_name.clone(),
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
            closure: environment,
            is_initializer: self.is_initializer,
        }
    }
}