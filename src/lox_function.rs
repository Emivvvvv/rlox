use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_value::LoxValue;
use crate::lexer::token::Token;
use crate::lox_callable::LoxCallable;
use crate::lox_instance::LoxInstance;
use crate::stmt::Stmt;

pub struct LoxFunction {
    display_name: String,
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    pub fn new(
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
        closure: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            display_name: "fn ".to_string() + &name.lexeme,
            name,
            params,
            body,
            closure,
        }
    }
}

impl LoxCallable for LoxFunction {

    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        let environment = Environment::with_enclosing(Rc::clone(&self.closure));

        for (param, argument) in self.params.iter().zip(arguments.iter()) {
            environment.borrow_mut().define(param.lexeme.clone(), argument.clone());
        }

        interpreter.evaluate_block_stmt(&self.body, Some(environment))
    }

    fn get_name(&self) -> &str {
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
    pub fn get_raw_name(&self) -> &str {
        &self.name.lexeme
    }

    pub fn bind(&self, rc_refcell_instance: Rc<RefCell<LoxInstance>>) -> LoxFunction {
        let environment = Environment::with_enclosing(Rc::clone(&self.closure));
        environment.borrow_mut().define("this".to_string(), LoxValue::Callable(rc_refcell_instance.clone()));

        LoxFunction {
            display_name: self.display_name.clone(),
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
            closure: environment,
        }
    }
}