use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::interpreter::{Interpreter, LoxValue, RuntimeError};
use crate::lexer::token::Token;
use crate::lox_callable::LoxCallable;
use crate::stmt::Stmt;

pub struct LoxFunction {
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
            name,
            params,
            body,
            closure,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        let mut environment = Environment::with_enclosing(Rc::clone(&self.closure));

        for (param, argument) in self.params.iter().zip(arguments.iter()) {
            environment.define(param.lexeme.clone(), argument.clone());
        }

        interpreter.evaluate_block_stmt(&self.body, Some(Rc::new(RefCell::new(environment))))
    }

    fn arity(&self) -> usize {
        self.params.len()
    }

    fn get_name(&self) -> &str {
        &self.name.lexeme
    }
}
