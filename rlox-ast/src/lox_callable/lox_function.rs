use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_value::{LoxCallable, LoxValue};
use crate::lexer::token::Token;
use crate::lox_callable::callable::Callable;
use crate::lox_callable::lox_instance::LoxInstance;
use crate::stmt::Stmt;
use crate::symbol::SymbolTable;

#[derive(Clone, Debug, PartialEq)]
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

impl Callable for LoxFunction {

    fn arity(&self, _symbol_table: &mut SymbolTable) -> usize {
        self.params.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        let environment = Environment::with_enclosing(Rc::clone(&self.closure));

        // Bind parameters
        for (param, argument) in self.params.iter().zip(arguments.iter()) {
            environment.borrow_mut().define(param.lexeme, argument.clone());
        }

        // Execute the function body
        match interpreter.evaluate_block_stmt(&self.body, Some(environment)) {
            Err(RuntimeError::Return(value)) => self.handle_return(value, interpreter.symbol_table),
            Err(err) => Err(err), // Propagate other errors
            Ok(_) => self.handle_return(LoxValue::Nil, interpreter.symbol_table),
        }
    }


    fn get_name(&self) -> String {
        self.display_name.clone()
    }
}

impl LoxFunction {
    fn handle_return(&self, value: LoxValue, symbol_table: &mut SymbolTable) -> Result<LoxValue, RuntimeError> {
        if self.is_initializer {
            Environment::get_at(Rc::clone(&self.closure), 0, &symbol_table.intern("this"), symbol_table)
                .map_err(|_| {
                    RuntimeError::CustomError("initializer function couldn't find `this`".to_string())
                })
        } else {
            Ok(value)
        }
    }

    pub fn bind(&self, rc_instance: &Rc<RefCell<LoxInstance>>, symbol_table: &mut SymbolTable) -> LoxFunction {
        let environment = Environment::with_enclosing(Rc::clone(&self.closure));
        environment.borrow_mut().define(symbol_table.intern("this"), LoxValue::Callable(LoxCallable::Instance(Rc::clone(rc_instance))));

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