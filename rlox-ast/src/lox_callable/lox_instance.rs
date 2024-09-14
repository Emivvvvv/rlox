use std::cell::RefCell;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_value::{LoxCallable, LoxValue};
use crate::lexer::token::Token;
use crate::lox_callable::callable::Callable;
use crate::lox_callable::lox_class::LoxClass;
use crate::symbol::{Symbol, SymbolTable};

#[derive(Clone, Debug, PartialEq)]
pub struct LoxInstance {
    display_name: String,
    klass: LoxClass,
    fields: FxHashMap<Symbol, LoxValue>,
}

impl LoxInstance {
    pub fn new(
        klass: &LoxClass,
    ) -> Self {
        Self {
            display_name: format!("{} instance", klass.get_raw_name()),
            klass: klass.clone(),
            fields: FxHashMap::default(),
        }
    }

    pub fn get(instance: Rc<RefCell<LoxInstance>>, name: &Token, symbol_table: &mut SymbolTable) -> Result<LoxValue, RuntimeError> {
        if let Some(lox_value) = instance.borrow().fields.get(&name.lexeme) {
            return Ok(lox_value.clone());
        }

        if let Some(LoxCallable::Function(method)) = instance.borrow().klass.find_method(&name.lexeme) {
            return Ok(LoxValue::Callable(LoxCallable::Function(Rc::new(method.bind(&instance, symbol_table)))));
        }

        Err(RuntimeError::InstanceError(
            name.clone(),
            format!("Undefined property '{}'.", name.lexeme),
        ))
    }

    pub fn set(&mut self, name: Token, value: LoxValue) {
        self.fields.insert(name.lexeme, value);
    }
}

impl Callable for LoxInstance {
    fn arity(&self, _symbol_table: &mut SymbolTable) -> usize {
        0
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        todo!()
    }

    fn get_name(&self) -> String {
        self.display_name.to_string()
    }

}