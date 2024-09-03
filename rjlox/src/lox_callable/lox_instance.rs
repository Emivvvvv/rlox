use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_value::{LoxCallable, LoxValue};
use crate::lexer::token::Token;
use crate::lox_callable::lox_class::LoxClass;

#[derive(Clone, Debug)]
pub struct LoxInstance {
    display_name: String,
    klass: LoxClass,
    fields: FxHashMap<String, LoxValue>,
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

    pub fn get(instance: Rc<RefCell<LoxInstance>>, name: &Token) -> Result<LoxValue, RuntimeError> {
        let instance_borrowed = instance.borrow();

        if let Some(lox_value) = instance_borrowed.fields.get(&name.lexeme) {
            return Ok(lox_value.clone());
        }

        if let Some(method_lox_value) = instance_borrowed.klass.find_method(&name.lexeme) {
            if let LoxValue::Callable(callable) = method_lox_value {
                if let LoxCallable::Function(method) = callable {
                    return Ok(LoxValue::Callable(LoxCallable::Function(Rc::clone(method))));
                }
            }
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

impl LoxInstance {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        todo!()
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