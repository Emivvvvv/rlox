use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_value::LoxValue;
use crate::lexer::token::Token;
use crate::lox_callable::lox_callable::LoxCallable;
use crate::lox_callable::lox_class::LoxClass;
use crate::lox_callable::lox_function::LoxFunction;

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

        if let Some(method) = instance_borrowed.klass.find_method(&name.lexeme) {
            if let Some(method) = method.extract_callable().unwrap().borrow().as_any().downcast_ref::<LoxFunction>() {
                return Ok(LoxValue::Callable(Rc::new(RefCell::new(method.bind(instance.clone())))));
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

impl LoxCallable for LoxInstance {
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