use core::fmt;
use std::any::Any;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_value::LoxValue;

pub trait LoxCallable {
    fn arity(&self) -> usize;
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError>;
    fn get_name(&self) -> &str;

    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl fmt::Display for dyn LoxCallable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>", self.get_name())
    }
}