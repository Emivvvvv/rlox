use core::fmt;

use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_value::LoxValue;
use crate::symbol::SymbolTable;

pub trait Callable {
    fn arity(&self, symbol_table: &mut SymbolTable) -> usize;
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError>;
    fn get_name(&self) -> String;
}

impl fmt::Display for dyn Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>", self.get_name())
    }
}