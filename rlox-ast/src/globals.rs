use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_value::{LoxCallable, LoxValue, NativeFunctions};

pub fn define_globals(global_environment: &Rc<RefCell<Environment>>, symbol_table: &mut SymbolTable) {
    global_environment.borrow_mut().define(
        symbol_table.intern("clock"),
        LoxValue::Callable(LoxCallable::NativeFunction(NativeFunctions::ClockFunction(Rc::new(ClockFunction)))),
    );

    global_environment.borrow_mut().define(
        symbol_table.intern("input"),
        LoxValue::Callable(LoxCallable::NativeFunction(NativeFunctions::InputFunction(Rc::new(InputFunction)))),
    );
}

use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Clone, Debug, PartialEq)]
pub struct ClockFunction;

impl Callable for ClockFunction {
    fn arity(&self, _symbol_table: &mut SymbolTable) -> usize {
        0
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        let start = SystemTime::now();
        let since_the_epoch = start
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards");
        Ok(LoxValue::Number(since_the_epoch.as_secs_f64()))
    }

    fn get_name(&self) -> String {
        "<native fn clock>".to_string()
    }
}

use std::io;
use std::io::Write;
use crate::lox_callable::callable::Callable;
use crate::symbol::SymbolTable;

#[derive(Clone, Debug, PartialEq)]
pub struct InputFunction;

impl Callable for InputFunction {
    fn arity(&self, _symbol_table: &mut SymbolTable) -> usize {
        1
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        if let LoxValue::String(prompt) = &arguments[0] {
            print!("{}", prompt);
        } else {
            return Err(RuntimeError::InputError(
                "Argument to input must be a string.".to_string(),
            ));
        }

        io::stdout().flush().unwrap(); // Flush stdout to ensure the prompt is shown
        let mut input = String::new();
        io::stdin().read_line(&mut input).map_err(|e| {
            RuntimeError::InputError(format!("Failed to read input: {}", e))
        })?;
        Ok(LoxValue::String(input.trim().to_string())) // Trim the input and return as LoxValue::String
    }

    fn get_name(&self) -> String {"<native fn input>".to_string()}
}