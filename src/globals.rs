use std::any::Any;
use crate::environment::Environment;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_value::LoxValue;
use crate::lox_callable::LoxCallable;
use std::cell::RefCell;
use std::rc::Rc;

pub fn define_globals(global_environment: &Rc<RefCell<Environment>>) {
    global_environment.borrow_mut().define(
        "clock".to_string(),
        LoxValue::Callable(Rc::new(RefCell::new(ClockFunction))),
    );

    global_environment.borrow_mut().define(
        "input".to_string(),
        LoxValue::Callable(Rc::new(RefCell::new(InputFunction))),
    );
}

use std::time::{SystemTime, UNIX_EPOCH};

pub struct ClockFunction;

impl LoxCallable for ClockFunction {
    fn arity(&self) -> usize {
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

    fn get_name(&self) -> &str {
        "<native fn clock>"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

use std::io;
use std::io::Write;

pub struct InputFunction;

impl LoxCallable for InputFunction {
    fn arity(&self) -> usize {
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

    fn get_name(&self) -> &str {
        "<native fn input>"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}