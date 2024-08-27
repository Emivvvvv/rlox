use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::interpreter::{Interpreter, LoxValue, RuntimeError};
use crate::lox_callable::LoxCallable;

pub fn define_globals(global: &Rc<RefCell<Environment>>) {
    global.borrow_mut().define(
        "clock".to_string(),
        LoxValue::Callable(Rc::new(ClockFunction)),
    );

    global.borrow_mut().define(
        "input".to_string(),
        LoxValue::Callable(Rc::new(InputFunction)),
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
}

use std::io::{self};

pub struct InputFunction;

impl LoxCallable for InputFunction {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = input.trim().to_string();
                Ok(LoxValue::String(input))
            }
            Err(error) => Err(RuntimeError::Input(format!(
                "Failed to read input: {}",
                error
            ))),
        }
    }

    fn get_name(&self) -> &str {
        "<native fn input>"
    }
}
