use crate::environment::Environment;
use crate::interpreter::{Interpreter, LoxValue, RuntimeError};
use crate::lox_callable::LoxCallable;
use std::cell::RefCell;
use std::rc::Rc;

pub fn define_globals(environment: &Rc<RefCell<Environment>>) {
    environment.borrow_mut().define(
        "clock".to_string(),
        LoxValue::Callable(Rc::new(ClockFunction)),
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
