use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_callable::LoxCallable;
use crate::lox_function::LoxFunction;
use crate::lox_instance::LoxInstance;
use crate::lox_value::LoxValue;

#[derive(Clone, Debug)]
pub struct LoxClass {
    display_name: String,
    name: String,
    methods: HashMap<String, LoxValue>,
}

impl LoxClass {
    pub fn new(
        name: String,
        methods: HashMap<String, LoxValue>,
    ) -> Self {
        Self {
            display_name: "class ".to_string() + &name,
            name,
            methods,
        }
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> usize {
        let initializer_option = self.find_method(&"init".to_string());
        if let Some(initializer) = initializer_option {
            initializer.extract_callable().unwrap().borrow().arity()
        } else {
            0
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        let instance = Rc::new(RefCell::new(LoxInstance::new(self)));
        let initializer_option = self.find_method(&"init".to_string());
        if let Some(initializer_value) = initializer_option {
            if let Some(initializer) = initializer_value.extract_callable().unwrap().borrow().as_any().downcast_ref::<LoxFunction>() {
                initializer.bind(Rc::clone(&instance)).call(interpreter, arguments)?;
            }
        }
        Ok(LoxValue::Callable(instance))
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

impl LoxClass {
    pub(crate) fn get_raw_name(&self) -> &str {
        &self.name
    }

    pub fn find_method(&self, name: &String) -> Option<&LoxValue> {
        self.methods.get(name)
    }
}