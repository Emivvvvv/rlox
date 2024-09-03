use std::any::Any;
use rustc_hash::FxHashMap;
use std::rc::Rc;

use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_callable::lox_instance::LoxInstance;
use crate::lox_value::{LoxCallable, LoxValue};

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    display_name: String,
    name: String,
    superclass: Option<Box<LoxClass>>,
    methods: FxHashMap<String, LoxValue>,
}

impl LoxClass {
    pub fn new(
        name: String,
        superclass: Option<Box<LoxClass>>,
        methods: FxHashMap<String, LoxValue>,
    ) -> Self {
        Self {
            display_name: format!("class {}", name),
            name,
            superclass,
            methods,
        }
    }
}

impl LoxClass {
    fn arity(&self) -> usize {
        let initializer_option = self.find_method(&"init".to_string());
        if let Some(initializer_value) = initializer_option {
            if let LoxValue::Callable(callable) = initializer_value {
                if let LoxCallable::Function(init_func) = callable {
                    return init_func.arity();
                }
            }
        }

        0
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        let instance = Rc::new(LoxInstance::new(self));

        let initializer_option = self.find_method(&"init".to_string());
        if let Some(initializer_value) = initializer_option {
            if let LoxValue::Callable(callable) = initializer_value {
                if let LoxCallable::Function(init_func) = callable {
                    init_func.bind(Rc::clone(&instance)).call(interpreter, arguments)?;
                }
            }
        }

        Ok(LoxValue::Callable(LoxCallable::Instance(instance)))
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

impl LoxClass {
    pub fn get_raw_name(&self) -> &str {
        &self.name
    }

    pub fn find_method(&self, name: &String) -> Option<&LoxValue> {
        self.methods.get(name).or_else(|| {
            self.superclass.as_ref().and_then(|superclass| superclass.find_method(name))
        })
    }
}