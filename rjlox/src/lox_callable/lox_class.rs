use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;

use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_callable::callable::Callable;
use crate::lox_callable::lox_instance::LoxInstance;
use crate::lox_value::{LoxCallable, LoxValue};

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    display_name: String,
    name: String,
    superclass: Option<Rc<LoxClass>>,
    methods: FxHashMap<String, LoxCallable>,
}

impl LoxClass {
    pub fn new(
        name: String,
        superclass: Option<Rc<LoxClass>>,
        methods: FxHashMap<String, LoxCallable>,
    ) -> Self {
        Self {
            display_name: format!("class {}", name),
            name,
            superclass,
            methods,
        }
    }
}

impl Callable for LoxClass {
    fn arity(&self) -> usize {
        let initializer_option = self.find_method(&"init".to_string());
        if let Some(callable) = initializer_option {
            if let LoxCallable::Function(initializer_function) = callable {
                return initializer_function.arity();
            }
        }

        0
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, RuntimeError> {
        let instance = Rc::new(RefCell::new(LoxInstance::new(self)));

        let initializer_option = self.find_method(&"init".to_string());
        if let Some(callable) = initializer_option {
            if let LoxCallable::Function(initializer_function) = callable {
                initializer_function.bind(Rc::clone(&instance)).call(interpreter, arguments)?;
            }
        }

        Ok(LoxValue::Callable(LoxCallable::Instance(instance)))
    }

    fn get_name(&self) -> String {
        self.display_name.clone()
    }
}

impl LoxClass {
    pub fn get_raw_name(&self) -> &str {
        &self.name
    }

    pub fn find_method(&self, name: &String) -> Option<&LoxCallable> {
        self.methods.get(name).or_else(|| {
            self.superclass.as_ref().and_then(|superclass| superclass.find_method(name))
        })
    }
}