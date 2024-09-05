use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use crate::interner::Symbol;
use crate::interpreter::{Interpreter, RuntimeError};
use crate::lox_callable::callable::Callable;
use crate::lox_callable::lox_instance::LoxInstance;
use crate::lox_value::{LoxCallable, LoxValue};

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    display_name: String,
    name: String,
    superclass: Option<Rc<LoxClass>>,
    methods: FxHashMap<Symbol, LoxCallable>,
}

impl LoxClass {
    pub fn new(
        name: String,
        superclass: Option<Rc<LoxClass>>,
        methods: FxHashMap<Symbol, LoxCallable>,
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
        if let Some(LoxCallable::Function(initializer_function)) = initializer_option {
            initializer_function.arity();
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
        if let Some(LoxCallable::Function(initializer_function)) = initializer_option {
            initializer_function.bind(&instance).call(interpreter, arguments)?;
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

    pub fn find_method(&self, name: &Symbol) -> Option<&LoxCallable> {
        self.methods.get(name).or_else(|| {
            self.superclass.as_ref().and_then(|superclass| superclass.find_method(name))
        })
    }
}