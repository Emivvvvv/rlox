use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;

use crate::lox_value::LoxValue;
use crate::symbol::{Symbol, SymbolTable};

#[derive(Debug, PartialEq)]
pub enum EnvironmentError {
    UndefinedVariable(String),
    AssignVariableError(String),
    CustomError(String),
}

impl EnvironmentError {
    pub fn get_string(self) -> String {
        match self {
            EnvironmentError::UndefinedVariable(err_str)
            | EnvironmentError::AssignVariableError(err_str)
            | EnvironmentError::CustomError(err_str) => err_str,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    values: FxHashMap<Symbol, LoxValue>,
    pub(crate) enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            values: FxHashMap::default(),
            enclosing: None,
        }))
    }

    pub fn with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            values: FxHashMap::default(),
            enclosing: Some(enclosing),
        }))
    }

    pub fn define(&mut self, name: Symbol, value: LoxValue) -> Option<LoxValue> {
        self.values.insert(name, value)
    }

    pub fn get(&self, name: Symbol, symbol_table: &SymbolTable) -> Result<LoxValue, EnvironmentError> {
        match self.values.get(&name) {
            Some(value) => Ok(value.clone()),
            None => {
                if let Some(enclosing) = &self.enclosing {
                    enclosing.borrow().get(name, symbol_table)
                } else {
                    Err(EnvironmentError::UndefinedVariable(format!(
                        "Undefined variable '{}'.",
                        symbol_table.resolve(name)
                    )))
                }
            }
        }
    }

    pub fn assign(&mut self, name: Symbol, value: LoxValue, symbol_table: &SymbolTable) -> Result<LoxValue, EnvironmentError> {
        if self.values.contains_key(&name) {
            self.values.insert(name, value);
            Ok(LoxValue::Boolean(true))
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value, symbol_table)
        } else {
            Err(EnvironmentError::UndefinedVariable(format!(
                "Undefined variable '{}'.",
                symbol_table.resolve(name)
            )))
        }
    }

    pub fn get_at(env: Rc<RefCell<Environment>>, distance: usize, name: &Symbol, symbol_table: &SymbolTable) -> Result<LoxValue, EnvironmentError> {
        Environment::ancestor(env, distance)
            .borrow()
            .values
            .get(name)
            .cloned()
            .ok_or_else(|| EnvironmentError::UndefinedVariable(format!(
                "Undefined variable '{}' at distance '{}'.", name.display_with_table(&symbol_table), distance
            )))
    }

    pub fn assign_at(env: Rc<RefCell<Environment>>, distance: usize, name: Symbol, value: LoxValue, symbol_table: &SymbolTable) -> Result<LoxValue, EnvironmentError> {
        let binding = Environment::ancestor(env, distance);
        let mut ancestor_env = binding.borrow_mut();

        ancestor_env.values.insert(name, value)
            .ok_or_else(|| EnvironmentError::AssignVariableError(format!(
                "Couldn't assign variable '{}' at distance '{}'.", name.display_with_table(&symbol_table), distance
            )))
    }


    pub fn ancestor(env: Rc<RefCell<Environment>>, distance: usize) -> Rc<RefCell<Environment>> {
        let mut environment = env;
        for _ in 0..distance {
            let next_env = Rc::clone(environment.borrow().enclosing.as_ref()
                .expect("Ancestor not found."));
            environment = next_env;
        }
        environment
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::SymbolTable;

    // Helper function to intern a string in the symbol table
    fn intern_symbol(table: &mut SymbolTable, name: &str) -> Symbol {
        table.intern(name)
    }

    #[test]
    fn test_define_and_get() {
        let mut symbol_table = SymbolTable::new();
        let env = Environment::new();
        let symbol = intern_symbol(&mut symbol_table, "x");
        let value = LoxValue::Number(10.0);
        env.borrow_mut().define(symbol, value.clone());

        assert_eq!(env.borrow().get(symbol, &symbol_table).unwrap(), value);
    }

    #[test]
    fn test_get_undefined_variable() {
        let mut symbol_table = SymbolTable::new();
        let env = Environment::new();
        let symbol = intern_symbol(&mut symbol_table, "y");

        let result = env.borrow().get(symbol, &symbol_table);
        assert!(matches!(result, Err(EnvironmentError::UndefinedVariable(err)) if err == "Undefined variable 'y'."));
    }

    #[test]
    fn test_assign_existing_variable() {
        let mut symbol_table = SymbolTable::new();
        let env = Environment::new();
        let symbol = intern_symbol(&mut symbol_table, "z");
        env.borrow_mut().define(symbol, LoxValue::Number(10.0));

        let new_value = LoxValue::Number(20.0);
        env.borrow_mut().assign(symbol, new_value.clone(), &symbol_table).unwrap();
        assert_eq!(env.borrow().get(symbol, &symbol_table).unwrap(), new_value);
    }

    #[test]
    fn test_enclosing_environment() {
        let mut symbol_table = SymbolTable::new();
        let outer = Environment::new();
        let symbol = intern_symbol(&mut symbol_table, "var");
        let value = LoxValue::Number(100.0);
        outer.borrow_mut().define(symbol, value.clone());

        let inner = Environment::with_enclosing(Rc::clone(&outer));
        assert_eq!(inner.borrow().get(symbol, &symbol_table).unwrap(), value);
    }

    #[test]
    fn test_shadowing_by_inner_environment() {
        let mut symbol_table = SymbolTable::new();
        let outer = Environment::new();
        let symbol = intern_symbol(&mut symbol_table, "var");
        outer.borrow_mut().define(symbol, LoxValue::Number(100.0));

        let inner = Environment::with_enclosing(Rc::clone(&outer));
        let inner_value = LoxValue::Number(200.0);
        inner.borrow_mut().define(symbol, inner_value.clone());

        assert_eq!(inner.borrow().get(symbol, &symbol_table).unwrap(), inner_value);
    }

    #[test]
    fn test_assign_undefined_variable() {
        let mut symbol_table = SymbolTable::new();
        let env = Environment::new();

        let symbol = intern_symbol(&mut symbol_table, "w");
        let value = LoxValue::Number(31.0);

        let result = env.borrow_mut().assign(symbol, value, &symbol_table);

        assert!(
            matches!(result, Err(EnvironmentError::UndefinedVariable(ref err)) if err == "Undefined variable 'w'."),
            "Expected UndefinedVariable error, but got: {:?}",
            result
        );
    }

    #[test]
    fn test_ancestor_retrieval() {
        let mut symbol_table = SymbolTable::new();
        let global_env = Environment::new();
        let first_child_env = Environment::with_enclosing(Rc::clone(&global_env));
        let second_child_env = Environment::with_enclosing(Rc::clone(&first_child_env));

        // Define variables at different levels
        let symbol = intern_symbol(&mut symbol_table, "a");
        global_env.borrow_mut().define(symbol, LoxValue::String("global".to_string()));
        first_child_env.borrow_mut().define(symbol, LoxValue::String("first_child".to_string()));
        second_child_env.borrow_mut().define(symbol, LoxValue::String("second_child".to_string()));

        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 0)
                .borrow()
                .get(symbol, &symbol_table),
            Ok(LoxValue::String("second_child".to_string()))
        );

        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 1)
                .borrow()
                .get(symbol, &symbol_table),
            Ok(LoxValue::String("first_child".to_string()))
        );

        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 2)
                .borrow()
                .get(symbol, &symbol_table),
            Ok(LoxValue::String("global".to_string()))
        );
    }

    #[test]
    fn test_assign_existing_global_variable() {
        let mut symbol_table = SymbolTable::new();
        let global_env = Environment::new();
        let child_env = Environment::with_enclosing(Rc::clone(&global_env));

        // Define a variable in the global environment
        let symbol = intern_symbol(&mut symbol_table, "global_var");
        global_env.borrow_mut().define(symbol, LoxValue::String("initial".to_string()));

        // Attempt to assign to the global variable from the child environment
        let new_value = LoxValue::String("updated".to_string());
        let result = child_env.borrow_mut().assign(symbol, new_value.clone(), &symbol_table);

        // Ensure assignment succeeded and the global variable was updated
        assert!(result.is_ok());
        assert_eq!(child_env.borrow().get(symbol, &symbol_table).unwrap(), new_value);
        assert_eq!(global_env.borrow().get(symbol, &symbol_table).unwrap(), new_value);
    }

    #[test]
    fn test_assign_nonexistent_variable_should_error() {
        let mut symbol_table = SymbolTable::new();
        let global_env = Environment::new();
        let child_env = Environment::with_enclosing(Rc::clone(&global_env));

        // Attempt to assign to a variable that doesn't exist
        let symbol = intern_symbol(&mut symbol_table, "undefined_var");
        let value = LoxValue::String("value".to_string());
        let result = child_env.borrow_mut().assign(symbol, value, &symbol_table);

        // Ensure that the assignment failed with the correct error
        assert!(
            matches!(result, Err(EnvironmentError::UndefinedVariable(err)) if err == "Undefined variable 'undefined_var'.")
        );
    }

    #[test]
    fn test_ancestor_at_various_distances() {
        let mut symbol_table = SymbolTable::new();
        let global_env = Environment::new();
        let first_child_env = Environment::with_enclosing(Rc::clone(&global_env));
        let second_child_env = Environment::with_enclosing(Rc::clone(&first_child_env));

        // Define variables in different levels
        let global_symbol = intern_symbol(&mut symbol_table, "var_global");
        global_env.borrow_mut().define(global_symbol, LoxValue::String("global".to_string()));
        let first_child_symbol = intern_symbol(&mut symbol_table, "var_first_child");
        first_child_env.borrow_mut().define(first_child_symbol, LoxValue::String("first_child".to_string()));
        let second_child_symbol = intern_symbol(&mut symbol_table, "var_second_child");
        second_child_env.borrow_mut().define(second_child_symbol, LoxValue::String("second_child".to_string()));

        // Test ancestor at distance 0 (current environment)
        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 0)
                .borrow()
                .get(second_child_symbol, &symbol_table),
            Ok(LoxValue::String("second_child".to_string()))
        );

        // Test ancestor at distance 1 (first child environment)
        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 1)
                .borrow()
                .get(first_child_symbol, &symbol_table),
            Ok(LoxValue::String("first_child".to_string()))
        );

        // Test ancestor at distance 2 (global environment)
        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 2)
                .borrow()
                .get(global_symbol, &symbol_table),
            Ok(LoxValue::String("global".to_string()))
        );
    }

    #[test]
    fn test_assign_at_non_existent_variable_should_error() {
        let mut symbol_table = SymbolTable::new();
        let global_env = Environment::new();
        let first_child_env = Environment::with_enclosing(Rc::clone(&global_env));
        let second_child_env = Environment::with_enclosing(Rc::clone(&first_child_env));

        let symbol = intern_symbol(&mut symbol_table, "z");
        let value = LoxValue::Number(30.0);

        // Attempt to assign to 'z' at a distance of 1, which doesn't exist
        let result = Environment::assign_at(Rc::clone(&second_child_env), 1, symbol, value, &symbol_table);

        // Ensure that an error is returned, indicating the variable could not be assigned
        let expected_error = Err(EnvironmentError::AssignVariableError("Couldn't assign variable 'z' at distance '1'.".to_string()));
        assert_eq!(result, expected_error)
    }

    #[test]
    fn test_assign_at_correct_distance() {
        let mut symbol_table = SymbolTable::new();
        let global_env = Environment::new();
        let symbol = intern_symbol(&mut symbol_table, "x");
        global_env.borrow_mut().define(symbol, LoxValue::Number(10.0));

        let first_child_env = Environment::with_enclosing(Rc::clone(&global_env));
        let second_child_env = Environment::with_enclosing(Rc::clone(&first_child_env));

        // Assign to 'x' at a distance of 2 (global scope)
        let result = Environment::assign_at(Rc::clone(&second_child_env), 2, symbol, LoxValue::Number(30.0), &symbol_table);

        // Ensure the assignment was successful
        assert_eq!(result, Ok(LoxValue::Number(10.0))); // This should match the old value.

        // Check that the value in the global environment was updated
        assert_eq!(global_env.borrow().get(symbol, &symbol_table), Ok(LoxValue::Number(30.0)));
    }
}
