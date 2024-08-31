use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;

use crate::lox_value::LoxValue;
use crate::lexer::token::Token;

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

#[derive(Debug)]
pub struct Environment {
    values: FxHashMap<String, LoxValue>,
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

    pub fn define(&mut self, name: String, value: LoxValue) -> Option<LoxValue> {
        self.values.insert(name, value)
    }

    pub fn get(&self, name: &Token) -> Result<LoxValue, EnvironmentError> {
        match self.values.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => {
                if let Some(enclosing) = &self.enclosing {
                    enclosing.borrow().get(name)
                } else {
                    Err(EnvironmentError::UndefinedVariable(format!(
                        "Undefined variable '{}'.",
                        name.lexeme
                    )))
                }
            }
        }
    }

    pub fn assign(&mut self, name: &Token, value: LoxValue) -> Result<LoxValue, EnvironmentError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value.clone());
            Ok(value)
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(EnvironmentError::UndefinedVariable(format!(
                "Undefined variable '{}'.",
                name.lexeme
            )))
        }
    }

    pub fn get_at(env: Rc<RefCell<Environment>>, distance: usize, name: &String) -> Result<LoxValue, EnvironmentError> {
        Environment::ancestor(env, distance)
            .borrow()
            .values
            .get(name)
            .cloned()
            .ok_or_else(|| EnvironmentError::UndefinedVariable(format!(
                "Undefined variable '{}' at distance '{}'.", name, distance
            )))
    }

    pub fn assign_at(env: Rc<RefCell<Environment>>, distance: usize, name: &Token, value: LoxValue) -> Result<LoxValue, EnvironmentError> {
        let binding = Environment::ancestor(env, distance);
        let mut ancestor_env = binding.borrow_mut();

        ancestor_env.values.insert(name.lexeme.clone(), value.clone())
            .ok_or_else(|| EnvironmentError::AssignVariableError(format!(
                "Couldn't assign variable '{}' at distance '{}'.", name.lexeme, distance
            )))
    }


    pub fn ancestor(env: Rc<RefCell<Environment>>, distance: usize) -> Rc<RefCell<Environment>> {
        let mut environment = env;
        for _ in 0..distance {
            let next_env = environment.borrow().enclosing.as_ref()
                .expect("Ancestor not found.")
                .clone();
            environment = next_env;
        }
        environment
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::{Literal, TokenType};

    #[test]
    fn test_define_and_get() {
        let env = Environment::new();
        let token = Token::new(
            TokenType::Identifier,
            "x".to_string(),
            Literal::Num(10.0.into()),
            1,
        );
        let value = LoxValue::Number(10.0);
        env.borrow_mut().define("x".to_string(), value.clone());

        assert_eq!(env.borrow().get(&token).unwrap(), value);
    }

    #[test]
    fn test_get_undefined_variable() {
        let env = Environment::new();
        let token = Token::new(TokenType::Identifier, "y".to_string(), Literal::Nil, 1);

        assert!(
            matches!(env.borrow().get(&token), Err(EnvironmentError::UndefinedVariable(err)) if err == "Undefined variable 'y'.")
        );
    }

    #[test]
    fn test_assign_existing_variable() {
        let env = Environment::new();
        let token = Token::new(
            TokenType::Identifier,
            "z".to_string(),
            Literal::Num(10.0.into()),
            1,
        );
        env.borrow_mut().define("z".to_string(), LoxValue::Number(10.0));

        let new_value = LoxValue::Number(20.0);
        env.borrow_mut().assign(&token, new_value.clone()).unwrap();
        assert_eq!(env.borrow().get(&token).unwrap(), new_value);
    }

    #[test]
    fn test_enclosing_environment() {
        let outer = Environment::new();
        let token = Token::new(
            TokenType::Identifier,
            "var".to_string(),
            Literal::Num(100.0.into()),
            1,
        );
        let value = LoxValue::Number(100.0);
        outer.borrow_mut().define("var".to_string(), value.clone());

        let inner = Environment::with_enclosing(Rc::clone(&outer));
        assert_eq!(inner.borrow().get(&token).unwrap(), value);
    }

    #[test]
    fn test_shadowing_by_inner_environment() {
        let outer = Environment::new();
        let token = Token::new(
            TokenType::Identifier,
            "var".to_string(),
            Literal::Num(100.0.into()),
            1,
        );
        outer.borrow_mut().define("var".to_string(), LoxValue::Number(100.0));

        let inner = Environment::with_enclosing(Rc::clone(&outer));
        let inner_value = LoxValue::Number(200.0);
        inner.borrow_mut().define("var".to_string(), inner_value.clone());

        assert_eq!(inner.borrow().get(&token).unwrap(), inner_value);
    }

    #[test]
    fn test_assign_in_nested_environment() {
        let outer = Environment::new();
        outer.borrow_mut().define("x".to_string(), LoxValue::Number(10.0));

        let inner = Environment::with_enclosing(Rc::clone(&outer));
        assert_eq!(
            inner
                .borrow_mut()
                .assign(
                    &Token::new(TokenType::Identifier, "x".to_string(), Literal::Nil, 1),
                    LoxValue::Number(20.0)
                )
                .unwrap(),
            LoxValue::Number(20.0)
        );
        assert_eq!(
            inner.borrow().get(&Token::new(
                TokenType::Identifier,
                "x".to_string(),
                Literal::Nil,
                1
            ))
                .unwrap(),
            LoxValue::Number(20.0)
        );
    }

    #[test]
    fn test_assign_undefined_variable() {
        let env = Environment::new();

        let token = Token::new(TokenType::Identifier, "w".to_string(), Literal::Nil, 1);

        let value = LoxValue::Number(31.0);

        let result = env.borrow_mut().assign(&token, value);

        assert!(
            matches!(result, Err(EnvironmentError::UndefinedVariable(ref err)) if err == "Undefined variable 'w'."),
            "Expected UndefinedVariable error, but got: {:?}",
            result
        );
    }

    #[test]
    fn test_ancestor_retrieval() {
        let global_env = Environment::new();
        let first_child_env = Environment::with_enclosing(Rc::clone(&global_env));
        let second_child_env = Environment::with_enclosing(Rc::clone(&first_child_env));

        // Define variables at different levels
        global_env
            .borrow_mut()
            .define("a".to_string(), LoxValue::String("global".to_string()));
        first_child_env
            .borrow_mut()
            .define("a".to_string(), LoxValue::String("first_child".to_string()));
        second_child_env.borrow_mut().define(
            "a".to_string(),
            LoxValue::String("second_child".to_string()),
        );

        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 0)
                .borrow()
                .get(&Token::new(
                    TokenType::Identifier,
                    "a".to_string(),
                    Literal::Nil,
                    1
                )),
            Ok(LoxValue::String("second_child".to_string()))
        );

        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 1)
                .borrow()
                .get(&Token::new(
                    TokenType::Identifier,
                    "a".to_string(),
                    Literal::Nil,
                    1
                )),
            Ok(LoxValue::String("first_child".to_string()))
        );

        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 2)
                .borrow()
                .get(&Token::new(
                    TokenType::Identifier,
                    "a".to_string(),
                    Literal::Nil,
                    1
                )),
            Ok(LoxValue::String("global".to_string()))
        );
    }

    #[test]
    fn test_assign_existing_global_variable() {
        let global_env = Environment::new();
        let child_env = Environment::with_enclosing(Rc::clone(&global_env));

        // Define a variable in the global environment
        global_env.borrow_mut().define(
            "global_var".to_string(),
            LoxValue::String("initial".to_string()),
        );

        // Attempt to assign to the global variable from the child environment
        let token = Token::new(
            TokenType::Identifier,
            "global_var".to_string(),
            Literal::Nil,
            1,
        );
        let new_value = LoxValue::String("updated".to_string());
        let result = child_env.borrow_mut().assign(&token, new_value.clone());

        // Ensure assignment succeeded and the global variable was updated
        assert!(result.is_ok());
        assert_eq!(child_env.borrow().get(&token).unwrap(), new_value);
        assert_eq!(global_env.borrow().get(&token).unwrap(), new_value);
    }

    #[test]
    fn test_assign_nonexistent_variable_should_error() {
        let global_env = Environment::new();
        let child_env = Environment::with_enclosing(Rc::clone(&global_env));

        // Attempt to assign to a variable that doesn't exist
        let token = Token::new(
            TokenType::Identifier,
            "undefined_var".to_string(),
            Literal::Nil,
            1,
        );
        let value = LoxValue::String("value".to_string());
        let result = child_env.borrow_mut().assign(&token, value);

        // Ensure that the assignment failed with the correct error
        assert!(
            matches!(result, Err(EnvironmentError::UndefinedVariable(err)) if err == "Undefined variable 'undefined_var'.")
        );
    }

    #[test]
    fn test_ancestor_at_various_distances() {
        let global_env = Environment::new();
        let first_child_env = Environment::with_enclosing(Rc::clone(&global_env));
        let second_child_env = Environment::with_enclosing(Rc::clone(&first_child_env));

        // Define variables in different levels
        global_env.borrow_mut().define(
            "var_global".to_string(),
            LoxValue::String("global".to_string()),
        );
        first_child_env.borrow_mut().define(
            "var_first_child".to_string(),
            LoxValue::String("first_child".to_string()),
        );
        second_child_env.borrow_mut().define(
            "var_second_child".to_string(),
            LoxValue::String("second_child".to_string()),
        );

        // Test ancestor at distance 0 (current environment)
        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 0)
                .borrow()
                .get(&Token::new(
                    TokenType::Identifier,
                    "var_second_child".to_string(),
                    Literal::Nil,
                    1
                )),
            Ok(LoxValue::String("second_child".to_string()))
        );

        // Test ancestor at distance 1 (first child environment)
        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 1)
                .borrow()
                .get(&Token::new(
                    TokenType::Identifier,
                    "var_first_child".to_string(),
                    Literal::Nil,
                    1
                )),
            Ok(LoxValue::String("first_child".to_string()))
        );

        // Test ancestor at distance 2 (global environment)
        assert_eq!(
            Environment::ancestor(Rc::clone(&second_child_env), 2)
                .borrow()
                .get(&Token::new(
                    TokenType::Identifier,
                    "var_global".to_string(),
                    Literal::Nil,
                    1
                )),
            Ok(LoxValue::String("global".to_string()))
        );
    }

    #[test]
    fn test_assign_at_non_existent_variable_should_error() {
        let global_env = Environment::new();
        let first_child_env = Environment::with_enclosing(Rc::clone(&global_env));
        let second_child_env = Environment::with_enclosing(Rc::clone(&first_child_env));

        let token = Token::new(
            TokenType::Identifier,
            "z".to_string(),
            Literal::Num(30.0.into()),
            1,
        );
        let value = LoxValue::Number(30.0);

        // Attempt to assign to 'z' at a distance of 1, which doesn't exist
        let result = Environment::assign_at(Rc::clone(&second_child_env), 1, &token, value);

        // Ensure that an error is returned, indicating the variable could not be assigned
        let expected_error = Err(EnvironmentError::AssignVariableError("Couldn't assign variable 'z' at distance '1'.".to_string()));
        assert_eq!(result, expected_error)
    }


    #[test]
    fn test_assign_at_correct_distance() {
        let global_env = Environment::new();
        global_env
            .borrow_mut()
            .define("x".to_string(), LoxValue::Number(10.0));

        let first_child_env = Environment::with_enclosing(Rc::clone(&global_env));
        first_child_env.borrow_mut().define("y".to_string(), LoxValue::Number(20.0));

        let second_child_env = Environment::with_enclosing(Rc::clone(&first_child_env));

        let token = Token::new(
            TokenType::Identifier,
            "x".to_string(),
            Literal::Num(30.0.into()),
            1,
        );

        // Assign to 'x' at a distance of 2 (global scope)
        let result = Environment::assign_at(Rc::clone(&second_child_env), 2, &token, LoxValue::Number(30.0));

        // Ensure the assignment was successful
        assert_eq!(result, Ok(LoxValue::Number(10.0))); // This should match the old value.

        // Check that the value in the global environment was updated
        assert_eq!(global_env.borrow().get(&token), Ok(LoxValue::Number(30.0)));
        // This should be the updated value
    }
}