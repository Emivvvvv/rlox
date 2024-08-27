use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{interpreter::LoxValue, lexer::token::Token};

#[derive(Debug, PartialEq)]
pub enum EnvironmentError {
    UndefinedVariable(String),
}

impl EnvironmentError {
    pub fn get_string(self) -> String {
        match self {
            EnvironmentError::UndefinedVariable(err_str) => err_str,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, LoxValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::<String, LoxValue>::new(),
            enclosing: None,
        }
    }

    pub fn with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::<String, LoxValue>::new(),
            enclosing: Some(enclosing),
        }
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

    pub fn get_at(&self, distance: usize, name: &Token) -> Result<LoxValue, EnvironmentError> {
        let ancestor_env = self.ancestor(distance);
        let values_map = ancestor_env.borrow().values.clone();

        match values_map.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => Err(EnvironmentError::UndefinedVariable(format!(
                "Undefined variable '{}'.",
                name
            ))),
        }
    }

    pub fn ancestor(&self, distance: usize) -> Rc<RefCell<Environment>> {
        let mut environment = Rc::new(RefCell::new(self.clone())); // Wrap `self` into an Rc<RefCell>

        for _ in 0..distance {
            let enclosing_env = {
                let borrowed_env = environment.borrow();
                match &borrowed_env.enclosing {
                    Some(enclosing_env) => Rc::clone(enclosing_env), // Move to the next environment
                    None => panic!("No enclosing environment found at distance {}", distance),
                }
            };
            environment = enclosing_env;
        }

        environment
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

    pub fn assign_at(
        &mut self,
        distance: usize,
        name: &Token,
        value: LoxValue,
    ) -> Result<LoxValue, EnvironmentError> {
        let ancestor_env = self.ancestor(distance);
        let mut env = ancestor_env.borrow_mut(); // Borrow the environment mutably

        println!(
            "[assign_at] Environment at distance {}: 0x{:x}",
            distance,
            Rc::as_ptr(&ancestor_env) as usize
        );
        println!("[assign_at] Before assignment: {:?}", env.values);

        match env.values.get_mut(&name.lexeme) {
            Some(existing_value) => {
                *existing_value = value.clone();
                println!("[assign_at] After assignment: {:?}", env.values);
                Ok(value)
            }
            None => Err(EnvironmentError::UndefinedVariable(format!(
                "Undefined variable '{}'.",
                name.lexeme
            ))),
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::{Literal, TokenType};

    #[test]
    fn test_define_and_get() {
        let mut env = Environment::new();
        let token = Token::new(
            TokenType::Identifier,
            "x".to_string(),
            Literal::Num(10.0.into()),
            1,
        );
        let value = LoxValue::Number(10.0);
        env.define("x".to_string(), value.clone());

        assert_eq!(env.get(&token).unwrap(), value);
    }

    #[test]
    fn test_get_undefined_variable() {
        let env = Environment::new();
        let token = Token::new(TokenType::Identifier, "y".to_string(), Literal::Nil, 1);

        assert!(
            matches!(env.get(&token), Err(EnvironmentError::UndefinedVariable(err)) if err == "Undefined variable 'y'.")
        );
    }

    #[test]
    fn test_assign_existing_variable() {
        let mut env = Environment::new();
        let token = Token::new(
            TokenType::Identifier,
            "z".to_string(),
            Literal::Num(10.0.into()),
            1,
        );
        env.define("z".to_string(), LoxValue::Number(10.0));

        let new_value = LoxValue::Number(20.0);
        env.assign(&token, new_value.clone()).unwrap();
        assert_eq!(env.get(&token).unwrap(), new_value);
    }

    #[test]
    fn test_enclosing_environment() {
        let outer = Rc::new(RefCell::new(Environment::new()));
        let token = Token::new(
            TokenType::Identifier,
            "var".to_string(),
            Literal::Num(100.0.into()),
            1,
        );
        let value = LoxValue::Number(100.0);
        outer.borrow_mut().define("var".to_string(), value.clone());

        let inner = Environment::with_enclosing(outer);
        assert_eq!(inner.get(&token).unwrap(), value);
    }

    #[test]
    fn test_shadowing_by_inner_environment() {
        let outer = Rc::new(RefCell::new(Environment::new()));
        let token = Token::new(
            TokenType::Identifier,
            "var".to_string(),
            Literal::Num(100.0.into()),
            1,
        );
        outer
            .borrow_mut()
            .define("var".to_string(), LoxValue::Number(100.0));

        let mut inner = Environment::with_enclosing(outer);
        let inner_value = LoxValue::Number(200.0);
        inner.define("var".to_string(), inner_value.clone());

        assert_eq!(inner.get(&token).unwrap(), inner_value);
    }

    #[test]
    fn test_assign_in_nested_environment() {
        let outer = Rc::new(RefCell::new(Environment::new()));
        outer
            .borrow_mut()
            .define("x".to_string(), LoxValue::Number(10.0));

        let mut inner = Environment::with_enclosing(outer);
        assert_eq!(
            inner
                .assign(
                    &Token::new(TokenType::Identifier, "x".to_string(), Literal::Nil, 1),
                    LoxValue::Number(20.0)
                )
                .unwrap(),
            LoxValue::Number(20.0)
        );
        assert_eq!(
            inner
                .get(&Token::new(
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
        let mut env = Environment::new();

        let token = Token::new(TokenType::Identifier, "w".to_string(), Literal::Nil, 1);

        let value = LoxValue::Number(31.0);

        let result = env.assign(&token, value);

        assert!(
            matches!(result, Err(EnvironmentError::UndefinedVariable(ref err)) if err == "Undefined variable 'w'."),
            "Expected UndefinedVariable error, but got: {:?}",
            result
        );
    }

    #[test]
    fn test_ancestor_retrieval() {
        let global_env = Rc::new(RefCell::new(Environment::new()));
        let first_child_env = Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(
            &global_env,
        ))));
        let second_child_env = Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(
            &first_child_env,
        ))));

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
            second_child_env
                .borrow()
                .ancestor(0)
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
            second_child_env
                .borrow()
                .ancestor(1)
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
            second_child_env
                .borrow()
                .ancestor(2)
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
        let global_env = Rc::new(RefCell::new(Environment::new()));
        let mut child_env = Environment::with_enclosing(Rc::clone(&global_env));

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
        let result = child_env.assign(&token, new_value.clone());

        // Ensure assignment succeeded and the global variable was updated
        assert!(result.is_ok());
        assert_eq!(child_env.get(&token).unwrap(), new_value);
        assert_eq!(global_env.borrow().get(&token).unwrap(), new_value);
    }

    #[test]
    fn test_assign_nonexistent_variable_should_error() {
        let global_env = Rc::new(RefCell::new(Environment::new()));
        let mut child_env = Environment::with_enclosing(Rc::clone(&global_env));

        // Attempt to assign to a variable that doesn't exist
        let token = Token::new(
            TokenType::Identifier,
            "undefined_var".to_string(),
            Literal::Nil,
            1,
        );
        let value = LoxValue::String("value".to_string());
        let result = child_env.assign(&token, value);

        // Ensure that the assignment failed with the correct error
        assert!(
            matches!(result, Err(EnvironmentError::UndefinedVariable(err)) if err == "Undefined variable 'undefined_var'.")
        );
    }

    #[test]
    fn test_ancestor_at_various_distances() {
        let global_env = Rc::new(RefCell::new(Environment::new()));
        let first_child_env = Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(
            &global_env,
        ))));
        let mut second_child_env = Environment::with_enclosing(first_child_env.clone());

        // Define variables in different levels
        global_env.borrow_mut().define(
            "var_global".to_string(),
            LoxValue::String("global".to_string()),
        );
        first_child_env.borrow_mut().define(
            "var_first_child".to_string(),
            LoxValue::String("first_child".to_string()),
        );
        second_child_env.define(
            "var_second_child".to_string(),
            LoxValue::String("second_child".to_string()),
        );

        // Test ancestor at distance 0 (current environment)
        assert_eq!(
            second_child_env.ancestor(0).borrow().get(&Token::new(
                TokenType::Identifier,
                "var_second_child".to_string(),
                Literal::Nil,
                1
            )),
            Ok(LoxValue::String("second_child".to_string()))
        );

        // Test ancestor at distance 1 (first child environment)
        assert_eq!(
            second_child_env.ancestor(1).borrow().get(&Token::new(
                TokenType::Identifier,
                "var_first_child".to_string(),
                Literal::Nil,
                1
            )),
            Ok(LoxValue::String("first_child".to_string()))
        );

        // Test ancestor at distance 2 (global environment)
        assert_eq!(
            second_child_env.ancestor(2).borrow().get(&Token::new(
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
        let global_env = Rc::new(RefCell::new(Environment::new()));
        let first_child_env = Environment::with_enclosing(Rc::clone(&global_env));
        let mut second_child_env =
            Environment::with_enclosing(Rc::new(RefCell::new(first_child_env.clone())));

        let token = Token::new(
            TokenType::Identifier,
            "z".to_string(),
            Literal::Num(30.0.into()),
            1,
        );
        let value = LoxValue::Number(30.0);

        // Attempt to assign to 'z' at a distance of 1, which doesn't exist
        let result = second_child_env.assign_at(1, &token, value);

        // Ensure that an error is returned, indicating the variable is undefined
        assert!(
            matches!(result, Err(EnvironmentError::UndefinedVariable(ref err)) if err == "Undefined variable 'z'."),
            "Expected UndefinedVariable error, but got: {:?}",
            result
        );
    }

    #[test]
    fn test_assign_at_correct_distance() {
        let global_env = Rc::new(RefCell::new(Environment::new()));
        global_env
            .borrow_mut()
            .define("x".to_string(), LoxValue::Number(10.0));

        let mut first_child_env = Environment::with_enclosing(Rc::clone(&global_env));
        first_child_env.define("y".to_string(), LoxValue::Number(20.0));

        let mut second_child_env =
            Environment::with_enclosing(Rc::new(RefCell::new(first_child_env.clone())));

        let token = Token::new(
            TokenType::Identifier,
            "x".to_string(),
            Literal::Num(30.0.into()),
            1,
        );

        // Assign to 'x' at a distance of 2 (global scope)
        let result = second_child_env.assign_at(2, &token, LoxValue::Number(30.0));

        // Ensure the assignment was successful
        assert_eq!(result, Ok(LoxValue::Number(30.0))); // This should match the new value

        // Check that the value in the global environment was updated
        assert_eq!(global_env.borrow().get(&token), Ok(LoxValue::Number(30.0)));
        // This should be the updated value
    }
}
