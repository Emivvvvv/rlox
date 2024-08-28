use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{interpreter::LoxValue, lexer::token::Token};

#[derive(Debug)]
pub enum EnvironmentError {
    UndefinedVariable(String),
    AssignVariableError(String)
}

impl EnvironmentError {
    pub fn get_string(self) -> String {
        match self {
            EnvironmentError::UndefinedVariable(err_str) => err_str,
            EnvironmentError::AssignVariableError(err_str) => err_str,
        }
    }
}

#[derive(Debug)]
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

    pub fn assign(&mut self, name: &Token, value: LoxValue) -> Result<LoxValue, EnvironmentError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value.clone());
            Ok(value)
        } else {
            if let Some(enclosing) = &self.enclosing {
                enclosing.borrow_mut().assign(name, value)
            } else {
                Err(EnvironmentError::UndefinedVariable(format!(
                    "Undefined variable '{}'.",
                    name.lexeme
                )))
            }
        }
    }

    pub fn get_at(&mut self, distance: usize, name: &String) -> Result<LoxValue, EnvironmentError> {
        match self.ancestor(distance)?.borrow_mut().values.get(name) {
            Some(value) => Ok(value.clone()),
            None => Err(EnvironmentError::UndefinedVariable(format!(
                "Undefined variable '{}' at distance '{}'.",
                name,
                distance
            )))
        }
    }
    
    pub fn assign_at(&mut self, distance: usize, name: Token, value: LoxValue) -> Result<LoxValue, EnvironmentError> {
        let binding = self.ancestor(distance)?;
        let ancestor_values = &mut binding.borrow_mut().values;
        match ancestor_values.insert(name.lexeme.clone(), value) {
            Some(value) => Ok(value),
            None => Err(EnvironmentError::AssignVariableError(format!(
                "Couldn't assign variable '{}' at distance '{}'.",
                name,
                distance,
            )))
        }
    }

    pub fn ancestor(&self, distance: usize) -> Result<Rc<RefCell<Environment>>, EnvironmentError> {
        let mut environment = match &self.enclosing {
            Some(enclosing) => Rc::clone(enclosing),
            None => {
                return Err(EnvironmentError::UndefinedVariable(format!(
                    "No enclosing environment found at distance {}.",
                    distance
                )));
            }
        };

        for _ in 1..distance {
            let next_environment = match &environment.borrow().enclosing {
                Some(enclosing) => Rc::clone(enclosing),
                None => {
                    return Err(EnvironmentError::UndefinedVariable(format!(
                        "No enclosing environment found at distance {}.",
                        distance
                    )))
                }
            };
            environment = next_environment;
        }

        Ok(environment)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::{Hf64, Literal, TokenType};

    #[test]
    fn test_define_and_get() {
        let mut env = Environment::new();
        let token = Token::new(
            TokenType::Identifier,
            "x".to_string(),
            Literal::Num(Hf64::from(10.0)),
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
            Literal::Num(Hf64::from(10.0)),
            1,
        );
        env.define("z".to_string(), LoxValue::Number(10.0));

        let new_value = LoxValue::Number(20.0);
        env.assign(&token, new_value.clone()).unwrap();
        assert_eq!(env.get(&token).unwrap(), new_value);
    }

    #[test]
    fn test_assign_undefined_variable() {
        let mut env = Environment::new();
        let token = Token::new(
            TokenType::Identifier,
            "w".to_string(),
            Literal::Num(Hf64::from(20.0)),
            1,
        );
        let value = LoxValue::Number(20.0);

        assert!(
            matches!(env.assign(&token, value), Err(EnvironmentError::UndefinedVariable(err)) if err == "Undefined variable 'w'.")
        );
    }

    #[test]
    fn test_enclosing_environment() {
        let outer = Rc::new(RefCell::new(Environment::new()));
        let token = Token::new(
            TokenType::Identifier,
            "var".to_string(),
            Literal::Num(Hf64::from(100.0)),
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
            Literal::Num(Hf64::from(100.0)),
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
}
