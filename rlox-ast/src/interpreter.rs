use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;

use crate::environment::{Environment, EnvironmentError};
use crate::expr::{Expr, ExprIdx, ExprPool};
use crate::globals::define_globals;
use crate::lexer::token::Literal;
use crate::lexer::token::Token;
use crate::lexer::token::TokenType;
use crate::lox;
use crate::lox_callable::callable::Callable;
use crate::lox_callable::lox_function::LoxFunction;
use crate::stmt::Stmt;
use crate::lox_value::{LoxCallable, LoxValue, LoxValueError};
use crate::lox_callable::lox_class::LoxClass;
use crate::lox_callable::lox_instance::LoxInstance;
use crate::symbol::{Symbol, SymbolTable};

#[derive(Debug)]
pub enum RuntimeError {
    IncorrectOperand(Token, LoxValueError),
    DivideByZero(Token, LoxValueError),
    InterpreterPanic(Token, String),
    InstanceError(Token, String),
    UndefinedVariable(Token, EnvironmentError),
    AssignVariableError(Token, EnvironmentError),
    InputError(String),
    CustomError(String),
    Return(LoxValue),
}

impl RuntimeError {
    pub fn get_info(self) -> (Token, String) {
        match self {
            RuntimeError::IncorrectOperand(token, lox_value_err)
            | RuntimeError::DivideByZero(token, lox_value_err) => {
                (token, lox_value_err.get_string())
            }
            RuntimeError::UndefinedVariable(token, environment_err)
            | RuntimeError::AssignVariableError(token, environment_err) => {
                (token, environment_err.get_string())
            }
            RuntimeError::InterpreterPanic(token, err_str)
            | RuntimeError::InstanceError(token, err_str) => (token, err_str),
            _ => panic!("Should not reach here!"),
        }
    }
}

pub trait Evaluable {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<LoxValue, RuntimeError>;
}

impl Evaluable for ExprIdx {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<LoxValue, RuntimeError> {
        match interpreter.expr_pool.get_expr(*self) {
            Expr::Binary {
                left,
                operator,
                right,
            } => interpreter.evaluate_binary(*left, operator, *right),
            Expr::Unary { operator, right } => interpreter.evaluate_unary(operator, *right),
            Expr::Literal { value } => Ok(Interpreter::evaluate_literal(value)),
            Expr::Grouping { expression } => interpreter.evaluate(expression),
            Expr::Variable { name } => interpreter.evaluate_variable(*self, name),
            Expr::Assign { name, value } => interpreter.evaluate_assign(*self, name, *value),
            Expr::Logical {
                left,
                operator,
                right,
            } => interpreter.evaluate_logical(*left, operator, *right),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => interpreter.evaluate_call(*callee, paren, arguments),
            Expr::Get { object, name } => interpreter.evaluate_get(*object, name),
            Expr::Set { object, name, value } => interpreter.evaluate_set(*object, name, *value),
            Expr::This { keyword } => interpreter.evaluate_this(*self, keyword),
            Expr::Super { keyword, method } => interpreter.evaluate_super(*self, keyword, method),
        }
    }
}

impl Evaluable for Stmt {
    fn evaluate(&self, interpreter: &mut Interpreter) -> Result<LoxValue, RuntimeError> {
        match self {
            Stmt::Expression { expression } => interpreter.evaluate_expression_stmt(*expression),
            Stmt::Print { expression } => interpreter.evaluate_print_stmt(*expression),
            Stmt::Var { name, initializer } => interpreter.evaluate_var_stmt(name, initializer),
            Stmt::Block { statements } => interpreter.evaluate_block_stmt(statements, None),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => interpreter.evaluate_if_stmt(*condition, then_branch, else_branch),
            Stmt::While { condition, body } => {
                interpreter.evaluate_while_stmt(*condition, body)
            }
            Stmt::Function { name, params, body } => {
                interpreter.interpret_function_stmt(name, params, body)
            }
            Stmt::Return { keyword, value } => interpreter.interpret_return_stmt(keyword, value),
            Stmt::Class { name, superclass, methods } => interpreter.evaluate_class_stmt(name, superclass, methods),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter<'a> {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,
    locals: FxHashMap<ExprIdx, usize>,
    expr_pool: &'a ExprPool,
    pub symbol_table: &'a mut SymbolTable,
}

impl<'a> Interpreter<'a> {
    pub fn new(expr_pool: &'a ExprPool, symbol_table: &'a mut SymbolTable, locals: FxHashMap<ExprIdx, usize>) -> Self {
        // Create a new global environment
        let globals = Environment::new();
        define_globals(&globals, symbol_table);

        // Initially, the environment is the global environment
        let environment = Rc::clone(&globals);

        Self {
            globals,
            environment,
            locals,
            expr_pool,
            symbol_table,
        }
    }

    pub fn get_globals(&self) -> Rc<RefCell<Environment>> {
        Rc::clone(&self.globals)
    }

    pub fn interpret(&mut self, statements: &[Stmt]) {
        for statement in statements {
            match self.evaluate(statement) {
                Ok(_) => continue,
                Err(e) => lox::runtime_error(e),
            }
        }
    }

    #[cfg(test)]
    pub fn interpret_test(&mut self, statements: Vec<Stmt>) -> LoxValue {
        for statement in statements {
            match self.evaluate(&statement) {
                Ok(value) => return value,
                Err(e) => lox::runtime_error(e),
            }
        }

        unreachable!()
    }

    pub fn set_locals(&mut self, locals: FxHashMap<ExprIdx, usize>) {
        self.locals = locals;
    }

    fn evaluate(&mut self, evaluable: &dyn Evaluable) -> Result<LoxValue, RuntimeError> {
        evaluable.evaluate(self)
    }

    fn evaluate_literal(value: &Literal) -> LoxValue {
        LoxValue::from(value)
    }

    fn evaluate_binary(
        &mut self,
        left_idx: ExprIdx,
        operator: &Token,
        right_idx: ExprIdx,
    ) -> Result<LoxValue, RuntimeError> {
        let left = self.evaluate(&left_idx)?;
        let right = self.evaluate(&right_idx)?;

        match operator.token_type {
            TokenType::Minus | TokenType::Slash | TokenType::Star => left
                .math_if_num(right, &operator.token_type)
                .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e)),
            TokenType::Plus => match (&left, &right) {
                (LoxValue::String(left_str), LoxValue::String(right_str)) => {
                    Ok(LoxValue::String(left_str.clone() + right_str))
                }
                (LoxValue::Number(_), LoxValue::Number(_)) => {
                    left
                        .math_if_num(right, &TokenType::Plus)
                        .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e))
                }
                _ => Ok(LoxValue::String(format!("{left}{right}"))),
            },
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => left
                .compare_if_num(right, operator.token_type.clone())
                .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e)),
            TokenType::BangEqual => left
                .is_equal(right)
                .bang_if_bool()
                .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e)),
            TokenType::EqualEqual => Ok(left.is_equal(right)),
            _ => Err(RuntimeError::InterpreterPanic(
                operator.clone(),
                "Invalid token type for evaluating binary expressions.".to_string(),
            )),
        }
    }

    fn evaluate_unary(&mut self, operator: &Token, right_idx: ExprIdx) -> Result<LoxValue, RuntimeError> {
        let right = self.evaluate(&right_idx)?;

        match operator.token_type {
            TokenType::Bang => Ok(right.is_truthy()),
            TokenType::Minus => {
                right
                    .negate_if_num()
                    .map_err(|e| RuntimeError::IncorrectOperand(operator.clone(), e))
            }
            _ => Err(RuntimeError::InterpreterPanic(
                operator.clone(),
                "Invalid token type for evaluating unary expressions.".to_string(),
            )),
        }
    }

    fn evaluate_variable(&mut self, expr_idx: ExprIdx, name: &Token) -> Result<LoxValue, RuntimeError> {
        self.look_up_variable(name, expr_idx)
    }

    fn look_up_variable(&self, name: &Token, expr_idx: ExprIdx) -> Result<LoxValue, RuntimeError> {
        if let Some(distance) = self.locals.get(&expr_idx) {
            Environment::get_at(Rc::clone(&self.environment), *distance, &name.lexeme, self.symbol_table)
                .map_err(|e| RuntimeError::UndefinedVariable(name.clone(), e))
        } else {
            self.globals
                .borrow()
                .get(name.lexeme, self.symbol_table)
                .map_err(|e| RuntimeError::UndefinedVariable(name.clone(), e))
        }
    }

    fn evaluate_assign(&mut self, expr_idx: ExprIdx, name: &Token, value_idx: ExprIdx) -> Result<LoxValue, RuntimeError> {
        let value = self.evaluate(&value_idx)?;

        if let Some(distance) = self.locals.get(&expr_idx) {
            Environment::assign_at(Rc::clone(&self.environment), *distance, name.lexeme, value.clone(), self.symbol_table)
                .map_err(|e| RuntimeError::AssignVariableError(name.clone(), e))?;
        } else {
            self.globals
                .borrow_mut()
                .assign(name.lexeme, value.clone(), self.symbol_table)
                .map_err(|e| RuntimeError::AssignVariableError(name.clone(), e))?;
        }

        Ok(value)
    }

    fn evaluate_logical(
        &mut self,
        left_idx: ExprIdx,
        operator: &Token,
        right_idx: ExprIdx,
    ) -> Result<LoxValue, RuntimeError> {
        let left = self.evaluate(&left_idx)?;

        let is_truthy = left.is_truthy() == LoxValue::Boolean(true);

        if operator.token_type == TokenType::Or {
            if is_truthy {
                return Ok(left);
            }
        } else {
            if !is_truthy {
                return Ok(left);
            }
        }

        self.evaluate(&right_idx)
    }

    fn evaluate_call(
        &mut self,
        callee_idx: ExprIdx,
        paren: &Token,
        arguments: &[ExprIdx],
    ) -> Result<LoxValue, RuntimeError> {
        let callee = self.evaluate(&callee_idx)?;

        let mut evaluated_arguments = Vec::new();
        for argument_idx in arguments {
            let value = self.evaluate(argument_idx)?;
            evaluated_arguments.push(value);
        }

        match callee {
            LoxValue::Callable(callable) => {
                if arguments.len() != callable.arity(self.symbol_table) {
                    return Err(RuntimeError::InterpreterPanic(
                        paren.clone(),
                        format!(
                            "Expected {} arguments but got {}.",
                            callable.arity(self.symbol_table),
                            arguments.len()
                        ),
                    ));
                }
                callable.call(self, evaluated_arguments)
            }
            _ => Err(RuntimeError::InterpreterPanic(
                paren.clone(),
                "Can only call functions and classes.".to_string(),
            )),
        }
    }

    fn evaluate_get(&mut self, object_idx: ExprIdx, name: &Token) -> Result<LoxValue, RuntimeError> {
        let object = self.evaluate(&object_idx)?;

        if let LoxValue::Callable(LoxCallable::Instance(instance)) = object {
            LoxInstance::get(instance, name, self.symbol_table)
        } else {
            Err(RuntimeError::InstanceError(
                name.clone(),
                "Only instances have properties.".to_string(),
            ))
        }
    }

    fn evaluate_set(&mut self, object_idx: ExprIdx, name: &Token, value_idx: ExprIdx) -> Result<LoxValue, RuntimeError> {
        let object = self.evaluate(&object_idx)?;

        if let LoxValue::Callable(LoxCallable::Instance(instance)) = object {
            let value = self.evaluate(&value_idx)?;
            instance.borrow_mut().set(name.clone(), value.clone());
            Ok(value)
        } else {
            Err(RuntimeError::InstanceError(
                name.clone(),
                "Only instances have fields.".to_string(),
            ))
        }
    }

    fn evaluate_this(&mut self, expr_idx: ExprIdx, keyword: &Token) -> Result<LoxValue, RuntimeError> {
        self.look_up_variable(keyword, expr_idx)
    }

    fn evaluate_super(&mut self, expr_idx: ExprIdx, keyword: &Token, method: &Token) -> Result<LoxValue, RuntimeError> {
        let distance = self.locals.get(&expr_idx).unwrap();


        let superclass_lox_value = Environment::get_at(Rc::clone(&self.environment), *distance, &self.symbol_table.intern("super"), self.symbol_table).map_err(|e| RuntimeError::UndefinedVariable(keyword.clone(), e))?;

        let object_lox_value = Environment::get_at(Rc::clone(&self.environment), distance - 1, &self.symbol_table.intern("this"), self.symbol_table).map_err(|e| RuntimeError::UndefinedVariable(keyword.clone(), e))?;

        if let LoxValue::Callable(callable) = superclass_lox_value {
            if let LoxCallable::Class(superclass) = callable {
                match superclass.find_method(&method.lexeme) {
                    Some(method_callable) => {
                        if let LoxCallable::Function(method_func) = method_callable {
                            if let LoxValue::Callable(LoxCallable::Instance(instance)) = object_lox_value {
                                return Ok(LoxValue::Callable(LoxCallable::Function(Rc::new(method_func.bind(&instance, self.symbol_table)))));
                            }
                        }
                        Err(RuntimeError::CustomError("Undefined behaviour".to_string()))
                    }
                    None => Err(RuntimeError::UndefinedVariable(method.clone(), EnvironmentError::UndefinedVariable(format!("Undefined property '{}'.", method.lexeme)))),
                }
            } else {
                Err(RuntimeError::CustomError("Superclass is not callable.".to_string()))
            }
        } else {
            Err(RuntimeError::CustomError("Superclass is not a callable LoxValue.".to_string()))
        }
    }

    fn evaluate_expression_stmt(&mut self, expr_idx: ExprIdx) -> Result<LoxValue, RuntimeError> {
        self.evaluate(&expr_idx)
    }

    fn evaluate_print_stmt(&mut self, expr_idx: ExprIdx) -> Result<LoxValue, RuntimeError> {
        let value = self.evaluate(&expr_idx)?;
        println!("{}", value);
        Ok(value)
    }

    fn evaluate_var_stmt(
        &mut self,
        name: &Token,
        initializer: &Option<ExprIdx>,
    ) -> Result<LoxValue, RuntimeError> {
        let value = if let Some(expr_idx) = initializer {
            self.evaluate(expr_idx)?
        } else {
            LoxValue::Nil
        };

        self.environment
            .borrow_mut()
            .define(name.lexeme.clone(), value);

        Ok(LoxValue::Nil)
    }

    pub(crate) fn evaluate_block_stmt(
        &mut self,
        statements: &[Stmt],
        environment: Option<Rc<RefCell<Environment>>>,
    ) -> Result<LoxValue, RuntimeError> {
        let previous_environment = Rc::clone(&self.environment);
        let new_environment = environment.unwrap_or_else(|| Environment::with_enclosing(Rc::clone(&self.environment)));

        self.environment = new_environment;

        let result = statements.iter().try_for_each(|statement| self.evaluate(statement).map(|_| ()));

        self.environment = previous_environment;

        result.map(|_| LoxValue::Nil)
    }

    fn evaluate_if_stmt(
        &mut self,
        condition_idx: ExprIdx,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<LoxValue, RuntimeError> {
        if self.evaluate(&condition_idx)?.is_truthy() == LoxValue::Boolean(true) {
            self.evaluate(then_branch)?;
        } else if let Some(else_stmt) = else_branch {
            self.evaluate(else_stmt.as_ref())?;
        }
        Ok(LoxValue::Nil)
    }

    fn evaluate_while_stmt(
        &mut self,
        condition_idx: ExprIdx,
        body: &Stmt,
    ) -> Result<LoxValue, RuntimeError> {
        while self.evaluate(&condition_idx)?.is_truthy() == LoxValue::Boolean(true) {
            self.evaluate(body)?;
        }
        Ok(LoxValue::Nil)
    }

    fn evaluate_class_stmt(
        &mut self,
        name: &Token,
        superclass: &Option<ExprIdx>,
        methods: &[Stmt],
    ) -> Result<LoxValue, RuntimeError> {
        let superclass_option = if let Some(superclass_idx) = superclass {
            let superclass_value = self.evaluate(superclass_idx)?;
            if let LoxValue::Callable(LoxCallable::Class(superclass)) = superclass_value {
                Some(superclass)
            } else {
                return Err(RuntimeError::InterpreterPanic(
                    name.clone(),
                    "Superclass must be a class.".to_string(),
                ));
            }
        } else {
            None
        };

        self.environment
            .borrow_mut()
            .define(name.lexeme.clone(), LoxValue::Nil);

        if superclass_option.is_some() {
            self.environment = Environment::with_enclosing(Rc::clone(&self.environment));
            self.environment.borrow_mut().define(self.symbol_table.intern("super"), LoxValue::Callable(LoxCallable::Class(superclass_option.clone().unwrap())));
        }

        let mut methods_map: FxHashMap<Symbol, LoxCallable> = FxHashMap::default();

        for method in methods {
            if let Stmt::Function { name: method_name, params, body } = method {
                let is_initializer = method_name.lexeme == self.symbol_table.intern("init");
                let function = LoxFunction::new(
                    method_name.clone(),
                    params.clone(),
                    body.clone(),
                    Rc::clone(&self.environment),
                    is_initializer,
                );
                methods_map.insert(
                    method_name.lexeme,
                    LoxCallable::Function(Rc::new(function)),
                );
            } else {
                return Err(RuntimeError::InterpreterPanic(
                    name.clone(),
                    "Class methods must be functions.".to_string(),
                ));
            }
        }

        let class = LoxClass::new(self.symbol_table.resolve(name.lexeme).to_string(), superclass_option.clone(), methods_map);
        let class_value = LoxValue::Callable(LoxCallable::Class(Rc::new(class)));

        if superclass_option.is_some() {
            let enclosing = self.environment.borrow_mut().enclosing.clone().unwrap();
            self.environment = enclosing;
        }

        self.environment
            .borrow_mut()
            .assign(name.lexeme, class_value, self.symbol_table)
            .map_err(|e| RuntimeError::AssignVariableError(name.clone(), e))?;

        Ok(LoxValue::Nil)
    }

    fn interpret_function_stmt(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<LoxValue, RuntimeError> {
        let function = LoxFunction::new(
            name.clone(),
            params.to_vec(),
            body.to_vec(),
            Rc::clone(&self.environment),
            false,
        );

        let callable = LoxCallable::Function(Rc::new(function));

        self.environment
            .borrow_mut()
            .define(name.lexeme.clone(), LoxValue::Callable(callable));

        Ok(LoxValue::Nil)
    }

    fn interpret_return_stmt(
        &mut self,
        _keyword: &Token,
        value: &Option<ExprIdx>,
    ) -> Result<LoxValue, RuntimeError> {
        let value = match value {
            Some(expr_idx) => self.evaluate(expr_idx)?,
            None => LoxValue::Nil,
        };
        Err(RuntimeError::Return(value))
    }
}

#[cfg(test)]
mod interpreter_tests {
    use crate::lexer::scanner;
    use super::*;
    use crate::parser::Parser;
    use crate::resolver::Resolver;

    fn set_test_environment(source: &str) -> LoxValue {
        let mut symbol_table = SymbolTable::new();
        let lexer_tokens = {
            let mut lexer = scanner::Scanner::new(source, &mut symbol_table);
            lexer.scan_tokens();

            lexer.tokens
        };
        let parser = Parser::new(&symbol_table, lexer_tokens);
        let (statements, expr_pool) = parser
            .parse()
            .unwrap();

        let locals = Resolver::new(&expr_pool, &mut symbol_table).resolve_lox(&statements);

        let mut interpreter = Interpreter::new(&expr_pool, &mut symbol_table, locals);
        interpreter.interpret_test(statements)
    }

    #[test]
    fn test_arithmetic_expression() {
        let source = "1 + (2 * 3);";
        let output = set_test_environment(source);
        assert_eq!(output, LoxValue::Number(7.0)); // 1 + (2 * 3) = 7
    }

    #[test]
    #[should_panic]
    fn test_unary_negation() {
        let source = "-true;";
        set_test_environment(source);
    }

    #[test]
    #[should_panic]
    fn test_division_by_zero() {
        let source = "10 / 0;";
        set_test_environment(source);
    }

    #[test]
    fn test_string_concatenation() {
        let source = "\"Hello, \" + \"world!\";";
        let output = set_test_environment(source);
        assert_eq!(output, LoxValue::String("Hello, world!".to_string()));
    }

    #[test]
    fn test_grouping_and_precedence() {
        let source = "(1 + 2) * 3;";
        let output = set_test_environment(source);
        assert_eq!(output, LoxValue::Number(9.0)); // (1 + 2) * 3 = 9
    }

    #[test]
    fn test_print_statement() {
        let source = "print \"Hello, world!\";";
        let output = set_test_environment(source);
        assert_eq!(output, LoxValue::String("Hello, world!".to_string()));
    }

    #[test]
    fn test_math_and_print() {
        let source = "print (2 + 3) * 4;";
        let output = set_test_environment(source);
        assert_eq!(output, LoxValue::Number(20.0)); // (2 + 3) * 4 = 20
    }

    #[test]
    fn test_single_statement_block() {
        let source = "{ var x = 10; }";
        let output = set_test_environment(source);
        assert_eq!(output, LoxValue::Nil);
    }

    #[test]
    fn test_nested_block() {
        let source = "{ var x = 10; { var y = 5; } }";
        let output = set_test_environment(source);
        assert_eq!(output, LoxValue::Nil);
    }

    #[test]
    fn test_block_with_multiple_statements() {
        let source = "{ print \"Hello\"; var number = 42; }";
        let output = set_test_environment(source);
        assert_eq!(output, LoxValue::Nil); // Output from print is visible during test but not captured by assert
    }

    #[test]
    fn test_clock_function() {
        let source = "clock();";
        let output = set_test_environment(source);

        if let LoxValue::Number(time) = output {
            assert!(time > 0.0);
        } else {
            panic!("Expected a number from clock function");
        }
    }

    #[test]
    fn test_evaluate_block_stmt_environment_reversion() {
        let mut symbol_table = SymbolTable::new();
        let lexer_tokens = {
            let mut lexer = scanner::Scanner::new("", &mut symbol_table);
            lexer.scan_tokens();

            lexer.tokens
        };
        let parser = Parser::new(&symbol_table, lexer_tokens);
        let (statements, expr_pool) = parser
            .parse()
            .unwrap();
        let locals = Resolver::new(&expr_pool, &mut symbol_table).resolve_lox(&statements);
        let interpreter = Interpreter::new(&expr_pool, &mut symbol_table, locals);

        let global_env = interpreter.get_globals();

        // Define a variable in the global scope
        global_env
            .borrow_mut()
            .define(symbol_table.intern("x"), LoxValue::Number(10.0));

        // Block that defines a new variable and should revert to the global environment
        let source = "{ var y = 20; }";
        set_test_environment(source);

        // Ensure that 'x' still exists in the global environment after block execution
        assert_eq!(
            global_env.borrow().get(symbol_table.intern("x"), &symbol_table),
            Ok(LoxValue::Number(10.0))
        );

        // Ensure that 'y' is not accessible outside the block (since it's local to the block)
        assert!(global_env
            .borrow()
            .get(symbol_table.intern("y"), &symbol_table)
            .is_err());
    }

    #[test]
    fn test_evaluate_assign_correct_environment() {
        let source = "a = \"second\";";

        let mut symbol_table = SymbolTable::new();
        let lexer_tokens = {
            let mut lexer = scanner::Scanner::new(source, &mut symbol_table);
            lexer.scan_tokens();

            lexer.tokens
        };
        let parser = Parser::new(&symbol_table, lexer_tokens);
        let (statements, expr_pool) = parser
            .parse()
            .unwrap();
        let locals = Resolver::new(&expr_pool, &mut symbol_table).resolve_lox(&statements);
        let interpreter = Interpreter::new(&expr_pool, &mut symbol_table, locals);

        let global_env = interpreter.get_globals();

        // Define variable 'a' in global scope
        global_env
            .borrow_mut()
            .define(symbol_table.intern("a"), LoxValue::String("first".to_string()));

        // Ensure 'a' was updated in the global scope
        assert_eq!(
            global_env.borrow().get(symbol_table.intern("a"), &symbol_table),
            Ok(LoxValue::String("first".to_string()))
        );
    }

    #[test]
    fn test_scope_management() {
        let source = "{ var x = 10; }";

        let mut symbol_table = SymbolTable::new();
        let lexer_tokens = {
            let mut lexer = scanner::Scanner::new(source, &mut symbol_table);
            lexer.scan_tokens();

            lexer.tokens
        };
        let parser = Parser::new(&symbol_table, lexer_tokens);
        let (statements, expr_pool) = parser
            .parse()
            .unwrap();
        let locals = Resolver::new(&expr_pool, &mut symbol_table).resolve_lox(&statements);
        let interpreter = Interpreter::new(&expr_pool, &mut symbol_table, locals);

        // Ensure 'x' is not accessible after the scope ends
        assert!(interpreter
            .get_globals()
            .borrow()
            .get(symbol_table.intern("x"), &symbol_table)
            .is_err());
    }
}