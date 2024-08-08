use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::expr::Expr;
use crate::interpreter::Interpreter;
use crate::lexer::token::Token;
use crate::lox;
use crate::stmt::Stmt;

#[derive(Debug)]
pub enum ResolveError {
    Error(String),
}

pub trait Resolvable {
    fn resolve(&self, resolver: &mut Resolver);
}

impl Resolvable for Expr {
    fn resolve(&self, resolver: &mut Resolver) {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => resolver.binary_expr(left, operator, right),
            Expr::Grouping { expression } => resolver.grouping_expr(expression),
            Expr::Literal { value: _value } => return,
            Expr::Unary { operator, right } => resolver.unary_expr(operator, right),
            Expr::Variable { name } => resolver.variable_expr(name),
            Expr::Assign { name, value } => resolver.assign_expr(name, value),
            Expr::Logical {
                left,
                operator,
                right,
            } => resolver.logical_expr(left, operator, right),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => resolver.call_expr(callee, paren, arguments),
        }
    }
}

impl Resolvable for Stmt {
    fn resolve(&self, resolver: &mut Resolver) {
        match self {
            Stmt::Expression { expression } => resolver.expression_stmt(expression),
            Stmt::Print { expression } => resolver.print_stmt(expression),
            Stmt::Var { name, initializer } => resolver.var_stmt(name, initializer),
            Stmt::Block { statements } => resolver.block_stmt(statements),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => resolver.if_stmt(condition, then_branch, else_branch),
            Stmt::While { condition, body } => resolver.while_stmt(condition, body),
            Stmt::Function { name, params, body } => resolver.function_stmt(name, params, body),
            Stmt::Return { keyword, value } => resolver.return_stmt(keyword, value),
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
enum FunctionType {
    Function,
    None,
}

pub struct Resolver {
    interpreter: Rc<RefCell<Interpreter>>, // Reference to the interpreter
    scopes: Vec<HashMap<String, bool>>,    // Stack of scopes
    current_function: FunctionType,
}

impl Resolver {
    pub fn new(interpreter: Rc<RefCell<Interpreter>>) -> Self {
        let hmap: HashMap<String, bool> = HashMap::new();
        Resolver {
            interpreter,
            scopes: vec![hmap],
            current_function: FunctionType::None,
        }
    }

    pub fn resolve_vec(&mut self, statements: &Vec<Stmt>) {
        for statement in statements {
            self.resolve(statement);
        }
    }

    pub fn resolve(&mut self, resolvable: &dyn Resolvable) {
        resolvable.resolve(self)
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                lox::error(
                    name.clone(),
                    "Already a variable with this name in this scope.",
                );
            } else {
                scope.insert(name.lexeme.clone(), false); // Variable is declared but not yet defined
            }
        }
    }

    fn define(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true); // True indicates the variable is fully initialized
        }
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter
                    .borrow_mut()
                    .resolve(expr.clone(), self.scopes.len() - 1 - i);
                return;
            }
        }
    }

    fn resolve_function(
        &mut self,
        _name: &Token,
        params: &Vec<Token>,
        body: &Vec<Stmt>,
        func_type: FunctionType,
    ) {
        let enclosing_function = self.current_function;
        self.current_function = func_type;

        self.begin_scope();
        for param in params {
            self.declare(param);
            self.define(param);
        }
        self.resolve_vec(body);
        self.end_scope();

        self.current_function = enclosing_function
    }
}

impl Resolver {
    fn block_stmt(&mut self, statements: &Vec<Stmt>) {
        self.begin_scope();
        self.resolve_vec(statements);
        self.end_scope();
    }

    fn var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) {
        self.declare(name);
        if let Some(init) = initializer {
            self.resolve(init);
        }
        self.define(name)
    }

    fn function_stmt(&mut self, name: &Token, params: &Vec<Token>, body: &Vec<Stmt>) {
        self.declare(name);
        self.define(name);

        self.resolve_function(name, params, body, FunctionType::Function)
    }

    fn expression_stmt(&mut self, expression: &Expr) {
        self.resolve(expression)
    }

    fn if_stmt(&mut self, condition: &Expr, then_branch: &Stmt, else_branch: &Option<Box<Stmt>>) {
        self.resolve(condition);
        self.resolve(then_branch);
        if let Some(else_branch) = else_branch {
            self.resolve(&**else_branch)
        }
    }

    fn print_stmt(&mut self, expression: &Expr) {
        self.resolve(expression)
    }

    fn return_stmt(&mut self, keyword: &Token, value: &Option<Expr>) {
        if self.current_function == FunctionType::None {
            lox::error(keyword.clone(), "Can't return from top-level code.")
        }

        if let Some(value) = value {
            self.resolve(value)
        }
    }

    fn while_stmt(&mut self, condition: &Expr, body: &Stmt) {
        self.resolve(condition);
        self.resolve(body)
    }

    fn binary_expr(&mut self, left: &Expr, _operator: &Token, right: &Expr) {
        self.resolve(left);
        self.resolve(right);
    }

    fn call_expr(&mut self, callee: &Expr, _paren: &Token, arguments: &Vec<Expr>) {
        self.resolve(callee);

        for argument in arguments {
            self.resolve(argument)
        }
    }

    fn grouping_expr(&mut self, expression: &Expr) {
        self.resolve(expression)
    }

    fn logical_expr(&mut self, left: &Expr, _operator: &Token, right: &Expr) {
        self.resolve(left);
        self.resolve(right);
    }

    fn unary_expr(&mut self, _operator: &Token, right: &Expr) {
        self.resolve(right)
    }

    fn variable_expr(&mut self, name: &Token) {
        if !self.scopes.is_empty() {
            if let Some(scope) = self.scopes.last() {
                if let Some(&value) = scope.get(&name.lexeme) {
                    if value == false {
                        lox::error(
                            name.clone(),
                            "Can't read local variable in its own initializer.",
                        );
                    }
                }
            }
        }
        self.resolve_local(&Expr::Variable { name: name.clone() }, name);
    }

    fn assign_expr(&mut self, name: &Token, value: &Box<Expr>) {
        self.resolve(&**value);
        self.resolve_local(
            &Expr::Assign {
                name: name.clone(),
                value: value.clone(),
            },
            name,
        )
    }
}
