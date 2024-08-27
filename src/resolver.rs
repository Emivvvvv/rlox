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
            Expr::Literal { value: _value } => (),
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
    interpreter: Rc<RefCell<Interpreter>>,
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    pub fn new(interpreter: Rc<RefCell<Interpreter>>) -> Self {
        Resolver {
            interpreter,
            scopes: Vec::new(),
        }
    }

    pub fn resolve_array(&mut self, statements: &[Stmt]) {
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
        let scope = self.scopes.last_mut();
        match scope {
            Some(scope) => {
                scope.insert(name.lexeme.clone(), false);
            }
            None => return
        }
    }

    fn define(&mut self, name: &Token) {
        let scope = self.scopes.last_mut();
        match scope {
            Some(scope) => {
                scope.insert(name.lexeme.clone(), true);
            }
            None => return
        }
    }
}

impl Resolver {
    pub fn block_stmt(&mut self, statements: &[Stmt]) {
        self.begin_scope();
        self.resolve_array(statements);
        self.end_scope();
    }

    pub fn var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) {
        self.declare(name);
        if let Some(initial_value) = initializer {
            self.resolve(initial_value)
        }
        self.define(name);
    }

    pub fn variable_expr(&mut self, name: &Token) {
        let scope = self.scopes.last_mut();
        if let Some(scope) = scope {
            if !scope.get(&name.lexeme.clone()) {
                lox::error(name.clone(), "Can't read local variable in its own initializer.")
            }
        }
    }
}