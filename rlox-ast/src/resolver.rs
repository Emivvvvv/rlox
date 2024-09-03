use rustc_hash::FxHashMap;

use crate::expr::Expr;
use crate::lexer::token::Token;
use crate::lox;
use crate::stmt::Stmt;

#[derive(Debug)]
pub enum ResolveError {
    Error(String),
}

pub trait Resolvable<'a> {
    fn resolve(&'a self, resolver: &mut Resolver<'a>);
}

impl<'a> Resolvable<'a> for Expr {
    fn resolve(&'a self, resolver: &mut Resolver<'a>) {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => resolver.binary_expr(left, operator, right),
            Expr::Grouping { expression } => resolver.grouping_expr(expression),
            Expr::Literal { value: _value } => (),
            Expr::Unary { operator, right } => resolver.unary_expr(operator, right),
            Expr::Variable { name } => resolver.variable_expr(self, name),
            Expr::Assign { name, value } => resolver.assign_expr(self, name, value),
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
            Expr::Get {object, name} => resolver.get_expr(object, name),
            Expr::Set {object, name, value} => resolver.set_expr(object, name, value),
            Expr::This { keyword} => resolver.this_expr(self, keyword),
            Expr::Super { keyword, ..} => resolver.super_expr(self, keyword),
        }
    }
}

impl<'a> Resolvable<'a> for Stmt {
    fn resolve(&'a self, resolver: &mut Resolver<'a>) {
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
            Stmt::Class {name, superclass, methods} => resolver.class_stmt(name, superclass, methods),
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
enum FunctionType {
    Function,
    Initializer,
    Method,
    None,
}

#[derive(Copy, Clone, PartialEq)]
enum ClassType {
    Class,
    Subclass,
    None,
}

pub struct Resolver<'a> {
    scopes: Vec<FxHashMap<&'a str, bool>>,
    locals: FxHashMap<&'a Expr, usize>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl<'a> Default for Resolver<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Resolver<'a> {
    pub fn new() -> Self {
        Resolver {
            scopes: Vec::new(),
            locals: FxHashMap::default(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve_lox(mut self, statements: &'a [Stmt]) -> FxHashMap<&'a Expr, usize> {
        self.resolve_array(statements);

        self.locals
    }

    pub fn resolve_array(&mut self, statements: &'a [Stmt]) {
        for statement in statements {
            self.resolve(statement);
        }
    }

    pub fn resolve(&mut self, resolvable: &'a dyn Resolvable<'a>) {
        resolvable.resolve(self)
    }

    fn begin_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &'a Token) {
        let scope = self.scopes.last_mut();
        if let Some(scope) = scope {
            if scope.contains_key(name.lexeme.as_str()) {
                lox::error(name, "Already a variable with this name in this scope.");
            }
            scope.insert(name.lexeme.as_str(), false);
        }
    }

    fn define(&mut self, name: &'a Token) {
        let scope = self.scopes.last_mut();
        if let Some(scope) = scope {
            scope.insert(name.lexeme.as_str(), true);
        }
    }

    fn resolve_local(&mut self, expr: &'a Expr, name: &Token) {
        for i in (0..self.scopes.len()).rev() {
            if let Some(scope) = self.scopes.get(i) {
                if scope.contains_key(name.lexeme.as_str()) {
                    self.locals.insert(expr, self.scopes.len() - 1 - i);
                }
            }
        }
    }

    fn resolve_function(&mut self, _name: &Token, params: &'a [Token], body: &'a [Stmt], function_type: FunctionType) {
        let enclosing_function = self.current_function;
        self.current_function = function_type;

        self.begin_scope();
        for param in params {
            self.declare(param);
            self.define(param);
        }
        self.resolve_array(body);
        self.end_scope();

        self.current_function = enclosing_function;
    }
}

impl<'a> Resolver<'a> {
    pub fn block_stmt(&mut self, statements: &'a [Stmt]) {
        self.begin_scope();
        self.resolve_array(statements);
        self.end_scope();
    }

    pub fn var_stmt(&mut self, name: &'a Token, initializer: &'a Option<Expr>) {
        self.declare(name);
        if let Some(initial_value) = initializer {
            self.resolve(initial_value)
        }
        self.define(name);
    }
    pub fn variable_expr(&mut self, expr: &'a Expr, name: &Token) {
        let scope = self.scopes.last_mut();
        if let Some(scope) = scope {
            if let Some(is_defined) = scope.get(name.lexeme.as_str()) {
                if !is_defined {
                    lox::error(name, "Can't read local variable in its own initializer.");
                }
            }
        }
        self.resolve_local(expr, name);
    }

    pub fn assign_expr(&mut self, expr: &'a Expr, name: &Token, value: &'a Expr) {
        self.resolve(value);
        self.resolve_local(expr, name);
    }

    pub fn get_expr(&mut self, object: &'a Expr, _name: &Token) {
        self.resolve(object);
    }

    pub fn set_expr(&mut self, object:&'a Expr, _name: &Token, value: &'a Expr) {
        self.resolve(value);
        self.resolve(object);
    }

    pub fn this_expr(&mut self, expr: &'a Expr, keyword: &Token) {
        if self.current_class == ClassType::None {
            lox::error(keyword, "Can't use 'this' outside of a class.");
        }
        self.resolve_local(expr, keyword);
    }

    pub fn super_expr(&mut self, expr: &'a Expr, keyword: &Token) {
        if self.current_class == ClassType::None {
            lox::error(keyword, "Can't use 'super' outside of a class.");
        } else if self.current_class != ClassType::Subclass {
            lox::error(keyword, "Can't use 'super' in a class with no superclass.");
        }
        self.resolve_local(expr, keyword);
    }

    pub fn function_stmt(&mut self, name: &'a Token, params: &'a [Token], body: &'a [Stmt]) {
        self.declare(name);
        self.define(name);
        self.resolve_function(name, params, body, FunctionType::Function);
    }

    fn expression_stmt(&mut self, expression: &'a Expr) {
        self.resolve(expression)
    }

    fn if_stmt(&mut self, condition: &'a Expr, then_branch: &'a Stmt, else_branch: &'a Option<Box<Stmt>>) {
        self.resolve(condition);
        self.resolve(then_branch);
        if let Some(else_branch) = else_branch {
            self.resolve(&**else_branch)
        }
    }

    fn print_stmt(&mut self, expression: &'a Expr) {
        self.resolve(expression)
    }

    fn return_stmt(&mut self, keyword: &Token, value: &'a Option<Expr>) {
        if self.current_function == FunctionType::None {
            lox::error(keyword, "Can't return from top-level code.");
        }
        if let Some(value) = value {
            if self.current_function == FunctionType::Initializer {
                lox::error(keyword, "Can't return a value from an initializer.");
            }
            self.resolve(value)
        }
    }

    fn while_stmt(&mut self, condition: &'a Expr, body: &'a Stmt) {
        self.resolve(condition);
        self.resolve(body);
    }

    fn class_stmt(&mut self, class_name: &'a Token, superclass: &'a Option<Expr>, methods: &'a [Stmt]) {
        let enclosing_class = self.current_class;
        self.current_class = ClassType::Class;

        self.declare(class_name);
        self.define(class_name);

        if let Some(superclass) = superclass {
            self.current_class = ClassType::Subclass;
            if let Expr::Variable{name} = superclass {
                if name.lexeme == class_name.lexeme {
                    lox::error(name, "A class can't inherit from itself.");
                }
            }
            self.resolve(superclass);
        }

        if superclass.is_some() {
            self.begin_scope();
            let scope = self.scopes.last_mut();
            match scope {
                Some(scope) => {
                    scope.insert("super", true);
                }
                None => return
            }
        }

        self.begin_scope();
        let scope = self.scopes.last_mut();
        match scope {
            Some(scope) => {
                scope.insert("this", true);
            }
            None => return
        }

        for method in methods {
            match method {
                Stmt::Function {name, params, body} => {
                    let declaration = if name.lexeme == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };
                    self.resolve_function(name, params, body, declaration)
                },
                _ => panic!("Method's must be functions.")
            };
        }

        self.end_scope();

        if superclass.is_some() {
            self.end_scope();
        }

        self.current_class = enclosing_class;
    }

    fn binary_expr(&mut self, left: &'a Expr, _operator: &Token, right: &'a Expr) {
        self.resolve(left);
        self.resolve(right);
    }

    fn call_expr(&mut self, callee: &'a Expr, _paren: &Token, arguments: &'a[Expr]) {
        self.resolve(callee);

        for argument in arguments {
            self.resolve(argument)
        }
    }

    fn grouping_expr(&mut self, expression: &'a Expr) {
        self.resolve( expression)
    }

    fn logical_expr(&mut self, left: &'a Expr, _operator: &Token, right: &'a Expr) {
        self.resolve(left);
        self.resolve(right);
    }

    fn unary_expr(&mut self, _operator: &Token, right: &'a Expr) {
        self.resolve(right);
    }
}