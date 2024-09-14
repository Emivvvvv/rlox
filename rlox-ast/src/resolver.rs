use rustc_hash::FxHashMap;

use crate::expr::{Expr, ExprIdx, ExprPool};
use crate::lexer::token::{ErrorToken, Token};
use crate::lox;
use crate::stmt::Stmt;
use crate::symbol::{Symbol, SymbolTable};

#[derive(Debug)]
pub enum ResolveError {
    Error(String),
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
    scopes: Vec<FxHashMap<Symbol, bool>>,
    locals: FxHashMap<ExprIdx, usize>,
    current_function: FunctionType,
    current_class: ClassType,
    expr_pool: &'a ExprPool,
    symbol_table: &'a mut SymbolTable,
}

impl<'a> Resolver<'a> {
    pub fn new(expr_pool: &'a ExprPool, symbol_table: &'a mut SymbolTable) -> Self {
        Resolver {
            scopes: Vec::new(),
            locals: FxHashMap::default(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
            expr_pool,
            symbol_table
        }
    }

    pub fn resolve_lox(mut self, statements: &'a [Stmt]) -> FxHashMap<ExprIdx, usize> {
        self.resolve_statements(statements);
        self.locals
    }

    fn resolve_statements(&mut self, statements: &'a [Stmt]) {
        for statement in statements {
            self.resolve_statement(statement);
        }
    }

    fn resolve_statement(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::Expression { expression } => self.expression_stmt(*expression),
            Stmt::Print { expression } => self.print_stmt(*expression),
            Stmt::Var { name, initializer } => {
                let initializer_idx = initializer.as_ref().copied();
                self.var_stmt(name, initializer_idx);
            }
            Stmt::Block { statements } => self.block_stmt(statements),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let else_branch = else_branch.as_deref();
                self.if_stmt(*condition, then_branch, else_branch);
            }
            Stmt::While { condition, body } => self.while_stmt(*condition, body),
            Stmt::Function { name, params, body } => self.function_stmt(name, params, body),
            Stmt::Return { keyword, value } => {
                let value_idx = value.as_ref().copied();
                self.return_stmt(keyword, value_idx);
            }
            Stmt::Class {
                name,
                superclass,
                methods,
            } => {
                let superclass_idx = superclass.as_ref().copied();
                self.class_stmt(name, superclass_idx, methods);
            }
        }
    }

    fn resolve_expr(&mut self, expr_idx: ExprIdx) {
        let expr = self.expr_pool.get_expr(expr_idx);
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => self.binary_expr(*left, operator, *right),
            Expr::Grouping { expression } => self.grouping_expr(*expression),
            Expr::Literal { .. } => (),
            Expr::Unary { operator, right } => self.unary_expr(operator, *right),
            Expr::Variable { name } => self.variable_expr(expr_idx, name),
            Expr::Assign { name, value } => self.assign_expr(expr_idx, name, *value),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.logical_expr(*left, operator, *right),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => self.call_expr(*callee, paren, arguments),
            Expr::Get { object, name } => self.get_expr(*object, name),
            Expr::Set { object, name, value } => self.set_expr(*object, name, *value),
            Expr::This { keyword } => self.this_expr(expr_idx, keyword),
            Expr::Super { keyword, .. } => self.super_expr(expr_idx, keyword),
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &'a Token) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                lox::error(&ErrorToken::new(name, self.symbol_table), "Already a variable with this name in this scope.");
            }
            scope.insert(name.lexeme, false);
        }
    }

    fn define(&mut self, name: &'a Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme, true);
        }
    }

    fn resolve_local(&mut self, expr_idx: ExprIdx, name: &Token) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                self.locals.insert(expr_idx, i);
                return;
            }
        }
    }

    // Expression methods
    fn binary_expr(&mut self, left_idx: ExprIdx, _operator: &Token, right_idx: ExprIdx) {
        self.resolve_expr(left_idx);
        self.resolve_expr(right_idx);
    }

    fn grouping_expr(&mut self, expression_idx: ExprIdx) {
        self.resolve_expr(expression_idx);
    }

    fn unary_expr(&mut self, _operator: &Token, right_idx: ExprIdx) {
        self.resolve_expr(right_idx);
    }

    fn variable_expr(&mut self, expr_idx: ExprIdx, name: &Token) {
        if let Some(scope) = self.scopes.last() {
            if let Some(is_defined) = scope.get(&name.lexeme) {
                if !*is_defined {
                    lox::error(&ErrorToken::new(name, self.symbol_table), "Can't read local variable in its own initializer.");
                }
            }
        }
        self.resolve_local(expr_idx, name);
    }

    fn assign_expr(&mut self, expr_idx: ExprIdx, name: &Token, value_idx: ExprIdx) {
        self.resolve_expr(value_idx);
        self.resolve_local(expr_idx, name);
    }

    fn logical_expr(&mut self, left_idx: ExprIdx, _operator: &Token, right_idx: ExprIdx) {
        self.resolve_expr(left_idx);
        self.resolve_expr(right_idx);
    }

    fn call_expr(&mut self, callee_idx: ExprIdx, _paren: &Token, arguments: &[ExprIdx]) {
        self.resolve_expr(callee_idx);
        for &argument_idx in arguments {
            self.resolve_expr(argument_idx);
        }
    }

    fn get_expr(&mut self, object_idx: ExprIdx, _name: &Token) {
        self.resolve_expr(object_idx);
    }

    fn set_expr(&mut self, object_idx: ExprIdx, _name: &Token, value_idx: ExprIdx) {
        self.resolve_expr(value_idx);
        self.resolve_expr(object_idx);
    }

    fn this_expr(&mut self, expr_idx: ExprIdx, keyword: &Token) {
        if self.current_class == ClassType::None {
            lox::error(&ErrorToken::new(keyword, self.symbol_table), "Can't use 'this' outside of a class.");
            return;
        }
        self.resolve_local(expr_idx, keyword);
    }

    fn super_expr(&mut self, expr_idx: ExprIdx, keyword: &Token) {
        if self.current_class == ClassType::None {
            lox::error(&ErrorToken::new(keyword, self.symbol_table), "Can't use 'super' outside of a class.");
        } else if self.current_class != ClassType::Subclass {
            lox::error(&ErrorToken::new(keyword, self.symbol_table), "Can't use 'super' in a class with no superclass.");
        }
        self.resolve_local(expr_idx, keyword);
    }

    // Statement methods
    fn expression_stmt(&mut self, expr_idx: ExprIdx) {
        self.resolve_expr(expr_idx);
    }

    fn print_stmt(&mut self, expr_idx: ExprIdx) {
        self.resolve_expr(expr_idx);
    }

    fn var_stmt(&mut self, name: &'a Token, initializer_idx: Option<ExprIdx>) {
        self.declare(name);
        if let Some(initializer_idx) = initializer_idx {
            self.resolve_expr(initializer_idx);
        }
        self.define(name);
    }

    fn block_stmt(&mut self, statements: &'a [Stmt]) {
        self.begin_scope();
        self.resolve_statements(statements);
        self.end_scope();
    }

    fn if_stmt(
        &mut self,
        condition_idx: ExprIdx,
        then_branch: &'a Stmt,
        else_branch: Option<&'a Stmt>,
    ) {
        self.resolve_expr(condition_idx);
        self.resolve_statement(then_branch);
        if let Some(else_branch) = else_branch {
            self.resolve_statement(else_branch);
        }
    }

    fn while_stmt(&mut self, condition_idx: ExprIdx, body: &'a Stmt) {
        self.resolve_expr(condition_idx);
        self.resolve_statement(body);
    }

    fn function_stmt(&mut self, name: &'a Token, params: &'a [Token], body: &'a [Stmt]) {
        self.declare(name);
        self.define(name);
        self.resolve_function(params, body, FunctionType::Function);
    }

    fn resolve_function(
        &mut self,
        params: &'a [Token],
        body: &'a [Stmt],
        function_type: FunctionType,
    ) {
        let enclosing_function = self.current_function;
        self.current_function = function_type;

        self.begin_scope();
        for param in params {
            self.declare(param);
            self.define(param);
        }
        self.resolve_statements(body);
        self.end_scope();

        self.current_function = enclosing_function;
    }

    fn return_stmt(&mut self, keyword: &Token, value_idx: Option<ExprIdx>) {
        if self.current_function == FunctionType::None {
            lox::error(&ErrorToken::new(keyword, self.symbol_table), "Can't return from top-level code.");
        }
        if let Some(value_idx) = value_idx {
            if self.current_function == FunctionType::Initializer {
                lox::error(&ErrorToken::new(keyword, self.symbol_table), "Can't return a value from an initializer.");
            }
            self.resolve_expr(value_idx);
        }
    }

    fn class_stmt(
        &mut self,
        class_name: &'a Token,
        superclass_idx: Option<ExprIdx>,
        methods: &'a [Stmt],
    ) {
        let enclosing_class = self.current_class;
        self.current_class = ClassType::Class;

        self.declare(class_name);
        self.define(class_name);

        if let Some(superclass_idx) = superclass_idx {
            self.current_class = ClassType::Subclass;

            let superclass_expr = self.expr_pool.get_expr(superclass_idx);
            if let Expr::Variable { name } = superclass_expr {
                if name.lexeme == class_name.lexeme {
                    lox::error(&ErrorToken::new(name, self.symbol_table), "A class can't inherit from itself.");
                }
            } else {
                lox::error(&ErrorToken::new(class_name, self.symbol_table), "Superclass must be a variable.");
            }

            self.resolve_expr(superclass_idx);

            self.begin_scope();
            if let Some(scope) = self.scopes.last_mut() {
                scope.insert(self.symbol_table.intern("super"), true);
            }
        }

        self.begin_scope();
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(self.symbol_table.intern("this"), true);
        }

        for method in methods {
            if let Stmt::Function { name, params, body } = method {
                let declaration = if name.lexeme == self.symbol_table.intern("init") {
                    FunctionType::Initializer
                } else {
                    FunctionType::Method
                };
                self.resolve_function(params, body, declaration);
            } else {
                panic!("Methods must be functions.");
            }
        }

        self.end_scope();

        if superclass_idx.is_some() {
            self.end_scope();
        }

        self.current_class = enclosing_class;
    }
}
