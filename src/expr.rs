use crate::lexer::token::{Literal, Token};

pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: Literal,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
    },
}

pub enum Stmt {
    Expression {
        expression: Expr,
    },
    Print {
        expression: Expr,
    },
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
}

impl Into<Option<Expr>> for Stmt {
    fn into(self) -> Option<Expr> {
        match self {
            Stmt::Expression { expression } | Stmt::Print { expression } => Some(expression),
            Stmt::Var {
                name: _,
                initializer,
            } => initializer,
        }
    }
}
