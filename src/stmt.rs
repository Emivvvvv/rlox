use crate::{expr::Expr, lexer::token::Token};

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
    Block {
        statements: Vec<Stmt>,
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
            _ => panic!("Should not be reached!"),
        }
    }
}
