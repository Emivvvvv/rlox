use crate::{expr::Expr, lexer::token::Token};

#[derive(Debug, Clone)]
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
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
    },
    Return {
        keyword: Token,
        value: Option<Expr>,
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
