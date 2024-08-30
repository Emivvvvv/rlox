use crate::expr::Expr;
use crate::lexer::token::Token;

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
    Class {
        name: Token,
        methods: Vec<Stmt>,
    },
}

impl From<Stmt> for Option<Expr> {
    fn from(val: Stmt) -> Self {
        match val {
            Stmt::Expression { expression } | Stmt::Print { expression } => Some(expression),
            Stmt::Var {
                name: _,
                initializer,
            } => initializer,
            _ => panic!("Should not be reached!"),
        }
    }
}
