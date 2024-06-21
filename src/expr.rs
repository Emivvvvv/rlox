use crate::lexer::token::{Literal, Token};

enum Expr {
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
}

impl Expr {
    fn accept<R, F: FnOnce(&Expr) -> R>(&self, visit: F) -> R {
        visit(self)
    }
}
