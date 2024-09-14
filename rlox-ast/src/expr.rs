use crate::lexer::token::{Literal, Token};

#[derive(Debug, Clone, Hash, Eq, PartialEq, Copy)]
pub struct ExprIdx(pub usize);

#[derive(Debug)]
pub struct ExprPool {
    pub exprs: Vec<Expr>,
}

impl ExprPool {
    pub fn add_expr(&mut self, expr: Expr) -> ExprIdx {
        let idx = self.exprs.len();
        self.exprs.push(expr);
        ExprIdx(idx)
    }

    pub fn get_expr(&self, idx: ExprIdx) -> &Expr {
        &self.exprs[idx.0]
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Expr {
    Binary {
        left: ExprIdx,
        operator: Token,
        right: ExprIdx,
    },
    Grouping {
        expression: ExprIdx,
    },
    Literal {
        value: Literal,
    },
    Unary {
        operator: Token,
        right: ExprIdx,
    },
    Variable {
        name: Token,
    },
    Assign {
        name: Token,
        value: ExprIdx,
    },
    Logical {
        left: ExprIdx,
        operator: Token,
        right: ExprIdx,
    },
    Call {
        callee: ExprIdx,
        paren: Token,
        arguments: Vec<ExprIdx>,
    },
    Get {
        object: ExprIdx,
        name: Token,
    },
    Set {
        object: ExprIdx,
        name: Token,
        value: ExprIdx,
    },
    This {
        keyword: Token,
    },
    Super {
        keyword: Token,
        method: Token,
    }
}