use crate::expr::ExprIdx;
use crate::lexer::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expression {
        expression: ExprIdx,
    },
    Print {
        expression: ExprIdx,
    },
    Var {
        name: Token,
        initializer: Option<ExprIdx>,
    },
    Block {
        statements: Vec<Stmt>,
    },
    If {
        condition: ExprIdx,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: ExprIdx,
        body: Box<Stmt>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
    },
    Return {
        keyword: Token,
        value: Option<ExprIdx>,
    },
    Class {
        name: Token,
        superclass: Option<ExprIdx>,
        methods: Vec<Stmt>,
    },
}

impl From<Stmt> for Option<ExprIdx> {
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
