use crate::lex::Token;
use std::collections::HashMap;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Assignment {
        lvalue: Box<Expression>,
        rvalue: Box<Expression>,
    },
    Variable {
        name: String,
    },
    Int {
        value: u64,
    },
    Str {
        value: String,
    },
    Dec {
        value: f64,
    },
    Char {
        value: char,
    },
    FunctionCall {
        func: String,
        params: Vec<Box<Expression>>,
    },
    Binary {
        op: Token,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
}

#[derive(PartialEq)]
pub enum Statement {
    Conditional {
        condition: Box<Expression>,
        true_statement: Box<Statement>,
        false_statement: Option<Box<Statement>>,
    },
    Block {
        statement: Box<Statement>,
        next: Option<Box<Statement>>,
    },
    // func decl
}
