use crate::lex::Token;
use std::collections::HashMap;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Binary {
        op: Token,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Unary {
        op: Option<Token>,
        operand: Box<Expression>,
    },
    FunctionCall {
        function_name: String,
        parameters: Vec<Box<Expression>>,
    },
    VariableReference(String),
    IntLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),
    NoOp, // just a semicolon
}

#[derive(PartialEq, Debug)]
pub struct Conditional {
    pub condition: Box<Expression>,
    pub true_op: Box<Statement>,
    pub false_op: Option<Box<Statement>>,
}

#[derive(PartialEq, Debug)]
pub struct Loop {
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct FunctionDeclaration {
    // we can allow closures-ish if this is a branch off expression
    pub name: String,
    pub parameters: HashMap<String, Box<TypeSpecification>>,
    pub return_type: Option<Box<TypeSpecification>>,
    pub body: Box<Block>,
}

#[derive(PartialEq, Debug)]
pub struct TypeSpecification {
    pub base: String,
    pub contracts: Vec<Box<ContractInvocation>>,
}

#[derive(PartialEq, Debug)]
pub struct ContractInvocation {
    pub contract: String,
    pub parameters: Vec<Box<Expression>>,
}

#[derive(PartialEq, Debug)]
pub struct TypeAliasDeclaration {
    pub target: String,
    pub source: Box<TypeSpecification>,
}

#[derive(PartialEq, Debug)]
pub struct ContractDeclaration {
    pub name: String,
    pub parameters: HashMap<String, Box<TypeSpecification>>,
    pub body: Box<Block>,
}

#[derive(PartialEq, Debug)]
pub struct Block {
    pub statement: Box<Statement>,
    pub next: Option<Box<Statement>>,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Expression(Expression),
    Conditional(Conditional),
    Loop(Loop),
    Return(Expression),
    FunctionDeclaration(FunctionDeclaration),
    TypeAliasDeclaration(TypeAliasDeclaration),
    ContractDeclaration(ContractDeclaration),
    Block(Block),
}
