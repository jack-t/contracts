use std::collections::HashMap;
use crate::lex::Token;

pub enum Expression {
	Binary{
		op: Token,
		lhs: Box<Expression>,
		rhs: Box<Expression>,
	},
	Unary{
		op: Option<Token>,
		operand: Box<Expression>,
	},
	FunctionCall{
		function_name: String,
		parameters: Vec<Box<Expression>>
	},
	NoOp, // just a semicolon
}

pub struct Conditional {
	pub condition: Box<Expression>,
	pub true_op: Option<Box<Statement>>,
	pub false_op: Option<Box<Statement>>,
}

pub struct Loop {
	pub condition: Box<Expression>,
	pub body: Box<Statement>,
}

pub struct FunctionDeclaration { // we can allow closures-ish if this is a branch off expression
	pub name: String,
	pub parameters: HashMap<String, Box<TypeSpecification>>,
	pub return_type: Option<Box<TypeSpecification>>,
	pub body: Box<Block>,
}

pub struct TypeSpecification {
	pub base: String,
	pub contracts: Vec<Box<ContractInvocation>>,
}

pub struct ContractInvocation {
	pub contract: String,
	pub parameters: Vec<Box<Expression>>,
}

pub struct TypeAliasDeclaration {
	pub target: String,
	pub source: Box<TypeSpecification>,
}

pub struct ContractDeclaration {
	pub name: String,
	pub parameters: HashMap<String, Box<TypeSpecification>>,
	pub body: Box<Block>,
}

pub struct Block {
	pub statement: Box<Statement>,
	pub next: Option<Box<Statement>>,
}

pub enum Statement {
	Expression(Expression),
	Conditional(Conditional),
	Loop(Loop),
	ReturnStatement(Expression),
	FunctionDeclaration(FunctionDeclaration),
	TypeAliasDeclaration(TypeAliasDeclaration),
	ContractDeclaration(ContractDeclaration),
	Block(Block),
}