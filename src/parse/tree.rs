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
	condition: Box<Expression>,
	true_op: Option<Box<Statement>>,
	false_op: Option<Box<Statement>>,
}

pub struct Loop {
	condition: Box<Expression>,
	body: Box<Statement>,
}

pub struct FunctionDeclaration { // we can allow closures-ish if this is a branch off expression
	name: String,
	parameters: HashMap<String, Box<TypeSpecification>>,
	return_type: Option<Box<TypeSpecification>>,
	body: Box<Block>,
}

pub struct TypeSpecification {
	base: String,
	contracts: Vec<Box<ContractInvocation>>,
}

pub struct ContractInvocation {
	contract: String,
	parameters: Vec<Box<Expression>>,
}

pub struct TypeAliasDeclaration {
	target: String,
	source: Box<TypeSpecification>,
}

pub struct ContractDeclaration {
	name: String,
	parameters: HashMap<String, Box<TypeSpecification>>,
	body: Box<Block>,
}

pub struct Block {
	statement: Box<Statement>,
	next: Option<Box<Statement>>,
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