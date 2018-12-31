use std::collections::HashMap;
use super::lex::Token;

fn parse_tokens(tokens: Vec<Token>) -> Box<Block> {
	unimplemented!();
}

enum Expression {
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

struct Conditional {
	condition: Box<Expression>,
	true_op: Option<Box<Statement>>,
	false_op: Option<Box<Statement>>,
}

struct Loop {
	condition: Box<Expression>,
	body: Box<Statement>,
}

struct FunctionDeclaration { // we can allow closures-ish if this is a branch off expression
	name: String,
	parameters: HashMap<String, Box<TypeSpecification>>,
	return_type: Option<Box<TypeSpecification>>,
	body: Box<Block>,
}

struct TypeSpecification {
	base: String,
	contracts: Vec<Box<ContractInvocation>>,
}

struct ContractInvocation {
	contract: String,
	parameters: Vec<Box<Expression>>,
}

struct TypeAliasDeclaration {
	target: String,
	source: Box<TypeSpecification>,
}

struct ContractDeclaration {
	name: String,
	parameters: HashMap<String, Box<TypeSpecification>>,
	body: Box<Block>,
}

struct Block {
	statement: Box<Statement>,
	next: Option<Box<Statement>>,
}

enum Statement {
	Expression(Expression),
	Conditional(Conditional),
	Loop(Loop),
	ReturnStatement(Expression),
	FunctionDeclaration(FunctionDeclaration),
	TypeAliasDeclaration(TypeAliasDeclaration),
	ContractDeclaration(ContractDeclaration),
	Block(Block),
}