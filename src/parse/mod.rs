pub mod tree;
pub use self::tree::*;
use super::lex::Token;

fn parse_tokens(mut tokens: Vec<Token>) -> Option<Box<Block>> {

	if tokens.is_empty() {
		return None;
	}

	let mut root = Box::new(Block{
		statement: parse_statement(&mut tokens).expect("Could not parse initial tokens."),
		next: parse_statement(&mut tokens),
	});

	unimplemented!();
}

fn parse_statement(tokens: &mut Vec<Token>) -> Option<Box<Statement>> {
		if let Some(tok) = tokens.first() {
			match tok {
				Token::Id(_) | Token::LeftParen => parse_expression(tokens),
				Token::If => parse_conditional(tokens),
				Token::While => parse_loop(tokens),
				Token::Return => parse_return(tokens),
				Token::Fn => parse_fn_decl(tokens),
				Token::Type => parse_type_alias_decl(tokens),
				Token::Contract => parse_contract(tokens),
				Token::LeftBrace => parse_block(tokens),
				_ => panic!("Unexpected {:?}; expected statement-initial token.", tok)
			}
		} else {
			None
		}
}

fn parse_expression(tokens: &mut Vec<Token>) -> Option<Box<Statement>> {
	None
}
fn parse_conditional(tokens: &mut Vec<Token>) -> Option<Box<Statement>> {
	None
}
fn parse_loop(tokens: &mut Vec<Token>) -> Option<Box<Statement>> {
	None
}
fn parse_return(tokens: &mut Vec<Token>) -> Option<Box<Statement>> {
	None
}
fn parse_fn_decl(tokens: &mut Vec<Token>) -> Option<Box<Statement>> {
	None
}
fn parse_type_alias_decl(tokens: &mut Vec<Token>) -> Option<Box<Statement>> {
	unimplemented!();
}
fn parse_contract(tokens: &mut Vec<Token>) -> Option<Box<Statement>> {
	unimplemented!();
}
fn parse_block(tokens: &mut Vec<Token>) -> Option<Box<Statement>> {
	None
}