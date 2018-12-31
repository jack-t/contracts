pub mod tree;
use core::iter::Peekable;
pub use self::tree::*;
use super::lex::Token;

fn parse_tokens(mut tokens: Vec<Token>) -> Option<Box<Block>> {

	if tokens.is_empty() {
		return None;
	}

	let mut root = Box::new(Block{
		statement: parse_statement(&mut tokens).unwrap(),
		next: parse_statement(&mut tokens),
	});

	unimplemented!();
}

fn parse_statement(tokens: &mut Vec<Token>) -> Option<Box<Statement>> {
	let iter = tokens.iter().peekable();
	// now, a big match to figure out what kind of statement we've got 
}