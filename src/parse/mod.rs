pub mod tree;
pub use self::tree::*;
use super::lex::Token;
use std::collections::VecDeque;

fn parse_tokens(mut tokens: VecDeque<Token>) -> Option<Box<Block>> {
    let initial = parse_statement(&mut tokens);

    if let Some(initial) = initial {
        Some(Box::new(Block {
            statement: initial,
            next: parse_statement(&mut tokens),
        }))
    } else {
        None
    }
}

fn parse_statement(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    if let Some(tok) = tokens.front() {
        match tok {
            Token::Id(_) | Token::LeftParen => parse_expression(tokens),
            Token::If => parse_conditional(tokens),
            Token::While => parse_loop(tokens),
            Token::Return => parse_return(tokens),
            Token::Fn => parse_fn_decl(tokens),
            Token::Type => parse_type_alias_decl(tokens),
            Token::Contract => parse_contract(tokens),
            Token::LeftBrace => parse_block(tokens),
            _ => panic!("Unexpected {:?}; expected statement-initial token.", tok),
        }
    } else {
        None
    }
}

fn parse_expression(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    None
}
fn parse_conditional(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    None
}
fn parse_loop(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    None
}
fn parse_return(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    None
}
fn parse_fn_decl(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    None
}
fn parse_type_alias_decl(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    unimplemented!();
}
fn parse_contract(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    unimplemented!();
}
fn parse_block(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    None
}

fn next_tok_is(tokens: &mut VecDeque<Token>, tok: Token) {
	match tokens.pop_front() {
		Some(ref next) if next == &tok => (),
		next @ _ => panic!("Parse error: Expected {:?}, got {:?}", tok, next),
	}
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_handles_empty_tok_stream() {
        let toks = VecDeque::new();
        let result = parse_tokens(toks);
        assert_eq!(result, None);
    }

    mod internal {
    	use super::super::*;
    	#[test]
    	fn test_assertion_utility_good() {
    		let mut toks = VecDeque::from(vec![Token::Id("abc".to_string()), Token::LeftParen]);
    		let result = next_tok_is(&mut toks, Token::Id("abc".to_string()));
    		assert_eq!(result, ());
    		assert_eq!(toks, vec![Token::LeftParen]);
    	}

    	#[test]
    	#[should_panic]
    	fn test_assertion_utility_bad() {
    		let mut toks = VecDeque::from(vec![Token::Id("abc".to_string()), Token::LeftParen]);
    		let result = next_tok_is(&mut toks, Token::LeftParen);
    	}
    }
}
