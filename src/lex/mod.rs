use regex::Regex;
use std::str::Chars;

#[derive(Clone)]
pub enum Token {
	KeyType,
	KeyFn,
	KeyFloat,
	KeyInt,
	KeyChar,
	KeyArray,
	KeyStruct,
	KeyContract,
	KeyReturn,
	KeyWhile,
	KeyIf,
	Id(String),
	Integer(u64),
	Float(f64),
	StringLiteral(String),
	Char(char),
	OpPlus,
	OpMinus,
	OpStar,
	OpSlash,
	OpPercent,
	OpEquality,
	OpAssign,
	OpInequality,
	OpLess,
	OpLessOrEqual,
	OpGreater,
	OpGreaterOrEqual,
	OpLeftParen,
	OpRightParen,
	SymDoubleQuote,
	SymSingleQuote,
	SymSemicolon,
	SymComma,
	SymPipe,
	SymLeftBracket,
	SymRightBracket,
}

type TokenProducer = Fn(&str) -> Option<(Token, usize)>;

pub struct Lexer {
	producers: Vec<Box<TokenProducer>>
}

impl Lexer {
	pub fn new() -> Lexer {
		Lexer {
			producers: vec![
				lex_literal("type", Token::KeyType),
				lex_literal("fn", Token::KeyFn),
				lex_literal("float", Token::KeyFloat),
				lex_literal("int", Token::KeyInt),
				lex_literal("char", Token::KeyChar),
				lex_literal("array", Token::KeyArray),
				lex_literal("struct", Token::KeyStruct),
				lex_literal("contract", Token::KeyContract),
				lex_literal("return", Token::KeyReturn),
				lex_literal("while", Token::KeyWhile),
				lex_literal("if", Token::KeyIf),
			],
		}
	}

	pub fn lex(&self, code: &mut str) -> Vec<Token> {
		let mut ret = Vec::new();
		let mut chars = code.chars();
		while code.len() > 0 {
			if let Some(tok) = self.tokenize(&mut chars) {
				ret.push(tok);
			} else {
				panic!("Error during tokenization, code starting here: {}", code);
			}
		}
		ret
	}

	fn tokenize(&self, code: &mut Chars) -> Option<Token> {
		for &prod in self.producers {
			if let Some((tok, offset)) = prod(code.as_str()) {
				code.skip(offset);
				return Some(tok);
			}
		}
		None
	}
}

fn lex_literal(lit: &'static str, tok: Token) -> Box<TokenProducer> {
	Box::new(move |code: &str| {
		let regex = Regex::new(((r"^\s*(?P<cap>".to_string() + lit).to_string() + ">.*").as_str()).unwrap();
		if let Some(captures) = regex.captures(code) {
			match captures.name("cap") {
				Some(cap) => Some((tok.clone(), cap.as_str().len())),
				_ => unreachable!()
			}
		} else {
			None
		}
	})
}

#[cfg(test)]
mod tests {

	mod keywords {
		#[test]
		fn it_gets_key_type() {

			let code = " type  xyz";

			

		}
	}

}