use regex::Regex;
use lazy_static::*;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
	Type,
	Fn,
	Float,
	Int,
	Char,
	Array,
	Struct,
	Contract,
	Return,
	While,
	If,
	Id(String),
	FloatLiteral(f64),
	IntLiteral(u64),
	StringLiteral(String),
	CharLiteral(char),
	Plus,
	Minus,
	Star,
	Slash,
	Percent,
	Equality,
	Assign,
	Inequality,
	Less,
	LessOrEqual,
	Greater,
	GreaterOrEqual,
	LeftParen,
	RightParen,
	Semicolon,
	Comma,
	Pipe,
	LeftBracket,
	RightBracket,
}

type TokenProducer = Fn(&str) -> Option<(Token, usize)>;

pub struct Lexer {
	producers: Vec<Box<TokenProducer>>
}

impl Lexer {
	pub fn new() -> Lexer {
		Lexer {
			producers: vec![
				lex_exactly("type", Token::Type),
				lex_exactly("fn", Token::Fn),
				lex_exactly("float", Token::Float),
				lex_exactly("int", Token::Int),
				lex_exactly("char", Token::Char),
				lex_exactly("array", Token::Array),
				lex_exactly("struct", Token::Struct),
				lex_exactly("contract", Token::Contract),
				lex_exactly("return", Token::Return),
				lex_exactly("while", Token::While),
				lex_exactly("if", Token::If),
				Box::new(lex_id),
				Box::new(lex_float_lit), // this comes before int_lit because the float regex is greedy
				Box::new(lex_int_lit),
				Box::new(lex_string_lit),
				Box::new(lex_char),
				lex_exactly("\\+", Token::Plus),
				lex_exactly("\\-", Token::Minus),
				lex_exactly("\\*", Token::Star),
				lex_exactly("/", Token::Slash),
				lex_exactly("%", Token::Percent),
				lex_exactly("==", Token::Equality),
				lex_exactly("=", Token::Assign),
				lex_exactly("!=", Token::Inequality),
				lex_exactly("<=", Token::LessOrEqual), // order matters with these tokens
				lex_exactly("<", Token::Less),
				lex_exactly(">=", Token::GreaterOrEqual),
				lex_exactly(">", Token::Greater),
				lex_exactly("\\(", Token::LeftParen),
				lex_exactly("\\)", Token::RightParen),
				lex_exactly(";", Token::Semicolon),
				lex_exactly(",", Token::Comma),
				lex_exactly("\\|", Token::Pipe),
				lex_exactly("\\]", Token::RightBracket),
				lex_exactly("\\[", Token::LeftBracket),

			],
		}
	}

	pub fn lex(&self, code: &str) -> Vec<Token> {
		let mut ret = Vec::new();
		let mut offset = 0;
		while (&code[offset..]).trim().len() > 0 {
			if let Some((tok, adv)) = self.tokenize(&code[offset..]) {
				ret.push(tok);
				offset += adv;
			} else {
				panic!("Couldn't tokenize code starting here: '{}'", &code[offset..]);
			}
		}
		ret
	}

	fn tokenize(&self, code: &str) -> Option<(Token, usize)> {
		for prod in &self.producers {
			if let Some(tuple) = prod(code) {
				return Some(tuple);
			}
		}
		None
	}
}

fn lex_exactly(lit: &'static str, tok: Token) -> Box<TokenProducer> {
	Box::new(move |code: &str| {
		let regex = Regex::new(((r"^\s*(?P<cap>".to_string() + lit).to_string() + ").*").as_str()).unwrap();
		if let Some(caps) = regex.captures(code) {
			match caps.name("cap") {
				Some(mat) => Some((tok.clone(), mat.end())),
				_ => unreachable!()
			}
		} else {
			None
		}
	})
}

fn lex_id(code: &str) -> Option<(Token, usize)> {
	lazy_static! {
		static ref RE: Regex = Regex::new(r"^\s*(?P<cap>_*[a-zA-Z]+\w*).*").unwrap();
	}

	if let Some(caps) = RE.captures(code) {
		match caps.name("cap") {
			Some(mat) => Some((Token::Id(mat.as_str().to_string()), mat.end())),
			_ => unreachable!() // because it captured something and there's only one capture group
		}
	} else {
		None
	}
}

fn lex_int_lit(code: &str) -> Option<(Token, usize)> {
	lazy_static! {
		static ref RE: Regex = Regex::new(r"^\s*(?P<cap>\d+)\b").unwrap();
	}

	if let Some(caps) = RE.captures(code) {
		match caps.name("cap") {
			Some(mat) => Some((Token::IntLiteral(mat.as_str().parse::<u64>().unwrap()), mat.end())),
			_ => unreachable!() // because it captured something and there's only one capture group
		}
	} else {
		None
	}
}

fn lex_float_lit(code: &str) -> Option<(Token, usize)> {
	lazy_static! {
		static ref RE: Regex = Regex::new(r"^\s*(?P<cap>\d+\.\d+).*").unwrap();
	}

	if let Some(caps) = RE.captures(code) {
		match caps.name("cap") {
			Some(mat) => Some((Token::FloatLiteral(mat.as_str().parse::<f64>().unwrap()), mat.end())),
			_ => unreachable!() // because it captured something and there's only one capture group
		}
	} else {
		None
	}
}

fn lex_string_lit(code: &str) -> Option<(Token, usize)> {
	lazy_static! {
		static ref RE: Regex = Regex::new(r#"^\s*"(?P<cap>(\\"|.*)*)".*"#).unwrap();
	}

	if let Some(caps) = RE.captures(code) {
		match caps.name("cap") {
			Some(mat) => {
				let string = mat.as_str().to_string().replace("\\\"", "\""); // handle the escape sequence
				Some((Token::StringLiteral(string), mat.end() + 1))
			}, // + 1 to account for the end quote
			_ => unreachable!() // because it captured something and there's only one capture group
		}
	} else {
		None
	}
}

fn lex_char(code: &str) -> Option<(Token, usize)> {
	lazy_static! {
		static ref RE: Regex = Regex::new(r#"^\s*'(?P<cap>.)'.*"#).unwrap();
	}

	if let Some(caps) = RE.captures(code) {
		match caps.name("cap") {
			Some(mat) => {
					let ch = mat.as_str().chars().next().unwrap();
					Some((Token::CharLiteral(ch), mat.end() + 1))
				}, // + 1 to account for the end quote
			_ => unreachable!() // because it captured something and there's only one capture group
		}
	} else {
		None
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn it_gets_key_type() {
		
		let code = " type ";
		let lexer = Lexer::new();
		let results = lexer.lex(code);

		assert_eq!(results, vec![Token::Type]);

	}
	#[test]
	fn it_gets_id() {
		
		let code = " type _x1yz ";
		let lexer = Lexer::new();
		let results = lexer.lex(code);

		assert_eq!(results, vec![Token::Type, Token::Id("_x1yz".to_string())]);

	}
	#[test]
	#[should_panic]
	fn it_rejects_ids_with_initial_numeral() {
		
		let code = " _1 ";
		let lexer = Lexer::new();
		lexer.lex(code);
	}

	#[test]
	#[should_panic]
	fn it_rejects_ids_with_initial_numeral_no_underscore() {
		
		let code = " 1a ";
		let lexer = Lexer::new();
		lexer.lex(code);
	}


	#[test]
	fn it_gets_short_id() {
		
		let code = " a ";
		let lexer = Lexer::new();
		let results = lexer.lex(code);

		assert_eq!(results, vec![Token::Id("a".to_string())]);

	}

	#[test]
	fn it_gets_int_ignores_octal() {
		
		let code = " 010 ";
		let lexer = Lexer::new();
		let results = lexer.lex(code);

		assert_eq!(results, vec![Token::IntLiteral(10)]);

	}

	#[test]
	#[should_panic]
	fn it_rejects_ints_with_letters() {
		let code = " 1b ";
		let lexer = Lexer::new();
		lexer.lex(code);
	}

	#[test]
	fn it_gets_packed_arith() {

		let code = " 1+2 ";
		let lexer = Lexer::new();
		let results = lexer.lex(code);

		assert_eq!(results, vec![Token::IntLiteral(1), Token::Plus, Token::IntLiteral(2)]);

	}

	#[test]
	fn it_gets_float() {
		
		let code = " 1.25 ";
		let lexer = Lexer::new();
		let results = lexer.lex(code);

		assert_eq!(results, vec![Token::FloatLiteral(1.25)]);

	}


	#[test]
	fn it_gets_strings_with_quotes() {
		
		let code = r#" "a\"'b\"c" def "#;
		let lexer = Lexer::new();
		let results = lexer.lex(code);

		assert_eq!(results, vec![Token::StringLiteral("a\"'b\"c".to_string()), Token::Id("def".to_string())]);

	}

	#[test]
	fn it_gets_chars() {
		
		let code = r#" '	' a 'b' "#; // it even gets raw tabs!
		let lexer = Lexer::new();
		let results = lexer.lex(code);

		assert_eq!(results, vec![Token::CharLiteral('\t'), Token::Id("a".to_string()), Token::CharLiteral('b')]);

	}

	// I didn't want to clutter the list of tests with things that (I think?) will rarely go wrong.
	#[test]
	fn it_gets_ops() {
		{
			let code = r#" + "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Plus]);
		}
		{
			let code = r#" - "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Minus]);
		}
		{
			let code = r#" * "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Star]);
		}
		{
			let code = r#" / "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Slash]);
		}
		{
			let code = r#" % "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Percent]);
		}
		{
			let code = r#" == "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Equality]);
		}
		{
			let code = r#" = "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Assign]);
		}
		{
			let code = r#" != "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Inequality]);
		}
		{
			let code = r#" < "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Less]);
		}
		{
			let code = r#" <= "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::LessOrEqual]);
		}
		{
			let code = r#" > "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Greater]);
		}
		{
			let code = r#" >= "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::GreaterOrEqual]);
		}
		{
			let code = r#" ( "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::LeftParen]);
		}
		{
			let code = r#" ) "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::RightParen]);
		}
		{
			let code = r#" ;  "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Semicolon]);
		}
		{
			let code = r#" ,  "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Comma]);
		}
		{
			let code = r#" | "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::Pipe]);
		}
		{
			let code = r#" ] "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::RightBracket]);
		}
		{
			let code = r#" [ "#;
			let lexer = Lexer::new();
			let results = lexer.lex(code);
			assert_eq!(results, vec![Token::LeftBracket]);
		}
	}

	#[test]
	fn it_handles_empty_string() {
		let code = "";
		let lexer = Lexer::new();
		let results = lexer.lex(code);

		assert_eq!(results, vec![]);
	}
	#[test]
	fn it_handles_whitespace_string() {
		let code = " 	 ";
		let lexer = Lexer::new();
		let results = lexer.lex(code);

		assert_eq!(results, vec![]);
	}

}