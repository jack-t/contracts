use regex::Regex;
use lazy_static::*;

#[derive(PartialEq, Clone, Debug)]
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
	FloatLiteral(f64),
	IntLiteral(u64),
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
				Box::new(lex_id),
				Box::new(lex_float_lit), // this comes before int_lit because the float regex is greedy
				Box::new(lex_int_lit),
				Box::new(lex_string_lit),
				Box::new(lex_char),
				lex_literal("\\+", Token::OpPlus),
				lex_literal("\\-", Token::OpMinus),
				lex_literal("\\*", Token::OpStar),
				lex_literal("/", Token::OpSlash),
				lex_literal("%", Token::OpPercent),
				lex_literal("==", Token::OpEquality),
				lex_literal("=", Token::OpAssign),
				lex_literal("!=", Token::OpInequality),
				lex_literal("<=", Token::OpLessOrEqual), // order matters with these tokens
				lex_literal("<", Token::OpLess),
				lex_literal(">=", Token::OpGreaterOrEqual),
				lex_literal(">", Token::OpGreater),
				lex_literal("\\(", Token::OpLeftParen),
				lex_literal("\\)", Token::OpRightParen),
				lex_literal(";", Token::SymSemicolon),
				lex_literal(",", Token::SymComma),
				lex_literal("\\|", Token::SymPipe),
				lex_literal("\\]", Token::SymRightBracket),
				lex_literal("\\[", Token::SymLeftBracket),

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

fn lex_literal(lit: &'static str, tok: Token) -> Box<TokenProducer> {
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
					Some((Token::Char(ch), mat.end() + 1))
				}, // + 1 to account for the end quote
			_ => unreachable!() // because it captured something and there's only one capture group
		}
	} else {
		None
	}
}

#[cfg(test)]
mod tests {
	mod keywords {
		use super::super::*;
		#[test]
		fn it_gets_key_type() {
			
			let code = " type ";
			let lexer = Lexer::new();
			let results = lexer.lex(code);

			assert_eq!(results, vec![Token::KeyType]);

		}
		#[test]
		fn it_gets_id() {
			
			let code = " type _x1yz ";
			let lexer = Lexer::new();
			let results = lexer.lex(code);

			assert_eq!(results, vec![Token::KeyType, Token::Id("_x1yz".to_string())]);

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

			assert_eq!(results, vec![Token::IntLiteral(1), Token::OpPlus, Token::IntLiteral(2)]);

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

			assert_eq!(results, vec![Token::Char('\t'), Token::Id("a".to_string()), Token::Char('b')]);

		}

		// I didn't want to clutter the list of tests with things that (I think?) will rarely go wrong.
		#[test]
		fn it_gets_ops() {
			{
				let code = r#" + "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpPlus]);
			}
			{
				let code = r#" - "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpMinus]);
			}
			{
				let code = r#" * "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpStar]);
			}
			{
				let code = r#" / "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpSlash]);
			}
			{
				let code = r#" % "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpPercent]);
			}
			{
				let code = r#" == "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpEquality]);
			}
			{
				let code = r#" = "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpAssign]);
			}
			{
				let code = r#" != "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpInequality]);
			}
			{
				let code = r#" < "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpLess]);
			}
			{
				let code = r#" <= "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpLessOrEqual]);
			}
			{
				let code = r#" > "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpGreater]);
			}
			{
				let code = r#" >= "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpGreaterOrEqual]);
			}
			{
				let code = r#" ( "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpLeftParen]);
			}
			{
				let code = r#" ) "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::OpRightParen]);
			}
			{
				let code = r#" ;  "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::SymSemicolon]);
			}
			{
				let code = r#" ,  "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::SymComma]);
			}
			{
				let code = r#" | "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::SymPipe]);
			}
			{
				let code = r#" ] "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::SymRightBracket]);
			}
			{
				let code = r#" [ "#;
				let lexer = Lexer::new();
				let results = lexer.lex(code);
				assert_eq!(results, vec![Token::SymLeftBracket]);
			}
		}
	}

}