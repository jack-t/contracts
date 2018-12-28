use regex::Regex;

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

type TokenProducer = Fn(&mut str) -> Option<(Token, usize)>;

pub struct Lexer {
	producers: Vec<Box<TokenProducer>>
}

impl Lexer {
	pub fn new() -> Lexer {
		Lexer {
			producers: vec![lex_literal("type", Token::KeyType)],
		}
	}
}

fn lex_literal(lit: &str, tok: Token) -> Box<TokenProducer> {
	Box::new(move |code: &mut str| {
		let regex = Regex::new((("^\s*(?P<cap>".to_string() + lit).to_string() + ">.*").as_str()).unwrap();
		if let Some(captures) = regex.captures(code) {
			match captures.name("cap") {
				Some(cap) => Some((tok, cap.as_str().len())),
				_ => unreachable!()
			}
		} else {
			None
		}
	})
}