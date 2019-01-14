use super::super::lex::Token;
use super::tree::Expression;
use std::collections::VecDeque;

pub fn parse_expression(tokens: &mut VecDeque<Token>) -> Option<Expression> {
	// algorithm: start with a backbone, then do a series of rotations until it's right


	None
}

fn get_backbone(tokens: &mut VecDeque<Token>) -> Expression {
	let tok = tokens.pop_front();
	let ll = tokens.front();

	match ll {
		Some(eq @ &Token::Equals) => {
			let eq = eq.clone();
			tokens.pop_front();
			Expression::Assignment {
				op: eq.clone(),
				lhs: Box::new(parse_nbexpr(&mut VecDeque::from(vec![eq.clone()]))),
				rhs: Box::new(parse_expression(tokens).expect("rhs of assignment must be expression"))
			}
		},
		_ => panic!()
	}
}

// parse non-binary expression
fn parse_nbexpr(tokens: &mut VecDeque<Token>) -> Expression {
	match tokens.front() {
		Some(&Token::Id(_)) => parse_id_expr(tokens),
		Some(&Token::FloatLiteral(val)) => {
			tokens.pop_front();
			Expression::FloatLiteral(val)
		},
		Some(&Token::IntLiteral(val)) => {
			tokens.pop_front();
			Expression::IntLiteral(val)
		},
		Some(&Token::StringLiteral(ref val)) => {
			let val = val.clone();
			tokens.pop_front();
			Expression::StringLiteral(val)
		},
		Some(&Token::CharLiteral(val)) => {
			tokens.pop_front();
			Expression::CharLiteral(val)
		},
		_ => panic!("")
	}
}

fn parse_id_expr(tokens: &mut VecDeque<Token>) -> Expression {
	let id = tokens.pop_front();
	let ll = tokens.front().clone();
	if let Some(Token::Id(id)) = id {
		match ll {
			Some(&Token::LeftParen) => {
				unimplemented!("Haven't implemented function calls")
			},
			_ => Expression::VariableReference(id)
		}
	} else {
		panic!("to parse an id, you need an id")
	}
}
