pub mod tree;
pub use self::tree::*;
use super::lex::Token;
use std::collections::VecDeque;

fn parse(tokens: VecDeque<Token>) -> Statement {
    unimplemented!();
}

// must return -- panics if it has to
fn parse_expression(tokens: &mut VecDeque<Token>) -> Expression {
    match tokens.get(1) {
        Some(&Token::Equals) => parse_assignment(tokens),
        Some(_) => parse_rvalue(tokens),
        None => panic!("Parse error: Expected an expression, got end-of-stream."),
    }
}

fn parse_assignment(tokens: &mut VecDeque<Token>) -> Expression {
    let lv = build_var_ref(
        tokens
            .pop_front()
            .expect("Parse error: l-value can only be a variable reference."),
    );
    tokens.pop_front(); // == =
    let rv = parse_rvalue(tokens);
    Expression::Assignment {
        lvalue: Box::new(lv),
        rvalue: Box::new(rv),
    }
}

fn parse_rvalue(tokens: &mut VecDeque<Token>) -> Expression {
    let initial = match tokens.pop_front() {
        Some(Token::IntLiteral(val)) => Expression::Int { value: val },
        Some(Token::Id(name)) => parse_id_expr(name, tokens),
        _ => unimplemented!(),
    };

    if (is_bin_op(tokens.front())) {
        let op = tokens
            .pop_front()
            .expect("Binary operations need a binary operation");
        let rhs = parse_rvalue(tokens);
        return Expression::Binary {
            op: op,
            lhs: Box::new(initial),
            rhs: Box::new(rhs),
        };
    } else {
        return initial;
    }
}

fn parse_id_expr(name: String, tokens: &mut VecDeque<Token>) -> Expression {
    match tokens.front() {
        Some(&Token::LeftParen) => unimplemented!(),
        _ => Expression::Variable { name: name },
    }
}

fn build_var_ref(tok: Token) -> Expression {
    match tok {
        Token::Id(name) => Expression::Variable { name: name },
        _ => panic!("Parse error: variable reference must be an identifier"),
    }
}

fn is_bin_op(tok: Option<&Token>) -> bool {
    match tok {
        Some(Token::Plus) => true,
        Some(Token::Minus) => true,
        Some(Token::Star) => true,
        Some(Token::Slash) => true,
        Some(Token::Percent) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_gets_int_assignment() {
        let mut toks = VecDeque::from(vec![
            Token::Id("a".to_string()),
            Token::Equals,
            Token::IntLiteral(5),
        ]);
        assert_eq!(
            parse_assignment(&mut toks),
            Expression::Assignment {
                lvalue: Box::new(Expression::Variable {
                    name: "a".to_string()
                }),
                rvalue: Box::new(Expression::Int { value: 5 }),
            }
        );
        assert_eq!(toks, VecDeque::new());
    }
    #[test]
    fn it_gets_id_assignment() {
        let mut toks = VecDeque::from(vec![
            Token::Id("a".to_string()),
            Token::Equals,
            Token::Id("a".to_string()),
        ]);
        assert_eq!(
            parse_assignment(&mut toks),
            Expression::Assignment {
                lvalue: Box::new(Expression::Variable {
                    name: "a".to_string()
                }),
                rvalue: Box::new(Expression::Variable {
                    name: "a".to_string()
                }),
            }
        );
        assert_eq!(toks, VecDeque::new());
    }

    #[test]
    fn it_gets_sequential_arith() {
        let mut toks = VecDeque::from(vec![
            Token::Id("a".to_string()),
            Token::Star,
            Token::Id("a".to_string()),
            Token::Star,
            Token::Id("a".to_string()),
        ]);
        assert_eq!(
            parse_rvalue(&mut toks),
            Expression::Binary {
                op: Token::Star,
                lhs: Box::new(Expression::Variable {
                    name: "a".to_string()
                }),
                rhs: Box::new(Expression::Binary {
                    op: Token::Star,
                    lhs: Box::new(Expression::Variable {
                        name: "a".to_string()
                    }),
                    rhs: Box::new(Expression::Variable {
                        name: "a".to_string()
                    }),
                }),
            }
        );
        assert_eq!(toks, VecDeque::new());
    }

}
