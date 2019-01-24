pub mod tree;
pub use self::tree::*;
use super::lex::Token;
use std::collections::VecDeque;

fn parse(mut tokens: VecDeque<Token>) -> Option<Statement> {
    Some(parse_statement(&mut tokens))
}

fn parse_statement(tokens: &mut VecDeque<Token>) -> Statement {
    match tokens.front() {
        Some(&Token::Id(_)) => parse_statement_expr(tokens),
        Some(&Token::LeftBrace) => parse_block(tokens),
        Some(&Token::Semicolon) => {
            tokens.pop_front();
            Statement::NoOp
        }
        x @ Some(_) => unimplemented!("Not implemented: {:?}", x),
        _ => panic!("Parse error: Statement-initial token required"),
    }
}

fn parse_statement_expr(tokens: &mut VecDeque<Token>) -> Statement {
    let expr = parse_expression(tokens);
    next_tok_is(tokens, Token::Semicolon);
    return Statement::Expression {
        expression: Box::new(expr),
    };
}

fn parse_block(tokens: &mut VecDeque<Token>) -> Statement {
    next_tok_is(tokens, Token::LeftBrace);

    let block = match tokens.front() {
        Some(&Token::RightBrace) => Statement::NoOp,
        _ => Statement::Block {
            statement: Box::new(parse_statement(tokens)),
            next: Box::new(parse_statement(tokens)),
        },
    };

    next_tok_is(tokens, Token::RightBrace);

    block
}

// must return -- panics if it has to
fn parse_expression(tokens: &mut VecDeque<Token>) -> Expression {
    match tokens.get(1) {
        Some(&Token::Equals) => parse_assignment(tokens),
        _ => match tokens.get(0) {
            Some(_) => parse_rvalue(tokens),
            None => panic!("Parse error: Expected an expression, got end-of-stream."),
        },
    }
}

fn parse_assignment(tokens: &mut VecDeque<Token>) -> Expression {
    let lv = build_var_ref(
        tokens
            .pop_front()
            .expect("Parse error: l-value can only be a variable reference."),
    );

    next_tok_is(tokens, Token::Equals);

    Expression::Assignment {
        lvalue: Box::new(lv),
        rvalue: Box::new(parse_rvalue(tokens)),
    }
}

fn parse_rvalue(tokens: &mut VecDeque<Token>) -> Expression {
    let initial = match tokens.pop_front() {
        Some(Token::IntLiteral(val)) => Expression::Int { value: val },
        Some(Token::Id(name)) => parse_id_expr(name, tokens),
        _ => unimplemented!(),
    };

    // a smarter version of this would allow for nested assignments
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
    fn it_gets_int_assignment() {
        let mut toks = VecDeque::from(vec![
            Token::Id("a".to_string()),
            Token::Equals,
            Token::IntLiteral(5),
        ]);
        assert_eq!(
            parse_expression(&mut toks),
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
    fn it_gets_int_assignment_as_statement() {
        let mut toks = VecDeque::from(vec![
            Token::Id("a".to_string()),
            Token::Equals,
            Token::IntLiteral(5),
            Token::Semicolon,
        ]);
        assert_eq!(
            parse(toks),
            Some(Statement::Expression {
                expression: Box::new(Expression::Assignment {
                    lvalue: Box::new(Expression::Variable {
                        name: "a".to_string()
                    }),
                    rvalue: Box::new(Expression::Int { value: 5 }),
                })
            }),
        );
    }
    #[test]
    fn it_gets_id_assignment() {
        let mut toks = VecDeque::from(vec![
            Token::Id("a".to_string()),
            Token::Equals,
            Token::Id("a".to_string()),
        ]);
        assert_eq!(
            parse_expression(&mut toks),
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
            parse_expression(&mut toks),
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
    #[test]
    fn it_gets_assignment_and_arith() {
        let mut toks = VecDeque::from(vec![
            Token::Id("a".to_string()),
            Token::Equals,
            Token::Id("a".to_string()),
            Token::Star,
            Token::Id("a".to_string()),
            Token::Star,
            Token::Id("a".to_string()),
        ]);
        assert_eq!(
            parse_expression(&mut toks),
            Expression::Assignment {
                lvalue: Box::new(Expression::Variable {
                    name: "a".to_string()
                }),
                rvalue: Box::new(Expression::Binary {
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
                }),
            }
        );
        assert_eq!(toks, VecDeque::new());
    }

    #[test]
    fn it_gets_a_block() {
        let mut toks = VecDeque::from(vec![
            Token::LeftBrace,
            Token::Id("a".to_string()),
            Token::Equals,
            Token::Id("a".to_string()),
            Token::Semicolon,
            Token::Id("a".to_string()),
            Token::Equals,
            Token::Id("a".to_string()),
            Token::Semicolon,
            Token::RightBrace,
        ]);

        assert_eq!(
            parse_statement(&mut toks),
            Statement::Block {
                statement: Box::new(Statement::Expression {
                    expression: Box::new(Expression::Assignment {
                        lvalue: Box::new(Expression::Variable {
                            name: "a".to_string()
                        }),
                        rvalue: Box::new(Expression::Variable {
                            name: "a".to_string()
                        }),
                    }),
                }),
                next: Box::new(Statement::Expression {
                    expression: Box::new(Expression::Assignment {
                        lvalue: Box::new(Expression::Variable {
                            name: "a".to_string()
                        }),
                        rvalue: Box::new(Expression::Variable {
                            name: "a".to_string()
                        }),
                    }),
                })
            }
        );

        assert_eq!(toks, VecDeque::new()); // should empty the list
    }

    #[test]
    #[should_panic]
    fn it_rejects_a_block_without_semis() {
        let mut toks = VecDeque::from(vec![
            Token::LeftBrace,
            Token::Id("a".to_string()),
            Token::Equals,
            Token::Id("a".to_string()),
            Token::Id("a".to_string()),
            Token::Equals,
            Token::Id("a".to_string()),
            Token::Semicolon,
            Token::RightBrace,
        ]);

        parse_statement(&mut toks);
    }
    #[test]
    #[should_panic]
    fn it_rejects_a_block_without_close_brace() {
        let mut toks = VecDeque::from(vec![
            Token::LeftBrace,
            Token::Id("a".to_string()),
            Token::Equals,
            Token::Id("a".to_string()),
            Token::Semicolon,
            Token::Id("a".to_string()),
            Token::Equals,
            Token::Id("a".to_string()),
            Token::Semicolon,
        ]);

        parse_statement(&mut toks);
    }
    #[test]
    fn it_gets_empty_block() {
        let mut toks = VecDeque::from(vec![Token::LeftBrace, Token::RightBrace]);

        assert_eq!(parse_statement(&mut toks), Statement::NoOp);
    }

    #[test]
    fn it_gets_empty_block_with_noop() {
        let mut toks = VecDeque::from(vec![Token::LeftBrace, Token::Semicolon, Token::RightBrace]);

        assert_eq!(
            parse_statement(&mut toks),
            Statement::Block {
                statement: Box::new(Statement::NoOp),
                next: Box::new(Statement::NoOp),
            }
        );
    }
}
