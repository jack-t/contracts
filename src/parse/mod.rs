pub mod tree;
pub use self::tree::*;
use super::lex::Token;
use std::collections::VecDeque;

fn parse(mut tokens: VecDeque<Token>) -> Statement {
    tokens.push_front(Token::LeftBrace);
    tokens.push_back(Token::RightBrace);

    parse_statement(&mut tokens)
}

fn parse_statement(tokens: &mut VecDeque<Token>) -> Statement {
    match tokens.front() {
        Some(&Token::Id(_)) => parse_statement_expr(tokens),
        Some(&Token::LeftBrace) => parse_block(tokens),
        Some(&Token::If) => parse_if(tokens),
        Some(&Token::Fn) => parse_fn_decl(tokens),
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

    let block = Statement::Block {
        code: parse_series(tokens),
    };

    next_tok_is(tokens, Token::RightBrace);

    block
}

fn parse_series(tokens: &mut VecDeque<Token>) -> Vec<Statement> {
    let mut statements = Vec::new();
    loop {
        match tokens.front() {
            Some(&Token::RightBrace) => break,
            _ => statements.push(parse_statement(tokens)),
        }
    }
    statements
}

fn parse_fn_decl(tokens: &mut VecDeque<Token>) -> Statement {
    next_tok_is(tokens, Token::Fn);

    let name = match tokens.pop_front() {
        Some(Token::Id(name)) => name,
        _ => panic!("Parse error: Function decl needs a function id"),
    };

    next_tok_is(tokens, Token::LeftParen);

    let mut p_names = Vec::new();
    let mut expect_id = false;
    loop {
        if expect_id {
            match tokens.pop_front() {
                Some(Token::Id(name)) => {
                    p_names.push(name);
                    expect_id = false;
                }
                Some(Token::RightParen) => break,
                _ => panic!("Parse error: after a left paren or a comma, arg list must have an id or a right paren"),
            }
        } else {
            match tokens.pop_front() {
                Some(Token::Comma) => {
                    expect_id = true;
                    continue
                }
                Some(Token::RightParen) => break,
                _ => panic!("Parse error: expecting a comma in arg list, has to be either a comma or a right paren"),
            }
        }
    }

    let block = parse_statement(tokens); // this means that fn abc(); is a valid no-op function

    Statement::FuncDecl {
        name: name,
        params: p_names,
        block: Box::new(block),
    }
}

fn parse_if(tokens: &mut VecDeque<Token>) -> Statement {
    next_tok_is(tokens, Token::If);
    next_tok_is(tokens, Token::LeftParen);

    let condition = Box::new(parse_expression(tokens));

    next_tok_is(tokens, Token::RightParen);

    let true_statement = Box::new(parse_statement(tokens));

    let false_statement = match tokens.front() {
        Some(&Token::Else) => {
            tokens.pop_front();
            Some(Box::new(parse_statement(tokens)))
        }
        _ => None,
    };

    Statement::Conditional {
        condition,
        true_statement,
        false_statement,
    }
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
        x @ _ => unimplemented!("Parse error: {:?} cannot be rvalue-initial", x),
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
        Some(&Token::LeftParen) => {
            next_tok_is(tokens, Token::LeftParen);

            let mut params = Vec::new();
            let mut expect_expr = true;
            loop {
                if (expect_expr) {
                    match tokens.front() {
                        Some(&Token::RightParen) => break,
                        _ => {
                            expect_expr = false;
                            params.push(Box::new(parse_expression(tokens)))
                        }
                    }
                } else {
                    match tokens.front() {
                        Some(&Token::RightParen) => break,
                        Some(&Token::Comma) => {
                            expect_expr = true;
                            tokens.pop_front();
                            continue;
                        }
                        x => panic!("Parse error: {:?} is not a comma or a right paren", x),
                    }
                }
            }

            next_tok_is(tokens, Token::RightParen);
            Expression::FunctionCall {
                func: name,
                params: params,
            }
        }
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
            Statement::Block {
                code: vec![Statement::Expression {
                    expression: Box::new(Expression::Assignment {
                        lvalue: Box::new(Expression::Variable {
                            name: "a".to_string()
                        }),
                        rvalue: Box::new(Expression::Int { value: 5 }),
                    })
                }]
            },
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
            Token::RightBrace,
        ]);

        let results = parse_statement(&mut toks);

        assert_eq!(
            results,
            Statement::Block {
                code: vec![Statement::Expression {
                    expression: Box::new(Expression::Assignment {
                        lvalue: Box::new(Expression::Variable {
                            name: "a".to_string()
                        }),
                        rvalue: Box::new(Expression::Variable {
                            name: "a".to_string()
                        }),
                    })
                }]
            }
        );
    }

    #[test]
    fn it_gets_a_long_block() {
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
            Token::Id("a".to_string()),
            Token::Equals,
            Token::Id("a".to_string()),
            Token::Semicolon,
            Token::RightBrace,
        ]);

        let results = parse_statement(&mut toks);

        assert_eq!(
            results,
            Statement::Block {
                code: vec![
                    Statement::Expression {
                        expression: Box::new(Expression::Assignment {
                            lvalue: Box::new(Expression::Variable {
                                name: "a".to_string()
                            }),
                            rvalue: Box::new(Expression::Variable {
                                name: "a".to_string()
                            }),
                        })
                    },
                    Statement::Expression {
                        expression: Box::new(Expression::Assignment {
                            lvalue: Box::new(Expression::Variable {
                                name: "a".to_string()
                            }),
                            rvalue: Box::new(Expression::Variable {
                                name: "a".to_string()
                            }),
                        })
                    },
                    Statement::Expression {
                        expression: Box::new(Expression::Assignment {
                            lvalue: Box::new(Expression::Variable {
                                name: "a".to_string()
                            }),
                            rvalue: Box::new(Expression::Variable {
                                name: "a".to_string()
                            }),
                        })
                    }
                ]
            }
        );
    }

    #[test]
    fn it_gets_an_empty_block() {
        let mut toks = VecDeque::from(vec![Token::LeftBrace, Token::RightBrace]);

        let results = parse_statement(&mut toks);

        assert_eq!(results, Statement::Block { code: vec![] });
    }

    #[test]
    fn it_gets_a_noop_block() {
        let mut toks = VecDeque::from(vec![Token::LeftBrace, Token::Semicolon, Token::RightBrace]);

        let results = parse_statement(&mut toks);

        assert_eq!(
            results,
            Statement::Block {
                code: vec![Statement::NoOp]
            }
        );
    }

    #[test]
    fn parse_gets_empty_blocks() {
        let mut toks = VecDeque::from(vec![]);
        let results = parse(toks);

        assert_eq!(results, Statement::Block { code: vec![] });
    }
    #[test]
    fn parse_gets_noop_blocks() {
        let mut toks = VecDeque::from(vec![Token::Semicolon]);
        let results = parse(toks);

        assert_eq!(
            results,
            Statement::Block {
                code: vec![Statement::NoOp]
            }
        );
    }

    #[test]
    fn it_gets_no_params() {
        let mut toks = VecDeque::from(vec![
            Token::Id("abc".to_string()),
            Token::LeftParen,
            Token::RightParen,
        ]);
        let results = parse_expression(&mut toks);

        assert_eq!(
            results,
            Expression::FunctionCall {
                func: "abc".to_string(),
                params: vec![]
            }
        );
    }
    #[test]
    fn it_gets_params() {
        let mut toks = VecDeque::from(vec![
            Token::Id("abc".to_string()),
            Token::LeftParen,
            Token::IntLiteral(5),
            Token::Comma,
            Token::Id("def".to_string()),
            Token::RightParen,
        ]);
        let results = parse_expression(&mut toks);

        assert_eq!(
            results,
            Expression::FunctionCall {
                func: "abc".to_string(),
                params: vec![
                    Box::new(Expression::Int { value: 5 }),
                    Box::new(Expression::Variable {
                        name: "def".to_string()
                    })
                ]
            }
        );
    }

    #[test]
    fn it_gets_an_empty_fn_decl() {
        let mut toks = VecDeque::from(vec![
            Token::Fn,
            Token::Id("abc".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Semicolon,
        ]);
        let results = parse_statement(&mut toks);

        assert_eq!(
            results,
            Statement::FuncDecl {
                name: "abc".to_string(),
                params: vec![],
                block: Box::new(Statement::NoOp),
            }
        );
    }

    #[test]
    fn it_gets_a_real_fn_decl() {
        let mut toks = VecDeque::from(vec![
            Token::Fn,
            Token::Id("abc".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBrace,
            Token::Id("abc".to_string()),
            Token::LeftParen,
            Token::IntLiteral(5),
            Token::Comma,
            Token::Id("def".to_string()),
            Token::RightParen,
            Token::Semicolon,
            Token::RightBrace,
        ]);
        let results = parse_statement(&mut toks);

        assert_eq!(
            results,
            Statement::FuncDecl {
                name: "abc".to_string(),
                params: vec![],
                block: Box::new(Statement::Block {
                    code: vec![Statement::Expression {
                        expression: Box::new(Expression::FunctionCall {
                            func: "abc".to_string(),
                            params: vec![
                                Box::new(Expression::Int { value: 5 }),
                                Box::new(Expression::Variable {
                                    name: "def".to_string()
                                })
                            ],
                        })
                    }]
                }),
            }
        );
    }

    #[test]
    fn it_gets_an_if() {
        let mut toks = VecDeque::from(vec![
            Token::If,
            Token::LeftParen,
            Token::Id("abc".to_string()),
            Token::RightParen,
            Token::Semicolon,
        ]);
        let results = parse(toks);

        assert_eq!(
            results,
            Statement::Block {
                code: vec![Statement::Conditional {
                    condition: Box::new(Expression::Variable {
                        name: "abc".to_string()
                    }),
                    true_statement: Box::new(Statement::NoOp),
                    false_statement: None,
                }]
            }
        );
    }
    #[test]
    fn it_gets_an_else() {
        let mut toks = VecDeque::from(vec![
            Token::If,
            Token::LeftParen,
            Token::Id("abc".to_string()),
            Token::RightParen,
            Token::LeftBrace,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::RightBrace,
        ]);
        let results = parse(toks);

        assert_eq!(
            results,
            Statement::Block {
                code: vec![Statement::Conditional {
                    condition: Box::new(Expression::Variable {
                        name: "abc".to_string()
                    }),
                    true_statement: Box::new(Statement::Block { code: vec![] }),
                    false_statement: Some(Box::new(Statement::Block { code: vec![] })),
                }]
            }
        );
    }
}
