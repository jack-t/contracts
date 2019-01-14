pub mod tree;
pub use self::tree::*;
use super::lex::Token;
use std::collections::VecDeque;

fn parse_tokens(mut tokens: VecDeque<Token>) -> Option<Box<Statement>> {
    parse_block(&mut tokens)
}

// each of the called functions will consume its own terminal -- whether } or ;
fn parse_statement(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    if let Some(tok) = tokens.front() {
        match tok {
            Token::Id(_) | Token::LeftParen => parse_expression_statement(tokens),
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

fn parse_expression_statement(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    match parse_expression(tokens) {
        Some(expr) => Some(Box::new(Statement::Expression(expr))),
        None => None,
    }
}

fn parse_expression(tokens: &mut VecDeque<Token>) -> Option<Expression> {
    let tok = tokens.pop_front();
    let initial = match tok {
        Some(Token::Id(name)) => parse_id_expr(name, tokens),
        Some(Token::IntLiteral(val)) => Expression::IntLiteral(val),
        Some(Token::FloatLiteral(val)) => Expression::FloatLiteral(val),
        Some(Token::StringLiteral(val)) => Expression::StringLiteral(val),
        Some(Token::CharLiteral(val)) => Expression::CharLiteral(val),
        _ => panic!(
            "Expected expression-initial token at code starting {:?}",
            tokens
        ),
    };

    match tokens.front() {
        Some(&Token::RightParen) | Some(&Token::Semicolon) | Some(&Token::Comma) => {
            tokens.pop_front();
            Some(initial)
        }
        Some(&Token::Plus) | Some(&Token::Minus) | Some(&Token::Star) | Some(&Token::Slash) => {
            let got = tokens
                .pop_front()
                .expect("Got an op that doesn't make sense, I guess");
            Some(Expression::Binary {
                op: got,
                lhs: Box::new(initial),
                rhs: Box::new(
                    parse_expression(tokens).expect("Binary expression expected a right operand"),
                ),
            })
        }
        x @ _ => panic!("{:?} is not a valid expression terminal", x),
    }
}

fn parse_id_expr(name: String, tokens: &mut VecDeque<Token>) -> Expression {
    let next = match tokens.front() {
        Some(x) => Some(x.clone()),
        None => None,
    };

    match next {
        Some(Token::LeftParen) => {
            next_tok_is(tokens, Token::LeftParen);
            let mut p = Vec::new();
            while tokens.front() != Some(&Token::RightParen) {
                p.push(Box::new(
                    parse_expression(tokens)
                        .expect("Parsing parameter didn't yield valid expression"),
                ));
            }
            p.reverse();
            Expression::FunctionCall {
                function_name: name,
                parameters: p,
            }
        }
        _ => Expression::VariableReference(name),
    }
}
fn parse_conditional(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    next_tok_is(tokens, Token::If);
    next_tok_is(tokens, Token::LeftParen);

    let condition = parse_expression(tokens);
    // parse_expression consumes its right paren

    let true_block = parse_statement(tokens);

    let false_block = if tokens.front() == Some(&Token::Else) {
        parse_statement(tokens)
    } else {
        None
    };

    Some(Box::new(Statement::Conditional(Conditional {
        condition: Box::new(condition.expect("A conditional has to have a condition")),
        true_op: true_block.expect("A conditional has to have a true block"),
        false_op: false_block,
    })))
}
fn parse_loop(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    next_tok_is(tokens, Token::While);

    let condition = parse_expression(tokens);

    let block = parse_statement(tokens);

    Some(Box::new(Statement::Loop(Loop {
        condition: Box::new(condition.expect("Loops need conditions")),
        body: block.expect("Loops need bodies"),
    })))
}
fn parse_return(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    next_tok_is(tokens, Token::Return);

    Some(Box::new(Statement::Return(
        parse_expression(tokens).expect("Return statements need things to return"),
    )))
}
fn parse_fn_decl(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    unimplemented!();
}
fn parse_type_alias_decl(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    unimplemented!();
}
fn parse_contract(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    unimplemented!();
}
fn parse_block(tokens: &mut VecDeque<Token>) -> Option<Box<Statement>> {
    let statement = parse_statement(tokens);
    match statement {
        Some(bs) => Some(Box::new(Statement::Block(Block {
            statement: bs,
            next: parse_statement(tokens),
        }))),
        None => None,
    }
    // I think I need to require a } here?

}

fn next_tok_is(tokens: &mut VecDeque<Token>, tok: Token) {
    match tokens.pop_front() {
        Some(ref next) if next == &tok => (),
        next @ _ => panic!("Parse error: Expected {:?}, got {:?}", tok, next),
    }
}

fn is_operator(tok: &Token) -> bool {
    match tok {
        Token::Plus | Token::Minus | Token::Star | Token::Slash => true,
        _ => false,
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
