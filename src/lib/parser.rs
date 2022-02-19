extern crate core;

use core::panicking::panic;
use std::iter::Peekable;

use super::ast::{Comparison, Equality, Expression, Factor, Primary, Term, Unary};
use super::scanner::{ReservedWord, Token};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    EndOfTokens,
    UnclosedParen,
    ExpectedStartOfPrimary(Token),
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Expression>, Vec<ParseError>> {
    let tokens = &mut tokens.into_iter().peekable();
    let mut expressions = vec![];
    let mut errors = vec![];
    while tokens.peek().is_some() && tokens.peek() != Some(&Token::EOF) {
        match expr(tokens) {
            Ok(expr) => expressions.push(expr),
            Err(err) => {
                errors.push(err);
                loop {
                    match tokens.next() {
                        None | Some(Token::SemiColon(_) | Token::EOF) => break,
                        Some(_) => continue,
                    }
                }
            }
        };
    }
    if errors.is_empty() {
        Ok(expressions)
    } else {
        Err(errors)
    }
}

fn expr(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, ParseError> {
    Ok(Expression::Equality(equality(tokens)?))
}

fn equality(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Equality, ParseError> {
    let lhs = comparison(tokens)?;
    match tokens.peek() {
        Some(Token::BangEqual(_)) => {
            tokens.next();
            let rhs = comparison(tokens)?;
            Ok(Equality::BangEqual(lhs, rhs))
        }
        Some(Token::EqualEqual(_)) => {
            tokens.next();
            let rhs = comparison(tokens)?;
            Ok(Equality::EqualEqual(lhs, rhs))
        }
        _ => Ok(Equality::Comparison(lhs)),
    }
}

fn comparison(
    tokens: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Comparison, ParseError> {
    let lhs = term(tokens)?;
    match tokens.peek() {
        Some(Token::Greater(_)) => {
            tokens.next();
            let rhs = term(tokens)?;
            Ok(Comparison::Greater(lhs, rhs))
        }
        Some(Token::Less(_)) => {
            tokens.next();
            let rhs = term(tokens)?;
            Ok(Comparison::Less(lhs, rhs))
        }
        Some(Token::GreaterEqual(_)) => {
            tokens.next();
            let rhs = term(tokens)?;
            Ok(Comparison::GreaterEqual(lhs, rhs))
        }
        Some(Token::LessEqual(_)) => {
            tokens.next();
            let rhs = term(tokens)?;
            Ok(Comparison::LessEqual(lhs, rhs))
        }
        _ => Ok(Comparison::Term(lhs)),
    }
}

fn term(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Term, ParseError> {
    let lhs = factor(tokens)?;
    match tokens.peek() {
        Some(Token::Plus(_)) => {
            tokens.next();
            let rhs = factor(tokens)?;
            Ok(Term::Plus(lhs, rhs))
        }
        Some(Token::Minus(_)) => {
            tokens.next();
            let rhs = factor(tokens)?;
            Ok(Term::Minus(lhs, rhs))
        }
        _ => Ok(Term::Factor(lhs)),
    }
}

fn factor(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Factor, ParseError> {
    let lhs = unary(tokens)?;
    match tokens.peek() {
        Some(Token::ForwardSlash(_)) => {
            tokens.next();
            let rhs = unary(tokens)?;
            Ok(Factor::Divide(lhs, rhs))
        }
        Some(Token::Star(_)) => {
            tokens.next();
            let rhs = unary(tokens)?;
            Ok(Factor::Multiply(lhs, rhs))
        }
        _ => Ok(Factor::Unary(lhs)),
    }
}

fn unary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Unary, ParseError> {
    match tokens.peek() {
        Some(Token::Minus(_)) => {
            tokens.next();
            Ok(Unary::Minus(Box::new(unary(tokens)?)))
        }
        Some(Token::Bang(_)) => {
            tokens.next();
            Ok(Unary::Bang(Box::new(unary(tokens)?)))
        }
        _ => Ok(Unary::Primary(primary(tokens)?)),
    }
}

fn primary(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Primary, ParseError> {
    match tokens.next() {
        None => Err(ParseError::EndOfTokens),
        Some(Token::ReservedWord(_, ReservedWord::TRUE)) => Ok(Primary::True),
        Some(Token::ReservedWord(_, ReservedWord::FALSE)) => Ok(Primary::False),
        Some(Token::ReservedWord(_, ReservedWord::NIL)) => Ok(Primary::Nil),
        Some(Token::NumericLiteral(_, number)) => Ok(Primary::Number(number)),
        Some(Token::StringLiteral(_, string)) => Ok(Primary::String(string)),
        Some(Token::OpenParen(_)) => {
            let expression = expr(tokens)?;
            match tokens.next() {
                Some(Token::CloseParen(_)) => Ok(Primary::Expression(Box::new(expression))),
                _ => Err(ParseError::UnclosedParen),
            }
        }
        Some(Token::SemiColon(_)) => {
            if matches!(tokens.peek(), Some(Token::EOF)) {
                todo!("this should not error but does")
            } else {
                let expression = expr(tokens)?;
                Ok(Primary::Expression(Box::new(expression)))
            }
        }
        Some(token) => Err(ParseError::ExpectedStartOfPrimary(token)),
    }
}

#[cfg(test)]
mod tests {
    use crate::lib::scanner::ScanState;

    use super::*;

    #[test]
    fn primary_string() {
        let tokens = &mut vec![Token::StringLiteral(
            ScanState::default(),
            "string".to_string(),
        )]
        .into_iter()
        .peekable();
        assert_eq!(primary(tokens), Ok(Primary::String("string".to_string())));
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn neg_primary_number() {
        let tokens = &mut vec![
            Token::Bang(ScanState::default()),
            Token::NumericLiteral(ScanState::default(), 1.12),
        ]
        .into_iter()
        .peekable();
        assert_eq!(
            unary(tokens),
            Ok(Unary::Bang(Box::new(Unary::Primary(Primary::Number(1.12)))))
        );
        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn simple_expr() {
        let tokens = vec![
            Token::NumericLiteral(ScanState::default(), 0.0),
            Token::EqualEqual(ScanState::default()),
            Token::NumericLiteral(ScanState::default(), 1.12),
            Token::NumericLiteral(ScanState::default(), 0.0),
            Token::EqualEqual(ScanState::default()),
            Token::NumericLiteral(ScanState::default(), 1.12),
        ];
        assert_eq!(
            parse(tokens),
            Ok(vec![Expression::Equality(Equality::EqualEqual(
                Comparison::Term(Term::Factor(Factor::Unary(Unary::Primary(
                    Primary::Number(0.0)
                )))),
                Comparison::Term(Term::Factor(Factor::Unary(Unary::Primary(
                    Primary::Number(1.12)
                ))))
            )),])
        );
    }
}
