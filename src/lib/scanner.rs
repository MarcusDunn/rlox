use std::str::FromStr;

use ReservedWord::{
    AND, CLASS, ELSE, FALSE, FOR, FUN, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,
};
use ScanningErrorKind::{UnexpectedChar, UnexpectedEof};
use Token::{
    Bang, BangEqual, CloseBrace, CloseParen, Comma, Dot, Equal, EqualEqual, ForwardSlash, Greater,
    GreaterEqual, Identifier, Less, LessEqual, Minus, NumericLiteral, OpenBrace, OpenParen, Plus,
    ReservedWord as ReservedWordTok, SemiColon, Star, StringLiteral,
};

#[derive(Debug)]
pub struct ScanningError {
    scan_state: ScanState,
    kind: ScanningErrorKind,
}

#[derive(Debug)]
enum ScanningErrorKind {
    UnexpectedChar(char),
    UnexpectedEof(String),
}

#[derive(Debug, PartialEq)]
pub enum Token {
    EOF,
    OpenParen(ScanState),
    CloseParen(ScanState),
    OpenBrace(ScanState),
    CloseBrace(ScanState),
    Comma(ScanState),
    Dot(ScanState),
    Minus(ScanState),
    Plus(ScanState),
    SemiColon(ScanState),
    Star(ScanState),
    Bang(ScanState),
    BangEqual(ScanState),
    EqualEqual(ScanState),
    Equal(ScanState),
    GreaterEqual(ScanState),
    Greater(ScanState),
    LessEqual(ScanState),
    Less(ScanState),
    ForwardSlash(ScanState),
    StringLiteral(ScanState, String),
    NumericLiteral(ScanState, f64),
    ReservedWord(ScanState, ReservedWord),
    Identifier(ScanState, String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ReservedWord {
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
}

impl FromStr for ReservedWord {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "and" {
            Ok(AND)
        } else if s == "class" {
            Ok(CLASS)
        } else if s == "else" {
            Ok(ELSE)
        } else if s == "false" {
            Ok(FALSE)
        } else if s == "for" {
            Ok(FOR)
        } else if s == "fun" {
            Ok(FUN)
        } else if s == "if" {
            Ok(IF)
        } else if s == "nil" {
            Ok(NIL)
        } else if s == "or" {
            Ok(OR)
        } else if s == "print" {
            Ok(PRINT)
        } else if s == "return" {
            Ok(RETURN)
        } else if s == "super" {
            Ok(SUPER)
        } else if s == "this" {
            Ok(THIS)
        } else if s == "true" {
            Ok(TRUE)
        } else if s == "var" {
            Ok(VAR)
        } else if s == "while" {
            Ok(WHILE)
        } else {
            Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct ScanState {
    start_line: u32,
    start_char: u32,
    current_line: u32,
    current_char: u32,
}

impl Default for ScanState {
    fn default() -> Self {
        ScanState {
            start_line: 1,
            start_char: 0,
            current_line: 1,
            current_char: 0,
        }
    }
}

pub fn scan(source_string: String) -> Result<Vec<Token>, Vec<ScanningError>> {
    let mut source = source_string.chars().peekable();
    let mut scan_state = ScanState::default();
    let mut tokens = vec![];
    let mut errors = vec![];
    loop {
        scan_state.start_char = scan_state.current_char;
        scan_state.start_line = scan_state.current_line;
        if let Some(c) = source.next() {
            scan_state.current_char += 1;
            match c {
                '(' | ')' | '}' | '{' | ',' | '.' | '-' | '+' | ';' | '*' => tokens.push(match c {
                    '(' => OpenParen(scan_state),
                    ')' => CloseParen(scan_state),
                    '{' => OpenBrace(scan_state),
                    '}' => CloseBrace(scan_state),
                    ',' => Comma(scan_state),
                    '.' => Dot(scan_state),
                    '-' => Minus(scan_state),
                    '+' => Plus(scan_state),
                    ';' => SemiColon(scan_state),
                    '*' => Star(scan_state),
                    _ => unreachable!(),
                }),
                '!' => match source.next_if_eq(&'=') {
                    Some('=') => {
                        scan_state.current_char += 1;
                        tokens.push(BangEqual(scan_state))
                    }
                    _ => tokens.push(Bang(scan_state)),
                },
                '=' => match source.next_if_eq(&'=') {
                    Some('=') => {
                        scan_state.current_char += 1;
                        tokens.push(EqualEqual(scan_state))
                    }
                    _ => tokens.push(Equal(scan_state)),
                },
                '>' => match source.next_if_eq(&'=') {
                    Some('=') => {
                        scan_state.current_char += 1;
                        tokens.push(GreaterEqual(scan_state))
                    }
                    _ => tokens.push(Greater(scan_state)),
                },
                '<' => match source.next_if_eq(&'=') {
                    Some('=') => {
                        scan_state.current_char += 1;
                        tokens.push(LessEqual(scan_state))
                    }
                    _ => tokens.push(Less(scan_state)),
                },
                '/' => match source.next_if_eq(&'/') {
                    Some('/') => loop {
                        match source.next() {
                            Some('\n') => {
                                scan_state.current_line += 1;
                                break;
                            }
                            None => {
                                break;
                            }
                            _ => {}
                        }
                    },
                    _ => tokens.push(ForwardSlash(scan_state)),
                },
                '"' => {
                    let mut literal = String::new();
                    loop {
                        match source.peek() {
                            Some('\n') => {
                                scan_state.current_line += 1;
                                source.next();
                                scan_state.current_char = 0;
                            }
                            Some('"') => {
                                source.next();
                                tokens.push(StringLiteral(scan_state, literal));
                                break;
                            }
                            Some(c) => {
                                literal.push(*c);
                                source.next();
                            }
                            None => {
                                errors.push(ScanningError {
                                    scan_state,
                                    kind: UnexpectedEof(
                                        "while scanning string literal".to_string(),
                                    ),
                                });
                                break;
                            }
                        }
                    }
                }
                c @ '0'..='9' => {
                    let mut literal = String::from(c);
                    loop {
                        match source.peek() {
                            Some(c @ '0'..='9') => {
                                scan_state.current_char += 1;
                                literal.push(*c);
                                source.next();
                            }
                            Some('.') => {
                                scan_state.current_char += 1;
                                source.next();
                                match source.peek() {
                                    Some(c @ '0'..='9') => {
                                        literal.push('.');
                                        literal.push(*c);
                                        scan_state.current_char += 1;
                                        source.next();
                                    }
                                    _ => {
                                        tokens.push(NumericLiteral(
                                            scan_state,
                                            literal.parse().unwrap(),
                                        ));
                                        tokens.push(Dot(scan_state));
                                        break;
                                    }
                                }
                            }
                            _ => {
                                tokens.push(NumericLiteral(scan_state, literal.parse().unwrap()));
                                break;
                            }
                        }
                    }
                }
                c @ ('a'..='z' | 'A'..='Z' | '_') => {
                    let mut ident = String::from(c);
                    loop {
                        match source.next() {
                            Some(c @ ('a'..='z' | 'A'..='Z' | '_')) => {
                                scan_state.current_char += 1;
                                ident.push(c)
                            }
                            _ => {
                                let token = match &ident.parse::<ReservedWord>() {
                                    Ok(reservedWord) => ReservedWordTok(scan_state, *reservedWord),
                                    Err(..) => Identifier(scan_state, ident.clone()),
                                };
                                tokens.push(token);
                                break;
                            }
                        }
                    }
                }
                ' ' | '\t' | '\r' => continue,
                '\n' => {
                    scan_state.current_line += 1;
                    continue;
                }
                _ => errors.push(ScanningError {
                    scan_state,
                    kind: UnexpectedChar(c),
                }),
            }
        } else {
            tokens.push(Token::EOF);
            break;
        }
    }

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eof() {
        let vec = scan("".to_string()).unwrap();
        assert!(matches!(vec[..], [Token::EOF]))
    }

    #[test]
    fn single_char_tokens() {
        let tokens = scan("(){},.-+;*".to_string()).unwrap();
        assert!(
            matches!(
                &tokens[..],
                [
                    OpenParen(..),
                    CloseParen(..),
                    OpenBrace(..),
                    CloseBrace(..),
                    Comma(..),
                    Dot(..),
                    Minus(..),
                    Plus(..),
                    SemiColon(..),
                    Star(..),
                    EOF
                ]
            ),
            "{:?}",
            tokens
        )
    }

    #[test]
    fn strange_token() {
        let errors = scan("&|".to_string()).unwrap_err();
        assert!(
            matches!(
                errors[..],
                [
                    ScanningError {
                        kind: UnexpectedChar('&',),
                        ..
                    },
                    ScanningError {
                        kind: UnexpectedChar('|',),
                        ..
                    }
                ]
            ),
            "{:?}",
            errors
        )
    }

    #[test]
    fn two_char_tokens() {
        let tokens = scan("== <= >= != = > < !".to_string()).unwrap();
        assert!(
            matches!(
                &tokens[..],
                [
                    EqualEqual(..),
                    LessEqual(..),
                    GreaterEqual(..),
                    BangEqual(..),
                    Equal(..),
                    Greater(..),
                    Less(..),
                    Bang(..),
                    EOF
                ]
            ),
            "{:?}",
            tokens
        )
    }

    #[test]
    fn comments() {
        let tokens = scan("// hello world".to_string()).unwrap();
        assert!(matches!(&tokens[..], [EOF]), "{:?}", tokens)
    }

    #[test]
    fn slash() {
        let tokens = scan("/".to_string()).unwrap();
        assert!(
            matches!(&tokens[..], [ForwardSlash(..), EOF]),
            "{:?}",
            tokens
        )
    }

    #[test]
    fn string_literal() {
        let tokens = scan(r#""hello world""#.to_string()).unwrap();
        match &tokens[0] {
            StringLiteral(_, string) => assert_eq!(&"hello world".to_string(), string),
            _ => panic!("tokens[0] was not a string literal."),
        };
    }

    #[test]
    fn numeric_literal() {
        let tokens = scan("0.5".to_string()).unwrap();
        match &tokens[0] {
            NumericLiteral(_, number) => assert!(number - 0.5 < 0.00001, "{}", number),
            _ => panic!("tokens[0] was not a string literal."),
        };
    }

    #[test]
    fn numeric_literal_trailing_dot() {
        let tokens = scan("0.".to_string()).unwrap();
        assert!(
            matches!(&tokens[..], [NumericLiteral(..), Dot(..), EOF]),
            "{:?}",
            tokens
        )
    }

    #[test]
    fn keyword() {
        let tokens = scan("and".to_string()).unwrap();
        assert!(
            matches!(&tokens[..], [ReservedWordTok(_, AND), EOF]),
            "{:?}",
            tokens
        )
    }
}
