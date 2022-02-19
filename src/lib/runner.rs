use std::error::Error;
use std::fmt::{Display, Formatter};

use super::evaluator::{eval_exp, EvalError, LoxValue};
use linefeed::{Interface, ReadResult, Signal};

use super::parser::{parse, ParseError};
use super::scanner::{scan, ScanningError, Token};

#[derive(Debug)]
pub enum ReplExit {
    Signal(Signal),
    Eof,
}

impl Display for ReplExit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReplExit::Signal(sig) => write!(f, "{:?}", sig),
            ReplExit::Eof => write!(f, "EOF"),
        }
    }
}

pub fn run() -> Result<ReplExit, Box<dyn Error>> {
    let interface = Interface::new("dpnd")?;
    interface.set_prompt("dpnd > ")?;

    loop {
        match interface.read_line()? {
            ReadResult::Eof => return Ok(ReplExit::Eof),
            ReadResult::Input(input) => eval(input),
            ReadResult::Signal(signal) => return Ok(ReplExit::Signal(signal)),
        }
    }
}

pub fn eval(input: String) {
    match scan(input) {
        Ok(tokens) => match parse(tokens) {
            Ok(exprs) => {
                for expression in exprs {
                    match eval_exp(expression) {
                        Ok(lox_value) => println!("{lox_value}"),
                        Err(err) => println!("Error while evaluating: {err:?}"),
                    }
                }
            }
            Err(err) => println!("Error while parsing: {err:?}"),
        },
        Err(err) => println!("Error while scanning: {err:?}"),
    };
}
