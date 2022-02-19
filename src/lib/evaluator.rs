use std::any::Any;
use std::fmt::{Display, Formatter, Pointer};

use super::ast::{Comparison, Equality, Expression, Factor, Primary, Term, Unary};

pub enum LoxValue {
    Object(Box<dyn Any>),
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Object(obj) => write!(f, "object with type id {:?}", obj.type_id()),
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Boolean(bool) => write!(f, "{bool}"),
            LoxValue::Number(num) => write!(f, "{num}"),
            LoxValue::String(str) => write!(f, "{str}"),
        }
    }
}

impl LoxValue {
    fn type_name(&self) -> String {
        match self {
            LoxValue::Object(_) => "object",
            LoxValue::Nil => "nil",
            LoxValue::Boolean(_) => "bool",
            LoxValue::Number(_) => "number",
            LoxValue::String(_) => "string",
        }
        .to_string()
    }
}

#[derive(Debug)]
pub enum EvalError {
    TypeError(String),
}

pub fn eval_exp(expression: Expression) -> Result<LoxValue, EvalError> {
    match expression {
        Expression::Equality(equality) => eval_equality(equality),
    }
}

fn eval_equality(equality: Equality) -> Result<LoxValue, EvalError> {
    match equality {
        Equality::BangEqual(lhs, rhs) => {
            let lhs = eval_comparison(lhs)?;
            let rhs = eval_comparison(rhs)?;
            Ok(LoxValue::Boolean(is_equal(lhs, rhs)?))
        }
        Equality::EqualEqual(lhs, rhs) => {
            let lhs = eval_comparison(lhs)?;
            let rhs = eval_comparison(rhs)?;
            Ok(LoxValue::Boolean(is_equal(lhs, rhs)?))
        }
        Equality::Comparison(comparison) => eval_comparison(comparison),
    }
}

fn is_equal(lhs: LoxValue, rhs: LoxValue) -> Result<bool, EvalError> {
    match lhs {
        LoxValue::Object(_) => Err(EvalError::TypeError(
            "cannot compare object to anything".to_string(),
        )),
        LoxValue::Nil => Ok(matches!(rhs, LoxValue::Nil)),
        LoxValue::Boolean(lhs) => match rhs {
            LoxValue::Boolean(rhs) => Ok(lhs == rhs),
            v => Err(EvalError::TypeError(format!(
                "cannot compare bool to {}",
                v.type_name()
            ))),
        },
        LoxValue::Number(lhs) => match rhs {
            LoxValue::Number(rhs) => Ok(lhs == rhs),
            v => Err(EvalError::TypeError(format!(
                "cannot compare number to {}",
                v.type_name()
            ))),
        },
        LoxValue::String(lhs) => match rhs {
            LoxValue::String(rhs) => Ok(lhs == rhs),
            v => Err(EvalError::TypeError(format!(
                "cannot compare string to {}",
                v.type_name()
            ))),
        },
    }
}

fn eval_comparison(comparison: Comparison) -> Result<LoxValue, EvalError> {
    match comparison {
        Comparison::Greater(lhs, rhs) => {
            let lhs = as_number(eval_term(lhs)?)?;
            let rhs = as_number(eval_term(rhs)?)?;
            Ok(LoxValue::Boolean(lhs > rhs))
        }
        Comparison::GreaterEqual(lhs, rhs) => {
            let lhs = as_number(eval_term(lhs)?)?;
            let rhs = as_number(eval_term(rhs)?)?;
            Ok(LoxValue::Boolean(lhs >= rhs))
        }
        Comparison::Less(lhs, rhs) => {
            let lhs = as_number(eval_term(lhs)?)?;
            let rhs = as_number(eval_term(rhs)?)?;
            Ok(LoxValue::Boolean(lhs < rhs))
        }
        Comparison::LessEqual(lhs, rhs) => {
            let lhs = as_number(eval_term(lhs)?)?;
            let rhs = as_number(eval_term(rhs)?)?;
            Ok(LoxValue::Boolean(lhs <= rhs))
        }
        Comparison::Term(term) => eval_term(term),
    }
}

fn eval_term(term: Term) -> Result<LoxValue, EvalError> {
    match term {
        Term::Minus(lhs, rhs) => {
            let lhs = as_number(eval_factor(lhs)?)?;
            let rhs = as_number(eval_factor(rhs)?)?;
            Ok(LoxValue::Number(lhs - rhs))
        }
        Term::Plus(lhs, rhs) => {
            let lhs = as_number(eval_factor(lhs)?)?;
            let rhs = as_number(eval_factor(rhs)?)?;
            Ok(LoxValue::Number(lhs + rhs))
        }
        Term::Factor(factor) => eval_factor(factor),
    }
}

fn eval_factor(factor: Factor) -> Result<LoxValue, EvalError> {
    match factor {
        Factor::Divide(lhs, rhs) => {
            let lhs = as_number(eval_unary(lhs)?)?;
            let rhs = as_number(eval_unary(rhs)?)?;
            Ok(LoxValue::Number(lhs / rhs))
        }
        Factor::Multiply(lhs, rhs) => {
            let lhs = as_number(eval_unary(lhs)?)?;
            let rhs = as_number(eval_unary(rhs)?)?;
            Ok(LoxValue::Number(lhs * rhs))
        }
        Factor::Unary(unary) => eval_unary(unary),
    }
}

fn eval_unary(unary: Unary) -> Result<LoxValue, EvalError> {
    match unary {
        Unary::Bang(unary) => Ok(LoxValue::Boolean(!as_bool(eval_unary(*unary)?)?)),
        Unary::Minus(unary) => Ok(LoxValue::Number(-as_number(eval_unary(*unary)?)?)),
        Unary::Primary(primary) => eval_primary(primary),
    }
}

fn as_bool(p0: LoxValue) -> Result<bool, EvalError> {
    match p0 {
        LoxValue::Boolean(bool) => Ok(bool),
        _ => Err(EvalError::TypeError(format!(
            "expected bool, found {}",
            p0.type_name()
        ))),
    }
}

fn as_number(p0: LoxValue) -> Result<f64, EvalError> {
    match p0 {
        LoxValue::Number(num) => Ok(num),
        _ => Err(EvalError::TypeError(format!(
            "expected number, found {}",
            p0.type_name()
        ))),
    }
}

fn eval_primary(primary: Primary) -> Result<LoxValue, EvalError> {
    let value = match primary {
        Primary::Number(num) => LoxValue::Number(num),
        Primary::String(string) => LoxValue::String(string),
        Primary::True => LoxValue::Boolean(true),
        Primary::False => LoxValue::Boolean(false),
        Primary::Nil => LoxValue::Nil,
        Primary::Expression(exp) => eval_exp(*exp)?,
    };
    Ok(value)
}
