#[derive(PartialOrd, PartialEq, Debug)]
pub enum Expression {
    Equality(Equality),
}

#[derive(PartialOrd, PartialEq, Debug)]
pub enum Equality {
    BangEqual(Comparison, Comparison),
    EqualEqual(Comparison, Comparison),
    Comparison(Comparison),
}

#[derive(PartialOrd, PartialEq, Debug)]
pub enum Comparison {
    Greater(Term, Term),
    GreaterEqual(Term, Term),
    Less(Term, Term),
    LessEqual(Term, Term),
    Term(Term),
}

#[derive(PartialOrd, PartialEq, Debug)]
pub enum Term {
    Minus(Factor, Factor),
    Plus(Factor, Factor),
    Factor(Factor),
}

#[derive(PartialOrd, PartialEq, Debug)]
pub enum Factor {
    Divide(Unary, Unary),
    Multiply(Unary, Unary),
    Unary(Unary),
}

#[derive(PartialOrd, PartialEq, Debug)]
pub enum Unary {
    Bang(Box<Unary>),
    Minus(Box<Unary>),
    Primary(Primary),
}

#[derive(PartialOrd, PartialEq, Debug)]
pub enum Primary {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Expression(Box<Expression>),
}
