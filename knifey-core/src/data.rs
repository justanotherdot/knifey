/// A dice roll between one and `value` inclusive.
#[derive(Debug, PartialEq, Clone)]
pub struct Dice {
    /// The upper bound of the roll.
    pub value: i64,
}

impl Dice {
    pub fn new(value: i64) -> Dice {
        Dice { value }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Constant {
    pub value: i64,
}

impl Constant {
    pub fn new(value: i64) -> Constant {
        Constant { value }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Dice(Dice),
    Constant(Constant),
    Paren(Expr),
}

impl Term {
    pub fn dice(value: i64) -> Term {
        Term::Dice(Dice::new(value))
    }

    pub fn constant(value: i64) -> Term {
        Term::Constant(Constant::new(value))
    }

    pub fn paren(expr: Expr) -> Term {
        Term::Paren(expr)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Add { lhs: Box<Term>, rhs: Box<Term> },
    Sub { lhs: Box<Term>, rhs: Box<Term> },
    Term(Box<Term>),
}

impl Expr {
    pub fn add(lhs: Term, rhs: Term) -> Expr {
        Expr::Add {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn sub(lhs: Term, rhs: Term) -> Expr {
        Expr::Sub {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn term(term: Term) -> Expr {
        Expr::Term(Box::new(term))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    /// A single scalar integer.
    Int64(i64),
    // FUTURE:
    // Rolls { rolls: HashMap<String, i64>, result: i64 }
}

impl Value {
    pub fn add(self, other: Self) -> Self {
        use Value::*;
        match (self, other) {
            (Int64(x), Int64(y)) => Int64(x + y),
        }
    }

    pub fn sub(self, other: Self) -> Self {
        use Value::*;
        match (self, other) {
            (Int64(x), Int64(y)) => Int64(x - y),
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        use Value::*;
        match self {
            Int64(value) => Some(*value),
        }
    }
}
