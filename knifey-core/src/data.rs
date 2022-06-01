use quickcheck::{Arbitrary, Gen};

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

impl Arbitrary for Dice {
    fn arbitrary(g: &mut Gen) -> Dice {
        let bound = (u64::arbitrary(g) % (i64::MAX as u64)) + 2;
        Dice {
            value: bound as i64,
        }
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

impl Arbitrary for Constant {
    fn arbitrary(g: &mut Gen) -> Constant {
        Constant {
            value: i64::arbitrary(g),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Dice(Dice),
    Constant(Constant),
    Paren(Expr),
}

impl Arbitrary for Term {
    fn arbitrary(g: &mut Gen) -> Term {
        frequency(
            &[
                (3, |g| Term::Constant(*Box::arbitrary(g))),
                (3, |g| Term::Dice(*Box::arbitrary(g))),
                (1, |g| Term::Paren(Expr::arbitrary(g))),
            ],
            g,
        )
    }
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
pub enum Factor {
    Mul { lhs: Box<Term>, rhs: Box<Factor> },
    Term(Box<Term>),
    // FUTURE:
    // Div { lhs: Box<Term>, rhs: Box<Expr> },
}

impl Arbitrary for Factor {
    fn arbitrary(g: &mut Gen) -> Factor {
        frequency(
            &[
                (1, |g| Factor::Mul {
                    lhs: Box::arbitrary(g),
                    rhs: Box::arbitrary(g),
                }),
                (2, |g| Factor::Term(Box::arbitrary(g))),
            ],
            g,
        )
    }
}

impl Factor {
    pub fn mul(lhs: Term, rhs: Factor) -> Factor {
        Factor::Mul {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn term(term: Term) -> Factor {
        Factor::Term(Box::new(term))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Add { lhs: Box<Factor>, rhs: Box<Expr> },
    Sub { lhs: Box<Factor>, rhs: Box<Expr> },
    Factor(Box<Factor>),
}

pub fn frequency<A: Clone>(weighted_options: &[(usize, fn(&mut Gen) -> A)], g: &mut Gen) -> A {
    assert!(!weighted_options.is_empty());
    assert!(!weighted_options.iter().all(|(w, _)| w == &0));
    assert!(!weighted_options.iter().any(|(w, _)| w < &0));
    let total: usize = weighted_options.iter().map(|(w, _)| w).sum();
    let mut choice = rand::random::<usize>() % total + 1;
    for (weight, option) in weighted_options {
        if choice <= *weight {
            return option(g);
        }
        choice -= weight;
    }
    std::unreachable!()
}

pub fn oneof<A: Clone>(options: &[fn(&mut Gen) -> A], g: &mut Gen) -> A {
    let weighted_options: Vec<_> = options.iter().map(|f| (1 as usize, *f)).collect();
    frequency(weighted_options.as_slice(), g)
}

impl Arbitrary for Expr {
    fn arbitrary(g: &mut Gen) -> Expr {
        frequency(
            &[
                (1, |g| Expr::Add {
                    lhs: Box::arbitrary(g),
                    rhs: Box::arbitrary(g),
                }),
                (1, |g| Expr::Sub {
                    lhs: Box::arbitrary(g),
                    rhs: Box::arbitrary(g),
                }),
                (3, |g| Expr::Factor(*Box::arbitrary(g))),
            ],
            g,
        )
    }
}

impl Expr {
    pub fn add(lhs: Factor, rhs: Expr) -> Expr {
        Expr::Add {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn sub(lhs: Factor, rhs: Expr) -> Expr {
        Expr::Sub {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn factor(factor: Factor) -> Expr {
        Expr::Factor(Box::new(factor))
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

    pub fn mul(self, other: Self) -> Self {
        use Value::*;
        match (self, other) {
            (Int64(x), Int64(y)) => Int64(x * y),
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        use Value::*;
        match self {
            Int64(value) => Some(*value),
        }
    }
}
