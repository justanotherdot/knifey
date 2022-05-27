use crate::data::*;

/// A simple pretty printer.
pub fn pretty_expr(expr: Expr) -> String {
    match expr {
        Expr::Add { lhs, rhs } => {
            let left = pretty_factor(*lhs);
            let right = pretty_expr(*rhs);
            format!("{} + {}", left, right)
        }
        Expr::Sub { lhs, rhs } => {
            let left = pretty_factor(*lhs);
            let right = pretty_expr(*rhs);
            format!("{} - {}", left, right)
        }
        Expr::Factor(factor) => pretty_factor(*factor),
    }
}

pub fn pretty_factor(factor: Factor) -> String {
    match factor {
        Factor::Mul { lhs, rhs } => {
            let left = pretty_term(*lhs);
            let right = pretty_factor(*rhs);
            format!("{} * {}", left, right)
        }
        Factor::Term(term) => pretty_term(*term),
    }
}

pub fn pretty_term(term: Term) -> String {
    match term {
        Term::Constant(Constant { value }) => format!("{}", value),
        Term::Dice(Dice { value }) => format!("d{}", value),
        Term::Paren(expr) => format!("({})", pretty_expr(expr)),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    //use crate::parse;

    //fn check_roundtrip_expr(input: &str) {
    //    let expr = parse::parse_expr(input).expect("parse);
    //    assert_eq!(pretty_term(expr), "d4");
    //}

    #[test]
    fn pretty_term_works_constant() {
        let constant = Term::constant(42);
        assert_eq!(&*pretty_term(constant), "42");
    }

    #[test]
    fn pretty_term_works_dice() {
        let dice = Term::dice(42);
        assert_eq!(&*pretty_term(dice), "d42");
    }

    #[test]
    fn pretty_factor_works_term() {
        let constant = Factor::term(Term::constant(42));
        assert_eq!(&*pretty_factor(constant), "42");
    }

    #[test]
    fn pretty_factor_works_mul() {
        let constant = Factor::mul(Term::constant(3), Factor::term(Term::constant(2)));
        assert_eq!(&*pretty_factor(constant), "3 * 2");
    }

    #[test]
    fn pretty_expr_works_mul() {
        let mul = Expr::factor(Factor::mul(
            Term::constant(3),
            Factor::term(Term::constant(2)),
        ));
        assert_eq!(&*pretty_expr(mul), "3 * 2");
    }

    #[test]
    fn pretty_expr_works_add() {
        let add = Expr::add(
            Factor::term(Term::constant(42)),
            Expr::factor(Factor::term(Term::constant(666))),
        );
        assert_eq!(&*pretty_expr(add), "42 + 666");
    }

    #[test]
    fn pretty_expr_works_sub() {
        let sub = Expr::sub(
            Factor::term(Term::constant(42)),
            Expr::factor(Factor::term(Term::constant(666))),
        );
        assert_eq!(&*pretty_expr(sub), "42 - 666");
    }

    #[test]
    fn pretty_expr_works_parens() {
        let paren = Expr::factor(Factor::term(Term::paren(Expr::factor(Factor::term(
            Term::constant(666),
        )))));
        assert_eq!(&*pretty_expr(paren), "(666)");
    }

    #[test]
    fn pretty_expr_idempotent() {
        let mul = Expr::factor(Factor::mul(
            Term::constant(3),
            Factor::term(Term::constant(2)),
        ));
        assert_eq!(&*pretty_expr(mul.clone()), "3 * 2");
        assert_eq!(&*pretty_expr(mul.clone()), "3 * 2");
    }
}
