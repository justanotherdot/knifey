use crate::data::*;

/// A simple tree walk interpreter.
pub fn eval(ast: Expr) -> i64 {
    match ast {
        Expr::Add { lhs, rhs } => eval_term(*lhs) + eval_term(*rhs),
        Expr::Sub { lhs, rhs } => eval_term(*lhs) - eval_term(*rhs),
        Expr::Term(term) => eval_term(*term),
    }
}

pub fn eval_term(term: Term) -> i64 {
    match term {
        Term::Constant(Constant { value }) => value,
        Term::Dice(Dice { value }) => fastrand::i64(1..value),
        Term::Paren(expr) => eval(expr),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse;

    #[test]
    fn eval_dice_works() {
        let value = 20;
        let dice = Expr::term(Term::dice(value));
        let roll = eval(dice);
        assert!(roll >= 1 && roll <= 20);
    }

    #[test]
    fn eval_constant_works() {
        let value = 20;
        let const_term = Expr::term(Term::constant(value));
        let constant = eval(const_term);
        assert_eq!(constant, value);
    }

    #[test]
    fn eval_add_works() {
        let value = 2;
        let constant = Term::constant(value);
        let addition = Expr::add(constant.clone(), constant.clone());
        let result = eval(addition);
        assert_eq!(result, value + value);
    }

    #[test]
    fn eval_sub_works() {
        let value = 2;
        let constant = Term::constant(value);
        let subtraction = Expr::sub(constant.clone(), constant.clone());
        let result = eval(subtraction);
        assert_eq!(result, value - value);
    }

    #[test]
    fn eval_nested_works_01() {
        let value = 2;
        let constant = Term::constant(value);
        let addition = Expr::add(constant.clone(), constant.clone());
        let nested = Expr::sub(constant.clone(), Term::paren(addition));
        let result = eval(nested);
        assert_eq!(result, value - (value + value));
    }

    #[test]
    fn eval_nested_works_02() {
        let value = 2;
        let constant = Term::constant(value);
        let addition = Expr::add(constant.clone(), constant.clone());
        let nested = Expr::sub(Term::paren(addition), constant.clone());
        let result = eval(nested);
        assert_eq!(result, (value + value) - value);
    }

    #[test]
    fn eval_dice_in_bounds() {
        let value = 20;
        let dice = Term::dice(value);
        let addition = Expr::add(dice.clone(), dice.clone());
        let result = eval(addition);
        assert!(result >= 1 && result <= (value * 2));
    }

    #[test]
    fn eval_expr_source_01() {
        let expr = parse::parse_expr("d20 + 5").expect("parse");
        let result = eval(expr);
        assert!(result >= 5 && result <= (5 + 20));
    }

    #[test]
    fn eval_expr_source_02() {
        let expr = parse::parse_expr("100 - 20").expect("parse");
        let result = eval(expr);
        assert_eq!(result, 80);
    }
}
