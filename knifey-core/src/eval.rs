use crate::data::*;

/// A simple tree walk interpreter.
pub fn eval(expr: Expr) -> Value {
    match expr {
        Expr::Add { lhs, rhs } => eval_term(*lhs).add(eval(*rhs)),
        Expr::Sub { lhs, rhs } => eval_term(*lhs).sub(eval(*rhs)),
        Expr::Term(term) => eval_term(*term),
    }
}

pub fn eval_term(term: Term) -> Value {
    match term {
        Term::Constant(Constant { value }) => Value::Int64(value),
        Term::Dice(Dice { value }) => Value::Int64(fastrand::i64(1..=value)),
        Term::Paren(expr) => eval(expr),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse;

    pub fn parse_eval(input: &str) -> Value {
        eval(parse::parse_expr(input).expect("parse"))
    }

    #[test]
    fn eval_dice_works() {
        let value = 20;
        let dice = Expr::term(Term::dice(value));
        let roll = eval(dice).as_i64().expect("cast");
        assert!(roll >= 1 && roll <= 20);
    }

    #[test]
    fn eval_constant_works() {
        let value = 20;
        let const_term = Expr::term(Term::constant(value));
        let constant = eval(const_term).as_i64().expect("cast");
        assert_eq!(constant, value);
    }

    #[test]
    fn eval_add_works() {
        let value = 2;
        let constant = Term::constant(value);
        let addition = Expr::add(constant.clone(), Expr::term(constant.clone()));
        let result = eval(addition).as_i64().expect("cast");
        assert_eq!(result, value + value);
    }

    #[test]
    fn eval_sub_works() {
        let value = 2;
        let constant = Term::constant(value);
        let subtraction = Expr::sub(constant.clone(), Expr::term(constant.clone()));
        let result = eval(subtraction).as_i64().expect("cast");
        assert_eq!(result, value - value);
    }

    #[test]
    fn eval_nested_works_01() {
        let value = 2;
        let constant = Term::constant(value);
        let addition = Expr::add(constant.clone(), Expr::term(constant.clone()));
        let nested = Expr::sub(constant.clone(), addition);
        let result = eval(nested).as_i64().expect("cast");
        assert_eq!(result, value - (value + value));
    }

    #[test]
    fn eval_nested_works_02() {
        let value = 2;
        let constant = Term::constant(value);
        let addition = Expr::add(constant.clone(), Expr::term(constant.clone()));
        let nested = Expr::sub(Term::paren(addition), Expr::term(constant.clone()));
        let result = eval(nested).as_i64().expect("cast");
        assert_eq!(result, (value + value) - value);
    }

    #[test]
    fn eval_dice_in_bounds() {
        let value = 20;
        let dice = Term::dice(value);
        let addition = Expr::add(dice.clone(), Expr::term(dice.clone()));
        let result = eval(addition).as_i64().expect("cast");
        assert!(result >= 1 && result <= (value * 2));
    }

    #[test]
    fn eval_expr_source_01() {
        let result = parse_eval("d20 + 5").as_i64().expect("cast");
        assert!(result >= 5 && result <= (5 + 20));
    }

    #[test]
    fn eval_expr_source_02() {
        assert_eq!(parse_eval("100 - 20").as_i64().expect("cast"), 80);
    }

    #[test]
    fn eval_parenthesis_01() {
        // (100 - 20) + 1 => 81
        let expr = Expr::add(
            Term::paren(Expr::sub(
                Term::constant(100),
                Expr::term(Term::constant(20)),
            )),
            Expr::term(Term::constant(1)),
        );
        let result = eval(expr).as_i64().expect("cast");
        assert_eq!(result, 81);
    }

    #[test]
    fn eval_parenthesis_02() {
        // 100 - (20 + 1) => 79
        let expr = Expr::sub(
            Term::constant(100),
            Expr::term(Term::paren(Expr::add(
                Term::constant(20),
                Expr::term(Term::constant(1)),
            ))),
        );
        let result = eval(expr).as_i64().expect("cast");
        assert_eq!(result, 79);
    }

    #[test]
    fn eval_parenthesis_03() {
        // ((100 + 10) - 20) + 1 => 91
        let expr = Expr::add(
            Term::paren(Expr::sub(
                Term::paren(Expr::add(
                    Term::constant(100),
                    Expr::term(Term::constant(10)),
                )),
                Expr::term(Term::constant(20)),
            )),
            Expr::term(Term::constant(1)),
        );
        let result = eval(expr).as_i64().expect("cast");
        assert_eq!(result, 91);
    }

    #[test]
    fn eval_parenthesis_04() {
        // (100 - (10 - 100)) + 1 => 191
        let expr = Expr::add(
            Term::paren(Expr::sub(
                Term::constant(100),
                Expr::term(Term::paren(Expr::sub(
                    Term::constant(10),
                    Expr::term(Term::constant(100)),
                ))),
            )),
            Expr::term(Term::constant(1)),
        );
        let result = eval(expr).as_i64().expect("cast");
        assert_eq!(result, 191);
    }

    #[test]
    fn eval_parenthesis_05() {
        // ((100 - 10) - 100) + 1 => -9
        let expr = Expr::add(
            Term::paren(Expr::sub(
                Term::paren(Expr::sub(
                    Term::constant(100),
                    Expr::term(Term::constant(10)),
                )),
                Expr::term(Term::constant(100)),
            )),
            Expr::term(Term::constant(1)),
        );
        let result = eval(expr).as_i64().expect("cast");
        assert_eq!(result, -9);
    }

    #[test]
    fn eval_parenthesis_06() {
        // (1)
        let expr = Expr::term(Term::paren(Expr::term(Term::constant(1))));
        let result = eval(expr).as_i64().expect("cast");
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_parenthesis_deeply_nested() {
        assert_eq!(parse_eval("(((1)))").as_i64().expect("cast"), 1);
    }

    #[test]
    fn eval_idempotent() {
        let expr = Expr::term(Term::constant(100));
        let result1 = eval(expr.clone()).as_i64().expect("cast");
        let result2 = eval(expr.clone()).as_i64().expect("cast");
        assert_eq!(result1, result2);
    }
}
