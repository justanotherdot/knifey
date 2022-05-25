use crate::data::*;

/// A simple tree walk interpreter.
pub fn eval(expr: Expr) -> Value {
    match expr {
        Expr::Add { lhs, rhs } => eval_factor(*lhs).add(eval(*rhs)),
        Expr::Sub { lhs, rhs } => eval_factor(*lhs).sub(eval(*rhs)),
        Expr::Factor(factor) => eval_factor(*factor),
    }
}

pub fn eval_factor(factor: Factor) -> Value {
    match factor {
        Factor::Mul { lhs, rhs } => eval_term(*lhs).mul(eval_factor(*rhs)),
        Factor::Term(term) => eval_term(*term),
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
        let roll = parse_eval("d20").as_i64().expect("cast");
        assert!(roll >= 1 && roll <= 20);
    }

    #[test]
    fn eval_constant_works() {
        assert_eq!(parse_eval("20").as_i64().expect("cast"), 20);
    }

    #[test]
    fn eval_add_works() {
        assert_eq!(parse_eval("2 + 3").as_i64().expect("cast"), 2 + 3);
    }

    #[test]
    fn eval_sub_works() {
        assert_eq!(parse_eval("4 - 10").as_i64().expect("cast"), 4 - 10);
    }

    #[test]
    fn eval_mul_works() {
        assert_eq!(parse_eval("2 * 3").as_i64().expect("cast"), 2 * 3);
    }

    #[test]
    fn eval_dice_in_bounds() {
        let result = parse_eval("d20 + d20").as_i64().expect("cast");
        assert!(result >= 1 && result <= (20 * 2));
    }

    #[test]
    fn eval_parenthesis() {
        assert_eq!(parse_eval("(100 - 20) + 1").as_i64().expect("cast"), 81);
        assert_eq!(parse_eval("100 - (20 + 1)").as_i64().expect("cast"), 79);
        assert_eq!(
            parse_eval("((100 + 10) - 20) + 1").as_i64().expect("cast"),
            91
        );
        assert_eq!(
            parse_eval("(100 - (10 - 100)) + 1").as_i64().expect("cast"),
            191
        );
        assert_eq!(
            parse_eval("((100 - 10) - 100) + 1").as_i64().expect("cast"),
            -9
        );
        assert_eq!(parse_eval("(1)").as_i64().expect("cast"), 1);
        assert_eq!(parse_eval("(100 - 20) * 3").as_i64().expect("cast"), 240);
        assert_eq!(parse_eval("100 - (20 * 3)").as_i64().expect("cast"), 40);
        assert_eq!(parse_eval("(((1)))").as_i64().expect("cast"), 1);
    }

    #[test]
    fn eval_idempotent() {
        let expr = parse::parse_expr("10 + 10").expect("parse");
        let result1 = eval(expr.clone()).as_i64().expect("cast");
        let result2 = eval(expr.clone()).as_i64().expect("cast");
        assert_eq!(result1, result2);
    }
}
