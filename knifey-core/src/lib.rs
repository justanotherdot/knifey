pub mod data {
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
}

pub mod parse {
    use nom::branch::alt;
    use nom::character::complete::{char, i64, space0};
    use nom::combinator::{eof, fail, map, opt, verify};
    use nom::sequence::tuple;
    use nom::IResult;

    use crate::data::*;

    pub fn constant(input: &str) -> IResult<&str, Constant> {
        let (input, value) = i64(input)?;
        Ok((input, Constant { value }))
    }

    /// Dice rolls are any positive integer prefaced with the letter `d`.
    pub fn dice(input: &str) -> IResult<&str, Dice> {
        let (input, _) = alt((char('d'), char('D')))(input)?;
        let (input, value) = verify(i64, |x: &i64| *x > 0)(input)?;
        Ok((input, Dice { value }))
    }

    pub fn fully<A>(parser: impl FnMut(&str) -> IResult<&str, A>, input: &str) -> IResult<&str, A> {
        map(tuple((space0, parser, eof)), |(_, expr, _)| expr)(input)
    }

    pub fn parse_expr(input: &str) -> Result<Expr, nom::Err<nom::error::Error<&str>>> {
        fully(expr, input).map(|(_, expr)| expr)
    }

    pub fn expr(input: &str) -> IResult<&str, Expr> {
        let (input, lhs) = alt((
            map(dice, |d: Dice| Term::Dice(d)),
            map(constant, |c: Constant| Term::Constant(c)),
        ))(input)?;
        let (input, _) = space0(input)?;
        let (input, rest) = opt(tuple((alt((char('+'), char('-'))), space0, expr)))(input)?;
        match rest {
            Some(('+', _, rhs)) => Ok((input, Expr::add(lhs, Term::paren(rhs)))),
            Some(('-', _, rhs)) => Ok((input, Expr::sub(lhs, Term::paren(rhs)))),
            Some(_) => fail("unrecognised operator"),
            None => Ok((input, Expr::term(lhs))),
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn parse_expr_works() {
            assert_eq!(parse_expr("d20"), Ok(Expr::term(Term::dice(20))));
            assert_eq!(parse_expr("20"), Ok(Expr::term(Term::constant(20))));
            assert_eq!(
                parse_expr("d20 + 20"),
                Ok(Expr::add(
                    Term::dice(20),
                    Term::paren(Expr::term(Term::constant(20)))
                ),)
            );
            assert_eq!(
                parse_expr("d20 + 20 + d2"),
                Ok(Expr::add(
                    Term::dice(20),
                    Term::paren(Expr::add(
                        Term::constant(20),
                        Term::paren(Expr::term(Term::dice(2))),
                    ))
                ),)
            );
            assert_eq!(
                parse_expr("d20 + 20 - d2"),
                Ok(Expr::add(
                    Term::dice(20),
                    Term::paren(Expr::sub(
                        Term::constant(20),
                        Term::paren(Expr::term(Term::dice(2))),
                    ))
                ),)
            );
            assert_eq!(
                parse_expr("d20 - 20"),
                Ok(Expr::sub(
                    Term::dice(20),
                    Term::paren(Expr::term(Term::constant(20)))
                ),)
            );
        }

        #[test]
        fn parse_expr_breaks() {
            assert!(parse_expr("d20 / 20").is_err());
        }

        #[test]
        fn dice_works() {
            #[rustfmt::skip]
            let cases = [
                20,
                i64::MAX,
            ];
            for value in cases {
                let input = format!("d{}", value);
                assert_eq!(dice(&input), Ok(("", Dice::new(value))));
                let input = format!("D{}", value);
                assert_eq!(dice(&input), Ok(("", Dice::new(value))));
            }
        }

        #[test]
        fn dice_must_be_postive() {
            let as_string = |x: i64| format!("d{}", x);
            #[rustfmt::skip]
            let cases = [
                as_string(-12),
                as_string(0),
            ];
            for case in cases {
                assert!(dice(&case).is_err());
            }
        }

        #[test]
        fn constant_works() {
            #[rustfmt::skip]
            let cases = [
                -12,
                20,
                i64::MAX
            ];
            for value in cases {
                let input = format!("{}", value);
                assert_eq!(constant(&input), Ok(("", Constant::new(value))));
            }
        }
    }
}

pub mod eval {
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
}
