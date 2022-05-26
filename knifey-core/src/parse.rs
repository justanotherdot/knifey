//! This module encapsulates parsing of the following grammar.
//!
//! ```plaintext
//! expr     := factor '+' expr | factor '-' expr | factor ;
//! factor   := term '*' factor | term ;
//! term     := dice | constant | '(' expr ')' ;
//! dice     := 'd' number ;
//! constant := number ;
//! number   := '0-9'* ;
//! ```

use nom::branch::alt;
use nom::character::complete::{char, i64, space0};
use nom::combinator::{eof, map, verify};
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
    let (input, value) = verify(i64, |x: &i64| *x > 1)(input)?;
    Ok((input, Dice { value }))
}

pub fn term(input: &str) -> IResult<&str, Term> {
    alt((
        map(dice, |d: Dice| Term::Dice(d)),
        map(constant, |c: Constant| Term::Constant(c)),
        map(tuple((char('('), expr, char(')'))), |(_, e, _)| {
            Term::Paren(e)
        }),
    ))(input)
}

pub fn factor(input: &str) -> IResult<&str, Factor> {
    alt((
        map(
            tuple((term, space0, char('*'), space0, factor)),
            |(lhs, _, _, _, rhs)| Factor::mul(lhs, rhs),
        ),
        //map(
        //    tuple((term, space0, char('/'), space0, expr)),
        //    |(lhs, _, _, _, rhs)| Expr::sub(lhs, rhs),
        //),
        map(term, |t| Factor::term(t)),
    ))(input)
}

pub fn expr(input: &str) -> IResult<&str, Expr> {
    alt((
        map(
            tuple((factor, space0, char('+'), space0, expr)),
            |(lhs, _, _, _, rhs)| Expr::add(lhs, rhs),
        ),
        map(
            tuple((factor, space0, char('-'), space0, expr)),
            |(lhs, _, _, _, rhs)| Expr::sub(lhs, rhs),
        ),
        map(factor, |f| Expr::factor(f)),
    ))(input)
}

pub fn fully<A>(parser: impl FnMut(&str) -> IResult<&str, A>, input: &str) -> IResult<&str, A> {
    map(tuple((space0, parser, eof)), |(_, expr, _)| expr)(input)
}

pub fn parse_expr(input: &str) -> Result<Expr, nom::Err<nom::error::Error<&str>>> {
    fully(expr, input).map(|(_, expr)| expr)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::golden::check_golden_expr;

    #[test]
    fn parse_expr_works() {
        check_golden_expr("d20");
        check_golden_expr("20");
        check_golden_expr("(20)");
        check_golden_expr("d20 + 20");
        check_golden_expr("d20 + 20 + d2");
        check_golden_expr("d20 + 20 - d2");
        check_golden_expr("d20 - 20");
        check_golden_expr("(d20)");
        check_golden_expr("(20)");
        check_golden_expr("(d20 + 20)");
        check_golden_expr("(d20 + 20) + 7");
        check_golden_expr("((d20 - 10) + 20) + 7");
        check_golden_expr("d20 - (10 + (20 + 7))");
        check_golden_expr("d20 + (20 + 7)");
        check_golden_expr("(d20 + 3) + (20 + 7)");
        check_golden_expr("(d20 + (20) + 11)");
    }

    #[test]
    fn parse_expr_breaks() {
        assert!(parse_expr("d20 / 20").is_err());
        assert!(parse_expr("+").is_err());
        assert!(parse_expr("-").is_err());
        assert!(parse_expr("(1 + 2").is_err());
        assert!(parse_expr("1 + 2)").is_err());
        assert!(parse_expr("1 +").is_err());
        assert!(parse_expr("+ 2").is_err());
        assert!(parse_expr("2d").is_err());
        assert!(parse_expr("d").is_err());
        assert!(parse_expr("0d2").is_err());
        assert!(parse_expr("1d1").is_err());
        assert!(parse_expr("d1").is_err());
        assert!(parse_expr("--12").is_err());
        assert!(parse_expr("d-12").is_err());
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
