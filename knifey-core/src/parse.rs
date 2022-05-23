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
