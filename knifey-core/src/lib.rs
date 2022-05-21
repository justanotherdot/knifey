use nom::branch::alt;
use nom::character::complete::{char, i64, space0, u64};
use nom::combinator::{eof, fail, map, opt};
use nom::sequence::tuple;
use nom::IResult;

#[derive(Debug, PartialEq)]
pub struct Dice {
    pub value: u64,
}

#[derive(Debug, PartialEq)]
pub struct Constant {
    pub value: i64,
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Dice(Dice),
    Constant(Constant),
    Paren(Expr),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Add { lhs: Box<Term>, rhs: Box<Term> },
    Sub { lhs: Box<Term>, rhs: Box<Term> },
    Term(Box<Term>),
}

pub fn constant(input: &str) -> IResult<&str, Constant> {
    let (input, value) = i64(input)?;
    Ok((input, Constant { value }))
}

pub fn dice(input: &str) -> IResult<&str, Dice> {
    let (input, _) = alt((char('d'), char('D')))(input)?;
    let (input, value) = u64(input)?;
    Ok((input, Dice { value }))
}

pub fn fully<A>(parser: impl FnMut(&str) -> IResult<&str, A>, input: &str) -> IResult<&str, A> {
    map(tuple((space0, parser, eof)), |(_, expr, _)| expr)(input)
}

pub fn expr(input: &str) -> IResult<&str, Expr> {
    let (input, lhs) = alt((
        map(dice, |d: Dice| Term::Dice(d)),
        map(constant, |c: Constant| Term::Constant(c)),
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, rest) = opt(tuple((alt((char('+'), char('-'))), space0, expr)))(input)?;
    match rest {
        Some(('+', _, rhs)) => Ok((
            input,
            Expr::Add {
                lhs: Box::new(lhs),
                rhs: Box::new(Term::Paren(rhs)),
            },
        )),
        Some(('-', _, rhs)) => Ok((
            input,
            Expr::Sub {
                lhs: Box::new(lhs),
                rhs: Box::new(Term::Paren(rhs)),
            },
        )),
        Some(_) => fail("unrecognised operator"),
        None => Ok((input, Expr::Term(Box::new(lhs)))),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn expr_works() {
        assert_eq!(
            expr("d20"),
            Ok(("", Expr::Term(Term::Dice(Dice { value: 20 }).into())))
        );
        assert_eq!(
            expr("20"),
            Ok((
                "",
                Expr::Term(Term::Constant(Constant { value: 20 }).into())
            ))
        );
        assert_eq!(
            expr("d20 + 20"),
            Ok((
                "",
                Expr::Add {
                    lhs: Term::Dice(Dice { value: 20 }).into(),
                    rhs: Term::Paren(Expr::Term(Term::Constant(Constant { value: 20 }).into()))
                        .into(),
                }
            ))
        );
        assert_eq!(
            expr("d20 + 20 + d2"),
            Ok((
                "",
                Expr::Add {
                    lhs: Term::Dice(Dice { value: 20 }).into(),
                    rhs: Term::Paren(Expr::Add {
                        lhs: Term::Constant(Constant { value: 20 }).into(),
                        rhs: Term::Paren(Expr::Term(Term::Dice(Dice { value: 2 }).into())).into()
                    })
                    .into()
                }
            ))
        );
        assert_eq!(
            expr("d20 + 20 - d2"),
            Ok((
                "",
                Expr::Add {
                    lhs: Term::Dice(Dice { value: 20 }).into(),
                    rhs: Term::Paren(Expr::Sub {
                        lhs: Term::Constant(Constant { value: 20 }).into(),
                        rhs: Term::Paren(Expr::Term(Term::Dice(Dice { value: 2 }).into())).into()
                    })
                    .into()
                }
            ))
        );
        assert_eq!(
            expr("d20 - 20"),
            Ok((
                "",
                Expr::Sub {
                    lhs: Term::Dice(Dice { value: 20 }).into(),
                    rhs: Term::Paren(Expr::Term(Term::Constant(Constant { value: 20 }).into()))
                        .into(),
                }
            ))
        );
    }

    #[test]
    fn expr_breaks() {
        assert!(fully(expr, "d20 / 20").is_err());
    }

    #[test]
    fn dice_works() {
        #[rustfmt::skip]
        let cases = [
            20,
            u64::MAX,
        ];
        for value in cases {
            let input = format!("d{}", value);
            assert_eq!(dice(&input), Ok(("", Dice { value })));
            let input = format!("D{}", value);
            assert_eq!(dice(&input), Ok(("", Dice { value })));
        }
    }

    #[test]
    fn dice_negative() {
        let value: i64 = -12;
        let input = format!("d{}", value);
        assert!(dice(&input).is_err());
    }

    #[test]
    fn constant_works() {
        let cases = [-12, 20, i64::MAX];
        for value in cases {
            let input = format!("{}", value);
            assert_eq!(constant(&input), Ok(("", Constant { value })));
        }
    }
}
