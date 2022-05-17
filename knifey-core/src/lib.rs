use nom::branch::alt;
use nom::character::complete::u64;
use nom::character::complete::{char, multispace1};
use nom::combinator::map;
use nom::IResult;

#[derive(Debug, PartialEq)]
pub struct Dice {
    pub value: u64,
}

#[derive(Debug, PartialEq)]
pub struct Constant {
    pub value: u64,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Dice(Dice),
    Constant(Constant),
    BinOp(BinOp),
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add { lhs: Box<Expr>, rhs: Box<Expr> },
    // FUTURE:
    // Subtract { lhs: Expr, rhs: Expr },
}

pub fn constant(input: &str) -> IResult<&str, Constant> {
    let (input, value) = u64(input)?;
    Ok((input, Constant { value }))
}

pub fn dice(input: &str) -> IResult<&str, Dice> {
    let (input, _) = alt((char('d'), char('D')))(input)?;
    let (input, value) = u64(input)?;

    Ok((input, Dice { value }))
}

pub fn bin_op(input: &str) -> IResult<&str, BinOp> {
    let (input, lhs) = expr(input)?;
    let (input, _) = multispace1(input)?;
    let (input, _) = char('+')(input)?;
    let (input, _) = multispace1(input)?;
    let (input, rhs) = expr(input)?;

    Ok((
        input,
        BinOp::Add {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

pub fn expr(input: &str) -> IResult<&str, Expr> {
    let (input, expr) = alt((
        map(dice, |d: Dice| Expr::Dice(d)),
        map(constant, |c: Constant| Expr::Constant(c)),
        //map(bin_op, |op: BinOp| Expr::BinOp(op)),
    ))(input)?;
    Ok((input, expr))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn expr_works() {
        assert_eq!(expr("d20"), Ok(("", Expr::Dice(Dice { value: 20 }))));
        assert_eq!(expr("20"), Ok(("", Expr::Constant(Constant { value: 20 }))));
        // stack overflow.
        //assert_eq!(
        //    expr("d20 + 20"),
        //    Ok((
        //        "",
        //        Expr::BinOp(BinOp::Add {
        //            lhs: Expr::Dice(Dice { value: 20 }).into(),
        //            rhs: Expr::Constant(Constant { value: 20 }).into(),
        //        })
        //    ))
        //);
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
        let cases = [20, u64::MAX];
        for value in cases {
            let input = format!("{}", value);
            assert_eq!(constant(&input), Ok(("", Constant { value })));
        }
    }

    #[test]
    fn constant_negative() {
        let value: i64 = -12;
        let input = format!("{}", value);
        assert!(constant(&input).is_err());
    }

    #[test]
    fn bin_op_works() {
        #[rustfmt::skip]
        let cases = [
            "d20 + d8",
            "d20   + d8",
            "d20   +   d8",
            "d20   +   D8",
        ];
        for input in cases {
            assert_eq!(
                bin_op(input),
                Ok((
                    "",
                    BinOp::Add {
                        lhs: Box::new(Expr::Dice(Dice { value: 20 })),
                        rhs: Box::new(Expr::Dice(Dice { value: 8 })),
                    }
                )),
            );
        }
    }
}
