use std::fmt::Debug;
use std::str::FromStr;

use nom::{
    bytes::complete::take_while1, character::complete::line_ending, combinator::opt,
    error::ParseError, multi::separated_list0, sequence::tuple, IResult, Parser,
};

// parses a decimal number like "42"
pub fn decimal_number<T>(input: &str) -> IResult<&str, T>
where
    T: FromStr,
    <T as FromStr>::Err: Debug,
{
    take_while1(|c: char| c.is_ascii_digit())
        .map(|number: &str| number.parse().expect("must parse"))
        .parse(input)
}

pub fn line_separated<'a, T, E>(
    line_parser: impl Parser<&'a str, T, E>,
) -> impl Parser<&'a str, Vec<T>, E>
where
    E: ParseError<&'a str>,
{
    tuple((separated_list0(line_ending, line_parser), opt(line_ending)))
        .map(|(list, _trailing_newline)| list)
}

pub fn final_parser<'a, O>(
    parser: impl Parser<&'a str, O, nom::error::Error<&'a str>>,
) -> impl FnMut(&'a str) -> Result<O, nom::error::Error<&'a str>> {
    nom_supreme::final_parser::final_parser(parser)
}
