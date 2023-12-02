use nom::{
    branch::alt,
    bytes::complete::take_while1,
    character::complete::{char, crlf, line_ending},
    combinator::opt,
    error::ParseError,
    multi::separated_list0,
    sequence::tuple,
    IResult, Parser,
};

// parses a decimal number like "42"
pub fn decimal_number(input: &str) -> IResult<&str, u32> {
    take_while1(|c: char| c.is_ascii_digit())
        .map(|number: &str| number.parse().expect("must parse"))
        .parse(input)
}

pub fn line_separated<'a, T, E>(
    line_parser: impl FnMut(&'a str) -> IResult<&'a str, T, E>,
) -> impl Parser<&'a str, Vec<T>, E>
where
    E: ParseError<&'a str>,
{
    tuple((separated_list0(line_ending, line_parser), opt(line_ending)))
        .map(|(list, _trailing_newline)| list)
}
