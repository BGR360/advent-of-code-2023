use std::fmt::Debug;
use std::str::FromStr;

pub use nom::{
    branch::*, bytes::complete::*, character::complete::*, combinator::*, error::ParseError,
    multi::*, sequence::*, *,
};
pub use nom_supreme::ParserExt;

pub use crate::grid::parse_grid as grid;

mod numbers;
mod separated_tuple;

pub use numbers::{
    decimal_number, decimal_number_m_n, hex_number, hex_number_m_n, single_digit_number,
};
pub use separated_tuple::{separated_tuple, ws_tuple, SeparatedTuple};

pub fn word(input: &str) -> IResult<&str, &str> {
    alpha1(input)
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
