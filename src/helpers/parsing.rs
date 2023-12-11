use std::fmt::Debug;
use std::str::FromStr;

use nom::error::ParseError;
pub use nom::{
    branch::*, bytes::complete::*, character::complete::*, combinator::*, multi::*, sequence::*, *,
};

use crate::helpers::grid::Grid;

/// Parses a decimal number like "42"
pub fn decimal_number<T>(input: &str) -> IResult<&str, T>
where
    T: FromStr,
    <T as FromStr>::Err: Debug,
{
    take_while1(|c: char| c.is_ascii_digit())
        .map(|number: &str| number.parse().expect("must parse"))
        .parse(input)
}

/// Returns a parser that parses a 2D grid of (probably single-character)
/// parseable tiles using the provided tile parser, like:
///
/// ```txt
/// ...#..
/// ..#..#
/// #..#.#
/// ...#..
/// ```
pub fn grid<'a, Pos, T, E>(
    mut tile_parser: impl Parser<&'a str, T, E>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Grid<T, Pos>, E>
where
    Pos: Default,
    E: ParseError<&'a str>,
{
    move |input: &str| -> IResult<&str, Grid<T, Pos>, E> {
        let n_cols = input
            .lines()
            .next()
            .expect("there must be at least one line in the input")
            .len();

        let tile = terminated(|input| tile_parser.parse(input), many0(line_ending));

        let (input, tiles) = many1(tile)(input)?;
        let grid = Grid::from_vec(tiles, n_cols);

        Ok((input, grid))
    }
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
