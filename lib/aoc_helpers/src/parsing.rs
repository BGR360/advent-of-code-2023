use std::fmt::Debug;
use std::str::FromStr;

use nom::error::ParseError;
pub use nom::{
    branch::*, bytes::complete::*, character::complete::*, combinator::*, multi::*, sequence::*, *,
};

use crate::grid::Grid;

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
        let (mut input, first_row) =
            terminated(many1(|i| tile_parser.parse(i)), line_ending)(input)?;

        // Require line ending after first

        let n_cols = first_row.len();

        let mut tiles = first_row;

        // See if there's another row to process and try to.
        // while let Ok((next_row_input, _)) = line_ending::<_, E>(input) {
        for n_rows in 1.. {
            // println!("trying '{next_row_input}'");

            // Opportunistically allocated enough space for a full new row
            // as soon as we see the first element of this new row.
            let mut allocated_new_row = false;

            let result = fold_many_m_n(
                n_cols,
                n_cols,
                |i| tile_parser.parse(i),
                || (),
                |(), tile| {
                    if !allocated_new_row {
                        tiles.reserve(n_cols);
                        allocated_new_row = true;
                    }
                    tiles.push(tile);
                },
            )
            .and(line_ending::<_, E>)
            .parse(input);

            match result {
                Ok((rest, _)) => {
                    input = rest;
                }
                // Break out of the loop on non-fatal error.
                Err(nom::Err::Error(_)) => {
                    // Discard any partially-parsed last row.
                    tiles.truncate(n_rows * n_cols);
                    tiles.shrink_to_fit();
                    break;
                }
                Err(e) => return Err(e),
            }
        }

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

#[cfg(test)]
mod tests {
    use super::*;

    type Pos = glam::UVec2;

    fn grid<'a, T>(
        mut tile_parser: impl Parser<&'a str, T, nom::error::Error<&'a str>>,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, Grid<T, Pos>> {
        move |input: &'a str| super::grid::<Pos, _, _>(|i| tile_parser.parse(i))(input)
    }

    #[test]
    fn parse_grid() {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, crate::grid::Tile)]
        enum Tile {
            #[tile('.')]
            Empty,
            #[tile('#')]
            Full,
        }

        let input = ".#.\n.#.\n";

        let (rest, grid) = grid(Tile::parse)(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(grid.row_count(), 2);
        assert_eq!(grid.col_count(), 3);
        assert_eq!(
            grid.iter().copied().collect::<Vec<_>>(),
            vec![
                Tile::Empty,
                Tile::Full,
                Tile::Empty,
                Tile::Empty,
                Tile::Full,
                Tile::Empty
            ]
        )
    }

    #[test]
    fn parse_grid_incomplete() {
        let tile = char('.');

        let input = "...\n..";

        let (rest, grid) = grid(tile)(input).unwrap();

        assert_eq!(rest, "..");
        assert_eq!(grid.row_count(), 1);
        assert_eq!(grid.col_count(), 3);
    }

    #[test]
    fn parse_grid_complete_except_for_newline() {
        let tile = char('.');

        let input = "...\n...";

        let (rest, grid) = grid(tile)(input).unwrap();

        assert_eq!(rest, "...");
        assert_eq!(grid.row_count(), 1);
        assert_eq!(grid.col_count(), 3);
    }

    #[test]
    fn parse_grid_extra() {
        let tile = char('.');

        let input = "...\n....\n";

        let (rest, grid) = grid(tile)(input).unwrap();

        assert_eq!(rest, "....\n");
        assert_eq!(grid.row_count(), 1);
        assert_eq!(grid.col_count(), 3);
    }

    #[test]
    fn parse_grid_invalid_in_second_row() {
        let tile = char('.');

        let input = "...\n.X.\n";

        let (rest, grid) = grid(tile)(input).unwrap();

        assert_eq!(rest, ".X.\n");
        assert_eq!(grid.row_count(), 1);
        assert_eq!(grid.col_count(), 3);
    }

    #[test]
    fn parse_grid_invalid_in_first_row() {
        let tile = char('.');

        let input = ".X.\n...\n";

        grid(tile)(input).unwrap_err();
    }
}
