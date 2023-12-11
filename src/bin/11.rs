advent_of_code::solution!(11);

use std::fmt;

use advent_of_code::helpers::grid;
use itertools::Itertools;

use types::*;
mod types {

    use advent_of_code::debugln;

    use super::*;

    pub type Pos = glam::UVec2;
    pub type Grid<T> = grid::Grid<T, Pos>;

    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Tile {
        #[default]
        Space, // '.'
        Galaxy, // '#'
    }

    impl Tile {
        pub fn symbol(&self) -> u8 {
            match self {
                Tile::Space => b'.',
                Tile::Galaxy => b'#',
            }
        }
    }

    impl fmt::Display for Tile {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.symbol() as char)
        }
    }

    pub type Image = Grid<Tile>;

    #[derive(Debug, Clone)]
    pub struct Map {
        image: Image,
        galaxies: Vec<Pos>,
        empty_rows: Vec<usize>,
        empty_cols: Vec<usize>,
    }

    impl Map {
        pub fn new(image: Image) -> Self {
            let (rows, cols) = image.size();

            let mut galaxies = Vec::new();

            // Start with all rows and cols empty and remove indices from those
            // lists as we find galaxies in the image.
            let mut empty_rows = (0..rows).collect_vec();
            let mut empty_cols = (0..cols).collect_vec();

            for (pos, &tile) in image.indexed_iter() {
                if tile == Tile::Galaxy {
                    galaxies.push(pos);

                    let (row, col) = image.pos_to_row_col(pos).unwrap();
                    empty_rows.retain(|&r| r != row);
                    empty_cols.retain(|&c| c != col);
                }
            }

            Self {
                image,
                galaxies,
                empty_rows,
                empty_cols,
            }
        }

        // pub fn image(&self) -> &Image {
        //     &self.image
        // }

        pub fn galaxies(&self) -> impl Iterator<Item = Pos> + Clone + '_ {
            self.galaxies.iter().copied()
        }

        /// Computes the distance bewteen two points in the map, taking into
        /// account the expansion of empty rows and columns as specified in
        /// puzzle part one.
        pub fn distance_between(&self, a: Pos, b: Pos, empty_multipler: usize) -> usize {
            debugln!("=== DISTANCE BETWEEN {} AND {} ===", a, b);

            if a == b {
                return 0;
            }

            let (a_row, a_col) = self.image.pos_to_row_col(a).unwrap();
            let (b_row, b_col) = self.image.pos_to_row_col(b).unwrap();

            let beg_row = a_row.min(b_row);
            let end_row = a_row.max(b_row);
            debugln!("beg_row={beg_row}, end_row={end_row}");

            let beg_col = a_col.min(b_col);
            let end_col = a_col.max(b_col);
            debugln!("beg_col={beg_col}, end_col={end_col}");

            let n_empty_rows_between = self
                .empty_rows
                .iter()
                .filter(|row| (beg_row..end_row).contains(row))
                .count();

            let n_empty_cols_between = self
                .empty_cols
                .iter()
                .filter(|col| (beg_col..end_col).contains(col))
                .count();

            debugln!("n_empty_row={n_empty_rows_between}, n_empty_col={n_empty_cols_between}");

            let distance = (end_row - beg_row)
                + (end_col - beg_col)
                + (n_empty_rows_between * (empty_multipler - 1))
                + (n_empty_cols_between * (empty_multipler - 1));

            debugln!("Distance between {} and {}: {}", a, b, distance);

            distance
        }
    }
}

fn solution(input: &str, empty_multiplier: usize) -> Option<usize> {
    let map = parsing::parse_input(input);

    let galaxy_pairs = map
        .galaxies()
        .cartesian_product(map.galaxies())
        .collect_vec();

    let sum: usize = galaxy_pairs
        .into_iter()
        .map(|(a, b)| map.distance_between(a, b, empty_multiplier))
        .sum();

    Some(sum / 2)
}

pub fn part_one(input: &str) -> Option<usize> {
    solution(input, 2)
}

pub fn part_two(input: &str) -> Option<usize> {
    solution(input, 1_000_000)
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    fn tile(input: &str) -> IResult<&str, Tile> {
        alt((
            char('.').map(|_| Tile::Space),
            char('#').map(|_| Tile::Galaxy),
        ))(input)
    }

    pub fn parse_input(input: &str) -> super::Map {
        let image = final_parser(grid(tile))(input).expect("input should be valid");

        super::Map::new(image)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(374));
    }

    #[test]
    fn test_part_two_10() {
        let result = solution(&advent_of_code::template::read_file("examples", DAY), 10);
        assert_eq!(result, Some(1030));
    }

    #[test]
    fn test_part_two_100() {
        let result = solution(&advent_of_code::template::read_file("examples", DAY), 100);
        assert_eq!(result, Some(8410));
    }
}
