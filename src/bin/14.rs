advent_of_code::solution!(14);

use std::fmt;

use advent_of_code::{debugln, helpers::grid};
use aoc_helpers::math::CheckedSub;

pub type Pos = grid::UVec2;
pub type Grid<T> = grid::Grid<T, Pos>;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, grid::Tile)]
pub enum Tile {
    #[default]
    #[tile('.')]
    Empty,
    #[tile('O')]
    Round,
    #[tile('#')]
    Cube,
}

#[derive(Debug)]
pub struct Platform {
    pub tiles: Grid<Tile>,
}

impl Platform {
    pub fn tilt_up(&mut self) {
        loop {
            let mut tiles = self.tiles.clone();
            Self::tilt_up_once(&mut tiles);

            if tiles == self.tiles {
                break;
            }

            self.tiles = tiles;
        }
    }

    fn tilt_up_once(tiles: &mut Grid<Tile>) {
        for row in 0..tiles.row_count() {
            for col in 0..tiles.col_count() {
                let pos = tiles.row_col_to_pos(row, col).unwrap();

                match tiles[pos] {
                    Tile::Empty | Tile::Cube => {}
                    Tile::Round => {
                        if let Some(above_pos) = pos.checked_sub(Pos::Y) {
                            if tiles[above_pos] == Tile::Empty {
                                Self::move_tile(tiles, pos, above_pos);
                            }
                        }
                    }
                }
            }
        }
    }

    fn move_tile(tiles: &mut Grid<Tile>, from: Pos, to: Pos) {
        tiles[to] = tiles[from];
        tiles[from] = Tile::Empty;
    }
}

impl fmt::Display for Platform {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.tiles)
    }
}

pub fn part_one(input: &str) -> Option<u32> {
    let mut platform = parsing::parse_input(input);

    debugln!("{platform}");
    debugln!();

    platform.tilt_up();

    debugln!("{platform}");
    debugln!();

    let sum = platform
        .tiles
        .indexed_iter()
        .map(|(pos, &tile)| match tile {
            Tile::Round => platform.tiles.row_count() as u32 - pos.y,
            _ => 0,
        })
        .sum();

    Some(sum)
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    pub fn parse_input(input: &str) -> Platform {
        let tiles = final_parser(grid(Tile::parse))(input).expect("input should be valid");

        Platform { tiles }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(136));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }
}
