advent_of_code::solution!(14);

use std::{collections::HashMap, fmt};

use advent_of_code::{debugln, helpers::grid};

pub type Pos = grid::IVec2;
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

#[derive(Debug, Clone)]
pub struct Platform {
    pub tiles: Grid<Tile>,
}

impl Platform {
    pub fn spin_cylce(&mut self) {
        self.tilt(Pos::NEG_Y);
        self.tilt(Pos::NEG_X);
        self.tilt(Pos::Y);
        self.tilt(Pos::X);
    }

    pub fn tilt(&mut self, dir: Pos) {
        let tiles = &mut self.tiles;

        let n_rows = tiles.row_count();
        let n_cols = tiles.col_count();

        if dir == Pos::NEG_Y {
            for row in 0..n_rows {
                for col in 0..n_cols {
                    let pos = tiles.row_col_to_pos(row, col).unwrap();

                    Self::tilt_one(tiles, pos, dir);
                }
            }
        } else if dir == Pos::Y {
            for row in (0..n_rows).rev() {
                for col in 0..n_cols {
                    let pos = tiles.row_col_to_pos(row, col).unwrap();

                    Self::tilt_one(tiles, pos, dir);
                }
            }
        } else if dir == Pos::X {
            for col in (0..n_cols).rev() {
                for row in 0..n_rows {
                    let pos = tiles.row_col_to_pos(row, col).unwrap();

                    Self::tilt_one(tiles, pos, dir);
                }
            }
        } else if dir == Pos::NEG_X {
            for col in 0..n_cols {
                for row in 0..n_rows {
                    let pos = tiles.row_col_to_pos(row, col).unwrap();

                    Self::tilt_one(tiles, pos, dir);
                }
            }
        } else {
            unreachable!();
        }
    }

    fn tilt_one(tiles: &mut Grid<Tile>, pos: Pos, dir: Pos) {
        match tiles[pos] {
            Tile::Empty | Tile::Cube => {}
            Tile::Round => {
                let mut new_pos = pos;
                let mut found = false;
                loop {
                    let Some(Tile::Empty) = tiles.get(new_pos + dir) else {
                        break;
                    };
                    new_pos += dir;

                    found = true;
                }
                if found {
                    Self::move_tile(tiles, pos, new_pos);
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

fn load(platform: &Platform) -> i32 {
    platform
        .tiles
        .indexed_iter()
        .map(|(pos, &tile)| match tile {
            Tile::Round => platform.tiles.row_count() as i32 - pos.y,
            _ => 0,
        })
        .sum()
}

pub fn part_one(input: &str) -> Option<i32> {
    let mut platform = parsing::parse_input(input);

    debugln!();
    debugln!("{platform}");

    platform.tilt(Pos::NEG_Y);

    debugln!();
    debugln!("{platform}");

    let sum = load(&platform);

    Some(sum)
}

fn encode(platform: &Platform) -> Vec<u8> {
    platform.tiles.iter().map(Tile::symbol).collect()
}

pub fn part_two(input: &str) -> Option<i32> {
    let mut platform = parsing::parse_input(input);

    debugln!();
    debugln!("{platform}");

    let mut seen = HashMap::new();

    seen.insert(encode(&platform), (0, platform.clone()));

    let mut first_seen_i = 0;
    let mut first_seen_prev = 0;

    for i in 1.. {
        platform.spin_cylce();

        debugln!();
        debugln!("{platform}");

        let encoded = encode(&platform);
        if let Some((prev_seen_i, _platform)) = seen.get(&encoded) {
            println!("SEEN BEFORE @ {prev_seen_i} (current = {i})");
            first_seen_i = i;
            first_seen_prev = *prev_seen_i;
            break;
        }
        seen.insert(encoded, (i, platform.clone()));
    }

    let period_len = first_seen_i - first_seen_prev;
    debugln!("period_len: {period_len}");

    let final_idx = first_seen_prev + (1_000_000_000 - first_seen_prev) % period_len;

    let final_platform = seen
        .values()
        .find_map(|(idx, platform)| (*idx == final_idx).then_some(platform))
        .unwrap();

    Some(load(final_platform))
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
        assert_eq!(result, Some(64));
    }
}
