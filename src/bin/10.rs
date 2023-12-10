advent_of_code::solution!(10);

use std::fmt;

use advent_of_code::{
    debugln,
    helpers::{
        grid,
        math::{CheckedAdd, CheckedSub},
    },
};
use itertools::Itertools;

mod types {
    use super::*;

    pub type Pos = glam::UVec2;
    pub type Grid = grid::Grid<Tile, Pos>;

    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Tile {
        #[default]
        Ground, // '.'
        Start, // 'S'
        Pipe(Pipe),
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Dir {
        N,
        E,
        S,
        W,
    }

    impl Dir {
        pub fn opposite(&self) -> Self {
            match self {
                Dir::N => Dir::S,
                Dir::E => Dir::W,
                Dir::S => Dir::N,
                Dir::W => Dir::E,
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Pipe {
        NtoS, // '|'
        WtoE, // '-'
        NtoE, // 'L'
        NtoW, // 'J'
        StoW, // '7'
        StoE, // 'F'
    }

    impl Pipe {
        pub fn is_connected_to(&self, dir: Dir) -> bool {
            use Dir::*;
            use Pipe::*;

            #[allow(clippy::match_like_matches_macro)]
            match (self, dir) {
                (NtoS, N | S) => true,
                (WtoE, W | E) => true,
                (NtoE, N | E) => true,
                (NtoW, N | W) => true,
                (StoW, S | W) => true,
                (StoE, S | E) => true,
                _ => false,
            }
        }
    }

    pub trait PosExt: Sized {
        /// Returns the pos one unit in the direction of `dir` from `self`, or
        /// `None` if it would over/underflow.
        fn add_dir(self, dir: Dir) -> Option<Self>;

        /// Returns the direction to get from `self` to `other`, or `None` if
        /// the two are not adjacent.
        fn dir_to(self, other: Self) -> Option<Dir>;
    }

    impl PosExt for Pos {
        fn add_dir(self, dir: Dir) -> Option<Self> {
            match dir {
                Dir::N => self.checked_sub(Pos::Y),
                Dir::E => self.checked_add(Pos::X),
                Dir::S => self.checked_add(Pos::Y),
                Dir::W => self.checked_sub(Pos::X),
            }
        }

        fn dir_to(self, other: Self) -> Option<Dir> {
            match other.checked_sub(self) {
                Some(Pos::X) => return Some(Dir::E),
                Some(Pos::Y) => return Some(Dir::S),
                _ => {}
            }
            match self.checked_sub(other) {
                Some(Pos::X) => return Some(Dir::W),
                Some(Pos::Y) => return Some(Dir::N),
                _ => {}
            }
            None
        }
    }

    #[derive(Debug, Clone)]
    pub struct Map {
        pub tiles: Grid,
    }

    impl Map {
        pub fn get(&self, pos: Pos) -> Option<Tile> {
            self.tiles.get(pos).copied()
        }

        // pub fn get_pipe(&self, pos: Pos) -> Option<Pipe> {
        //     match self.get(pos)? {
        //         Tile::Pipe(pipe) => Some(pipe),
        //         _ => None,
        //     }
        // }

        pub fn start_pos(&self) -> Option<Pos> {
            self.tiles.indexed_iter().find_map(|(pos, &tile)| {
                // debugln!("pos={pos}, tile={tile:?}");
                (tile == Tile::Start).then_some(pos)
            })
        }

        pub fn are_connected(&self, pos_a: Pos, pos_b: Pos) -> bool {
            let Some(a) = self.get(pos_a) else {
                return false;
            };
            let Some(b) = self.get(pos_b) else {
                return false;
            };
            let Some(a_to_b) = pos_a.dir_to(pos_b) else {
                return false;
            };
            let Some(b_to_a) = pos_b.dir_to(pos_a) else {
                return false;
            };

            match (a, b) {
                (Tile::Start, Tile::Pipe(pipe)) => {
                    // only the pipe tile (b) needs to be connected in the direction
                    // of start tile (a)
                    pipe.is_connected_to(b_to_a)
                }
                (Tile::Pipe(pipe), Tile::Start) => {
                    // only the pipe tile (a) needs to be connected in the direction
                    // of start tile (b)
                    pipe.is_connected_to(a_to_b)
                }
                (Tile::Pipe(a), Tile::Pipe(b)) => {
                    // Both need to be connected to each other
                    a.is_connected_to(a_to_b) && b.is_connected_to(b_to_a)
                }
                _ => false,
            }
        }

        /// Returns the positions containing pipes that connect to the tile at `pos`.
        pub fn pipes_connected_to(&self, pos: Pos) -> Vec<Pos> {
            [Dir::N, Dir::E, Dir::S, Dir::W]
                .into_iter()
                .filter_map(|dir| -> Option<Pos> {
                    let neighbor_pos = pos.add_dir(dir)?;

                    self.are_connected(pos, neighbor_pos)
                        .then_some(neighbor_pos)
                })
                .collect()
        }
    }
}
use types::*;

pub fn part_one(input: &str) -> Option<usize> {
    let map = parsing::parse_input(input);

    // Do BFS over (pos, distance) pairs
    #[derive(Clone, Copy)]
    struct SearchNode {
        pos: Pos,
        distance_from_start: usize,
    }

    // Only care about `pos` for BFS's visited nodes tracking.
    impl PartialEq for SearchNode {
        fn eq(&self, other: &Self) -> bool {
            self.pos == other.pos
        }
    }
    impl Eq for SearchNode {}
    impl std::hash::Hash for SearchNode {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.pos.hash(state);
        }
    }

    impl fmt::Debug for SearchNode {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "({}, {})", self.pos, self.distance_from_start)
        }
    }

    let start = SearchNode {
        pos: map.start_pos().expect("map should have a start position"),
        distance_from_start: 0,
    };
    let successors = |node: &SearchNode| {
        debugln!("successors of {node:?}");

        let neighbors = map.pipes_connected_to(node.pos);

        let succs = neighbors
            .into_iter()
            .map(|pos| SearchNode {
                pos,
                distance_from_start: node.distance_from_start + 1,
            })
            .collect_vec();

        debugln!("{succs:?}");

        succs
    };

    let furthest_reachable = pathfinding::directed::bfs::bfs_reach(start, successors)
        .max_by_key(|node| node.distance_from_start)
        .expect("should be at least one reachable tile");

    Some(furthest_reachable.distance_from_start)
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    fn pipe(input: &str) -> IResult<&str, Pipe> {
        alt((
            char('|').map(|_| Pipe::NtoS),
            char('-').map(|_| Pipe::WtoE),
            char('L').map(|_| Pipe::NtoE),
            char('J').map(|_| Pipe::NtoW),
            char('7').map(|_| Pipe::StoW),
            char('F').map(|_| Pipe::StoE),
        ))(input)
    }

    fn tile(input: &str) -> IResult<&str, Tile> {
        let tile = alt((
            char('.').map(|_| Tile::Ground),
            char('S').map(|_| Tile::Start),
            pipe.map(Tile::Pipe),
        ));

        terminated(tile, many0(line_ending))(input)
    }

    pub fn parse_input(input: &str) -> super::Map {
        let n_col = input.lines().next().unwrap().len();

        let tiles: Vec<Tile> = final_parser(many1(tile))(input).expect("input should be valid");
        let tiles = Grid::from_vec(tiles, n_col);

        super::Map { tiles }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one_a() {
        let result = part_one(&advent_of_code::template::read_file_part(
            "examples", DAY, 1,
        ));
        assert_eq!(result, Some(4));
    }

    #[test]
    fn test_part_one_b() {
        let result = part_one(&advent_of_code::template::read_file_part(
            "examples", DAY, 2,
        ));
        assert_eq!(result, Some(4));
    }

    #[test]
    fn test_part_one_c() {
        let result = part_one(&advent_of_code::template::read_file_part(
            "examples", DAY, 3,
        ));
        assert_eq!(result, Some(8));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 1,
        ));
        assert_eq!(result, None);
    }
}
