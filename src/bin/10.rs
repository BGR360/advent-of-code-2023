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
    pub type Grid<T> = grid::Grid<T, Pos>;

    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Tile {
        #[default]
        Ground, // '.'
        Start, // 'S'
        Pipe(Pipe),
    }

    impl Tile {
        pub fn symbol(&self) -> u8 {
            match self {
                Tile::Ground => b'.',
                Tile::Start => b'S',
                Tile::Pipe(pipe) => pipe.symbol(),
            }
        }
    }

    impl fmt::Display for Tile {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.symbol() as char)
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Dir {
        N,
        E,
        S,
        W,
    }

    impl Dir {
        pub fn turn_to(&self, other: Dir) -> Option<Side> {
            use Dir::*;
            let side = match (*self, other) {
                (N, E) => Side::Right,
                (N, W) => Side::Left,

                (E, S) => Side::Right,
                (E, N) => Side::Left,

                (S, W) => Side::Right,
                (S, E) => Side::Left,

                (W, N) => Side::Right,
                (W, S) => Side::Left,

                _ => return None,
            };
            Some(side)
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
        pub fn symbol(&self) -> u8 {
            match self {
                Self::NtoS => b'|',
                Self::WtoE => b'-',
                Self::NtoE => b'L',
                Self::NtoW => b'J',
                Self::StoW => b'7',
                Self::StoE => b'F',
            }
        }
    }

    impl fmt::Display for Pipe {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.symbol() as char)
        }
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
        pub tiles: Grid<Tile>,
    }

    impl Map {
        pub fn get(&self, pos: Pos) -> Option<Tile> {
            self.tiles.get(pos).copied()
        }

        pub fn get_pipe(&self, pos: Pos) -> Option<Pipe> {
            match self.get(pos)? {
                Tile::Pipe(pipe) => Some(pipe),
                _ => None,
            }
        }

        pub fn start_pos(&self) -> Option<Pos> {
            self.tiles.indexed_iter().find_map(|(pos, &tile)| {
                // debugln!("pos={pos}, tile={tile:?}");
                (tile == Tile::Start).then_some(pos)
            })
        }

        pub fn neighbors(&self, pos: Pos) -> SmallVec<[Pos; 4]> {
            [Dir::N, Dir::E, Dir::S, Dir::W]
                .into_iter()
                .filter_map(|dir| -> Option<Pos> {
                    let neighbor_pos = pos.add_dir(dir)?;
                    self.get(neighbor_pos).is_some().then_some(neighbor_pos)
                })
                .collect()
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
        pub fn pipes_connected_to(&self, pos: Pos) -> SmallVec<[Pos; 2]> {
            self.neighbors(pos)
                .into_iter()
                .filter(|&neighbor_pos| self.are_connected(pos, neighbor_pos))
                .collect()
        }

        /// Returns a DFS iterator over all the tiles reachable from `start`.
        pub fn iter_path(&self, start: Pos) -> impl Iterator<Item = (Pos, Tile)> + '_ {
            let start = (start, self.get(start).unwrap());

            type Node = (Pos, Tile);

            let successors = |&(pos, _tile): &Node| -> SmallVec<[Node; 2]> {
                self.pipes_connected_to(pos)
                    .into_iter()
                    .map(|pos| (pos, self.get(pos).unwrap()))
                    .collect()
            };
            pathfinding::directed::dfs::dfs_reach(start, successors)
        }
    }

    impl fmt::Display for Map {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.tiles)
        }
    }
}
use smallvec::{smallvec, SmallVec};
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
    let successors = |node: &SearchNode| -> SmallVec<[SearchNode; 2]> {
        debugln!("successors of {node:?}");

        let neighbors = map.pipes_connected_to(node.pos);

        let succs = neighbors
            .into_iter()
            .map(|pos| SearchNode {
                pos,
                distance_from_start: node.distance_from_start + 1,
            })
            .collect();

        debugln!("{succs:?}");

        succs
    };

    let furthest_reachable = pathfinding::directed::bfs::bfs_reach(start, successors)
        .max_by_key(|node| node.distance_from_start)
        .expect("should be at least one reachable tile");

    Some(furthest_reachable.distance_from_start)
}

struct PathPrinter<'a> {
    map: &'a Map,
}

impl fmt::Display for PathPrinter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (rows, cols) = self.map.tiles.size();
        let mut path_tiles = Grid::new(rows, cols);

        for (pos, tile) in self.map.iter_path(self.map.start_pos().unwrap()) {
            *path_tiles.get_mut(pos).unwrap() = tile;
        }

        write!(f, "{path_tiles}")
    }
}

/// Represents whether a given tile is to the left of, to the right of, or on
/// a path being traced by pipes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Side {
    Left,
    On,
    Right,
}

/// Returns the positions that lie "to the left" of a pipe section, "to the
/// right" of , if we're visiting the `pipe` at `pos` by traveling in the
/// direction of `dir`.
///
/// For example:
///
/// ```txt
///  ....    .L..  .R..  ....    ..L.  ..R.  ....    ....  ....  ....
///  .-7.    .O..  .O..  .O..    ..OL  ..OR  ..O.    ....  ....  ....
///  ..J.    .R..  .L..  ....    ....  ....  ....    ..OR  ..OL  ..O.
///
///  (a)     (b)   (c)   (d)     (e)   (f)   (g)     (h)   (i)   (j)
///
/// (a) Initial setup, three pipes
///
/// (b) Results of get_pipe_neighbors_on_side([1, 1], E, *)
/// (c) Results of get_pipe_neighbors_on_side([1, 1], W, *)
/// (d) Results of get_pipe_neighbors_on_side([1, 1], N | S, *)
///
/// (e) Results of get_pipe_neighbors_on_side([2, 1], E, *)
/// (f) Results of get_pipe_neighbors_on_side([2, 1], N, *)
/// (g) Results of get_pipe_neighbors_on_side([2, 1], W | S, *)
///
/// (h) Results of get_pipe_neighbors_on_side([2, 2], E, *)
/// (i) Results of get_pipe_neighbors_on_side([2, 2], S, *)
/// (j) Results of get_pipe_neighbors_on_side([2, 2], N | W, *)
/// ```
fn get_pipe_neighbors_on_side(map: &Map, pos: Pos, dir: Dir, side: Side) -> SmallVec<[Pos; 2]> {
    use Dir::*;
    use Pipe::*;
    use Side::{Left as L, On, Right as R};

    let Some(pipe) = map.get_pipe(pos) else {
        return smallvec![];
    };

    // Not sure if important, but returning these in clockwise order always.
    let dirs_to_neighbors: SmallVec<[Dir; 2]> = match (pipe, (dir, side)) {
        (_, (_, On)) => return smallvec![pos],

        // '|'
        (NtoS, (N, L) | (S, R)) => smallvec![W],
        (NtoS, (S, L) | (N, R)) => smallvec![E],

        // '-'
        (WtoE, (W, L) | (E, R)) => smallvec![S],
        (WtoE, (E, L) | (W, R)) => smallvec![N],

        // 'L'
        (NtoE, (S, L) | (W, R)) => smallvec![],
        (NtoE, (W, L) | (S, R)) => smallvec![S, W],

        // 'J'
        (NtoW, (S, L) | (E, R)) => smallvec![E, S],
        (NtoW, (E, L) | (S, R)) => smallvec![],

        // '7'
        (StoW, (N, L) | (E, R)) => smallvec![],
        (StoW, (E, L) | (N, R)) => smallvec![N, E],

        // 'F'
        (StoE, (N, L) | (W, R)) => smallvec![W, N],
        (StoE, (W, L) | (N, R)) => smallvec![],

        _ => smallvec![],
    };

    dirs_to_neighbors
        .into_iter()
        .filter_map(|dir| -> Option<Pos> {
            let neighbor_pos = pos.add_dir(dir)?;
            map.get(neighbor_pos).is_some().then_some(neighbor_pos)
        })
        .collect()
}

fn positions_and_directions(
    positions: impl IntoIterator<Item = Pos>,
) -> impl Iterator<Item = (Pos, Dir)> {
    positions.into_iter().tuple_windows().map(|(prev, pos)| {
        let dir = prev
            .dir_to(pos)
            .expect("subsequent positions should be adjacent");
        (pos, dir)
    })
}

pub fn part_two(input: &str) -> Option<usize> {
    let map = parsing::parse_input(input);

    debugln!("{}", PathPrinter { map: &map });

    // Traverse the whole pipe path and put it into a vector for easy access.
    let start_pos = map.start_pos().unwrap();
    let path: Vec<Pos> = map.iter_path(start_pos).map(|(pos, _tile)| pos).collect();

    // Go along the path and categorize each neighboring point as being either
    // on the left or right of the path.
    let (rows, cols) = map.tiles.size();
    let mut sides: Grid<Option<Side>> = Grid::new(rows, cols);

    sides[start_pos] = Some(Side::On);

    for (pos, dir) in positions_and_directions(path.iter().copied()) {
        let neighbors: [(Side, SmallVec<_>); 3] = [Side::Left, Side::On, Side::Right].map(|side| {
            let neighbors = get_pipe_neighbors_on_side(&map, pos, dir, side);
            (side, neighbors)
        });

        for (side, side_neighbors) in neighbors {
            for neighbor_pos in side_neighbors {
                let s = &mut sides[neighbor_pos];

                // Don't clobber known path tiles with anything.
                if *s != Some(Side::On) {
                    *s = Some(side);
                }
            }
        }
    }

    debugln!();
    debugln!(
        "{}",
        sides.display_with(|maybe_side| match maybe_side {
            None => '?',
            Some(Side::Left) => 'L',
            Some(Side::On) => '*',
            Some(Side::Right) => 'R',
        })
    );

    // Flood fill remaining tiles.
    let sides = {
        let mut filled_sides: Grid<Side> = Grid::init(rows, cols, Side::On);

        let visit = |&(pos, this_side): &(Pos, Side)| -> SmallVec<[(Pos, Side); 4]> {
            let mut successors = map.neighbors(pos);

            let this_side = match *sides.get(pos).unwrap() {
                Some(known_side) => known_side,
                None => this_side,
            };
            *filled_sides.get_mut(pos).unwrap() = this_side;

            successors.sort_by_key(|&pos| match sides.get(pos).unwrap() {
                // Prioritize those that are on the path first.
                Some(Side::On) => 0,
                // Then prioritize those that we already know the side of.
                Some(_) => 1,
                // Then visit the unknowns.
                None => 2,
            });

            successors
                .into_iter()
                .map(|neighbor_pos| {
                    let neighbor_side = match *sides.get(neighbor_pos).unwrap() {
                        Some(known_side) => known_side,
                        // This is the flood fill; propagate known sides to
                        // unknown tiles.
                        None => this_side,
                    };
                    (neighbor_pos, neighbor_side)
                })
                .collect()
        };
        let start = (start_pos, Side::On);

        pathfinding::directed::bfs::bfs_reach(start, visit).count();

        filled_sides
    };

    debugln!();
    debugln!(
        "{}",
        sides.display_with(|side| match side {
            Side::Left => 'L',
            Side::On => '*',
            Side::Right => 'R',
        })
    );

    // Figure out which side is inside and which side is outside.
    // Do this by calculating the winding number of the path.

    let path_connected = path.iter().copied().chain(std::iter::once(path[0]));
    let directions = positions_and_directions(path_connected).map(|(_pos, dir)| dir);
    let direction_changes = directions.dedup().collect_vec();

    debugln!();
    debugln!("direction changes: {direction_changes:?}");

    let winding_num: i32 = direction_changes
        .into_iter()
        .tuple_windows()
        .map(|(dir_a, dir_b)| {
            let side = dir_a.turn_to(dir_b).unwrap();
            match side {
                Side::Left => -1,
                Side::Right => 1,
                _ => unreachable!(),
            }
        })
        .sum();

    let inside = match winding_num {
        ..=-1 => Side::Left,
        1.. => Side::Right,
        _ => unreachable!(),
    };

    debugln!("Inside = {inside:?}");

    // Finally, count up the number of tiles on that side!

    let sum = sides.iter().filter(|&&side| side == inside).count();

    Some(sum)
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
    fn test_part_one_1() {
        let result = part_one(&advent_of_code::template::read_file_part(
            "examples", DAY, 1,
        ));
        assert_eq!(result, Some(4));
    }

    #[test]
    fn test_part_one_2() {
        let result = part_one(&advent_of_code::template::read_file_part(
            "examples", DAY, 2,
        ));
        assert_eq!(result, Some(4));
    }

    #[test]
    fn test_part_one_3() {
        let result = part_one(&advent_of_code::template::read_file_part(
            "examples", DAY, 3,
        ));
        assert_eq!(result, Some(8));
    }

    #[test]
    fn test_part_two_1() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 1,
        ));
        assert_eq!(result, Some(1));
    }

    #[test]
    fn test_part_two_2() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 2,
        ));
        assert_eq!(result, Some(1));
    }

    #[test]
    fn test_part_two_3() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 3,
        ));
        assert_eq!(result, Some(1));
    }

    #[test]
    fn test_part_two_4() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 4,
        ));
        assert_eq!(result, Some(4));
    }

    #[test]
    fn test_part_two_5() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 5,
        ));
        assert_eq!(result, Some(4));
    }

    #[test]
    fn test_part_two_6() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 6,
        ));
        assert_eq!(result, Some(8));
    }

    #[test]
    fn test_part_two_7() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 7,
        ));
        assert_eq!(result, Some(10));
    }
}
