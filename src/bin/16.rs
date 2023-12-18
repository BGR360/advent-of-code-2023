advent_of_code::solution!(16);

use std::collections::HashSet;

use advent_of_code::{debugln, helpers::grid};
use smallvec::SmallVec;

pub type Pos = glam::IVec2;
pub type Grid<T> = grid::Grid<T, Pos>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, grid::Tile)]
pub enum Tile {
    #[tile('.')]
    Empty,
    Splitter(Splitter),
    Mirror(Mirror),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, grid::Tile)]
pub enum Splitter {
    #[tile('|')]
    Vert,
    #[tile('-')]
    Horiz,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, grid::Tile)]
pub enum Mirror {
    #[tile('\\')]
    NtoE,
    #[tile('/')]
    WtoS,
}

impl Mirror {
    pub fn reflect(&self, dir: Pos) -> Pos {
        if dir == Pos::X {
            match self {
                Mirror::NtoE => Pos::Y,
                Mirror::WtoS => Pos::NEG_Y,
            }
        } else if dir == Pos::NEG_X {
            match self {
                Mirror::NtoE => Pos::NEG_Y,
                Mirror::WtoS => Pos::Y,
            }
        } else if dir == Pos::Y {
            match self {
                Mirror::NtoE => Pos::X,
                Mirror::WtoS => Pos::NEG_X,
            }
        } else if dir == Pos::NEG_Y {
            match self {
                Mirror::NtoE => Pos::NEG_X,
                Mirror::WtoS => Pos::X,
            }
        } else {
            panic!("invalid argument")
        }
    }
}

fn num_energized_tiles_starting_from(g: &Grid<Tile>, start: Pos, start_dir: Pos) -> usize {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    struct Node {
        dir: Pos,
        pos: Pos,
    }

    let start = Node {
        dir: start_dir,
        pos: start,
    };

    let successors = |node: &Node| -> SmallVec<[Node; 2]> {
        let &Node { dir, pos } = node;

        let tile = g.get(pos).expect("we filter out out-of-bounds successors");

        let succ = |in_dir: Pos| Node {
            dir: in_dir,
            pos: pos + in_dir,
        };

        let is_dir_vert = |dir| dir == Pos::Y || dir == Pos::NEG_Y;
        let is_dir_horiz = |dir| dir == Pos::X || dir == Pos::NEG_X;

        let succs = |succs: &[Node]| -> SmallVec<[Node; 2]> {
            succs
                .iter()
                .copied()
                .filter(|&node| g.contains(node.pos))
                .collect()
        };

        match (tile, dir) {
            (Tile::Empty, _) => succs(&[succ(dir)]),

            (Tile::Splitter(Splitter::Horiz), d) if is_dir_vert(d) => {
                succs(&[succ(Pos::X), succ(Pos::NEG_X)])
            }
            (Tile::Splitter(Splitter::Vert), d) if is_dir_horiz(d) => {
                succs(&[succ(Pos::Y), succ(Pos::NEG_Y)])
            }

            (Tile::Splitter(_), _) => succs(&[succ(dir)]),

            (Tile::Mirror(m), dir) => succs(&[succ(m.reflect(dir))]),
        }
    };

    let reached_positions: HashSet<Pos> = pathfinding::directed::bfs::bfs_reach(start, successors)
        .map(|node| node.pos)
        .collect();

    reached_positions.len()
}

pub fn part_one(input: &str) -> Option<usize> {
    let grid = parsing::parse_input(input);

    Some(num_energized_tiles_starting_from(
        &grid,
        Pos { x: 0, y: 0 },
        Pos::X,
    ))
}

pub fn part_two(input: &str) -> Option<usize> {
    let grid = parsing::parse_input(input);

    let n_rows = grid.row_count() as i32;
    let n_cols = grid.col_count() as i32;

    let make_pos = |(x, y)| Pos { x, y };

    let vert_points = |x| std::iter::repeat(x).zip(0..n_rows).map(make_pos);

    let horiz_points = |y| (0..n_cols).zip(std::iter::repeat(y)).map(make_pos);

    let left_edge = std::iter::repeat(Pos::X).zip(vert_points(0));
    let right_edge = std::iter::repeat(Pos::NEG_X).zip(vert_points(n_cols - 1));
    let top_edge = std::iter::repeat(Pos::Y).zip(horiz_points(0));
    let bot_edge = std::iter::repeat(Pos::NEG_Y).zip(horiz_points(n_rows - 1));

    let max = left_edge
        .chain(right_edge)
        .chain(top_edge)
        .chain(bot_edge)
        .map(|(dir, pos)| {
            let num = num_energized_tiles_starting_from(&grid, pos, dir);
            debugln!("num({pos}, {dir}) = {num}");
            num
        })
        .max()
        .unwrap();

    Some(max)
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    pub fn parse_input(input: &str) -> Grid<Tile> {
        final_parser(grid(Tile::parse))(input).expect("input should be valid")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(46));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(51));
    }
}
