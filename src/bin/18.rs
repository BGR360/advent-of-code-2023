advent_of_code::solution!(18);

use advent_of_code::{
    debugln,
    helpers::grid::{self, Dir, Grid, Pos, Side},
};
use itertools::Itertools;
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub struct DigPlan {
    pub steps: Vec<DigStep>,
}

#[derive(Debug, Clone, Copy)]
pub struct DigStep {
    pub dir: Dir,
    pub meters: i32,
    pub color: Color,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, grid::Tile)]
// pub enum Dir {
//     #[tile('U')]
//     Up,
//     #[tile('D')]
//     Down,
//     #[tile('L')]
//     Left,
//     #[tile('R')]
//     Right,
// }

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, grid::Tile)]
pub enum Tile {
    #[default]
    #[tile('.')]
    Empty,
    #[tile('#')]
    Dug,
    #[tile('L')]
    Left,
    #[tile('R')]
    Right,
}

fn winding_number(path: impl IntoIterator<Item = Dir>) -> i32 {
    let mut dirs: Vec<Dir> = path.into_iter().collect();
    dirs.push(dirs[0]);

    dirs.into_iter()
        .tuple_windows()
        .map(|(dir1, dir2)| match dir1.turn_to(dir2) {
            Some(Side::Left) => -1,
            Some(Side::Right) => 1,
            None => 0,
        })
        .sum()
}

fn area_enclosed_by_dig_path(path: &DigPlan) -> u32 {
    let mut pos = Pos::ZERO;
    let mut grid: Grid<Tile> = Grid::new(0, 0);

    // Step one: trace out the path specified by the dig plan.
    // Mark tiles as being either left or right of the path.

    for DigStep { dir, meters, .. } in path.steps.iter().copied() {
        debugln!("pos = {pos}");

        let stride = dir * meters;

        let next_pos = pos + stride;
        let left = dir.turn(Side::Left);
        let right = dir.turn(Side::Right);

        grid.grow_to_fit(next_pos);
        grid.grow_to_fit(next_pos + left);
        grid.grow_to_fit(next_pos + right);

        for dig_pos in dir.advance(pos).take(meters.try_into().unwrap()) {
            grid[dig_pos] = Tile::Dug;

            let left = dig_pos + left;
            let right = dig_pos + right;

            if grid[left] != Tile::Dug {
                grid[left] = Tile::Left;
            }
            if grid[right] != Tile::Dug {
                grid[right] = Tile::Right;
            }
        }

        pos = next_pos;

        // debugln!("{grid}");
        // debugln!();
    }

    debugln!();
    debugln!("{grid}");
    debugln!();

    // Step two: flood fill the Ls and the Rs

    let filled = {
        let mut filled = grid.clone();

        // type Node = (Pos, Option<Side>);

        // let start = (Pos::ZERO, None);
        let start = Pos::ZERO;

        let successors = |&pos: &Pos| -> SmallVec<[Pos; 4]> {
            let mut neighbors: SmallVec<[Pos; 4]> = Dir::all().map(|dir| pos + dir).into();
            neighbors.retain(|&mut pos| filled.contains(pos));

            match filled[pos] {
                Tile::Empty => panic!("bug in flood fill"),
                Tile::Dug => {}
                side @ (Tile::Left | Tile::Right) => {
                    for &neighbor_pos in neighbors.iter() {
                        if filled[neighbor_pos] == Tile::Empty {
                            filled[neighbor_pos] = side;
                        }
                    }
                }
            }

            neighbors.retain(|&mut pos| filled[pos] != Tile::Empty);

            neighbors
        };

        pathfinding::directed::bfs::bfs_reach(start, successors).count();

        filled
    };

    debugln!();
    debugln!("{filled}");

    // Decide which side is inside the path.

    let inside = match winding_number(path.steps.iter().map(|step| step.dir)) {
        1.. => Tile::Right,
        _ => Tile::Left,
    };

    // Count em up

    filled
        .iter()
        .filter(|&&tile| tile == Tile::Dug || tile == inside)
        .count()
        .try_into()
        .unwrap()
}

pub fn part_one(input: &str) -> Option<u32> {
    let plan = parsing::parse_input(input);

    Some(area_enclosed_by_dig_path(&plan))
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    fn dir(input: &str) -> IResult<&str, Dir> {
        alt((
            char('U').map(|_| Dir::N),
            char('D').map(|_| Dir::S),
            char('L').map(|_| Dir::W),
            char('R').map(|_| Dir::E),
        ))
        .parse(input)
    }

    fn hex_digit2(input: &str) -> IResult<&str, u8> {
        take_while_m_n(2, 2, |c: char| c.is_hex_digit())
            .map(|hex2| u8::from_str_radix(hex2, 16).expect("input already validated"))
            .parse(input)
    }

    fn color(input: &str) -> IResult<&str, Color> {
        let color = preceded(
            char('#'),
            tuple((hex_digit2, hex_digit2, hex_digit2)).map(|(r, g, b)| Color { r, g, b }),
        );

        delimited(char('('), color, char(')')).parse(input)
    }

    fn step(input: &str) -> IResult<&str, DigStep> {
        ws_tuple((dir, decimal_number, color))
            .map(|(dir, meters, color)| DigStep { dir, meters, color })
            .parse(input)
    }

    pub fn parse_input(input: &str) -> DigPlan {
        let steps = final_parser(line_separated(step))(input).expect("input should be valid");

        DigPlan { steps }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(62));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }
}
