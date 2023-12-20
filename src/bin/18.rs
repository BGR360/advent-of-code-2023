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

impl DigPlan {
    pub fn into_alt(self) -> Self {
        Self {
            steps: self.steps.into_iter().map(DigStep::into_alt).collect(),
        }
    }

    pub fn min_and_max_positions(&self) -> (Pos, Pos) {
        let mut pos = Pos::ZERO;

        let mut min = Pos::ZERO;
        let mut max = Pos::ZERO;

        for DigStep { dir, meters, .. } in self.steps.iter().copied() {
            let stride = dir * meters;
            let next_pos = pos + stride;
            pos = next_pos;

            min = min.min(pos);
            max = max.max(pos);
        }

        (min, max)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DigStep {
    pub dir: Dir,
    pub meters: i32,
    pub alt: AltStep,
}

impl DigStep {
    pub fn into_alt(self) -> Self {
        Self {
            dir: self.alt.dir,
            meters: self.alt.meters,
            alt: self.alt,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AltStep {
    pub dir: Dir,
    pub meters: i32,
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

fn winding_number(path: impl IntoIterator<Item = Dir>) -> i64 {
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

fn area_enclosed_by_dig_path(path: &DigPlan) -> u64 {
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

fn dir_to_char(dir: Dir) -> char {
    match dir {
        Dir::N => '^',
        Dir::E => '>',
        Dir::S => 'v',
        Dir::W => '<',
    }
}

/// Uses an algorithm loosely inspired by the [shoelace algorithm] but adapted
/// for the challenges of a discrete 2D grid.
///
/// [shoelace algorithm]: <https://en.wikipedia.org/wiki/Shoelace_formula>
fn area_enclosed_by_dig_path_fast(path: &DigPlan) -> u64 {
    // let winding_num = winding_number(path.steps.iter().map(|step| step.dir));

    let mut pos = Pos::ZERO;
    let mut area: i64 = 0;

    for step @ DigStep { dir, meters, .. } in path.steps.iter().copied() {
        let addtl_area = if dir.is_horizontal() {
            let stride = dir * meters;
            debug_assert_eq!(stride.y, 0);
            let stride = stride.x;

            // Add (or subtract) one to the width to ensure we include the tiles
            // carved out by the previous vertical edge.
            let signed_width = stride + stride.signum();
            let signed_height = pos.y + pos.y.signum();

            let signed_area = i64::from(signed_height) * i64::from(signed_width);

            signed_area
        } else {
            // Vertical edges contribute no area (the area carved out by them will
            // be included when we do the next horizontal edge).
            let stride = dir * meters;
            debug_assert_eq!(stride.x, 0);
            let stride = stride.y;

            // let signed_height =

            // let signed_area = i64::from(stride);
            0
        };

        debugln!("{} {}{} => {addtl_area}", pos, dir_to_char(dir), meters);

        area += addtl_area;

        pos += dir * meters;
    }

    let area: u64 = area.abs().try_into().unwrap();
    area
}

pub fn part_one(input: &str) -> Option<u64> {
    let plan = parsing::parse_input(input);

    Some(area_enclosed_by_dig_path_fast(&plan))
}

pub fn part_two(input: &str) -> Option<u64> {
    let plan = parsing::parse_input(input).into_alt();

    Some(area_enclosed_by_dig_path_fast(&plan))
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

    // fn hex_digits<'a, T>(count: usize) -> impl FnMut(&'a str) -> IResult<&'a str, T>
    // where
    //     T: FromStr,
    //     <T as FromStr>::Err: fmt::Debug,
    // {
    //     move |input: &str| {
    //         take_while_m_n(count, count, |c: char| c.is_hex_digit())
    //             .map(|hex| u8::from_str_radix(hex, 16).expect("input already validated"))
    //             .parse(input)
    //     }
    // }

    fn alt_step(input: &str) -> IResult<&str, AltStep> {
        let alt_step = preceded(
            char('#'),
            tuple((hex_number_m_n(5, 5), hex_number_m_n::<u8>(1, 1))).map_res(|(meters, dir)| {
                // 0 means R, 1 means D, 2 means L, and 3 means U.
                let dir = match dir {
                    0 => Dir::E,
                    1 => Dir::S,
                    2 => Dir::W,
                    3 => Dir::N,
                    _ => return Err(()),
                };
                Ok(AltStep { dir, meters })
            }),
        );

        delimited(char('('), alt_step, char(')')).parse(input)
    }

    fn step(input: &str) -> IResult<&str, DigStep> {
        ws_tuple((dir, decimal_number, alt_step))
            .map(|(dir, meters, alt)| DigStep { dir, meters, alt })
            .parse(input)
    }

    pub fn parse_input(input: &str) -> DigPlan {
        let steps = final_parser(line_separated(step))(input).expect("input should be valid");

        DigPlan { steps }
    }

    fn step_simple(input: &str) -> IResult<&str, DigStep> {
        ws_tuple((dir, decimal_number))
            .map(|(dir, meters)| DigStep {
                dir,
                meters,
                alt: AltStep { dir, meters },
            })
            .parse(input)
    }

    pub fn parse_input_simple(input: &str) -> DigPlan {
        let steps = final_parser(separated_list1(space1, step_simple))(input)
            .expect("input should be valid");

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
        assert_eq!(result, Some(952408144115));
    }

    #[track_caller]
    fn do_test(input: &str) {
        let plan = parsing::parse_input_simple(input);
        let expected = area_enclosed_by_dig_path(&plan);
        let actual = area_enclosed_by_dig_path_fast(&plan);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_misc01() {
        do_test("R 4 D 4 L 4 U 4");
        do_test("D 4 L 4 U 4 R 4");
        do_test("L 4 U 4 R 4 D 4");
        do_test("U 4 R 4 D 4 L 4");

        do_test("R 4 U 4 L 4 D 4");
        do_test("U 4 L 4 D 4 R 4");
        do_test("L 4 D 4 R 4 U 4");
        do_test("D 4 R 4 U 4 L 4");
    }

    /*
    #####
    #***#
    S##*#
    ..#*#
    ..###
     */
    #[test]
    fn test_misc02() {
        do_test("R 2 D 2 R 2 U 4 L 4 D 2");
    }

    /*
    #####
    #***#
    #***#
    S##*#
    ..#*#
    ..###
     */
    #[test]
    fn test_misc03() {
        do_test("R 2 D 2 R 2 U 5 L 4 D 3");
    }

    /*
    #####
    #***#
    S##*#
    ..###
     */
    #[test]
    fn test_misc04() {
        do_test("R 2 D 1 R 2 U 3 L 4 D 2");
    }

    /*
    #####
    #***#
    #***#
    S##*#
    ..###
     */
    #[test]
    fn test_misc05() {
        do_test("R 2 D 1 R 2 U 4 L 4 D 3");
    }

    /*
    S####
    #***#
    ###*#
    ..#*#
    ..###
     */
    #[test]
    fn test_misc06() {
        do_test("R 4 D 4 L 2 U 2 L 2 U 2");
    }

    /*
    ######
    #****#
    S###*#
    ...#*#
    ...###
     */
    #[test]
    fn test_misc07() {
        do_test("R 3 D 2 R 2 U 4 L 5 D 2");
    }

    /*
    ######
    #****#
    S##**#
    ..#**#
    ..####
     */
    #[test]
    fn test_misc08() {
        do_test("R 2 D 2 R 3 U 4 L 5 D 2");
    }

    /*
    ####
    #**#
    S#*#
    .###
     */
    #[test]
    fn test_misc09() {
        do_test("R 1 D 1 R 2 U 3 L 3 D 2");
    }

    /*
    #####
    #***#
    S#*##
    .###.
     */
    #[test]
    fn test_misc10() {
        do_test("R 1 D 1 R 2 U 1 R 1 U 2 L 4 D 2");
    }

    /*
    #####
    S***#
    ##*##
    .###.
     */
    #[test]
    fn test_misc11() {
        do_test("D 1 R 1 D 1 R 2 U 1 R 1 U 2 L 4 D 1");
    }
}
