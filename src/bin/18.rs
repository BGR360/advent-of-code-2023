advent_of_code::solution!(18);

use std::collections::btree_map::Range;

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

mod qt {
    use super::*;

    use quadtree_rs::{area::AreaBuilder, point::Point, Quadtree};

    type Area = quadtree_rs::area::Area<i32>;

    #[derive(Debug)]
    pub struct RegionMap {
        qt: Quadtree<i32, Option<Side>>,
        min: Pos,
        max: Pos,
    }

    impl RegionMap {
        pub fn new(min: Pos, max: Pos) -> Self {
            let xrange = max.x.abs_diff(min.x);
            let yrange = max.y.abs_diff(min.y);

            let range = xrange.max(yrange);

            let depth = range.checked_ilog2().unwrap() + 1;

            let mut qt = Quadtree::new(depth.try_into().unwrap());

            qt.insert(Self::make_area_minmax(min, max), None);

            Self { qt, min, max }
        }

        pub fn split(&mut self, cut_line_start: Pos, cut_line_end: Pos) {}

        fn make_point(pos: Pos) -> Point<i32> {
            Point { x: pos.x, y: pos.y }
        }

        fn make_area_minmax(min: Pos, max: Pos) -> Area {
            let width = max.x - min.x + 1;
            let height = max.y - min.y + 1;
            AreaBuilder::default()
                .anchor(Self::make_point(min))
                .dimensions((width, height))
                .build()
                .unwrap()
        }

        fn make_areas_cut_line(&self, cut_line_start: Pos, cut_line_end: Pos) -> (Area, Area) {
            let cut_line_dir = Dir::parallel_to(cut_line_end - cut_line_start)
                .expect("cut line endpoints must be colinear");

            let left = cut_line_dir.turn(Side::Left);
            let right = cut_line_dir.turn(Side::Right);

            let area_left =
                self.make_area_cut_line(cut_line_start + left, cut_line_end + left, Side::Left);

            let area_right =
                self.make_area_cut_line(cut_line_start + right, cut_line_end + right, Side::Right);

            (area_left, area_right)

            // let line_min = cut_line_start.min(cut_line_end);
            // let line_max = cut_line_start.max(cut_line_end);

            // let (top, bot, left, right) = match cut_line_dir {
            //     Dir::N | Dir::S => {
            //         let top = line_min.y;
            //         let bot = line_max.y;

            //         let left = self.min.x;
            //         let right = self.max.x;

            //         (top, bot, left, right)
            //     }
            //     Dir::E | Dir::W => {
            //         let top = self.min.y;
            //         let bot = self.max.y;

            //         let left = line_min.x;
            //         let right = line_max.x;

            //         (top, bot, left, right)
            //     }
            // };

            // let top_left = Pos::new(left, top);
            // let top_right = Pos::new(right, top);
            // let bot_left = Pos::new(left, bot);
            // let bot_right = Pos::new(right, bot);

            // match cut_line_dir {
            //     Dir::N => (
            //         Self::make_area_minmax(top_left, line_max),
            //         Self::make_area_minmax(line_min, bot_right),
            //     ),
            //     Dir::E => (
            //         Self::make_area_minmax(top_left, line_max),
            //         Self::make_area_minmax(line_min, bot_right),
            //     ),
            //     Dir::S => (
            //         Self::make_area_minmax(line_min, bot_right),
            //         Self::make_area_minmax(top_left, line_max),
            //     ),
            //     Dir::W => (
            //         Self::make_area_minmax(line_min, bot_right),
            //         Self::make_area_minmax(top_left, line_max),
            //     ),
            // }
        }

        // fn make_area_cut_line_and_dir(&self, line_a: Pos, line_b: Pos, dir: Dir) -> Area<i32> {
        //     let line_min = line_a.min(line_b);
        //     let line_max = line_a.max(line_b);

        //     let (min, max) = match dir {
        //         Dir::N => (Pos::new(line_min.x, self.min.y), Pos::new()),
        //         Dir::E => {
        //             line_c.x = self.max.x;
        //             line_d.x = self.max.x;
        //         }
        //         Dir::S => {
        //             line_c.y = self.max.y;
        //             line_d.y = self.max.y;
        //         }
        //         Dir::W => {
        //             line_c.x = self.min.x;
        //             line_d.x = self.min.x;
        //         }
        //     };

        //     let min = line_a.min();
        // }

        fn make_area_cut_line(&self, start: Pos, end: Pos, side: Side) -> Area {
            use Dir::*;
            use Side::*;

            let dir = Dir::parallel_to(end - start).unwrap();

            let line_min = start.min(end);
            let line_max = start.max(end);

            let (min, max) = match dir {
                N | S => {
                    let line_x = start.x;
                    let min_y = line_min.y;
                    let max_y = line_max.y;

                    match (dir, side) {
                        (N, Left) | (S, Right) => {
                            // ........
                            // ***|....
                            // ***|....
                            // ........
                            (Pos::new(self.min.x, min_y), Pos::new(line_x, max_y))
                        }

                        (N, Right) | (S, Left) => {
                            // ........
                            // ...|****
                            // ...|****
                            // ........
                            (Pos::new(line_x, min_y), Pos::new(self.max.x, max_y))
                        }

                        _ => unreachable!(),
                    }
                }
                E | W => {
                    let line_y = start.y;
                    let min_x = line_min.x;
                    let max_x = line_max.x;

                    match (dir, side) {
                        (E, Left) | (W, Right) => {
                            // ..****..
                            // ..----..
                            // ........
                            // ........
                            (Pos::new(min_x, self.min.y), Pos::new(max_x, line_y))
                        }

                        (E, Right) | (W, Left) => {
                            // ........
                            // ..----..
                            // ..****..
                            // ..****..
                            (Pos::new(min_x, line_y), Pos::new(max_x, self.max.y))
                        }

                        _ => unreachable!(),
                    }
                }
            };
        }

        // precondition: no part of `cut` escapes `from`
        fn cut_area_out(from: Area, cut: Area) -> SmallVec<[Area; 4]> {
            /*
              a  b   c  d
              +--+---+--+
              |         |
             e+ f+---+g +h
              |  |   |  |
             i+ j+---+k +l
              |         |
              +--+---+--+
              m  n   o  p
            */

            try_area(a, f);
            try_area(b, g);
            try_area(c, h);
            try_area(e, j);
            try_area(g, l);
            try_area(i, n);
            try_area(j, o);
            try_area(k, p);

            let (from_min, from_max) = Self::area_minmax(&from);
            let (cut_min, cut_max) = Self::area_minmax(&cut);

            let four_corners =
                |min: Pos, max: Pos| (min, Pos::new(max.x, min.y), Pos::new(min.x, max.y), max);

            let (a, d, m, p) = four_corners(from_min, from_max);
            let (f, g, j, k) = four_corners(cut_min, cut_max);

            let b = Pos::new(f.x, a.y);
            let c = Pos::new(k.x, a.y);

            let n = Pos::new(f.x, p.y);
            let o = Pos::new(k.x, p.y);

            let e = Pos::new(a.x, f.y);
            let i = Pos::new(a.x, k.y);

            let h = Pos::new(p.x, f.y);
            let l = Pos::new(p.x, k.y);

            let mut remaining_areas = SmallVec::new();

            let mut try_area = |min: Pos, max: Pos| {
                if min != max {
                    remaining_areas.push(Self::make_area_minmax(min, max));
                }
            };

            ()
        }

        fn area_minmax(area: &Area) -> (Pos, Pos) {
            let min = area.anchor();
            let min = Pos { x: min.x, y: min.y };
            let size = Pos {
                x: area.width(),
                y: area.height(),
            };

            let max = min + size - Pos::ONE;

            (min, max)
        }
    }
}
use qt::RegionMap;

fn area_enclosed_by_dig_path_fast(path: &DigPlan) -> u32 {
    let (min, max) = path.min_and_max_positions();

    let mut regions = RegionMap::new(min, max);

    0
}

pub fn part_one(input: &str) -> Option<u32> {
    let plan = parsing::parse_input(input);

    Some(area_enclosed_by_dig_path(&plan))
}

pub fn part_two(input: &str) -> Option<u32> {
    let plan = parsing::parse_input(input).into_alt();

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

    fn hex_digits<'a>(count: usize) -> impl FnMut(&'a str) -> IResult<&'a str, u8> {
        move |input: &str| {
            take_while_m_n(count, count, |c: char| c.is_hex_digit())
                .map(|hex| u8::from_str_radix(hex, 16).expect("input already validated"))
                .parse(input)
        }
    }

    fn alt_step(input: &str) -> IResult<&str, AltStep> {
        let alt_step = preceded(
            char('#'),
            tuple((hex_digits(5), hex_digits(1))).map_res(|(meters, dir)| {
                // 0 means R, 1 means D, 2 means L, and 3 means U.
                let dir = match dir {
                    0 => Dir::E,
                    1 => Dir::S,
                    2 => Dir::W,
                    3 => Dir::N,
                    _ => return Err(()),
                };
                Ok(AltStep {
                    dir,
                    meters: meters.into(),
                })
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
