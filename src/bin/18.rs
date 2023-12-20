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

/// Wraps an iterator and repeats its first element as the last element if the
/// wrapped iterator doesn't already form a closed loop.
pub struct CompleteLoop<I: Iterator> {
    inner: Option<I>,
    first: Option<I::Item>,
    last: Option<I::Item>,
}

impl<I: Iterator> CompleteLoop<I> {
    pub fn new(inner: impl IntoIterator<IntoIter = I>) -> Self {
        Self {
            inner: Some(inner.into_iter()),
            first: None,
            last: None,
        }
    }
}

impl<I> Iterator for CompleteLoop<I>
where
    I: Iterator,
    I::Item: Clone + PartialEq,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.as_mut().and_then(|inner| inner.next());

        match next {
            Some(item) => {
                self.last = Some(item.clone());

                if self.first.is_none() {
                    self.first = Some(item.clone());
                }

                Some(item)
            }
            None => {
                // Inner iterator is done, mark it None so we don't keep
                // calling it.
                self.inner = None;

                // Yield the first item now, if it wasn't yielded last.
                let first = self.first.take();
                let last = self.last.take();
                if last != first {
                    first
                } else {
                    None
                }
            }
        }
    }
}

struct RepeatFirst<I: Iterator> {
    inner: Option<I>,
    first: Option<I::Item>,
}

impl<I: Iterator> RepeatFirst<I> {
    pub fn new(inner: impl IntoIterator<IntoIter = I>) -> Self {
        Self {
            inner: Some(inner.into_iter()),
            first: None,
        }
    }
}

impl<I> Iterator for RepeatFirst<I>
where
    I: Iterator,
    I::Item: Clone + PartialEq,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.as_mut().and_then(|inner| inner.next());

        match next {
            Some(item) => {
                if self.first.is_none() {
                    self.first = Some(item.clone());
                }

                Some(item)
            }
            None => {
                // Inner iterator is done, mark it None so we don't keep
                // calling it.
                self.inner = None;

                // Yield the first item now
                self.first.take()
            }
        }
    }
}

#[cfg(test)]
mod iterator_tests {
    use super::*;

    use itertools::assert_equal;

    #[test]
    fn test_complete_loop() {
        assert_equal(CompleteLoop::new(Vec::<i32>::new()), []);
        assert_equal(CompleteLoop::new([1]), [1]);
        assert_equal(CompleteLoop::new([1, 2, 3]), [1, 2, 3, 1]);
        assert_equal(CompleteLoop::new([1, 2, 1]), [1, 2, 1]);
    }

    #[test]
    fn test_repeat_first() {
        assert_equal(RepeatFirst::new(Vec::<i32>::new()), []);
        assert_equal(RepeatFirst::new([1]), [1, 1]);
        assert_equal(RepeatFirst::new([1, 2, 3]), [1, 2, 3, 1]);
    }
}

/// Computes the area of an arbitrary, non-intersecting polygon using the
/// [shoelace algorithm].
///
/// [shoelace algorithm]: <https://en.wikipedia.org/wiki/Shoelace_formula>
fn area_of_polygon(vertices: impl IntoIterator<Item = Pos>) -> u64 {
    let vertices = CompleteLoop::new(vertices);

    let signed_area: i64 = vertices
        .tuple_windows()
        .map(|(a, b)| (i64::from(a.x) * i64::from(b.y)) - (i64::from(a.y) * i64::from(b.x)))
        .sum();

    signed_area.abs().try_into().unwrap()
}

/// Computes the total number of "city blocks" enclosed by the dig path.
///
/// This is done by:
///
///     1. converting the "tiles space" coordinates of the dig path vertices to
///        "lattice point" coordinates that fully enclose all of the dug tiles.
///
///     2. running the converted coordinates through the shoelace algorithm.
fn area_enclosed_by_dig_path_fast(path: &DigPlan) -> u64 {
    // Consider the dig path:
    //
    //      R 2
    //      D 1
    //      L 1
    //      D 1
    //      L 1
    //      U 2
    //
    // I want to go from this:
    //
    //        0 1 2       0 1 2
    //      0 # # #     0 a - b
    //      1 # # #     1 | d c
    //      2 # # .     2 f e
    //
    // To this:
    //
    //        0 1 2 3      0 1 2 3
    //      0 +-+-+-+    0 a-----b
    //        |#|#|#|      |# # #|
    //      1 +-+-+-+    1 |     |
    //        |#|#|#|      |# # #|
    //      2 +-+-+-+    2 |   d-c
    //        |#|#|.|      |# #|.
    //      3 +-+-+-+    3 f---e
    //
    // So it seems like the general rule for that is:
    //
    //      At every corner in the path, if you are turning "into" the dug tiles,
    //      then go one step further before you turn. If you're turning "away"
    //      from the dug tiles, then don't add one.
    //
    // So first figure out on which side of the path the dug tiles lie.

    let winding_num = winding_number(path.steps.iter().map(|step| step.dir));

    let inside = match winding_num {
        1.. => Side::Right,
        _ => Side::Left,
    };

    let start_pos = Pos::ZERO;

    #[derive(Debug, Clone, Copy)]
    struct ScanState {
        pos: Pos,
        prev_dir: Option<Dir>,
    }

    let polygon_vertices_surrounding_tiles =
        std::iter::once(start_pos).chain(path.steps.iter().scan(
            ScanState {
                pos: start_pos,
                prev_dir: None,
            },
            |state, &DigStep { dir, meters, .. }| {
                let mut pos = state.pos;

                // Advance pos by one more step in the previous direction if this
                // "corner" of the path turns "toward" the inside of the path.
                if let Some(prev_dir) = state.prev_dir {
                    let turn = prev_dir.turn_to(dir).unwrap();
                    if turn != inside {
                        pos += prev_dir;
                    }
                }

                let stride = dir * meters;
                let next_pos = pos + stride;

                state.pos = next_pos;
                state.prev_dir = Some(dir);

                Some(next_pos)
            },
        ));

    #[cfg(debug_assertions)]
    {
        let mut pos = Pos::ZERO;
        let mut grid: Grid<u8> = Grid::new(0, 0);

        let set_dug = |grid: &mut Grid<u8>, pos: Pos| {
            let pos = Pos::ONE + 2 * pos;
            *grid.get_or_grow(pos) = b'#';
        };

        let set_vertex = |grid: &mut Grid<u8>, pos: Pos| {
            let pos = 2 * pos;
            *grid.get_or_grow(pos) = b'*';
        };

        for DigStep { dir, meters, .. } in path.steps.iter().copied() {
            let stride = dir * meters;

            let next_pos = pos + stride;

            for dig_pos in dir.advance(pos).take(meters.try_into().unwrap()) {
                set_dug(&mut grid, dig_pos);
            }

            pos = next_pos;
        }

        for vertex in polygon_vertices_surrounding_tiles.clone() {
            set_vertex(&mut grid, vertex);
        }

        debugln!(
            "{}",
            grid.display_with(|&b| match b {
                0 => '.',
                _ => b as char,
            })
        );
    }

    area_of_polygon(polygon_vertices_surrounding_tiles)
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
