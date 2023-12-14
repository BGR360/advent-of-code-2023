advent_of_code::solution!(13);

use advent_of_code::{debugln, helpers::grid};

pub type Pos = grid::IVec2;
pub type Grid<T> = grid::Grid<T, Pos>;

pub type Pattern = Grid<Tile>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, grid::Tile)]
pub enum Tile {
    #[tile('.')]
    Ash,
    #[tile('#')]
    Rock,
}

#[derive(Debug, Clone)]
pub struct Valley {
    pub patterns: Vec<Pattern>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Line {
    Vert {
        /// The numbers of columns to the left of this line.
        cols_left: i32,
    },
    Horiz {
        /// The number of rows above this line.
        rows_up: i32,
    },
}

impl Line {
    pub fn vert(cols_left: i32) -> Self {
        Self::Vert { cols_left }
    }

    pub fn horiz(rows_up: i32) -> Self {
        Self::Horiz { rows_up }
    }
}

fn reflect_pos_about(mut pos: Pos, line: Line) -> Pos {
    let (move_me, about) = match line {
        Line::Vert { cols_left } => (&mut pos.x, cols_left),
        Line::Horiz { rows_up } => (&mut pos.y, rows_up),
    };

    let distance_to_line = about - *move_me;
    let distance_to_mirror_image = distance_to_line * 2;
    let mirror_image_pos = *move_me + distance_to_mirror_image - 1;

    *move_me = mirror_image_pos;

    pos
}

fn is_symmetric_about_line(pattern: &Pattern, line: Line) -> bool {
    let (line_pos, max_pos) = match line {
        Line::Vert { cols_left } => (cols_left, pattern.col_count() as i32),
        Line::Horiz { rows_up } => (rows_up, pattern.row_count() as i32),
    };
    if line_pos == 0 || line_pos == max_pos {
        return false;
    }

    let is_mirrored = |(pos, &tile): (Pos, &Tile)| {
        let mirror_pos = reflect_pos_about(pos, line);
        let mirror_tile = pattern.get(mirror_pos).copied();
        match mirror_tile {
            Some(t) if t == tile => true,
            Some(_) => false,
            // off the map, it's fine
            None => true,
        }
    };

    let is_symmetric = pattern.indexed_iter().all(is_mirrored);

    debugln!("is_symmetric_about({line:?}) = {is_symmetric}");

    is_symmetric
}

fn lines_of_reflection(pattern: &Pattern) -> impl Iterator<Item = Line> + '_ {
    let n_rows = pattern.row_count() as i32;
    let n_cols = pattern.col_count() as i32;

    let vert_lines = (0..n_cols).map(|i| Line::Vert { cols_left: i });
    let horiz_lines = (0..n_rows).map(|i| Line::Horiz { rows_up: i });

    vert_lines
        .chain(horiz_lines)
        .filter(|&line| is_symmetric_about_line(pattern, line))
}

pub fn part_one(input: &str) -> Option<i32> {
    let valley = parsing::parse_input(input);

    let sum = valley
        .patterns
        .iter()
        .flat_map(|pattern| {
            debugln!("{pattern}");
            lines_of_reflection(pattern)
        })
        .map(|line| match line {
            Line::Vert { cols_left } => cols_left,
            Line::Horiz { rows_up } => 100 * rows_up,
        })
        .sum();

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u32> {
    None
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    fn pattern(input: &str) -> Grid<Tile> {
        final_parser(grid(Tile::parse))(input).expect("input should be valid")
    }

    pub fn parse_input(input: &str) -> Valley {
        let patterns = input.split("\n\n").map(pattern).collect();

        Valley { patterns }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(405));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }

    #[test]
    fn test_reflect() {
        use glam::ivec2 as p;

        let zero = p(0, 0);

        assert_eq!(reflect_pos_about(zero, Line::vert(0)), p(-1, 0));
        assert_eq!(reflect_pos_about(zero, Line::vert(1)), p(1, 0));
        assert_eq!(reflect_pos_about(zero, Line::vert(2)), p(3, 0));
        assert_eq!(reflect_pos_about(zero, Line::vert(3)), p(5, 0));

        assert_eq!(reflect_pos_about(zero, Line::horiz(0)), p(0, -1));
        assert_eq!(reflect_pos_about(zero, Line::horiz(1)), p(0, 1));
        assert_eq!(reflect_pos_about(zero, Line::horiz(2)), p(0, 3));
        assert_eq!(reflect_pos_about(zero, Line::horiz(3)), p(0, 5));

        assert_eq!(reflect_pos_about(p(4, 4), Line::vert(4)), p(3, 4));
        assert_eq!(reflect_pos_about(p(4, 4), Line::vert(3)), p(1, 4));

        assert_eq!(reflect_pos_about(p(4, 4), Line::horiz(4)), p(4, 3));
        assert_eq!(reflect_pos_about(p(4, 4), Line::horiz(3)), p(4, 1));
    }
}
