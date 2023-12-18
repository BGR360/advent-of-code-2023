advent_of_code::solution!(17);

use std::fmt;

use advent_of_code::{
    debugln,
    helpers::grid::{Dir, Grid, Pos},
};
use smallvec::SmallVec;

#[derive(Debug, Clone)]
pub struct Map {
    pub blocks: Grid<u8>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct State {
    pub pos: Pos,
    // None at the start
    pub dir: Option<Dir>,
    pub steps_taken_without_turning: u8,
}

impl State {
    pub fn next(self, next_dir: Dir) -> Self {
        Self {
            pos: self.pos + next_dir,
            dir: Some(next_dir),
            steps_taken_without_turning: if Some(next_dir) == self.dir {
                self.steps_taken_without_turning + 1
            } else {
                1
            },
        }
    }

    pub fn can_proceed(
        &self,
        next_dir: Dir,
        mut movement_rule: impl FnMut(&State) -> MovementState,
    ) -> bool {
        if let Some(dir) = self.dir {
            if next_dir == dir.opposite() {
                return false;
            }
            let MovementState {
                can_turn,
                must_turn,
            } = (movement_rule)(self);

            if !can_turn && next_dir != dir {
                return false;
            }
            if must_turn && next_dir == dir {
                return false;
            }
        }
        true
    }
}

fn dir_to_char(dir: Dir) -> u8 {
    match dir {
        Dir::N => b'^',
        Dir::E => b'>',
        Dir::S => b'v',
        Dir::W => b'<',
    }
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // f.debug_struct("State")
        //     .field("pos", &self.pos)
        //     .field("dir", &self.dir)
        //     .field(
        //         "steps_taken_without_turning",
        //         &self.steps_taken_without_turning,
        //     )
        //     .finish()
        write!(
            f,
            "{}{}{}",
            self.steps_taken_without_turning,
            if let Some(dir) = self.dir {
                dir_to_char(dir) as char
            } else {
                ' '
            },
            self.pos
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MovementState {
    can_turn: bool,
    must_turn: bool,
}

impl Map {
    pub fn minimum_heat_loss(&self, movement_rule: impl Fn(&State) -> MovementState) -> u32 {
        let (n_rows, n_cols) = self.blocks.size();

        let start_pos = Pos::ZERO;
        let end_pos = self.blocks.row_col_to_pos(n_rows - 1, n_cols - 1).unwrap();

        let start = State {
            pos: start_pos,
            dir: None,
            steps_taken_without_turning: 0,
        };

        let success = |state: &State| state.pos == end_pos && movement_rule(state).can_turn;

        let successors = |state: &State| -> SmallVec<[(State, u32); 3]> {
            let succs: SmallVec<[(State, u32); 3]> = Dir::all()
                .into_iter()
                .filter(|&dir| state.can_proceed(dir, |s| movement_rule(s)))
                .map(|dir| state.next(dir))
                .filter(|state| self.blocks.contains(state.pos))
                .map(|state| {
                    let heat_loss = self.blocks[state.pos];
                    (state, heat_loss.into())
                })
                .collect();

            succs
        };

        let (path, total_cost) =
            pathfinding::directed::dijkstra::dijkstra(&start, successors, success)
                .expect("there should be a path");

        #[cfg(debug_assertions)]
        {
            let mut display_path = self.blocks.clone();

            for state in path {
                if let Some(dir) = state.dir {
                    display_path[state.pos] = dir_to_char(dir);
                }
            }

            debugln!(
                "{}",
                display_path.display_with(|&b| match b {
                    0..=9 => b'0' + b,
                    _ => b,
                } as char)
            );
        }

        total_cost
    }
}

pub fn part_one(input: &str) -> Option<u32> {
    let map = parsing::parse_input(input);

    let movement_rule = |state: &State| MovementState {
        can_turn: true,
        must_turn: state.steps_taken_without_turning >= 3,
    };

    Some(map.minimum_heat_loss(movement_rule))
}

pub fn part_two(input: &str) -> Option<u32> {
    let map = parsing::parse_input(input);

    let movement_rule = |state: &State| MovementState {
        can_turn: state.steps_taken_without_turning >= 4,
        must_turn: state.steps_taken_without_turning >= 10,
    };

    Some(map.minimum_heat_loss(movement_rule))
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    pub fn parse_input(input: &str) -> super::Map {
        let blocks = final_parser(grid(single_digit_number))(input).expect("input should be valid");

        super::Map { blocks }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(102));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(94));
    }
}
