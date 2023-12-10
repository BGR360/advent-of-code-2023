advent_of_code::solution!(8);

use std::collections::HashMap;

use advent_of_code::debugln;
use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub left: String,
    pub right: String,
}

#[derive(Debug, Clone)]
pub struct Map {
    pub instructions: Vec<Direction>,
    pub network: HashMap<String, Node>,
}

impl Map {
    /// Returns an infinite iterator over the node labels that would be visited
    /// when starting at `start_label`.
    pub fn traverse_from<'a>(&'a self, start_label: &'a str) -> impl Iterator<Item = &'a str> + 'a {
        Pos {
            map: self,
            steps_taken: 0,
            label: start_label,
        }
        .map(|pos| pos.label)
    }
}

#[derive(Debug, Clone, Copy)]
struct Pos<'a> {
    pub map: &'a Map,
    pub steps_taken: usize,
    pub label: &'a str,
}

impl<'a> Pos<'a> {
    pub fn next(self) -> Self {
        let Pos {
            map,
            steps_taken,
            label,
        } = self;

        let dir = map.instructions[self.instruction_index()];
        let next_label = match dir {
            Direction::Left => map.network[label].left.as_str(),
            Direction::Right => map.network[label].right.as_str(),
        };

        Pos {
            map,
            steps_taken: steps_taken + 1,
            label: next_label,
        }
    }

    pub fn instruction_index(&self) -> usize {
        self.steps_taken % self.map.instructions.len()
    }
}

impl PartialEq for Pos<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.label == other.label && self.instruction_index() == other.instruction_index()
    }
}

impl<'a> Iterator for Pos<'a> {
    type Item = Self;

    fn next(&mut self) -> Option<Self::Item> {
        let next = *self;
        *self = Pos::next(*self);
        Some(next)
    }
}

pub fn part_one(input: &str) -> Option<usize> {
    let map = parsing::parse_input(input);

    let start = "AAA";

    let (end_idx, _end_label) = map
        .traverse_from(start)
        .enumerate()
        .find(|(_idx, label)| *label == "ZZZ")
        .unwrap();

    Some(end_idx)
}

#[derive(Debug, Clone, Copy)]
pub struct Cycle {
    pub start: usize,
    pub len: usize,
}

/// Implements Floyd's "tortoise and hare" cycle detection algorithm.
///
/// References:
/// - <https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare>
/// - <https://visualgo.net/en/cyclefinding>
fn find_cycle<T: Clone + PartialEq>(start: T, next: impl Fn(T) -> T) -> Cycle {
    // Phase 1: chase each other til they meet.
    let meet_point = {
        let mut tort = next(start.clone());
        let mut hare = next(next(start.clone()));
        while tort != hare {
            tort = next(tort);
            hare = next(next(hare));
        }
        tort
    };

    // Phase 2: reset one cursor to the start and increment both one by one
    // until they meet. This is the start of the cycle.
    let (cycle_start_idx, cycle_start) = {
        let mut idx = 0;
        let mut cur1 = start;
        let mut cur2 = meet_point;
        while cur1 != cur2 {
            cur1 = next(cur1);
            cur2 = next(cur2);
            idx += 1;
        }
        (idx, cur1)
    };

    // Phase 3: Go once around the cycle to find its length.
    let mut cur = next(cycle_start.clone());
    let mut cycle_len = 0;
    while cur != cycle_start {
        cur = next(cur);
        cycle_len += 1;
    }

    Cycle {
        start: cycle_start_idx,
        len: cycle_len,
    }
}

impl Map {
    pub fn find_cycle<'a>(&'a self, start: &'a str) -> Cycle {
        let start = Pos {
            map: self,
            steps_taken: 0,
            label: start,
        };

        find_cycle(start, Pos::next)
    }
}

/*

      y = m1*x + b1
      y = m2*x + b2

Subtract both equations:

      0 = (m1-m2)*x + b1 - b2

  b2-b1 = (m1-m2)*x

  b2-b1
  ----- = x
  m1-m2

*/

/// An infinite iterator through all the potential end positions of a
/// path starting at `start_label`.
///
/// Assumes the path contains a cycle and avoids iterating through every single
/// step of the path for efficiency.
#[derive(Debug)]
struct PotentialEndPositions<'a> {
    // never changed
    step_sizes: Vec<usize>,
    // never changed
    cycle_len: usize,
    // updated every iteration
    pos: Pos<'a>,
    // popped from on every iteration, reset to full after each pass through
    // the cycle
    remaining_steps: std::vec::IntoIter<usize>,
    // updated on every iteration, reset to full after each pass through the
    // cycle
    remaining_cycle_len: usize,
}

impl<'a> PotentialEndPositions<'a> {
    pub fn new(map: &'a Map, start_label: &'a str) -> Self {
        debugln!("=== Creating end positions iterator for '{start_label}' ===");

        // Find the cycle.
        let cycle = map.find_cycle(start_label);

        debugln!("Cycle: {cycle:?}");

        // Find all the positions within that cycle where we visit a node ending in
        // 'Z'. Those are potential stop nodes.
        let end_offsets: Vec<usize> = map
            .traverse_from(start_label)
            .skip(cycle.start)
            .enumerate()
            .take(cycle.len + 1)
            .filter_map(|(offset, label)| {
                //debugln!("({offset}, {label})");
                label.ends_with('Z').then_some(offset)
            })
            .collect();

        debugln!("End offsets: {end_offsets:?}");

        // Convert list of offsets to list of step sizes by taking the difference
        // between each consecutive element.
        let step_sizes: Vec<usize> = std::iter::once(0)
            .chain(end_offsets)
            .tuple_windows()
            .map(|(a, b)| b - a)
            .collect();

        debugln!("Step sizes: {step_sizes:?}");

        // Start the iterator out at the start of the cycle.
        let pos = Pos {
            map,
            label: "ZZZ",
            steps_taken: cycle.start,
        };

        Self {
            cycle_len: cycle.len,
            pos,
            remaining_cycle_len: cycle.len,
            remaining_steps: step_sizes.clone().into_iter(),
            step_sizes,
        }
    }
}

impl<'a> Iterator for PotentialEndPositions<'a> {
    type Item = Pos<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(next_step_size) = self.remaining_steps.next() {
                self.pos.steps_taken += next_step_size;
                // XXX: not currently updating pos.label

                self.remaining_cycle_len -= next_step_size;

                return Some(self.pos);
            }

            // Add on any remaining cycle length to the current step count
            self.pos.steps_taken += self.remaining_cycle_len + 1;

            self.remaining_cycle_len = self.cycle_len;
            self.remaining_steps = self.step_sizes.clone().into_iter();
        }
    }
}

/*
Very intersting, check this out:

the difference between `len` and `end_offset` is 1:1 proportional with `cycle.start`

=== Creating end positions iterator for 'NVA' ===
Cycle: Cycle { start: 5, len: 20568 }
End offsets: [20564]
Step sizes: [20564]
=== Creating end positions iterator for 'GQA' ===
Cycle: Cycle { start: 4, len: 22410 }
End offsets: [22407]
Step sizes: [22407]
=== Creating end positions iterator for 'AAA' ===
Cycle: Cycle { start: 4, len: 18726 }
End offsets: [18723]
Step sizes: [18723]
=== Creating end positions iterator for 'HBA' ===
Cycle: Cycle { start: 2, len: 14428 }
End offsets: [14427]
Step sizes: [14427]
=== Creating end positions iterator for 'GVA' ===
Cycle: Cycle { start: 5, len: 16270 }
End offsets: [16266]
Step sizes: [16266]
=== Creating end positions iterator for 'XCA' ===
Cycle: Cycle { start: 2, len: 24252 }
End offsets: [24251]
Step sizes: [24251]


maybe LCM(20564+5, 22407+4, 18723+4, 14427+2, 16266+5, 24251+2)

Yep that's the answer!!!
*/

pub fn part_two(input: &str) -> Option<usize> {
    let map = parsing::parse_input(input);

    // Find all the nodes that end in 'A'. We start there.
    let start_labels: Vec<&str> = map
        .network
        .keys()
        .filter(|label| label.ends_with('A'))
        .map(String::as_str)
        .collect();

    // let mut iterators: Vec<(&str, _)> = start_labels
    //     .iter()
    //     .map(|&label| (label, PotentialEndPositions::new(&map, label)))
    //     .collect();

    // #[derive(Debug)]
    // struct State<'a> {
    //     pos: Pos<'a>,
    //     iter: PotentialEndPositions<'a>,
    // }

    // Create an iterator for each path that iterates through potential end
    // positions.
    struct State<'a> {
        pos: Pos<'a>,
        iter: PotentialEndPositions<'a>,
    }

    impl<'a> State<'a> {
        fn new(mut iter: PotentialEndPositions<'a>) -> Self {
            Self {
                pos: iter.next().expect("iterator is infinite"),
                iter,
            }
        }

        fn pos(&self) -> Pos<'a> {
            self.pos
        }

        fn advance(&mut self) {
            self.pos = self.iter.next().expect("iterator is infinite")
        }
    }

    type States<'a> = HashMap<&'a str, State<'a>>;

    let mut states: States<'_> = start_labels
        .iter()
        .map(|&label| (label, State::new(PotentialEndPositions::new(&map, label))))
        .collect();

    debugln!("===========");

    // THE REST OF THIS CODE TAKES TOO LONG
    return None;

    // Loop until all paths have the same position.
    let done = |states: &States<'_>| {
        let target = states.values().next().unwrap().pos().steps_taken;

        if states
            .values()
            .skip(1)
            .all(|state| state.pos().steps_taken == target)
        {
            Some(target)
        } else {
            None
        }
    };
    let final_steps = loop {
        if let Some(final_steps) = done(&states) {
            break final_steps;
        }

        #[cfg(debug_assertions)]
        {
            debugln!("Current state: {{");
            for (label, state) in &states {
                debugln!("    {label}: {}", state.pos().steps_taken);
            }
            debugln!("}}");
        }

        // Pick the path with the smallest step count and advance it by one.
        let (label, state) = states
            .iter_mut()
            .min_by_key(|(_, state)| state.pos().steps_taken)
            .unwrap();

        let before = state.pos().steps_taken;
        state.advance();
        let after = state.pos().steps_taken;

        debugln!("{label} {before} -> {after}");
    };

    Some(final_steps)
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    fn direction(input: &str) -> IResult<&str, Direction> {
        alt((
            char('L').map(|_| Direction::Left),
            char('R').map(|_| Direction::Right),
        ))(input)
    }

    fn label(input: &str) -> IResult<&str, String> {
        take(3usize).map(String::from).parse(input)
    }

    fn node(input: &str) -> IResult<&str, (String, Node)> {
        let label_pair = tuple((label, tag(", "), label)).map(|(left, _, right)| (left, right));

        tuple((
            label,
            tag(" = "),
            delimited(char('('), label_pair, char(')')),
        ))
        .map(|(label, _, (left, right))| (label, Node { left, right }))
        .parse(input)
    }

    fn parse_map(input: &str) -> IResult<&str, super::Map> {
        let instructions = many1(direction);

        tuple((instructions, tag("\n\n"), line_separated(node)))
            .map(|(instructions, _, nodes)| super::Map {
                instructions,
                network: nodes.into_iter().collect(),
            })
            .parse(input)
    }

    pub fn parse_input(input: &str) -> super::Map {
        final_parser(parse_map)(input).expect("input should be valid")
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
        assert_eq!(result, Some(2));
    }

    #[test]
    fn test_part_one_b() {
        let result = part_one(&advent_of_code::template::read_file_part(
            "examples", DAY, 2,
        ));
        assert_eq!(result, Some(6));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 3,
        ));
        assert_eq!(result, Some(6));
    }
}
