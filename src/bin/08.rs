advent_of_code::solution!(8);

mod types {
    use std::collections::HashMap;

    #[derive(Debug, Clone)]
    pub struct Map {
        pub instructions: Vec<Direction>,
        pub network: HashMap<String, Node>,
    }

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
}
use types::*;

pub fn part_one(input: &str) -> Option<u32> {
    let map = parsing::parse_input(input);

    let mut current = &map.network["AAA"];

    let mut steps = 0;
    let mut next: &str;

    #[allow(clippy::explicit_counter_loop)]
    for &direction in std::iter::repeat(map.instructions.iter()).flatten() {
        next = match direction {
            Direction::Left => &current.left,
            Direction::Right => &current.right,
        };

        steps += 1;
        if next == "ZZZ" {
            break;
        }

        current = &map.network[next];
    }

    Some(steps)
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
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
            "examples", DAY, 1,
        ));
        assert_eq!(result, None);
    }
}
