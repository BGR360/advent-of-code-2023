advent_of_code::solution!(15);

fn hash(string: &[u8]) -> usize {
    let mut value: usize = 0;
    for &b in string {
        value += b as usize;
        value *= 17;
        value %= 256;
    }
    value
}

pub fn part_one(input: &str) -> Option<usize> {
    let strings = parsing::parse_input(input);

    let sum = strings.into_iter().map(|s| hash(s.as_bytes())).sum();

    Some(sum)
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
}

mod parsing {
    // use advent_of_code::helpers::parsing::*;

    // use super::*;

    pub fn parse_input(input: &str) -> Vec<&str> {
        // final_parser(separated_list1(
        //     pair(char(','), opt(line_ending)),
        //     take_till(|c| c == ','),
        // ))(input)
        // .expect("input should be valid")

        input.lines().flat_map(|s| s.split(',')).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(1320));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }

    #[test]
    fn test_hash() {
        assert_eq!(hash(b"HASH"), 52);
    }
}
