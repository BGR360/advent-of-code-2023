use std::collections::HashMap;

advent_of_code::solution!(15);

#[derive(Debug, Default, Clone)]
pub struct HashMapFacility {
    pub boxes: HashMap<usize, Box>,
}

#[derive(Debug, Default, Clone)]
pub struct Box {
    pub lenses: Vec<Lens>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lens {
    pub label: String,
    pub focal_length: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operation {
    Remove { label: String },
    Insert { lens: Lens },
}

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
    let strings = parsing::parse_input_part_one(input);

    let sum = strings.into_iter().map(|s| hash(s.as_bytes())).sum();

    Some(sum)
}

pub fn part_two(input: &str) -> Option<usize> {
    let operations = parsing::parse_input_part_two(input);

    let mut facility = HashMapFacility::default();

    for operation in operations {
        match operation {
            Operation::Remove { label } => {
                let hash = hash(label.as_bytes());
                let bx = facility.boxes.entry(hash).or_insert(Default::default());

                bx.lenses.retain(|lens| lens.label != label);
            }
            Operation::Insert { lens } => {
                let hash = hash(lens.label.as_bytes());
                let bx = facility.boxes.entry(hash).or_insert(Default::default());

                if let Some(existing) = bx.lenses.iter_mut().find(|l| l.label == lens.label) {
                    existing.focal_length = lens.focal_length;
                } else {
                    bx.lenses.push(lens);
                }
            }
        }
    }

    let sum = facility
        .boxes
        .into_iter()
        .map(|(hash, bx)| {
            let box_num = hash + 1;
            let sum: usize = bx
                .lenses
                .into_iter()
                .enumerate()
                .map(|(idx, lens)| {
                    let slot_num = idx + 1;
                    box_num * slot_num * lens.focal_length
                })
                .sum();
            sum
        })
        .sum();

    Some(sum)
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    pub fn parse_input_part_one(input: &str) -> Vec<&str> {
        // final_parser(separated_list1(
        //     pair(char(','), opt(line_ending)),
        //     take_till(|c| c == ','),
        // ))(input)
        // .expect("input should be valid")

        input.lines().flat_map(|s| s.split(',')).collect()
    }

    fn label(input: &str) -> IResult<&str, String> {
        let (input, label) = alphanumeric1(input)?;
        Ok((input, label.into()))
    }

    fn operation(input: &str) -> IResult<&str, Operation> {
        alt((
            terminated(label, char('-')).map(|label| Operation::Remove { label }),
            separated_pair(label, char('='), decimal_number).map(|(label, focal_length)| {
                Operation::Insert {
                    lens: Lens {
                        label,
                        focal_length,
                    },
                }
            }),
        ))(input)
    }

    pub fn parse_input_part_two(input: &str) -> Vec<Operation> {
        let sep = char(',').and(opt(line_ending));

        final_parser(separated_list1(sep, operation))(input).expect("input should be valid")
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
        assert_eq!(result, Some(145));
    }

    #[test]
    fn test_hash() {
        assert_eq!(hash(b"HASH"), 52);
    }
}
