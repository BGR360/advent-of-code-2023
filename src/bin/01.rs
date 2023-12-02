advent_of_code::solution!(1);

fn get_number_from_line(line: &str) -> u32 {
    //let line = line.as_bytes();
    let first_digit = line
        .chars()
        .find(|&c| c.is_ascii_digit())
        .expect("there should be a first digit");
    let last_digit = line
        .chars()
        .rfind(|&c| c.is_ascii_digit())
        .expect("there should be a last digit");

    let first_digit = first_digit.to_digit(10).unwrap();
    let last_digit = last_digit.to_digit(10).unwrap();

    (first_digit * 10) + last_digit
}

pub fn part_one(input: &str) -> Option<u32> {
    Some(input.lines().map(get_number_from_line).sum())
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(142));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }
}
