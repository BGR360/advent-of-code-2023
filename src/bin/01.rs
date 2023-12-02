advent_of_code::solution!(1);

use advent_of_code::debugln;
use nom::{branch::alt, bytes::complete::tag, character::complete::satisfy, IResult, Parser};

pub fn part_one(input: &str) -> Option<u32> {
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

    Some(input.lines().map(get_number_from_line).sum())
}

fn ascii_digit(input: &str) -> IResult<&str, u32> {
    // surprisingly difficult to figure out how to take just a single character matching a predicate
    satisfy(|c| c.is_ascii_digit())
        .map(|digit| {
            digit
                .to_digit(10)
                .expect("already validated that it's a digit")
        })
        .parse(input)
}

fn english_digit(input: &str) -> IResult<&str, u32> {
    alt((
        tag("one").map(|_| 1),
        tag("two").map(|_| 2),
        tag("three").map(|_| 3),
        tag("four").map(|_| 4),
        tag("five").map(|_| 5),
        tag("six").map(|_| 6),
        tag("seven").map(|_| 7),
        tag("eight").map(|_| 8),
        tag("nine").map(|_| 9),
    ))(input)
}

fn digit(input: &str) -> IResult<&str, u32> {
    alt((ascii_digit, english_digit))(input)
}

fn get_number_from_line(mut line: &str) -> u32 {
    debugln!("{line}");

    let mut digits = Vec::new();

    while !line.is_empty() {
        if let Ok((_, digit)) = digit(line) {
            digits.push(digit);
        }

        // not kosher for unicode, but the puzzle input is ascii only :)
        line = &line[1..];
    }

    debugln!("{digits:?}");

    let first_digit = digits.first().expect("there should be a first digit");
    let last_digit = digits.last().expect("there should be a last digit");

    let number = (first_digit * 10) + last_digit;

    debugln!("{number}");

    number
}

pub fn part_two(input: &str) -> Option<u32> {
    Some(input.lines().map(get_number_from_line).sum())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file_part(
            "examples", DAY, 1,
        ));
        assert_eq!(result, Some(142));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 2,
        ));
        assert_eq!(result, Some(281));
    }

    #[test]
    fn test_trickies() {
        assert_eq!(part_two("eighthree"), Some(83));
    }
}
