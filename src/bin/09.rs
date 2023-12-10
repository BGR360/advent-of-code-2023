advent_of_code::solution!(9);

use advent_of_code::debugln;
use itertools::Itertools;

#[derive(Debug, Clone)]
pub struct OasisReport {
    pub histories: Vec<Vec<i32>>,
}

fn predict(seq: &[i32], get_next_fn: impl Fn(&[i32], i32) -> i32) -> i32 {
    fn predict(seq: &[i32], get_next_fn: &impl Fn(&[i32], i32) -> i32, depth: usize) -> i32 {
        #[cfg(debug_assertions)]
        let indent = String::from_utf8(vec![b' '; depth * 4]).unwrap();

        debugln!("{indent}seq:  {seq:?}");

        let next = if seq.iter().all(|&diff| diff == 0) {
            0
        } else {
            let differences = seq
                .iter()
                .copied()
                .tuple_windows()
                .map(|(a, b)| b - a)
                .collect_vec();

            let next_diff = predict(&differences, get_next_fn, depth + 1);

            get_next_fn(seq, next_diff)
        };

        debugln!("{indent}next: {next}");

        next
    }

    predict(seq, &get_next_fn, 0)
}

fn predict_next(seq: &[i32]) -> i32 {
    predict(seq, |seq, next_diff| seq.last().unwrap() + next_diff)
}

fn predict_prev(seq: &[i32]) -> i32 {
    predict(seq, |seq, next_diff| seq.first().unwrap() - next_diff)
}

pub fn part_one(input: &str) -> Option<i32> {
    let report = parsing::parse_input(input);

    let sum = report
        .histories
        .iter()
        .map(|history| predict_next(&history[..]))
        .sum();

    Some(sum)
}

pub fn part_two(input: &str) -> Option<i32> {
    let report = parsing::parse_input(input);

    let sum = report
        .histories
        .iter()
        .map(|history| predict_prev(&history[..]))
        .sum();

    Some(sum)
}

mod parsing {
    use std::str::FromStr;

    use super::*;

    pub fn parse_input(input: &str) -> OasisReport {
        let histories = input
            .lines()
            .map(|line| {
                line.split(' ')
                    .map(i32::from_str)
                    .try_collect()
                    .expect("input should be valid")
            })
            .collect();

        OasisReport { histories }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(114));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(2));
    }
}
