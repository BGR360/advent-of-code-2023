use std::fmt;

use advent_of_code::debugln;
use itertools::Itertools;

advent_of_code::solution!(12);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Condition {
    Unknown,     // '?'
    Damaged,     // '#'
    Operational, // '.'
}

impl Condition {
    pub fn symbol(&self) -> u8 {
        match self {
            Condition::Unknown => b'?',
            Condition::Damaged => b'#',
            Condition::Operational => b'.',
        }
    }
}

impl fmt::Debug for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol() as char)
    }
}

#[derive(Debug, Clone)]
pub struct ConditionRecord {
    /// The (possibly unknown) condition of each spring in this row.
    pub conditions: Vec<Condition>,

    /// The size of each contiguous group of damaged springs in this row.
    pub damaged_groups: Vec<usize>,
}

impl ConditionRecord {
    pub fn num_damaged_springs(&self) -> usize {
        self.damaged_groups.iter().copied().sum()
    }

    pub fn is_valid_arrangement(&self, conditions: &[Condition]) -> bool {
        let contiguous_runs = conditions.iter().group_by(|&condition| condition);

        let damaged_groups = (&contiguous_runs)
            .into_iter()
            .filter_map(|(&condition, run_iter)| {
                if condition == Condition::Damaged {
                    Some(run_iter.count())
                } else {
                    None
                }
            })
            .collect_vec();

        let is_valid = damaged_groups == self.damaged_groups;

        debugln!(
            "{} {}",
            display_conditions(conditions),
            if is_valid { "✅" } else { "❌" }
        );

        is_valid
    }

    pub fn damaged_indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.conditions
            .iter()
            .enumerate()
            .filter_map(|(idx, &condition)| (condition == Condition::Damaged).then_some(idx))
    }

    pub fn unknown_indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.conditions
            .iter()
            .enumerate()
            .filter_map(|(idx, &condition)| (condition == Condition::Unknown).then_some(idx))
    }

    /// "Unfold" the record as part Part Two instructions
    pub fn unfold(self) -> Self {
        let conditions = itertools::repeat_n(self.conditions, 5)
            .intersperse(vec![Condition::Unknown])
            .flatten()
            .collect();

        let damaged_groups = itertools::repeat_n(self.damaged_groups, 5)
            .flatten()
            .collect();

        Self {
            conditions,
            damaged_groups,
        }
    }
}

trait DisplayCondition {
    fn symbol(&self) -> u8;
}

impl DisplayCondition for Condition {
    fn symbol(&self) -> u8 {
        self.symbol()
    }
}

impl DisplayCondition for Option<Condition> {
    fn symbol(&self) -> u8 {
        match self {
            None => b'?',
            Some(c) => c.symbol(),
        }
    }
}

fn display_conditions<T: DisplayCondition>(conditions: &[T]) -> impl fmt::Display + '_ {
    struct DisplayConditions<'a, T> {
        conditions: &'a [T],
    }

    impl<'a, T> fmt::Display for DisplayConditions<'a, T>
    where
        T: DisplayCondition,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for c in self.conditions.iter() {
                let symbol = c.symbol() as char;
                write!(f, "{symbol}")?;
            }
            Ok(())
        }
    }

    DisplayConditions { conditions }
}

fn display_groups(groups: &[usize]) -> impl fmt::Display + '_ {
    struct DisplayGroups<'a> {
        groups: &'a [usize],
    }

    impl fmt::Display for DisplayGroups<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut first = true;
            for &group in self.groups {
                if !first {
                    write!(f, ",")?;
                }
                first = false;

                write!(f, "{group}")?;
            }
            Ok(())
        }
    }

    DisplayGroups { groups }
}

impl fmt::Display for ConditionRecord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}",
            display_conditions(&self.conditions),
            display_groups(&self.damaged_groups),
        )
    }
}

#[derive(Debug)]
struct Arrangements {
    num_allowed: usize,
    num_ending_in_operational: usize,
    num_beginning_in_operational: usize,
}

fn arrangements(record: &ConditionRecord) -> Arrangements {
    debugln!("===== arrangements =====");
    debugln!("record: {record}");

    let num_damaged_total = record.num_damaged_springs();
    let num_damaged_known = record.damaged_indices().count();
    let num_damaged_unknown = num_damaged_total - num_damaged_known;

    let unknown_indices = record.unknown_indices().collect_vec();

    debugln!("num_damaged_total: {num_damaged_total}");
    debugln!("num_damaged_known: {num_damaged_known}");
    debugln!("num_damaged_unknown: {num_damaged_unknown}");
    debugln!("unknown_indices: {unknown_indices:?}");

    // if num_damaged_unknown == 0 {
    //     return 1;
    // }

    let produce_arrangement = |damaged_indices: &[usize]| {
        record
            .conditions
            .iter()
            .copied()
            .enumerate()
            .map(|(idx, condition)| match condition {
                Condition::Unknown => {
                    if damaged_indices.contains(&idx) {
                        Condition::Damaged
                    } else {
                        Condition::Operational
                    }
                }
                _ => condition,
            })
            .collect_vec()
    };

    let allowed_arrangements_and_endpoints: Vec<(bool, bool)> = unknown_indices
        .into_iter()
        .combinations(num_damaged_unknown)
        .map(|damaged_indices| produce_arrangement(&damaged_indices))
        .filter(|arrangement| record.is_valid_arrangement(arrangement))
        .map(|arrangement| {
            let beg = *arrangement.first().unwrap() == Condition::Operational;
            let end = *arrangement.last().unwrap() == Condition::Operational;
            (beg, end)
        })
        .collect();

    let num_allowed = allowed_arrangements_and_endpoints.len();
    let num_beginning_in_operational = allowed_arrangements_and_endpoints
        .iter()
        .filter(|&&(beg, end)| beg)
        .count();
    let num_ending_in_operational = allowed_arrangements_and_endpoints
        .iter()
        .filter(|&&(beg, end)| end)
        .count();

    let arr = Arrangements {
        num_allowed,
        num_ending_in_operational,
        num_beginning_in_operational,
    };

    debugln!("{arr:#?}");

    arr
}

fn num_allowed_arrangements(record: &ConditionRecord) -> usize {
    arrangements(record).num_allowed
}

fn num_allowed_arrangements_unfolded(mut record: ConditionRecord) -> usize {
    debugln!("================= UNFOLDED ===================");

    let Arrangements {
        num_allowed,
        num_ending_in_operational,
        num_beginning_in_operational,
    } = arrangements(&record);

    let mut total_arrangements = 0;

    let num_not_ending_in_operational = num_allowed - num_ending_in_operational;
    total_arrangements +=
        num_not_ending_in_operational * num_allowed * num_allowed * num_allowed * num_allowed;

    if num_ending_in_operational > 0 {
        debugln!("===== ALT 1 =====");

        record.conditions.insert(0, Condition::Unknown);
        let num_allowed_alt = num_allowed_arrangements(&record);

        total_arrangements += num_ending_in_operational
            * num_allowed_alt
            * num_allowed_alt
            * num_allowed_alt
            * num_allowed_alt;

        record.conditions.remove(0);
    }

    // if num_beginning_in_operational > 0 {
    //     debugln!("===== ALT 2 =====");

    //     record.conditions.push(Condition::Unknown);
    //     let num_allowed_alt = num_allowed_arrangements(&record);

    //     total_arrangements += num_beginning_in_operational
    //         * num_allowed_alt
    //         * num_allowed_alt
    //         * num_allowed_alt
    //         * num_allowed_alt;

    //     record.conditions.pop();
    // }

    total_arrangements
}

pub fn part_one(input: &str) -> Option<usize> {
    let records = parsing::parse_input(input);

    let sum = records.iter().map(num_allowed_arrangements).sum();

    Some(sum)
}

pub fn part_two(input: &str) -> Option<usize> {
    let records = parsing::parse_input(input);

    // let unfolded = records
    //     .into_iter()
    //     .map(ConditionRecord::unfold)
    //     .collect_vec();

    let sum = records
        .into_iter()
        .map(num_allowed_arrangements_unfolded)
        .sum();

    Some(sum)
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    fn condition(input: &str) -> IResult<&str, Condition> {
        alt((
            char('?').map(|_| Condition::Unknown),
            char('#').map(|_| Condition::Damaged),
            char('.').map(|_| Condition::Operational),
        ))(input)
    }

    fn row(input: &str) -> IResult<&str, ConditionRecord> {
        let conditions = many1(condition);
        let damaged_groups = separated_list1(char(','), decimal_number);

        tuple((conditions, char(' '), damaged_groups))
            .map(|(conditions, _, damaged_groups)| ConditionRecord {
                conditions,
                damaged_groups,
            })
            .parse(input)
    }

    pub fn parse_input(input: &str) -> Vec<ConditionRecord> {
        final_parser(line_separated(row))(input).expect("input should be valid")
    }

    pub fn parse_record(input: &str) -> ConditionRecord {
        final_parser(row)(input).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one_1() {
        let result = part_one(&advent_of_code::template::read_file_part(
            "examples", DAY, 1,
        ));
        assert_eq!(result, Some(6));
    }

    #[test]
    fn test_part_one_2() {
        let result = part_one(&advent_of_code::template::read_file_part(
            "examples", DAY, 2,
        ));
        assert_eq!(result, Some(21));
    }

    #[test]
    fn test_part_two_2() {
        let result = part_two(&advent_of_code::template::read_file_part(
            "examples", DAY, 2,
        ));
        assert_eq!(result, Some(525152));
    }

    fn num_arrangements(record: &str) -> usize {
        let record = parsing::parse_record(record);
        num_allowed_arrangements(&record)
    }

    #[test]
    fn misc_a() {
        assert_eq!(num_arrangements("???.### 1,1,3"), 1);
        assert_eq!(num_arrangements(".??..??...?##. 1,1,3"), 4);
        assert_eq!(num_arrangements("?.??..??...?##. 1,1,3"), 8);
        assert_eq!(num_arrangements("?###???????? 3,2,1"), 10);
        assert_eq!(num_arrangements("?###????????? 3,2,1"), 15);
    }

    fn num_arrangements_unfolded(record: &str) -> usize {
        let record = parsing::parse_record(record);
        num_allowed_arrangements_unfolded(record)
    }

    #[test]
    fn misc_b() {
        assert_eq!(num_arrangements_unfolded("???.### 1,1,3"), 1);
        assert_eq!(num_arrangements_unfolded(".??..??...?##. 1,1,3"), 16384);
        assert_eq!(num_arrangements_unfolded("?#?#?#?#?#?#?#? 1,3,1,6"), 1);
        assert_eq!(num_arrangements_unfolded("????.#...#... 4,1,1"), 16);
        assert_eq!(num_arrangements_unfolded("????.######..#####. 1,6,5"), 2500);
        assert_eq!(num_arrangements_unfolded("?###???????? 3,2,1"), 506250);
    }
}
