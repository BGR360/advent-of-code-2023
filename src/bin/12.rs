use std::fmt;

use advent_of_code::{debugln, helpers::fmt::FmtExt};
use itertools::Itertools;

advent_of_code::solution!(12);

#[derive(Clone, Copy, PartialEq, Eq, Hash, advent_of_code::helpers::grid::Tile)]
pub enum Condition {
    #[tile('?')]
    Unknown,
    #[tile('#')]
    Damaged,
    #[tile('.')]
    Operational,
}

impl fmt::Debug for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
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

#[allow(unused)]
fn num_allowed_arrangements_slow(record: &ConditionRecord) -> usize {
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

    let valid_arrangements = unknown_indices
        .into_iter()
        .combinations(num_damaged_unknown)
        .map(|damaged_indices| produce_arrangement(&damaged_indices))
        .filter(|arrangement| record.is_valid_arrangement(arrangement));

    let num_allowed = valid_arrangements.count();

    debugln!("{num_allowed:?}");

    num_allowed
}

fn can_place_group_starting_at(record: &ConditionRecord, group_idx: usize, at: usize) -> bool {
    use Condition::*;

    let springs = &record.conditions;
    let group_len = record.damaged_groups[group_idx];

    #[cfg(debug_assertions)]
    let msg = |success: bool| {
        format!(
            "{}{}{}{}",
            "    ".repeated(group_idx),
            springs[0..at]
                .iter()
                .map(|c| c.terminated_by(' '))
                .separated_by(""),
            (at..(at + group_len))
                .filter_map(|i| springs.get(i))
                .map(|_| if success { "✅" } else { "❌" })
                .separated_by(""),
            ((at + group_len)..springs.len())
                .filter_map(|i| springs.get(i))
                .map(|c| c.terminated_by(' '))
                .separated_by("")
        )
    };

    // The spring to the left must be '?' or '.' (or not present).
    if let Some(prev) = at.checked_sub(1) {
        if springs[prev] == Damaged {
            debugln!("{} spring to the left isn't '?' or '.'", msg(false));
            return false;
        }
    }

    // All springs from `at` up to `at + group_len` must be '?' or '#'
    for i in at..(at + group_len) {
        match springs.get(i) {
            Some(Operational) => {
                debugln!("{} spring {i} isn't '?' or '#'", msg(false));
                return false;
            }
            Some(_) => {}
            None => {
                debugln!("{} not enough room for group", msg(false));
                return false;
            }
        }
    }

    // The spring at `at + group_len` must be '?' or '.' (or not present)
    if let Some(end) = springs.get(at + group_len) {
        if *end == Damaged {
            debugln!("{} spring after the end isn't '?' or '.'", msg(false));
            return false;
        }
    }

    debugln!("{}", msg(true));

    true
}

#[derive(Debug, Clone)]
struct Memo {
    springs: Vec<SpringMemo>,
}

#[derive(Debug, Clone)]
struct SpringMemo {
    groups: Vec<GroupMemo>,
}

#[derive(Debug, Default, Clone, Copy)]
struct GroupMemo {
    num_ways: Option<usize>,
}

fn num_ways_to_place_this_and_remaining_groups_at(
    record: &ConditionRecord,
    memo: &mut Memo,
    group_idx: usize,
    at: usize,
) -> usize {
    use Condition::*;

    let springs = &record.conditions;
    let groups = &record.damaged_groups;

    let group_len = groups[group_idx];

    #[cfg(debug_assertions)]
    let indent = format!("{}", "    ".repeated(group_idx));
    #[cfg(debug_assertions)]
    let msg = format!("group {group_idx} @ {at}");

    debugln!("{indent}{msg}");

    let num_ways = if let Some(memoized_result) = memo.springs[at].groups[group_idx].num_ways {
        memoized_result
    } else if group_idx == 0 && springs[0..at].iter().any(|&c| c == Damaged) {
        // If this is the first group, there must be no damaged springs before `at`
        debugln!("{indent}❌ {msg}: there are damaged springs before it");
        0
    } else if !can_place_group_starting_at(record, group_idx, at) {
        debugln!("{indent}❌ {msg}: cannot place");
        0
    } else if group_idx == groups.len() - 1 {
        // If this is the last group, there must be no damaged springs after
        // `at`.
        if ((at + group_len)..springs.len())
            .filter_map(|i| springs.get(i))
            .any(|&c| c == Damaged)
        {
            debugln!("{indent}❌ {msg}: there are damaged springs after it");
            0
        } else {
            1
        }
    } else {
        // Now see how to place the rest of the groups.

        let search_first = at + group_len + 1;

        // Only check as far as the next '#'
        let search_last = (search_first..springs.len())
            .find(|&i| springs.get(i).copied() == Some(Damaged))
            .unwrap_or(springs.len() - 1);

        (search_first..=search_last)
            .map(|place_next_group_at| {
                num_ways_to_place_this_and_remaining_groups_at(
                    record,
                    memo,
                    group_idx + 1,
                    place_next_group_at,
                )
            })
            .sum()
    };

    memo.springs[at].groups[group_idx].num_ways = Some(num_ways);

    if num_ways > 0 {
        debugln!("{indent}✅ {msg}: {num_ways} ways");
    }

    num_ways
}

fn num_allowed_arrangements_fast(record: &ConditionRecord) -> usize {
    let springs = &record.conditions;
    let groups = &record.damaged_groups;

    let n_springs = springs.len();
    let n_groups = groups.len();

    let mut memo = Memo {
        springs: vec![
            SpringMemo {
                groups: vec![GroupMemo::default(); n_groups]
            };
            n_springs
        ],
    };

    (0..springs.len())
        .map(|at| num_ways_to_place_this_and_remaining_groups_at(record, &mut memo, 0, at))
        .sum()
}

pub fn part_one(input: &str) -> Option<usize> {
    let records = parsing::parse_input(input);

    let sum = records
        .iter()
        .map(|record| {
            let num = num_allowed_arrangements_fast(record);
            debugln!("{record} => {num}");
            num
        })
        .sum();

    Some(sum)
}

pub fn part_two(input: &str) -> Option<usize> {
    let records = parsing::parse_input(input);

    let unfolded = records
        .into_iter()
        .map(ConditionRecord::unfold)
        .collect_vec();

    // let sum = unfolded.iter().map(num_allowed_arrangements_fast).sum();
    let sum = unfolded
        .iter()
        .map(|record| {
            let allowed = num_allowed_arrangements_fast(record);
            debugln!("{record} -> {allowed}");
            allowed
        })
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

    #[cfg(test)]
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
        // num_allowed_arrangements(&record)
        num_allowed_arrangements_fast(&record)
    }

    #[test]
    fn misc_a() {
        assert_eq!(num_arrangements("???.### 1,1,3"), 1);
        assert_eq!(num_arrangements(".??..??...?##. 1,1,3"), 4);
        assert_eq!(num_arrangements("?.??..??...?##. 1,1,3"), 8);
        assert_eq!(num_arrangements("?###???????? 3,2,1"), 10);
        assert_eq!(num_arrangements("???????# 2,1"), 5);
        assert_eq!(num_arrangements("?###????????# 3,2,1"), 5);
        assert_eq!(num_arrangements("?###????????? 3,2,1"), 15);

        assert_eq!(num_arrangements("??#??#???????? 1,5,1"), 13);
    }

    fn num_arrangements_unfolded(record: &str) -> usize {
        let record = parsing::parse_record(record);
        let record = record.unfold();
        num_allowed_arrangements_fast(&record)
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

    fn can_place(record: &str) {
        debugln!();
        debugln!("{record} ====================");

        let record = parsing::parse_record(record);

        for group_idx in 0..record.damaged_groups.len() {
            debugln!();
            for at in 0..record.conditions.len() {
                can_place_group_starting_at(&record, group_idx, at);
            }
        }
    }

    #[test]
    fn test_can_place() {
        can_place("???.### 1,1,3");
        can_place(".??..??...?##. 1,1,3");
        can_place("?#?#?#?#?#?#?#? 1,3,1,6");
        can_place("????.#...#... 4,1,1");
        can_place("????.######..#####. 1,6,5");
        can_place("?###???????? 3,2,1");
        // assert!(false);
    }
}
