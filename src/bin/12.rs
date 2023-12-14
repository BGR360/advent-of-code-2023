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

// #[derive(Debug)]
// struct Arrangements {
//     num_allowed: usize,
//     num_ending_in_operational: usize,
//     num_beginning_in_operational: usize,
// }

fn arrangements(record: &ConditionRecord) -> usize {
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

    // let allowed_arrangements_and_endpoints: Vec<(bool, bool)> =
    //     valid_arrangements.map(|arrangement| {
    //         let beg = *arrangement.first().unwrap() == Condition::Operational;
    //         let end = *arrangement.last().unwrap() == Condition::Operational;
    //         (beg, end)
    //     })
    //     .collect();

    let num_allowed = valid_arrangements.count();
    // let num_beginning_in_operational = allowed_arrangements_and_endpoints
    //     .iter()
    //     .filter(|&&(beg, end)| beg)
    //     .count();
    // let num_ending_in_operational = allowed_arrangements_and_endpoints
    //     .iter()
    //     .filter(|&&(beg, end)| end)
    //     .count();

    // let arr = Arrangements {
    //     num_allowed,
    //     num_ending_in_operational,
    //     num_beginning_in_operational,
    // };

    debugln!("{num_allowed:?}");

    num_allowed
}

fn num_allowed_arrangements(record: &ConditionRecord) -> usize {
    arrangements(record)
}

#[derive(Debug, Default, Clone)]
struct Memo {
    springs: Vec<SpringMemo>,
}

#[derive(Debug, Default, Clone)]
struct SpringMemo {
    groups: Vec<GroupMemo>,
}

#[derive(Debug, Default, Clone, Copy)]
struct GroupMemo {
    num_ways_prior: usize,
    num_ways_at: usize,
}

impl fmt::Display for Memo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n_groups = self.springs[0].groups.len();
        for group_idx in 0..n_groups {
            writeln!(
                f,
                "{group_idx}| {}",
                self.springs
                    .iter()
                    .map(|s_memo| s_memo.groups[group_idx].num_ways_at)
                    .separated_by(' ')
            )?;
            writeln!(
                f,
                "{group_idx}< {}",
                self.springs
                    .iter()
                    .map(|s_memo| s_memo.groups[group_idx].num_ways_prior)
                    .separated_by(' ')
            )?;
        }
        Ok(())
    }
}

/// Returns the number of ways that it's possible to arrange the unknown springs
/// (up to and including `spring_idx`) such that the `group_idx`-th contiguous
/// group of damaged springs terminates at `spring_idx`.
fn num_ways_group_can_end_at(
    record: &ConditionRecord,
    memo: &Memo,
    group_idx: usize,
    spring_idx: usize,
) -> usize {
    use Condition::*;

    let springs = &record.conditions;
    let groups = &record.damaged_groups;
    let group_size = groups[group_idx];

    #[cfg(debug_assertions)]
    let fail_msg = format!("❌ group {group_idx} (len={group_size}) cannot end at {spring_idx}");

    // The next spring must be either '?' or '.' (or nonexistent)
    if let Some(Damaged) = springs.get(spring_idx + 1) {
        debugln!("{fail_msg}: spring {} is '#'", spring_idx + 1);
        return 0;
    }

    // This spring and the previous `group_size - 1` springs must be either '#' or '?'
    for i in 0..group_size {
        if let Some(idx) = spring_idx.checked_sub(i) {
            if let Some(Operational) = springs.get(idx) {
                debugln!("{fail_msg}: spring {idx} is '.'");
                return 0;
            }
        } else {
            debugln!("{fail_msg}: not enough room for group");
            return 0;
        }
    }

    // The spring `group_size` before this spring must be either '?' or '.'
    if let Some(one_before_group_start) = spring_idx.checked_sub(group_size) {
        if let Some(Damaged) = springs.get(one_before_group_start) {
            debugln!("{fail_msg}: spring {one_before_group_start} is '#'");
            return 0;
        }
    }

    // // If this is the last group, there can't be any '#' after it.
    // if (group_idx == groups.len() - 1) && springs[(spring_idx + 1)..].iter().any(|&s| s == Damaged)
    // {
    //     debugln!("{fail_msg}: there are '#' after this index");
    //     return 0;
    // }

    // It can't go here if it would result in too many '#'.
    let damaged_budget: usize = groups[(group_idx + 1)..].iter().sum();
    let remaining_damaged = springs[(spring_idx + 1)..]
        .iter()
        .filter(|&&s| s == Damaged)
        .count();

    if remaining_damaged > damaged_budget {
        debugln!(
            "{fail_msg}: the {} remaining '#' exceed budget of {}",
            remaining_damaged,
            damaged_budget
        );
        return 0;
    }

    // It also can't go here if it wouldn't leave enough room for the rest of
    // the '#'.
    let remaining_usable = springs[(spring_idx + 1)..]
        .iter()
        .filter(|&&s| s == Damaged || s == Unknown)
        .count();
    if remaining_usable < damaged_budget {
        debugln!(
            "{fail_msg}: only {} remaining '#' or '?', need at least {}",
            remaining_usable,
            damaged_budget
        );
        return 0;
    }

    // If all that checks out, then see how many ways the previous groups can be
    // arranged.

    let num_ways = if group_idx == 0 {
        1
    } else if let Some(one_before_group_start) = spring_idx.checked_sub(group_size) {
        memo.springs[one_before_group_start].groups[group_idx - 1].num_ways_prior
    } else {
        debugln!("{fail_msg}: no space for previous groups");
        return 0;
    };

    debugln!(
        "{} group {group_idx} (len={group_size}) can end at {spring_idx} in {num_ways} ways",
        if num_ways == 0 { "❌" } else { "✅" }
    );

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

    for spring_idx in 0..n_springs {
        debugln!("=============== spring {spring_idx} ===============");
        for group_idx in 0..n_groups {
            let num_ways_at = num_ways_group_can_end_at(record, &memo, group_idx, spring_idx);

            let num_ways_prior = if let Some(prior_spring_idx) = spring_idx.checked_sub(1) {
                let prior_memo = &memo.springs[prior_spring_idx].groups[group_idx];

                prior_memo.num_ways_at + prior_memo.num_ways_prior
            } else {
                0
            };

            memo.springs[spring_idx].groups[group_idx] = GroupMemo {
                num_ways_prior,
                num_ways_at,
            };
        }
    }

    let last_memo = memo.springs.last().unwrap().groups.last().unwrap();

    debugln!("{memo}");

    last_memo.num_ways_at + last_memo.num_ways_prior
}

pub fn part_one(input: &str) -> Option<usize> {
    let records = parsing::parse_input(input);

    let sum = records.iter().map(num_allowed_arrangements).sum();

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
            let allowed = num_allowed_arrangements(record);
            println!("{record} -> {allowed}");
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
    }

    fn num_arrangements_unfolded(record: &str) -> usize {
        let record = parsing::parse_record(record);
        let record = record.unfold();
        num_allowed_arrangements_fast(&record)
    }

    #[cfg(no)]
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
