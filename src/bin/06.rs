advent_of_code::solution!(6);

mod types {
    #[derive(Debug, Clone, Copy)]
    pub struct Race {
        pub time: u32,
        pub distance: u32,
    }

    impl Race {
        pub const fn new(time: u32, distance: u32) -> Self {
            Self { time, distance }
        }
    }
}
use types::Race;

const INPUT: &[Race] = &[
    Race::new(44, 202),
    Race::new(82, 1076),
    Race::new(69, 1138),
    Race::new(81, 1458),
];

fn distance_traveled(time_limit: u32, button_held_for: u32) -> u32 {
    let speed_at_button_release = button_held_for;
    let time_remaining = time_limit.saturating_sub(button_held_for);

    time_remaining * speed_at_button_release
}

fn ways_to_win_race(race: &Race) -> u32 {
    (0..=race.time)
        .map(|button_held_for| distance_traveled(race.time, button_held_for))
        .filter(|&distance| distance > race.distance)
        .count()
        .try_into()
        .expect("integer overflow")
}

fn part_one_impl(races: &[Race]) -> Option<u32> {
    let product = races.iter().map(ways_to_win_race).product();
    Some(product)
}

pub fn part_one(_input: &str) -> Option<u32> {
    part_one_impl(INPUT)
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &[Race] = &[Race::new(7, 9), Race::new(15, 40), Race::new(30, 200)];

    #[test]
    fn test_part_one() {
        // let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        let result = part_one_impl(EXAMPLE);
        assert_eq!(result, Some(288));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }
}
