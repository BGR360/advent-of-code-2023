advent_of_code::solution!(6);

mod types {
    #[derive(Debug, Clone, Copy)]
    pub struct Race {
        pub time: u64,
        pub distance: u64,
    }

    impl Race {
        pub const fn new(time: u64, distance: u64) -> Self {
            Self { time, distance }
        }
    }
}
use advent_of_code::debugln;
use types::Race;

const INPUT_PART_ONE: &[Race] = &[
    Race::new(44, 202),
    Race::new(82, 1076),
    Race::new(69, 1138),
    Race::new(81, 1458),
];

fn distance_traveled(time_limit: u64, button_held_for: u64) -> u64 {
    let speed_at_button_release = button_held_for;
    let time_remaining = time_limit.saturating_sub(button_held_for);

    time_remaining * speed_at_button_release
}

fn ways_to_win_race(race: &Race) -> u64 {
    (0..=race.time)
        .map(|button_held_for| distance_traveled(race.time, button_held_for))
        .filter(|&distance| distance > race.distance)
        .count()
        .try_into()
        .expect("integer overflow")
}

fn part_one_impl(races: &[Race]) -> Option<u64> {
    let product = races.iter().map(ways_to_win_race).product();
    Some(product)
}

pub fn part_one(_input: &str) -> Option<u64> {
    part_one_impl(INPUT_PART_ONE)
}

const INPUT_PART_TWO: Race = Race::new(44826981, 202107611381458);

fn ways_to_win_race_fast(race: &Race) -> u64 {
    // Plus one because zero is a distinct option.
    let possible_options = race.time + 1;
    debugln!("possible_options: {possible_options}");

    // The output space is symmetric about the middle. In other words, holding
    // the button for `t` milliseconds is equivalent to holding the button for
    // `time_limit - t` milliseconds.
    //
    // Additionally, the distance traveled is strictly increasing as
    // `time_button_held` approaches `time_limit / 2`.
    //
    // So we can divide the search space in half and perform a binary search to
    // find the point at which we first start exceeding the target distance.

    // Need to add one to the final count if there's an odd number of
    // milliseconds to work with.
    let is_odd = possible_options % 2 == 1;

    // Honestly 22 M isn't that bad to fit in memory. Let's just binary search a
    // slice of that size.

    // Storing the number of milliseconds at each index of the slice would
    // require 22 M * 8 bytes. But I can just store `0u8` at each index and
    // cheat by using a pointer offset to determine the index.
    let len: usize = (possible_options / 2).try_into().expect("integer overflow");
    let search_space = vec![0; len];
    debugln!("len: {len}");

    // Our binary search key function will compute the distance traveled for the
    // given index (where the index is the time we hold the button for).
    let slice_start: *const u8 = &search_space[0] as *const _;
    let key_func = |elt: &u8| {
        // Here's the hack
        let elt_ptr: *const u8 = elt as *const _;
        // SAFETY: this meets all of the safety requirements of `offset_from`
        let elt_index = unsafe { elt_ptr.offset_from(slice_start) };

        let time_button_held = u64::try_from(elt_index).expect("integer_overflow");

        let distance = distance_traveled(race.time, time_button_held);

        debugln!("Tested {time_button_held}, got {distance}");

        distance
    };

    // Search for one past the target distance we want to beat.
    let needle = race.distance + 1;

    // Perform the search.
    let result = search_space.binary_search_by_key(&needle, key_func);

    let index_where_target_distance_first_achieved = match result {
        Ok(exact_index) => exact_index,
        Err(inexact_index) => {
            // I don't want to think too hard about which direction the right
            // answer will be in. So just try all three nearby indices.

            let index_is_good = |index: usize| {
                let time_held = index.try_into().unwrap();
                distance_traveled(race.time, time_held) > race.distance
            };

            if index_is_good(inexact_index - 2) {
                inexact_index - 2
            } else if index_is_good(inexact_index - 1) {
                inexact_index - 1
            } else if index_is_good(inexact_index) {
                inexact_index
            } else if index_is_good(inexact_index + 1) {
                inexact_index + 1
            } else {
                unreachable!("???")
            }
        }
    };

    // Count up how many indices work.
    let num_indices = len - index_where_target_distance_first_achieved;

    // Multiply by two for final answer.
    let num_ways_to_win = num_indices * 2 + if is_odd { 1 } else { 0 };

    debugln!("num_ways_to_win: {num_ways_to_win}");

    num_ways_to_win.try_into().unwrap()
}

fn part_two_impl(race: Race) -> Option<u64> {
    Some(ways_to_win_race_fast(&race))
}

pub fn part_two(_input: &str) -> Option<u64> {
    part_two_impl(INPUT_PART_TWO)
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_PART_ONE: &[Race] = &[Race::new(7, 9), Race::new(15, 40), Race::new(30, 200)];

    #[test]
    fn test_part_one() {
        // let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        let result = part_one_impl(EXAMPLE_PART_ONE);
        assert_eq!(result, Some(288));
    }

    const EXAMPLE_PART_TWO: Race = Race::new(71530, 940200);

    #[test]
    fn test_part_two() {
        // let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        let result = part_two_impl(EXAMPLE_PART_TWO);
        assert_eq!(result, Some(71503));
    }
}
