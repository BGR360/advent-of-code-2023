advent_of_code::solution!(4);

struct ScratchCard {
    pub _id: u32,
    pub winning_numbers: Vec<u32>,
    pub numbers: Vec<u32>,
}

impl ScratchCard {
    pub fn num_matches(&self) -> u32 {
        self.numbers
            .iter()
            .filter(|n| self.winning_numbers.contains(n))
            .count()
            .try_into()
            .expect("integer overflow")
    }

    pub fn score(&self) -> u32 {
        match self.num_matches() {
            0 => 0,
            n => {
                let shift = n - 1;
                1u32.checked_shl(shift).expect("integer overflow")
            }
        }
    }
}

pub fn part_one(input: &str) -> Option<u32> {
    let cards = parsing::parse_input(input);

    let sum = cards.into_iter().map(|card| card.score()).sum();

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u32> {
    let cards = parsing::parse_input(input);

    // Start with one of each card.
    let mut card_counts = vec![1u32; cards.len()];

    for (idx, card) in cards.iter().enumerate() {
        let num_matches: usize = card.num_matches().try_into().expect("integer overflow");

        // Indices of all the cards we win copies of.
        let copies_won = (idx + 1)..(idx + 1 + num_matches);

        // The number of copies we win of each card.
        let num_copies = card_counts[idx];

        for copy_idx in copies_won {
            card_counts[copy_idx] += num_copies;
        }
    }

    Some(card_counts.into_iter().sum())
}

mod parsing {
    use super::*;

    use nom::{
        bytes::complete::tag,
        character::complete::{char, space0, space1},
        multi::many1,
        sequence::tuple,
        IResult, Parser,
    };

    use advent_of_code::helpers::parsing::{decimal_number, final_parser, line_separated};

    fn number(input: &str) -> IResult<&str, u32> {
        tuple((space0, decimal_number)).map(|(_, n)| n).parse(input)
    }

    fn card(input: &str) -> IResult<&str, ScratchCard> {
        let header =
            tuple((tag("Card"), space1, decimal_number, char(':'))).map(|(_, _, id, _)| id);

        let bar = tuple((space0, char('|'))).map(|_| ());

        let mut card = tuple((header, many1(number), bar, many1(number))).map(
            |(id, winning_numbers, _, numbers)| ScratchCard {
                _id: id,
                winning_numbers,
                numbers,
            },
        );

        card.parse(input)
    }

    pub fn parse_input(input: &str) -> Vec<ScratchCard> {
        final_parser(line_separated(card))(input).expect("input should be valid")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(13));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(30));
    }
}
