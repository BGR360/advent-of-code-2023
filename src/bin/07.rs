advent_of_code::solution!(7);

mod types {
    use std::{cmp::Ordering, collections::HashMap, fmt};

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Card(u8);

    impl Card {
        pub fn label(&self) -> u8 {
            match self.0 {
                2..=9 => b'0' + self.0,
                10 => b'T',
                11 => b'J',
                12 => b'Q',
                13 => b'K',
                14 => b'A',
                _ => unreachable!(),
            }
        }
    }

    impl fmt::Debug for Card {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.label() as char)
        }
    }

    impl fmt::Display for Card {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.label() as char)
        }
    }

    impl TryFrom<u8> for Card {
        type Error = ();

        fn try_from(label: u8) -> Result<Self, Self::Error> {
            let numeric = match label {
                b'2'..=b'9' => label - b'0',
                b'T' => 10,
                b'J' => 11,
                b'Q' => 12,
                b'K' => 13,
                b'A' => 14,
                _ => return Err(()),
            };
            Ok(Card(numeric))
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Hand {
        pub cards: [Card; 5],
    }

    impl TryFrom<[u8; 5]> for Hand {
        type Error = ();

        fn try_from([a, b, c, d, e]: [u8; 5]) -> Result<Self, Self::Error> {
            Ok(Hand {
                cards: [
                    a.try_into()?,
                    b.try_into()?,
                    c.try_into()?,
                    d.try_into()?,
                    e.try_into()?,
                ],
            })
        }
    }

    impl fmt::Display for Hand {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for card in self.cards {
                write!(f, "{}", card)?;
            }
            Ok(())
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum HandKind {
        HighCard,
        OnePair,
        TwoPair,
        ThreeOfAKind,
        FullHouse,
        FourOfAKind,
        FiveOfAKind,
    }

    impl Hand {
        pub fn kind(&self) -> HandKind {
            let mut card_counts: HashMap<Card, usize> = HashMap::new();

            for card in self.cards {
                *card_counts.entry(card).or_default() += 1;
            }

            let count_ns_of_a_kind = |n: usize| card_counts.values().filter(|&&c| c == n).count();
            let has_n_of_a_kind = |n: usize| count_ns_of_a_kind(n) > 0;

            if has_n_of_a_kind(5) {
                return HandKind::FiveOfAKind;
            }
            if has_n_of_a_kind(4) {
                return HandKind::FourOfAKind;
            }
            if has_n_of_a_kind(3) {
                if has_n_of_a_kind(2) {
                    return HandKind::FullHouse;
                } else {
                    return HandKind::ThreeOfAKind;
                }
            }
            let npairs = count_ns_of_a_kind(2);
            if npairs == 2 {
                return HandKind::TwoPair;
            }
            if npairs == 1 {
                return HandKind::OnePair;
            }

            HandKind::HighCard
        }
    }

    impl PartialOrd for Hand {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for Hand {
        fn cmp(&self, other: &Self) -> Ordering {
            let kind = self.kind();
            let other_kind = other.kind();

            if kind == other_kind {
                self.cards.cmp(&other.cards)
            } else {
                kind.cmp(&other_kind)
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Game {
        pub hands_and_bids: Vec<(Hand, u32)>,
    }
}
use types::*;

pub fn part_one(input: &str) -> Option<u32> {
    let mut game = parsing::parse_input(input);

    game.hands_and_bids.sort_by_key(|&(hand, _)| hand);

    let answer = game
        .hands_and_bids
        .into_iter()
        .enumerate()
        .map(|(index, (_hand, bid))| (index + 1, bid))
        .map(|(rank, bid)| rank as u32 * bid)
        .sum();

    Some(answer)
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
}

mod parsing {
    use super::*;

    use advent_of_code::helpers::parsing::{decimal_number, final_parser, line_separated};
    use nom::{bytes::complete::take, character::complete::char, sequence::tuple, IResult, Parser};
    use nom_supreme::ParserExt;

    fn hand(input: &str) -> IResult<&str, Hand> {
        take(5usize)
            .map_res(|slice: &str| {
                let arr: [u8; 5] = slice
                    .as_bytes()
                    .try_into()
                    .expect("should be exactly 5 bytes");
                Hand::try_from(arr)
            })
            .parse(input)
    }

    fn game(input: &str) -> IResult<&str, Game> {
        let line = tuple((hand, char(' '), decimal_number)).map(|(hand, _, bid)| (hand, bid));

        line_separated(line)
            .map(|hands_and_bids| Game { hands_and_bids })
            .parse(input)
    }

    pub fn parse_input(input: &str) -> Game {
        final_parser(game)(input).expect("input should be valid")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(6440));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }
}
