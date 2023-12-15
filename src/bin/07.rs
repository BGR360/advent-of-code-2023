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

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct JokerCard(u8);

    impl JokerCard {
        pub fn label(&self) -> u8 {
            match self.0 {
                1 => b'J',
                2..=9 => b'0' + self.0,
                10 => b'T',
                12 => b'Q',
                13 => b'K',
                14 => b'A',
                _ => unreachable!(),
            }
        }

        pub fn is_joker(&self) -> bool {
            self.label() == b'J'
        }

        pub fn alternatives(&self) -> Vec<Card> {
            if self.is_joker() {
                vec![
                    Card::try_from(b'2').unwrap(),
                    Card::try_from(b'3').unwrap(),
                    Card::try_from(b'4').unwrap(),
                    Card::try_from(b'5').unwrap(),
                    Card::try_from(b'6').unwrap(),
                    Card::try_from(b'7').unwrap(),
                    Card::try_from(b'8').unwrap(),
                    Card::try_from(b'9').unwrap(),
                    Card::try_from(b'T').unwrap(),
                    Card::try_from(b'J').unwrap(),
                    Card::try_from(b'Q').unwrap(),
                    Card::try_from(b'K').unwrap(),
                    Card::try_from(b'A').unwrap(),
                ]
            } else {
                vec![Card::try_from(self.label()).unwrap()]
            }
        }
    }

    impl fmt::Debug for JokerCard {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.label() as char)
        }
    }

    impl fmt::Display for JokerCard {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.label() as char)
        }
    }

    impl TryFrom<u8> for JokerCard {
        type Error = ();

        fn try_from(label: u8) -> Result<Self, Self::Error> {
            let numeric = match label {
                b'J' => 1,
                b'2'..=b'9' => label - b'0',
                b'T' => 10,
                b'Q' => 12,
                b'K' => 13,
                b'A' => 14,
                _ => return Err(()),
            };
            Ok(JokerCard(numeric))
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Hand<C> {
        pub cards: [C; 5],
    }

    impl<C: TryFrom<u8>> TryFrom<[u8; 5]> for Hand<C> {
        type Error = <C as TryFrom<u8>>::Error;

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

    impl<C: fmt::Display + Copy> fmt::Display for Hand<C> {
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

    impl Hand<Card> {
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

    impl Hand<JokerCard> {
        pub fn kind(&self) -> HandKind {
            // Special-case 4 or more jokers because those can always be made to
            // be FiveOfAKind, and 13^4 is quite big.
            let joker_count = self
                .cards
                .into_iter()
                .filter(|card| card.is_joker())
                .count();
            if joker_count >= 4 {
                return HandKind::FiveOfAKind;
            }

            let [c1, c2, c3, c4, c5] = self.cards;

            let mut best: Option<HandKind> = None;

            for c1 in c1.alternatives() {
                for c2 in c2.alternatives() {
                    for c3 in c3.alternatives() {
                        for c4 in c4.alternatives() {
                            for c5 in c5.alternatives() {
                                let new_hand = Hand {
                                    cards: [c1, c2, c3, c4, c5],
                                };
                                let kind = new_hand.kind();

                                match best.as_mut() {
                                    Some(best) => {
                                        if kind > *best {
                                            *best = kind;
                                        }
                                    }
                                    None => {
                                        best = Some(kind);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            best.expect("set to Some in first iteration")
        }
    }

    impl PartialOrd for Hand<Card> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for Hand<Card> {
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

    impl PartialOrd for Hand<JokerCard> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for Hand<JokerCard> {
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
    pub struct Game<C> {
        pub hands_and_bids: Vec<(Hand<C>, u32)>,
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

pub fn part_two(input: &str) -> Option<u32> {
    let mut game = parsing::parse_input_part_two(input);

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

mod parsing {
    use super::*;

    use advent_of_code::helpers::parsing::*;

    fn hand<C: TryFrom<u8>>(input: &str) -> IResult<&str, Hand<C>> {
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

    fn game<'a, C>(
        hand_parser: impl FnMut(&'a str) -> IResult<&'a str, Hand<C>>,
    ) -> impl Parser<&'a str, Game<C>, nom::error::Error<&'a str>> {
        let line = ws_tuple((hand_parser, decimal_number));

        line_separated(line).map(|hands_and_bids| Game { hands_and_bids })
    }

    pub fn parse_input(input: &str) -> Game<Card> {
        final_parser(game(hand::<Card>))(input).expect("input should be valid")
    }

    pub fn parse_input_part_two(input: &str) -> Game<JokerCard> {
        final_parser(game(hand::<JokerCard>))(input).expect("input should be valid")
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
        assert_eq!(result, Some(5905));
    }
}
