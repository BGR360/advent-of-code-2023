use advent_of_code::debugln;

advent_of_code::solution!(2);

#[derive(Debug, Clone)]
struct Game {
    pub id: u32,
    pub rounds: Vec<Round>,
}

#[derive(Debug, Clone)]
struct Round {
    pub cubes_revealed: Vec<(u32, Color)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Color {
    Red,
    Green,
    Blue,
}

fn round_valid_for_part_one(round: &Round) -> bool {
    round
        .cubes_revealed
        .iter()
        .all(|&(count, color)| match color {
            Color::Red => count <= 12,
            Color::Green => count <= 13,
            Color::Blue => count <= 14,
        })
}

fn game_valid_for_part_one(game: &Game) -> bool {
    game.rounds.iter().all(round_valid_for_part_one)
}

pub fn part_one(input: &str) -> Option<u32> {
    let games = parsing::parse_input(input);

    let sum = games
        .into_iter()
        .filter(game_valid_for_part_one)
        .map(|game| game.id)
        .sum();

    Some(sum)
}

/// Returns the largest count of the given color that was observed in any round
/// of the given game.
fn minimum_count_required_for_color(game: &Game, target_color: Color) -> u32 {
    game.rounds
        .iter()
        .flat_map(|round| round.cubes_revealed.iter())
        .filter_map(|&(count, color)| {
            if color == target_color {
                Some(count)
            } else {
                None
            }
        })
        .max()
        .unwrap_or(0)
}

fn power_of_game(game: &Game) -> u32 {
    debugln!("{game:?}");

    let r = minimum_count_required_for_color(game, Color::Red);
    let g = minimum_count_required_for_color(game, Color::Green);
    let b = minimum_count_required_for_color(game, Color::Blue);

    debugln!("[r={r}, g={g}, b={b}]");

    r.checked_mul(g)
        .and_then(|rg| rg.checked_mul(b))
        .expect("integer overflow")
}

pub fn part_two(input: &str) -> Option<u32> {
    let games = parsing::parse_input(input);

    let sum = games
        .into_iter()
        .map(|game| power_of_game(&game))
        .reduce(|a, b| a.checked_add(b).expect("integer overflow"))
        .expect("there is at least one game");

    Some(sum)
}

mod parsing {
    use super::*;

    use advent_of_code::helpers::parsing::*;

    fn color(input: &str) -> IResult<&str, Color> {
        alt((
            tag("red").map(|_| Color::Red),
            tag("green").map(|_| Color::Green),
            tag("blue").map(|_| Color::Blue),
        ))(input)
    }

    /// Parses a round like:
    ///
    /// ```txt
    /// 3 red
    /// ```
    ///
    /// ```txt
    /// 3 green, 4 blue
    /// ```
    fn round(input: &str) -> IResult<&str, Round> {
        let count_and_color = ws_tuple((decimal_number, color));

        separated_list1(tag(", "), count_and_color)
            .map(|cubes_revealed| Round { cubes_revealed })
            .parse(input)
    }

    /// Parses a game like:
    ///
    /// ```txt
    /// Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    /// ```
    fn game(input: &str) -> IResult<&str, Game> {
        let id = tuple((tag("Game "), decimal_number, tag(": "))).map(|(_, id, _)| id);
        let rounds = separated_list1(tag("; "), round);

        tuple((id, rounds))
            .map(|(id, rounds)| Game { id, rounds })
            .parse(input)
    }

    /// Parses a list of games from the puzzle input.
    pub fn parse_input(input: &str) -> Vec<Game> {
        final_parser(line_separated(game))(input).expect("input should be valid")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(8));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(2286));
    }
}
