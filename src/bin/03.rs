use regex::Regex;

advent_of_code::solution!(3);

/// Return an iterator of `(row_idx, col_idx, character)`` tuples for each grid
/// cell in the schematic.
fn schematic_cells(schematic: &str) -> impl Iterator<Item = (usize, usize, u8)> + '_ {
    schematic
        .lines()
        .map(|line| line.as_bytes()) // convert to bytes for byte indexing
        .enumerate() // add row numbers
        .flat_map(|(row, line)| {
            std::iter::repeat(row)
                .zip(line.iter().enumerate())
                .map(|(row, (col, &c))| (row, col, c))
        })
}

fn is_engine_part(c: u8) -> bool {
    c != b'.' && !c.is_ascii_digit()
}

/// Returns `(row, col, symbol)` tuples for each the engine parts in the input.
fn engine_parts(schematic: &str) -> Vec<(usize, usize, u8)> {
    schematic_cells(schematic)
        .filter(|&(_row, _col, c)| is_engine_part(c))
        .collect()
}

/// A number found in the engine schematic
#[derive(Debug, Clone)]
struct Number {
    /// The parsed value of the number.
    pub value: u32,
    /// The `(row, col)` location where the number starts.
    pub start: (usize, usize),
    /// The length of the number in digits.
    pub len: usize,
}

impl Number {
    pub fn is_adjacent_to(&self, (other_row, other_col): (usize, usize)) -> bool {
        let (row, col) = self.start;

        let is_adjacent_row = row.abs_diff(other_row) <= 1;

        let is_adjacent_left = matches!(col.checked_sub(other_col), Some(0..=1));
        let is_adjacent_under_or_right =
            matches!(other_col.checked_sub(col), Some(n) if n <= self.len);

        is_adjacent_row && (is_adjacent_left || is_adjacent_under_or_right)
    }
}

/// Returns all of the numbers found in the engine schematic.
fn numbers(schematic: &str) -> Vec<Number> {
    let number_re = Regex::new(r"[0-9]+").unwrap();

    schematic
        .lines()
        .enumerate()
        .flat_map(|(row, line)| {
            number_re.find_iter(line).map(move |matchy| {
                let value = matchy
                    .as_str()
                    .parse()
                    .expect("regex guarantees valid decimal number");
                let col = matchy.start();
                let len = matchy.len();

                Number {
                    value,
                    start: (row, col),
                    len,
                }
            })
        })
        .collect()
}

pub fn part_one(input: &str) -> Option<u32> {
    let parts = engine_parts(input);
    let numbers = numbers(input);

    let sum = numbers
        .into_iter()
        .filter(|number| {
            parts
                .iter()
                .any(|&(row, col, _)| number.is_adjacent_to((row, col)))
        })
        .map(|number| number.value)
        .sum();

    Some(sum)
}

fn get_gear_ratio((row, col, c): (usize, usize, u8), numbers: &[Number]) -> Option<u32> {
    if c != b'*' {
        // not a gear; wrong symbol
        return None;
    }

    let adjacent_numbers: Vec<&Number> = numbers
        .iter()
        .filter(|number| number.is_adjacent_to((row, col)))
        .collect();

    let Ok([num1, num2]) = <[&Number; 2]>::try_from(adjacent_numbers) else {
        // not a gear; must be adjacent to exactly two numbers
        return None;
    };

    Some(num1.value * num2.value)
}

pub fn part_two(input: &str) -> Option<u32> {
    let numbers = numbers(input);

    let sum = schematic_cells(input)
        .filter_map(|(row, col, c)| get_gear_ratio((row, col, c), &numbers))
        .sum();

    Some(sum)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(4361));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(467835));
    }
}
