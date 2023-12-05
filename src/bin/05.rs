use std::ops::Range;

advent_of_code::solution!(5);

#[derive(Debug, Clone)]
struct Almanac {
    pub seeds: Vec<usize>,
    pub seed_to_soil_map: RangeMap,
    pub soil_to_fertilizer_map: RangeMap,
    pub fertilizer_to_water_map: RangeMap,
    pub water_to_light_map: RangeMap,
    pub light_to_temperature_map: RangeMap,
    pub temperature_to_humidity_map: RangeMap,
    pub humidity_to_location_map: RangeMap,
}

#[derive(Debug, Clone)]
struct RangeMap {
    pub ranges: Vec<RangePair>,
}

impl RangeMap {
    pub fn map(&self, src: usize) -> usize {
        self.ranges
            .iter()
            .find_map(|range| range.map(src))
            .unwrap_or(src)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RangePair {
    pub src: Range<usize>,
    pub dst: Range<usize>,
}

impl RangePair {
    pub fn new(dst_start: usize, src_start: usize, len: usize) -> Self {
        Self {
            src: src_start..(src_start + len),
            dst: dst_start..(dst_start + len),
        }
    }

    pub fn map(&self, src: usize) -> Option<usize> {
        if self.src.contains(&src) {
            let offset = src - self.src.start;
            let dst = self.dst.start + offset;
            Some(dst)
        } else {
            None
        }
    }
}

fn apply_maps<'a>(mut value: usize, maps: impl IntoIterator<Item = &'a RangeMap>) -> usize {
    for map in maps {
        value = map.map(value);
    }
    value
}

fn map_seed_to_location(almanac: &Almanac, seed: usize) -> usize {
    apply_maps(
        seed,
        [
            &almanac.seed_to_soil_map,
            &almanac.soil_to_fertilizer_map,
            &almanac.fertilizer_to_water_map,
            &almanac.water_to_light_map,
            &almanac.light_to_temperature_map,
            &almanac.temperature_to_humidity_map,
            &almanac.humidity_to_location_map,
        ],
    )
}

pub fn part_one(input: &str) -> Option<usize> {
    let almanac = parsing::parse_input(input);

    let min = almanac
        .seeds
        .iter()
        .map(|&seed| map_seed_to_location(&almanac, seed))
        .min()
        .expect("should be at least one seed");

    Some(min)
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
}

mod parsing {
    use nom::{
        bytes::complete::{tag, take_until},
        character::complete::{char, space0},
        multi::many1,
        sequence::tuple,
        IResult, Parser,
    };

    use advent_of_code::helpers::parsing::{decimal_number, line_separated};

    use super::*;

    fn range_pair(input: &str) -> IResult<&str, RangePair> {
        tuple((
            decimal_number,
            char(' '),
            decimal_number,
            char(' '),
            decimal_number,
        ))
        .map(|(dst_start, _, src_start, _, len)| RangePair::new(dst_start, src_start, len))
        .parse(input)
    }

    fn skip_line(input: &str) -> IResult<&str, ()> {
        let (input, _) = tuple((take_until("\n"), char('\n'))).parse(input)?;

        Ok((input, ()))
    }

    fn range_map(input: &str) -> IResult<&str, RangeMap> {
        let (input, _) = skip_line(input)?;
        line_separated(range_pair)
            .map(|ranges| RangeMap { ranges })
            .parse(input)
    }

    fn seeds(input: &str) -> IResult<&str, Vec<usize>> {
        let numbers = many1(tuple((space0, decimal_number)).map(|(_, n)| n));

        tuple((tag("seeds: "), numbers))
            .map(|(_, seeds)| seeds)
            .parse(input)
    }

    pub fn parse_input(input: &str) -> Almanac {
        let mut sections = input.split("\n\n");

        let (_, seeds) = seeds(sections.next().unwrap()).unwrap();
        let (_, seed_to_soil_map) = range_map(sections.next().unwrap()).unwrap();
        let (_, soil_to_fertilizer_map) = range_map(sections.next().unwrap()).unwrap();
        let (_, fertilizer_to_water_map) = range_map(sections.next().unwrap()).unwrap();
        let (_, water_to_light_map) = range_map(sections.next().unwrap()).unwrap();
        let (_, light_to_temperature_map) = range_map(sections.next().unwrap()).unwrap();
        let (_, temperature_to_humidity_map) = range_map(sections.next().unwrap()).unwrap();
        let (_, humidity_to_location_map) = range_map(sections.next().unwrap()).unwrap();

        Almanac {
            seeds,
            seed_to_soil_map,
            soil_to_fertilizer_map,
            fertilizer_to_water_map,
            water_to_light_map,
            light_to_temperature_map,
            temperature_to_humidity_map,
            humidity_to_location_map,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(35));
    }

    #[test]
    fn test_part_one_manual() {
        // let almanac = Almanac {
        //     seeds: vec![79, 14, 55, 13],
        //     seed_to_soil_map: RangeMap { ranges: () },
        //     soil_to_fertilizer_map: RangeMap {
        //         ranges: vec![RangePair::new(50, 98, 2), RangePair::new(52, 50, 48)],
        //     },
        //     fertilizer_to_water_map: RangeMap {
        //         ranges: vec![RangePair::new()],
        //     },
        //     water_to_light_map: RangeMap {
        //         ranges: vec![RangePair::new()],
        //     },
        //     light_to_temperature_map: RangeMap {
        //         ranges: vec![RangePair::new()],
        //     },
        //     temperature_to_humidity_map: RangeMap {
        //         ranges: vec![RangePair::new()],
        //     },
        //     humidity_to_location_map: RangeMap {
        //         ranges: vec![RangePair::new()],
        //     },
        // };
        // let result = part_one_impl(&almanac);
        // assert_eq!(result, Some(35));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }
}
