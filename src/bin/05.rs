use std::ops::Range;

use advent_of_code::debugln;
use itertools::Itertools;

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

impl Almanac {
    pub fn seed_ranges(&self) -> impl Iterator<Item = Range<usize>> + '_ {
        self.seeds
            .iter()
            .tuples()
            .map(|(&start, &len)| start..(start + len))
    }
}

// In a child module to enforce hiding the private field and maintaining the invariant.
mod range_map {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct RangeMap {
        // Invariant: sorted by pair.src.start ascending
        ranges: Vec<RangePair>,
    }

    impl RangeMap {
        pub fn new(mut ranges: Vec<RangePair>) -> Self {
            ranges.sort_unstable_by_key(|range| range.src.start);
            Self { ranges }
        }

        pub fn ranges(&self) -> &[RangePair] {
            &self.ranges
        }
    }
}
use range_map::RangeMap;

impl RangeMap {
    pub fn map(&self, src: usize) -> usize {
        self.ranges()
            .iter()
            .find_map(|range| range.map(src))
            .unwrap_or(src)
    }

    pub fn map_range(&self, mut input: Range<usize>) -> Vec<Range<usize>> {
        debugln!("=== map_range(self={:?}, {input:?}) ===", self as *const _);

        #[cfg(debug_assertions)]
        let original_input = input.clone();

        let overlapping = self
            .ranges()
            .iter()
            .filter(|range_pair| ranges_overlap(&range_pair.src, &input))
            .collect_vec();

        debugln!("overlapping: {overlapping:#?}");

        if overlapping.is_empty() {
            return vec![input];
        }

        let mut outputs = Vec::new();

        let mut push = |range: Range<usize>| {
            debugln!("pushing {range:?}");
            outputs.push(range);
        };

        for pair @ RangePair { src, .. } in overlapping.iter() {
            debugln!("loop: pair = {pair:?}");

            // Catch input up to the current range pair in the mapping,
            // inserting a no-op mapping for the gap.
            if input.start < src.start {
                push(Range {
                    start: input.start,
                    end: src.start,
                });
                input.start = src.start;
                debugln!("input -> {input:?}");
            }

            let overlap_end = std::cmp::min(input.end, src.end);

            debugln!("overlap_end = {overlap_end}");

            let mapped_first = pair.map(input.start).expect("must be in range");
            let mapped_last = pair.map(overlap_end - 1).expect("must be in range");
            let mapped = Range {
                start: mapped_first,
                end: mapped_last + 1,
            };
            push(mapped);

            input.start = overlap_end;
            debugln!("input -> {input:?}");
        }

        let last_range = &overlapping.last().unwrap().src;
        if last_range.end < input.end {
            push(Range {
                start: last_range.end,
                end: input.end,
            });
        }

        #[cfg(debug_assertions)]
        {
            let input_size = original_input.len();
            let final_size = outputs.iter().map(|range| range.len()).sum::<usize>();
            assert_eq!(input_size, final_size);
        }

        outputs
    }
}

fn ranges_overlap(a: &Range<usize>, b: &Range<usize>) -> bool {
    a.start < b.end && a.end > b.start
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

fn apply_maps_to_range<'a>(
    range: Range<usize>,
    maps: impl IntoIterator<Item = &'a RangeMap>,
) -> Vec<Range<usize>> {
    let mut ranges = vec![range];

    for map in maps {
        let next_ranges = ranges
            .iter()
            .flat_map(|range| map.map_range(range.clone()))
            .collect();
        ranges = next_ranges;
    }

    ranges
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

fn map_seed_range_to_location_ranges(
    almanac: &Almanac,
    seed_range: Range<usize>,
) -> Vec<Range<usize>> {
    apply_maps_to_range(
        seed_range,
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

pub fn part_two(input: &str) -> Option<usize> {
    let almanac = parsing::parse_input(input);

    let min = almanac
        .seed_ranges()
        .flat_map(|seed_range| map_seed_range_to_location_ranges(&almanac, seed_range))
        .map(|range| range.start)
        .min()
        .expect("should be at least one seed");

    Some(min)
}

mod parsing {
    use advent_of_code::helpers::parsing::*;

    use super::*;

    fn range_pair(input: &str) -> IResult<&str, RangePair> {
        ws_tuple((decimal_number, decimal_number, decimal_number))
            .map(|(dst_start, src_start, len)| RangePair::new(dst_start, src_start, len))
            .parse(input)
    }

    fn skip_line(input: &str) -> IResult<&str, ()> {
        let (input, _) = tuple((take_until("\n"), char('\n'))).parse(input)?;

        Ok((input, ()))
    }

    fn range_map(input: &str) -> IResult<&str, RangeMap> {
        let (input, _) = skip_line(input)?;
        line_separated(range_pair).map(RangeMap::new).parse(input)
    }

    fn seeds(input: &str) -> IResult<&str, Vec<usize>> {
        let numbers = many1(preceded(space0, decimal_number));

        preceded(tag("seeds: "), numbers).parse(input)
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
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(46));
    }
}
