mod data;

use std::iter::Iterator;
use itertools::Itertools;
use crate::day5::data::*;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    let data = parse_input("src/day5/problem-input.txt");
    find_minimum_seed_location(&data.seeds, &data)
}

fn part2() -> usize {
    let data = parse_input("src/day5/problem-input.txt");
    find_lowest_location_in_expanded_sets(&data)
}

fn find_minimum_seed_location(seeds: &Vec<usize>, data: &Data) -> usize {
    seeds.iter()
        .map(|&x| map_seed_to_location(x, &data))
        .min()
        .unwrap_or_else(|| panic!("Could not find minimum location"))
}

fn map_seed_to_location(seed: usize, data: &Data) -> usize {
    Some(seed)
        .map(|x| data.seed_to_soil.map(x))
        .map(|x| data.soil_to_fert.map(x))
        .map(|x| data.fert_to_water.map(x))
        .map(|x| data.water_to_light.map(x))
        .map(|x| data.light_to_temp.map(x))
        .map(|x| data.temp_to_humid.map(x))
        .map(|x| data.humid_to_location.map(x))
        .unwrap_or_else(|| panic!("Failed to map value"))
}

fn expand_seed_data(seeds: &Vec<usize>) -> Vec<(usize, usize)> {
    seeds.chunks(2)
        .map(|chunk| (chunk[0], chunk[0] + chunk[1]))
        .collect_vec()
}

fn find_lowest_location_in_expanded_sets(data: &Data) -> usize {
    let seeds = expand_seed_data(&data.seeds);
    (0usize..)
        .map(|loc| (loc, reverse_map_to_seed(loc, data)))
        .filter(|(_, seed)| seeds.iter().find(|&s| seed >= &s.0 && seed <= &s.1).is_some())
        .next()
        .unwrap_or_else(|| panic!("No solutions")).0
}

fn reverse_map_to_seed(location: usize, data: &Data) -> usize {
    Some(location)
        .map(|x| data.humid_to_location.reverse_map(x))
        .map(|x| data.temp_to_humid.reverse_map(x))
        .map(|x| data.light_to_temp.reverse_map(x))
        .map(|x| data.water_to_light.reverse_map(x))
        .map(|x| data.fert_to_water.reverse_map(x))
        .map(|x| data.soil_to_fert.reverse_map(x))
        .map(|x| data.seed_to_soil.reverse_map(x))
        .unwrap_or_else(|| panic!("Failed to map value"))
}

fn parse_input(file: &str) -> Data {
    let content =  common::read_file(file).to_string();
    let mut lines = content.lines();

    let mut data = Data::new(lines.next().unwrap()
                                 .split_once(':').unwrap().1
                                 .split_ascii_whitespace()
                                 .map(|x| x.parse::<usize>().unwrap())
                                 .collect_vec());

    let mut coll_ix = -1i32;
    for line in lines {
        if line.trim().is_empty() {
            continue;
        }
        else if line.ends_with(':') {
            coll_ix += 1;
        }
        else {
            let values = line.split_ascii_whitespace()
                .map(|x| x.parse::<usize>().unwrap_or_else(|_| panic!("Invalid non-numeric mapping value")))
                .collect_tuple::<(usize, usize, usize)>()
                .unwrap_or_else(|| panic!("Could not parse mapping line '{}'", line));

            data.get_data_collection_mut(coll_ix as usize).add(Mapping::new(values.1, values.0, values.2));
        }
    }

    data
}

#[cfg(test)]
mod tests {
    use super::{part1, part2, parse_input, map_seed_to_location, find_minimum_seed_location, expand_seed_data, find_lowest_location_in_expanded_sets};

    #[test]
    fn test_location_mapping() {
        let data = parse_input("src/day5/test-input-1.txt");

        assert_eq!(map_seed_to_location(79, &data), 82);
        assert_eq!(map_seed_to_location(14, &data), 43);
        assert_eq!(map_seed_to_location(55, &data), 86);
        assert_eq!(map_seed_to_location(13, &data), 35);
    }

    #[test]
    fn test_expanded_seed_mapping() {
        let data = parse_input("src/day5/test-input-1.txt");
        assert_eq!(find_lowest_location_in_expanded_sets(&data), 46);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 12);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
