use itertools::Itertools;
use crate::common::*;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    get_minimal_fuel(
        &parse_input("src/day7/problem-input.txt"),
        linear_fuel_usage)
}

fn part2() -> u32 {
    get_minimal_fuel(
        &parse_input("src/day7/problem-input.txt"),
        increasing_fuel_usage)
}

fn get_minimal_fuel(pos: &Vec<i32>, usage_fn: fn(i32) -> u32) -> u32 {
    let (&min, &max) = pos.iter().minmax().into_option().unwrap_or_else(|| panic!("Cannot parse positions"));
    (min..=max)
        .map(|tgt| pos.iter()
            .map(|&p| usage_fn((p - tgt).abs()))
            .sum())
        .min().unwrap_or_else(|| panic!("Cannot get best result"))
}

fn linear_fuel_usage(dist: i32) -> u32 {
    // Fuel usage equivalent to distance travelled
    dist as u32
}

fn increasing_fuel_usage(dist: i32) -> u32 {
    // Fuel usage increases with distance; equivalent to triangular number series, which can be calculated as (n^2 + n) / 2
    (((dist * dist) + dist) / 2) as u32
}


fn parse_input(file: &str) -> Vec<i32> {
    read_file(file)
        .split(',')
        .map(|s| s.parse::<i32>().unwrap_or_else(|_| panic!("Invalid input")))
        .collect()
}


#[cfg(test)]
mod test {
    use crate::day7::{get_minimal_fuel, increasing_fuel_usage, linear_fuel_usage, parse_input};

    #[test]
    fn test_minimal_linear_fuel() {
        assert_eq!(get_minimal_fuel(&parse_input("src/day7/test-input-1.txt"), linear_fuel_usage), 37);
    }

    #[test]
    fn test_minimal_increasing_fuel() {
        assert_eq!(get_minimal_fuel(&parse_input("src/day7/test-input-1.txt"), increasing_fuel_usage), 168);
    }

}

