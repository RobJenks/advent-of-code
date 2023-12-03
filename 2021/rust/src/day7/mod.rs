use itertools::Itertools;
use crate::common::*;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> u32 {
    get_minimal_fuel(
        &parse_input("src/day7/problem-input.txt"))
}

fn part2() -> usize {
    12
}

fn get_minimal_fuel(pos: &Vec<i32>) -> u32 {
    let (&min, &max) = pos.iter().minmax().into_option().unwrap_or_else(|| panic!("Cannot parse positions"));
    (min..=max)
        .map(|tgt| pos.iter()
            .map(|&p| (p - tgt).abs())
            .sum::<i32>())
        .min().unwrap_or_else(|| panic!("Cannot get best result")) as u32
}

fn parse_input(file: &str) -> Vec<i32> {
    read_file(file)
        .split(',')
        .map(|s| s.parse::<i32>().unwrap_or_else(|_| panic!("Invalid input")))
        .collect()
}


#[cfg(test)]
mod test {
    use crate::day7::{get_minimal_fuel, parse_input };

    #[test]
    fn test_minimal_fuel() {
        assert_eq!(get_minimal_fuel(&parse_input("src/day7/test-input-1.txt")), 37);
    }

}

