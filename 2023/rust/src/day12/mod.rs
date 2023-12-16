use std::iter::Iterator;
use itertools::Itertools;
use super::common;

pub fn run() {
    println!("Part 1 result: {}", part1());
    println!("Part 2 result: {}", part2());
}

fn part1() -> usize {
    12
}

fn part2() -> usize {
    12
}

fn parse_input(file: &str) -> usize {
    Grid::new_from_2d_data(
        &common::read_file(file)
            .lines()
            .map(|s| s.trim().chars().collect_vec())
            .collect_vec())
}

#[cfg(test)]
mod tests {
    use crate::day12::{part1, part2};

    #[test]
    fn test_part1() {
        assert_eq!(part1(), 12);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(), 12);
    }

}
